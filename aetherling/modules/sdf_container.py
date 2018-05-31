from magma import *
from magma.circuit import DefineCircuitKind
from typing import List
import numpy as np
import pandas as pd
from aetherling.helpers.rank_nullspace import nullspace
from fractions import Fraction
import math


class SDFElementDefinition(object):
    def __init__(self, circuitDef: DefineCircuitKind, tokensPerFiring: int):
        """
        :param circuitDef: the definition of the circuit
        :param tokensPerFiring: This circuit emits n tokens per firing
        """
        self.circuitDef = circuitDef
        self.tokensPerFiring = tokensPerFiring

class _Edge(object):
    def __init__(self, srcIdx: int, srcPort: str, dstIdx: int, dstPort: str):
        self.srcIdx = srcIdx
        self.srcPort = srcPort
        self.dstIdx = dstIdx
        self.dstPort = dstPort


    def to_dict(self):
        return {
            'srcIdx': self.srcIdx,
            'srcPort': self.srcPort,
            'dstIdx': self.dstIdx,
            'dstPort': self.dstPort
        }

class _Node(object):
    def __init__(self, idx: int, definition: SDFElementDefinition):
        self.idx = idx
        self.definition = definition
        self.instance = definition.circuitDef()
        self._firingRate = None

    def setDstFiringRate(self, srcNode):
        self._firingRate = srcNode.getFiringRate() / self.definition.tokensPerFiring

    def setFiringRateByLCM(self, lcm: int):
        """
        Given an lcm of all nodes' firing rates, multiply this nodes firing rate by it
        to get an integer firing rate that works in SDF schedule
        :param lcm: The LCM of all nodes' firing rates
        """
        self._firingRate = self._firingRate * lcm
        if self._firingRate.denominator != 1:
            raise Exception("Multiplying by LCM did not get rid of denominator")
        self._firingRate = self._firingRate.numerator

    def isFiringRateSet(self):
        return self._firingRate is None

    def getTokensPerRound(self):
        return self.getFiringRate() * self.definition.tokensPerFiring

    def getFiringRate(self):
        if self._firingRate is None:
            raise Exception("Haven't set firing rate yet for node {}".format(self))
        return self._firingRate

    def __str__(self):
        "_Node(idx={},circuitDef={},tokensPerFiring={},firingRate={}".format(
            self.idx, self.definition.circuitDef, self.definition.tokensPerFiring, self._firingRate)

class SDFSolver(object):
    def __init__(self):
        """
        Creates a solver that tracks a graph. Nodes are instances of _Node objects.
        edges are instances of _Edge objects.
        """
        self.nodes = []
        self.edges = []

    def add_node(self, sdfDefinition: SDFElementDefinition):
        """
        Add an instance of an SDFElementDefinition to the solver's graph
        :param definition: SDFElementDefinition to add an instance of
        :return: the reference to the node
        """
        newIdx = len(self.nodes)
        self.nodes.append(_Node(newIdx, sdfDefinition))
        return newIdx

    def createEdge(self, srcIdx: int, srcPort: str, dstIdx: int, dstPort: str):
        """
        Connect two SDF elements.
        Note: unlike Magma, direction matters here.
        :param srcIdx: The reference to the source node returned from addNode
        :param srcPort: The port of the source node to connect
        :param dstIdx: The reference to the destination node returned from addNode
        :param dstPort: The port of the destination node to connect
        :return:
        """
        self.edges.append(_Edge(srcIdx, srcPort, dstIdx, dstPort))

    def schedule(self):
        """
        Build the SDF matrix, solve for the firing rates, and wire up definitions.
        The prior wire calls using the solver have just been to create metadata.
        This does the actual wiring in Magma
        """
        # build an arc matrix \Gamma
        def createRowFromEdge(e: _Edge, nodes:List[_Node], numNodes: int) -> List[int]:
            srcTokensPerFiring = nodes[e.srcIdx].definition.tokensPerFiring
            dstTokensPerFiring = -1*nodes[e.dstIdx].definition.tokensPerFiring
            if e.srcIdx < e.dstIdx:
                minIdx = e.srcIdx
                minIdxTokensPerFiring = srcTokensPerFiring
                maxIdx = e.dstIdx
                maxIdxTokensPerFiring = dstTokensPerFiring
            else:
                maxIdx = e.srcIdx
                maxIdxTokensPerFiring = srcTokensPerFiring
                minIdx = e.dstIdx
                minIdxTokensPerFiring = dstTokensPerFiring
            return [0]*(minIdx-1) + [minIdxTokensPerFiring] + [0]*(maxIdx - minIdx - 1) \
                   + [maxIdxTokensPerFiring] + [0]*(numNodes - maxIdx - 1)

        numNodes = len(self.nodes)
        arcMatrix = np.asarray([createRowFromEdge(e, self.nodes, numNodes) for e in self.edges])
        if np.linalg.matrix_rank(arcMatrix) != numNodes - 1:
            raise Exception("Unschedulable SDF graph, rank is not numNodes - 1")

        edgesDF = pd.DataFrame.from_records([e.to_dict() for e in self.edges])
        originalEdgesDF = edgesDF
        # nullspace returns a matrix where each column is a vector in the nullspace
        # take first column to go one element of nullspace
        floatNullVec = nullspace(arcMatrix)[:,0]
        # convert them to ints so get firing rates
        minFiring = np.min(floatNullVec)
        firingRatesVec = np.rint(floatNullVec / minFiring)

        # start at top, set firing rates using approach from
        # http://users.ece.utexas.edu/~bevans/courses/ee382c/lectures/08_sdf/sdf2.html
        # do bfs search of node graph, setting firing rates of all nodes before adding them to search
        idxsToScheduleAsSrc = [0]
        self.nodes[0].setFiringRate(Fraction(1,1))
        while len(idxsToScheduleAsSrc) is not 0:
            curSrc = idxsToScheduleAsSrc.pop()
            # note: these are dicts, not original objects, but work well enough here
            edgesToWireThisIteration = edgesDF[edgesDF.srcIdx == curSrc].to_records()
            # only look at edges haven't already handled
            edgesDF = edgesDF[edgesDF.srcIdx != curSrc]
            for e in edgesToWireThisIteration:
                srcNode = self.nodes[e.srcIdx]
                dstNode = self.nodes[e.dstIdx]
                wire(getattr(srcNode, e.srcPort), getattr(dstNode, e.dstPort))
                # either get to set dst rate or verify that it matches src rate
                if dstNode.isFiringRateSet:
                    if srcNode.getTokensPerRound() != dstNode.getTokensPerRound():
                        raise Exception("SDF scheduling failed, got passed rank check but firing rates still failed. \n"
                                        "srcIdx: {}, dstIdx: {} \n"
                                        "srcNode: {}, dstNode: {} \n"
                                        "Edges: {} \n Nodes: {}".format(e.srcIdx, e.dstIdx, srcNode, dstNode,
                                                                        originalEdgesDF, self.nodes))
                else:
                    dstNode.setFiringRate(srcNode)
                    # if this destination has not already had its firing rate determined,
                    # then the edges where its the src have not been scheduled, so add
                    # it to the stack
                    idxsToScheduleAsSrc.append(e.dstIdx)

        # need to get all firing rates to ints, so get gcd of all and divide all by it
        # based on https://stackoverflow.com/questions/37237954/calculate-the-lcm-of-a-list-of-given-numbers-in-python
        def lcm(nodes):
            return reduce(lambda a, b: a * b // math.gcd(a, b), [n.getFiringRate().denominator for n in nodes])

        lcmOfFiringRates = lcm(self.nodes)
        for node in self.nodes:
            node.setFiringRateByLCM(lcmOfFiringRates)

        # now need to got through and add buffers where rates don't match





                        # connect all. For each edge, if the firing rates are the same, wire them directly up
        # else streamify the


# https://stackoverflow.com/questions/49981286/replacement-for-deprecated-fractions-gcd-function
# and https://stackoverflow.com/questions/43888801/gcd-of-list-of-floats-incorrect-output-from-fractions
# def lcm_rationals(a, b):
#     num_gcd = math.gcd(a.numerator, b.numerator)
#     denom_lcm = a.denominator * b.denominator // math.gcd(a.denominator, b.denominator)
#     return Fraction(num_gcd, denom_lcm)
#
#
# def lcm_reduce(L):
#     return reduce(fractions.gcd, map(fractions.Fraction, L))
#








