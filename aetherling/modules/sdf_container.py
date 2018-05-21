from magma import DefineCircuitKind, Type
from typing import List
import numpy as np
from aetherling.helpers.rank_nullspace import nullspace

class SDFElementDefinition(object):
    def __init__(self, circuitDef: DefineCircuitKind, tokensPerFiring: int):
        """
        :param circuitDef: the definition of the circuit
        :param tokensPerFiring: This circuit emits n tokens per firing
        """
        self.circuitDef = circuitDef
        self.tokensPerFiring = tokensPerFiring

class _Edge(object):
    def __init__(self, srcIdx: int, srcPort: Type, dstIdx: int, dstPort: Type):
        self.srcIdx = srcIdx
        self.srcPort = srcPort
        self.dstIdx = dstIdx
        self.dstPort = dstPort

class _Node(object):
    def __init__(self, idx: int, definition: SDFElementDefinition):
        self.idx = idx
        self.definition = definition
        self.instance = definition()

class SDFSolver(object):
    def __init__(self):
        """
        Creates a solver that tracks a graph. Nodes are instances of _Node objects.
        edges are instances of _Edge objects.
        """
        self.nodes = []
        self.edges = []

    def add_node(self, definition: DefineCircuitKind):
        """
        Add an instance of an SDFElementDefinition to the solver's graph
        :param definition: SDFElementDefinition to add an instance of
        :return: the reference to the node
        """
        newIdx = len(self.nodes)
        self.nodes.append(_Node(newIdx, definition.circuit_def))
        return newIdx

    def createEdge(self, srcIdx: int, srcPort: Type, dstIdx: int, dstPort: Type):
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
        # nullspace returns a matrix where each column is a vector in the nullspace
        # take first column to go one element of nullspace
        floatNullVec = nullspace(arcMatrix)[:,0]
        # convert them to ints so get firing rates
        absMin = np.min(floatNullVec)
        firingRatesVec = np.rint(floatNullVec / absMin)

        # connect all. For each









