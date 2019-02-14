from aetherling.modules.reduce import DefineReduceSequential, DefineReduceParallel, renameCircuitForReduce
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.noop import DefineNoop
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle.coreir.arith import *
from mantle.coreir.logic import *
from mantle.coreir.compare import *
from mantle.coreir import DefineCoreirConst
from mantle.coreir.LUT import *
from aetherling.modules.upsample import *
from aetherling.modules.downsample import *
from aetherling.modules.reduce import *
from aetherling.modules.native_linebuffer.two_dimensional_native_linebuffer import DefineTwoDimensionalLineBuffer

c = coreir.Context()
cirb = CoreIRBackend(c)

args = ['I0', Array(8, In(Bit)), 'I1', Array(8, In(Bit)), 'I2', Array(8, In(Bit)), 'I3', Array(8, In(Bit)), 'I4', Array(8, In(Bit)), 'I5', Array(8, In(Bit)), 'I6', Array(8, In(Bit)), 'I7', Array(8, In(Bit)), 'I8', Array(8, In(Bit)), 'I9', Array(8, In(Bit)), 'I10', Array(8, In(Bit)), 'I11', Array(8, In(Bit)), 'I12', Array(8, In(Bit)), 'I13', Array(8, In(Bit)), 'I14', Array(8, In(Bit)), 'I15', Array(8, In(Bit)), 'O0', Array(8, Out(Bit)), 'O1', Array(8, Out(Bit)), 'O2', Array(8, Out(Bit)), 'O3', Array(8, Out(Bit)), 'O4', Array(8, Out(Bit)), 'O5', Array(8, Out(Bit)), 'O6', Array(8, Out(Bit)), 'O7', Array(8, Out(Bit)), 'O8', Array(8, Out(Bit)), 'O9', Array(8, Out(Bit)), 'O10', Array(8, Out(Bit)), 'O11', Array(8, Out(Bit)), 'O12', Array(8, Out(Bit)), 'O13', Array(8, Out(Bit)), 'O14', Array(8, Out(Bit)), 'O15', Array(8, Out(Bit)), 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
partialParallel16Convolution = DefineCircuit('partialParallel16Convolution_Circuit', *args)
magmaInstance0 = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 8, 2, 2, 2, 8, 8, 1, 1, 0, 0)()
magmaInstance1 = DefineCoreirConst(8, 1)()
magmaInstance2 = DefineCoreirConst(8, 1)()
magmaInstance3 = DefineCoreirConst(8, 1)()
magmaInstance4 = DefineCoreirConst(8, 1)()
magmaInstance5 = DefineCoreirConst(8, 1)()
magmaInstance6 = DefineCoreirConst(8, 1)()
magmaInstance7 = DefineCoreirConst(8, 1)()
magmaInstance8 = DefineCoreirConst(8, 1)()
magmaInstance9 = DefineCoreirConst(8, 1)()
magmaInstance10 = DefineCoreirConst(8, 1)()
magmaInstance11 = DefineCoreirConst(8, 1)()
magmaInstance12 = DefineCoreirConst(8, 1)()
magmaInstance13 = DefineCoreirConst(8, 1)()
magmaInstance14 = DefineCoreirConst(8, 1)()
magmaInstance15 = DefineCoreirConst(8, 1)()
magmaInstance16 = DefineCoreirConst(8, 1)()
magmaInstance17 = DefineCoreirConst(8, 2)()
magmaInstance18 = DefineCoreirConst(8, 2)()
magmaInstance19 = DefineCoreirConst(8, 2)()
magmaInstance20 = DefineCoreirConst(8, 2)()
magmaInstance21 = DefineCoreirConst(8, 2)()
magmaInstance22 = DefineCoreirConst(8, 2)()
magmaInstance23 = DefineCoreirConst(8, 2)()
magmaInstance24 = DefineCoreirConst(8, 2)()
magmaInstance25 = DefineCoreirConst(8, 2)()
magmaInstance26 = DefineCoreirConst(8, 2)()
magmaInstance27 = DefineCoreirConst(8, 2)()
magmaInstance28 = DefineCoreirConst(8, 2)()
magmaInstance29 = DefineCoreirConst(8, 2)()
magmaInstance30 = DefineCoreirConst(8, 2)()
magmaInstance31 = DefineCoreirConst(8, 2)()
magmaInstance32 = DefineCoreirConst(8, 2)()
magmaInstance48 = DefineCoreirConst(8, 2)()
magmaInstance49 = DefineCoreirConst(8, 2)()
magmaInstance50 = DefineCoreirConst(8, 2)()
magmaInstance51 = DefineCoreirConst(8, 2)()
magmaInstance52 = DefineCoreirConst(8, 2)()
magmaInstance53 = DefineCoreirConst(8, 2)()
magmaInstance54 = DefineCoreirConst(8, 2)()
magmaInstance55 = DefineCoreirConst(8, 2)()
magmaInstance56 = DefineCoreirConst(8, 2)()
magmaInstance57 = DefineCoreirConst(8, 2)()
magmaInstance58 = DefineCoreirConst(8, 2)()
magmaInstance59 = DefineCoreirConst(8, 2)()
magmaInstance60 = DefineCoreirConst(8, 2)()
magmaInstance61 = DefineCoreirConst(8, 2)()
magmaInstance62 = DefineCoreirConst(8, 2)()
magmaInstance63 = DefineCoreirConst(8, 2)()
magmaInstance64 = DefineCoreirConst(8, 1)()
magmaInstance65 = DefineCoreirConst(8, 1)()
magmaInstance66 = DefineCoreirConst(8, 1)()
magmaInstance67 = DefineCoreirConst(8, 1)()
magmaInstance68 = DefineCoreirConst(8, 1)()
magmaInstance69 = DefineCoreirConst(8, 1)()
magmaInstance70 = DefineCoreirConst(8, 1)()
magmaInstance71 = DefineCoreirConst(8, 1)()
magmaInstance72 = DefineCoreirConst(8, 1)()
magmaInstance73 = DefineCoreirConst(8, 1)()
magmaInstance74 = DefineCoreirConst(8, 1)()
magmaInstance75 = DefineCoreirConst(8, 1)()
magmaInstance76 = DefineCoreirConst(8, 1)()
magmaInstance77 = DefineCoreirConst(8, 1)()
magmaInstance78 = DefineCoreirConst(8, 1)()
magmaInstance79 = DefineCoreirConst(8, 1)()
magmaInstance125 = DefineCoreirMul(8)()
magmaInstance126 = DefineCoreirMul(8)()
magmaInstance127 = DefineCoreirMul(8)()
magmaInstance128 = DefineCoreirMul(8)()
magmaInstance129 = DefineCoreirMul(8)()
magmaInstance130 = DefineCoreirMul(8)()
magmaInstance131 = DefineCoreirMul(8)()
magmaInstance132 = DefineCoreirMul(8)()
magmaInstance133 = DefineCoreirMul(8)()
magmaInstance134 = DefineCoreirMul(8)()
magmaInstance135 = DefineCoreirMul(8)()
magmaInstance136 = DefineCoreirMul(8)()
magmaInstance137 = DefineCoreirMul(8)()
magmaInstance138 = DefineCoreirMul(8)()
magmaInstance139 = DefineCoreirMul(8)()
magmaInstance140 = DefineCoreirMul(8)()
magmaInstance141 = DefineCoreirMul(8)()
magmaInstance142 = DefineCoreirMul(8)()
magmaInstance143 = DefineCoreirMul(8)()
magmaInstance144 = DefineCoreirMul(8)()
magmaInstance145 = DefineCoreirMul(8)()
magmaInstance146 = DefineCoreirMul(8)()
magmaInstance147 = DefineCoreirMul(8)()
magmaInstance148 = DefineCoreirMul(8)()
magmaInstance149 = DefineCoreirMul(8)()
magmaInstance150 = DefineCoreirMul(8)()
magmaInstance151 = DefineCoreirMul(8)()
magmaInstance152 = DefineCoreirMul(8)()
magmaInstance153 = DefineCoreirMul(8)()
magmaInstance154 = DefineCoreirMul(8)()
magmaInstance155 = DefineCoreirMul(8)()
magmaInstance156 = DefineCoreirMul(8)()
magmaInstance157 = DefineCoreirMul(8)()
magmaInstance158 = DefineCoreirMul(8)()
magmaInstance159 = DefineCoreirMul(8)()
magmaInstance160 = DefineCoreirMul(8)()
magmaInstance161 = DefineCoreirMul(8)()
magmaInstance162 = DefineCoreirMul(8)()
magmaInstance163 = DefineCoreirMul(8)()
magmaInstance164 = DefineCoreirMul(8)()
magmaInstance165 = DefineCoreirMul(8)()
magmaInstance166 = DefineCoreirMul(8)()
magmaInstance167 = DefineCoreirMul(8)()
magmaInstance168 = DefineCoreirMul(8)()
magmaInstance169 = DefineCoreirMul(8)()
magmaInstance170 = DefineCoreirMul(8)()
magmaInstance171 = DefineCoreirMul(8)()
magmaInstance172 = DefineCoreirMul(8)()
magmaInstance173 = DefineCoreirMul(8)()
magmaInstance174 = DefineCoreirMul(8)()
magmaInstance175 = DefineCoreirMul(8)()
magmaInstance176 = DefineCoreirMul(8)()
magmaInstance177 = DefineCoreirMul(8)()
magmaInstance178 = DefineCoreirMul(8)()
magmaInstance179 = DefineCoreirMul(8)()
magmaInstance180 = DefineCoreirMul(8)()
magmaInstance181 = DefineCoreirMul(8)()
magmaInstance182 = DefineCoreirMul(8)()
magmaInstance183 = DefineCoreirMul(8)()
magmaInstance184 = DefineCoreirMul(8)()
magmaInstance185 = DefineCoreirMul(8)()
magmaInstance186 = DefineCoreirMul(8)()
magmaInstance187 = DefineCoreirMul(8)()
magmaInstance188 = DefineCoreirMul(8)()
wire(magmaInstance0.O[0][0][0], magmaInstance125.I0)
wire(magmaInstance1.O, magmaInstance125.I1)
wire(magmaInstance0.O[0][0][1], magmaInstance126.I0)
wire(magmaInstance17.O, magmaInstance126.I1)
wire(magmaInstance0.O[0][1][0], magmaInstance127.I0)
wire(magmaInstance48.O, magmaInstance127.I1)
wire(magmaInstance0.O[0][1][1], magmaInstance128.I0)
wire(magmaInstance64.O, magmaInstance128.I1)
wire(magmaInstance0.O[1][0][0], magmaInstance129.I0)
wire(magmaInstance2.O, magmaInstance129.I1)
wire(magmaInstance0.O[1][0][1], magmaInstance130.I0)
wire(magmaInstance18.O, magmaInstance130.I1)
wire(magmaInstance0.O[1][1][0], magmaInstance131.I0)
wire(magmaInstance49.O, magmaInstance131.I1)
wire(magmaInstance0.O[1][1][1], magmaInstance132.I0)
wire(magmaInstance65.O, magmaInstance132.I1)
wire(magmaInstance0.O[2][0][0], magmaInstance133.I0)
wire(magmaInstance3.O, magmaInstance133.I1)
wire(magmaInstance0.O[2][0][1], magmaInstance134.I0)
wire(magmaInstance19.O, magmaInstance134.I1)
wire(magmaInstance0.O[2][1][0], magmaInstance135.I0)
wire(magmaInstance50.O, magmaInstance135.I1)
wire(magmaInstance0.O[2][1][1], magmaInstance136.I0)
wire(magmaInstance66.O, magmaInstance136.I1)
wire(magmaInstance0.O[3][0][0], magmaInstance137.I0)
wire(magmaInstance4.O, magmaInstance137.I1)
wire(magmaInstance0.O[3][0][1], magmaInstance138.I0)
wire(magmaInstance20.O, magmaInstance138.I1)
wire(magmaInstance0.O[3][1][0], magmaInstance139.I0)
wire(magmaInstance51.O, magmaInstance139.I1)
wire(magmaInstance0.O[3][1][1], magmaInstance140.I0)
wire(magmaInstance67.O, magmaInstance140.I1)
wire(magmaInstance0.O[4][0][0], magmaInstance141.I0)
wire(magmaInstance5.O, magmaInstance141.I1)
wire(magmaInstance0.O[4][0][1], magmaInstance142.I0)
wire(magmaInstance21.O, magmaInstance142.I1)
wire(magmaInstance0.O[4][1][0], magmaInstance143.I0)
wire(magmaInstance52.O, magmaInstance143.I1)
wire(magmaInstance0.O[4][1][1], magmaInstance144.I0)
wire(magmaInstance68.O, magmaInstance144.I1)
wire(magmaInstance0.O[5][0][0], magmaInstance145.I0)
wire(magmaInstance6.O, magmaInstance145.I1)
wire(magmaInstance0.O[5][0][1], magmaInstance146.I0)
wire(magmaInstance22.O, magmaInstance146.I1)
wire(magmaInstance0.O[5][1][0], magmaInstance147.I0)
wire(magmaInstance53.O, magmaInstance147.I1)
wire(magmaInstance0.O[5][1][1], magmaInstance148.I0)
wire(magmaInstance69.O, magmaInstance148.I1)
wire(magmaInstance0.O[6][0][0], magmaInstance149.I0)
wire(magmaInstance7.O, magmaInstance149.I1)
wire(magmaInstance0.O[6][0][1], magmaInstance150.I0)
wire(magmaInstance23.O, magmaInstance150.I1)
wire(magmaInstance0.O[6][1][0], magmaInstance151.I0)
wire(magmaInstance54.O, magmaInstance151.I1)
wire(magmaInstance0.O[6][1][1], magmaInstance152.I0)
wire(magmaInstance70.O, magmaInstance152.I1)
wire(magmaInstance0.O[7][0][0], magmaInstance153.I0)
wire(magmaInstance8.O, magmaInstance153.I1)
wire(magmaInstance0.O[7][0][1], magmaInstance154.I0)
wire(magmaInstance24.O, magmaInstance154.I1)
wire(magmaInstance0.O[7][1][0], magmaInstance155.I0)
wire(magmaInstance55.O, magmaInstance155.I1)
wire(magmaInstance0.O[7][1][1], magmaInstance156.I0)
wire(magmaInstance71.O, magmaInstance156.I1)
wire(magmaInstance0.O[8][0][0], magmaInstance157.I0)
wire(magmaInstance9.O, magmaInstance157.I1)
wire(magmaInstance0.O[8][0][1], magmaInstance158.I0)
wire(magmaInstance25.O, magmaInstance158.I1)
wire(magmaInstance0.O[8][1][0], magmaInstance159.I0)
wire(magmaInstance56.O, magmaInstance159.I1)
wire(magmaInstance0.O[8][1][1], magmaInstance160.I0)
wire(magmaInstance72.O, magmaInstance160.I1)
wire(magmaInstance0.O[9][0][0], magmaInstance161.I0)
wire(magmaInstance10.O, magmaInstance161.I1)
wire(magmaInstance0.O[9][0][1], magmaInstance162.I0)
wire(magmaInstance26.O, magmaInstance162.I1)
wire(magmaInstance0.O[9][1][0], magmaInstance163.I0)
wire(magmaInstance57.O, magmaInstance163.I1)
wire(magmaInstance0.O[9][1][1], magmaInstance164.I0)
wire(magmaInstance73.O, magmaInstance164.I1)
wire(magmaInstance0.O[10][0][0], magmaInstance165.I0)
wire(magmaInstance11.O, magmaInstance165.I1)
wire(magmaInstance0.O[10][0][1], magmaInstance166.I0)
wire(magmaInstance27.O, magmaInstance166.I1)
wire(magmaInstance0.O[10][1][0], magmaInstance167.I0)
wire(magmaInstance58.O, magmaInstance167.I1)
wire(magmaInstance0.O[10][1][1], magmaInstance168.I0)
wire(magmaInstance74.O, magmaInstance168.I1)
wire(magmaInstance0.O[11][0][0], magmaInstance169.I0)
wire(magmaInstance12.O, magmaInstance169.I1)
wire(magmaInstance0.O[11][0][1], magmaInstance170.I0)
wire(magmaInstance28.O, magmaInstance170.I1)
wire(magmaInstance0.O[11][1][0], magmaInstance171.I0)
wire(magmaInstance59.O, magmaInstance171.I1)
wire(magmaInstance0.O[11][1][1], magmaInstance172.I0)
wire(magmaInstance75.O, magmaInstance172.I1)
wire(magmaInstance0.O[12][0][0], magmaInstance173.I0)
wire(magmaInstance13.O, magmaInstance173.I1)
wire(magmaInstance0.O[12][0][1], magmaInstance174.I0)
wire(magmaInstance29.O, magmaInstance174.I1)
wire(magmaInstance0.O[12][1][0], magmaInstance175.I0)
wire(magmaInstance60.O, magmaInstance175.I1)
wire(magmaInstance0.O[12][1][1], magmaInstance176.I0)
wire(magmaInstance76.O, magmaInstance176.I1)
wire(magmaInstance0.O[13][0][0], magmaInstance177.I0)
wire(magmaInstance14.O, magmaInstance177.I1)
wire(magmaInstance0.O[13][0][1], magmaInstance178.I0)
wire(magmaInstance30.O, magmaInstance178.I1)
wire(magmaInstance0.O[13][1][0], magmaInstance179.I0)
wire(magmaInstance61.O, magmaInstance179.I1)
wire(magmaInstance0.O[13][1][1], magmaInstance180.I0)
wire(magmaInstance77.O, magmaInstance180.I1)
wire(magmaInstance0.O[14][0][0], magmaInstance181.I0)
wire(magmaInstance15.O, magmaInstance181.I1)
wire(magmaInstance0.O[14][0][1], magmaInstance182.I0)
wire(magmaInstance31.O, magmaInstance182.I1)
wire(magmaInstance0.O[14][1][0], magmaInstance183.I0)
wire(magmaInstance62.O, magmaInstance183.I1)
wire(magmaInstance0.O[14][1][1], magmaInstance184.I0)
wire(magmaInstance78.O, magmaInstance184.I1)
wire(magmaInstance0.O[15][0][0], magmaInstance185.I0)
wire(magmaInstance16.O, magmaInstance185.I1)
wire(magmaInstance0.O[15][0][1], magmaInstance186.I0)
wire(magmaInstance32.O, magmaInstance186.I1)
wire(magmaInstance0.O[15][1][0], magmaInstance187.I0)
wire(magmaInstance63.O, magmaInstance187.I1)
wire(magmaInstance0.O[15][1][1], magmaInstance188.I0)
wire(magmaInstance79.O, magmaInstance188.I1)
magmaInstance189 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance190 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance191 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance192 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance193 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance194 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance195 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance196 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance197 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance198 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance199 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance200 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance201 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance202 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance203 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance204 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance189_identityGen = DefineCoreirConst(8, 0)()
wire(magmaInstance189_identityGen.O, magmaInstance189.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance190.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance191.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance192.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance193.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance194.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance195.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance196.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance197.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance198.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance199.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance200.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance201.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance202.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance203.I.identity)
wire(magmaInstance189_identityGen.O, magmaInstance204.I.identity)
wire(magmaInstance125.O, magmaInstance189.I.data[0])
wire(magmaInstance126.O, magmaInstance189.I.data[1])
wire(magmaInstance127.O, magmaInstance189.I.data[2])
wire(magmaInstance128.O, magmaInstance189.I.data[3])
wire(magmaInstance129.O, magmaInstance190.I.data[0])
wire(magmaInstance130.O, magmaInstance190.I.data[1])
wire(magmaInstance131.O, magmaInstance190.I.data[2])
wire(magmaInstance132.O, magmaInstance190.I.data[3])
wire(magmaInstance133.O, magmaInstance191.I.data[0])
wire(magmaInstance134.O, magmaInstance191.I.data[1])
wire(magmaInstance135.O, magmaInstance191.I.data[2])
wire(magmaInstance136.O, magmaInstance191.I.data[3])
wire(magmaInstance137.O, magmaInstance192.I.data[0])
wire(magmaInstance138.O, magmaInstance192.I.data[1])
wire(magmaInstance139.O, magmaInstance192.I.data[2])
wire(magmaInstance140.O, magmaInstance192.I.data[3])
wire(magmaInstance141.O, magmaInstance193.I.data[0])
wire(magmaInstance142.O, magmaInstance193.I.data[1])
wire(magmaInstance143.O, magmaInstance193.I.data[2])
wire(magmaInstance144.O, magmaInstance193.I.data[3])
wire(magmaInstance145.O, magmaInstance194.I.data[0])
wire(magmaInstance146.O, magmaInstance194.I.data[1])
wire(magmaInstance147.O, magmaInstance194.I.data[2])
wire(magmaInstance148.O, magmaInstance194.I.data[3])
wire(magmaInstance149.O, magmaInstance195.I.data[0])
wire(magmaInstance150.O, magmaInstance195.I.data[1])
wire(magmaInstance151.O, magmaInstance195.I.data[2])
wire(magmaInstance152.O, magmaInstance195.I.data[3])
wire(magmaInstance153.O, magmaInstance196.I.data[0])
wire(magmaInstance154.O, magmaInstance196.I.data[1])
wire(magmaInstance155.O, magmaInstance196.I.data[2])
wire(magmaInstance156.O, magmaInstance196.I.data[3])
wire(magmaInstance157.O, magmaInstance197.I.data[0])
wire(magmaInstance158.O, magmaInstance197.I.data[1])
wire(magmaInstance159.O, magmaInstance197.I.data[2])
wire(magmaInstance160.O, magmaInstance197.I.data[3])
wire(magmaInstance161.O, magmaInstance198.I.data[0])
wire(magmaInstance162.O, magmaInstance198.I.data[1])
wire(magmaInstance163.O, magmaInstance198.I.data[2])
wire(magmaInstance164.O, magmaInstance198.I.data[3])
wire(magmaInstance165.O, magmaInstance199.I.data[0])
wire(magmaInstance166.O, magmaInstance199.I.data[1])
wire(magmaInstance167.O, magmaInstance199.I.data[2])
wire(magmaInstance168.O, magmaInstance199.I.data[3])
wire(magmaInstance169.O, magmaInstance200.I.data[0])
wire(magmaInstance170.O, magmaInstance200.I.data[1])
wire(magmaInstance171.O, magmaInstance200.I.data[2])
wire(magmaInstance172.O, magmaInstance200.I.data[3])
wire(magmaInstance173.O, magmaInstance201.I.data[0])
wire(magmaInstance174.O, magmaInstance201.I.data[1])
wire(magmaInstance175.O, magmaInstance201.I.data[2])
wire(magmaInstance176.O, magmaInstance201.I.data[3])
wire(magmaInstance177.O, magmaInstance202.I.data[0])
wire(magmaInstance178.O, magmaInstance202.I.data[1])
wire(magmaInstance179.O, magmaInstance202.I.data[2])
wire(magmaInstance180.O, magmaInstance202.I.data[3])
wire(magmaInstance181.O, magmaInstance203.I.data[0])
wire(magmaInstance182.O, magmaInstance203.I.data[1])
wire(magmaInstance183.O, magmaInstance203.I.data[2])
wire(magmaInstance184.O, magmaInstance203.I.data[3])
wire(magmaInstance185.O, magmaInstance204.I.data[0])
wire(magmaInstance186.O, magmaInstance204.I.data[1])
wire(magmaInstance187.O, magmaInstance204.I.data[2])
wire(magmaInstance188.O, magmaInstance204.I.data[3])
wire(partialParallel16Convolution.I0, magmaInstance0.I[0][0])
wire(partialParallel16Convolution.I1, magmaInstance0.I[0][1])
wire(partialParallel16Convolution.I2, magmaInstance0.I[0][2])
wire(partialParallel16Convolution.I3, magmaInstance0.I[0][3])
wire(partialParallel16Convolution.I4, magmaInstance0.I[0][4])
wire(partialParallel16Convolution.I5, magmaInstance0.I[0][5])
wire(partialParallel16Convolution.I6, magmaInstance0.I[0][6])
wire(partialParallel16Convolution.I7, magmaInstance0.I[0][7])
wire(partialParallel16Convolution.I8, magmaInstance0.I[1][0])
wire(partialParallel16Convolution.I9, magmaInstance0.I[1][1])
wire(partialParallel16Convolution.I10, magmaInstance0.I[1][2])
wire(partialParallel16Convolution.I11, magmaInstance0.I[1][3])
wire(partialParallel16Convolution.I12, magmaInstance0.I[1][4])
wire(partialParallel16Convolution.I13, magmaInstance0.I[1][5])
wire(partialParallel16Convolution.I14, magmaInstance0.I[1][6])
wire(partialParallel16Convolution.I15, magmaInstance0.I[1][7])
wire(partialParallel16Convolution.O0, magmaInstance189.out)
wire(partialParallel16Convolution.O1, magmaInstance190.out)
wire(partialParallel16Convolution.O2, magmaInstance191.out)
wire(partialParallel16Convolution.O3, magmaInstance192.out)
wire(partialParallel16Convolution.O4, magmaInstance193.out)
wire(partialParallel16Convolution.O5, magmaInstance194.out)
wire(partialParallel16Convolution.O6, magmaInstance195.out)
wire(partialParallel16Convolution.O7, magmaInstance196.out)
wire(partialParallel16Convolution.O8, magmaInstance197.out)
wire(partialParallel16Convolution.O9, magmaInstance198.out)
wire(partialParallel16Convolution.O10, magmaInstance199.out)
wire(partialParallel16Convolution.O11, magmaInstance200.out)
wire(partialParallel16Convolution.O12, magmaInstance201.out)
wire(partialParallel16Convolution.O13, magmaInstance202.out)
wire(partialParallel16Convolution.O14, magmaInstance203.out)
wire(partialParallel16Convolution.O15, magmaInstance204.out)
wire(magmaInstance0.ready, partialParallel16Convolution.ready_data_in)
wire(magmaInstance0.valid, partialParallel16Convolution.valid_data_out)
wire(partialParallel16Convolution.valid_data_in & partialParallel16Convolution.ready_data_out & bit(partialParallel16Convolution.CE), magmaInstance0.CE)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, partialParallel16Convolution.CE)
EndCircuit()
