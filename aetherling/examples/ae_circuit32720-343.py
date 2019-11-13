import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST
import magma as m

@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['I', In(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_Int())).magma_repr()), 'O', Out(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(3, 0, ST_TSeq(3, 0, ST_Int()))]
        st_out_t = ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))
        binary_op = False
        @classmethod
        def definition(cls):
            n136 = DefineConst(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_Int())), (0,1,0,1,2,1,0,1,0,), has_valid=True, delay=3)()
            wire(cls.valid_up, n136.valid_up)
            n138 = DefineMap2_T(3, 0, DefineMap2_T(3, 0, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True)))()
            wire(cls.I, n138.I0)
            wire(n136.O, n138.I1)
            wire(cls.valid_up & n136.valid_down, n138.valid_up)
            n149 = DefineMap_T(3, 0, DefineMap_T(3, 0, DefineLShift_Atom(True)))()
            wire(n138.O, n149.I)
            wire(n138.valid_down, n149.valid_up)
            n154 = DefineMap_T(3, 0, DefineReduce_T(3, 0, DefineAdd_Atom(False)))()
            wire(n149.O, n154.I)
            wire(n149.valid_down, n154.valid_up)
            n159 = DefineReduce_T(3, 0, DefineMap_T(1, 2, DefineAdd_Atom(False)))()
            wire(n154.O, n159.I)
            wire(n154.valid_down, n159.valid_up)
            n160 = DefineConst(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), (4,0,0,0,0,0,0,0,0,), has_valid=True, delay=13)()
            wire(cls.valid_up, n160.valid_up)
            n162 = DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True)))()
            wire(n159.O, n162.I0)
            wire(n160.O, n162.I1)
            wire(n159.valid_down & n160.valid_down, n162.valid_up)
            n173 = DefineMap_T(1, 2, DefineMap_T(1, 2, DefineRShift_Atom(True)))()
            wire(n162.O, n173.I)
            wire(n162.valid_down, n173.valid_up)
            wire(n173.O, cls.O)
            wire(n173.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "top"
        IO = ['hi', In(ST_TSeq(32, 32, ST_SSeq(2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))).magma_repr()), 'O', Out(ST_TSeq(16, 48, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(32, 32, ST_SSeq(2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))))]
        st_out_t = ST_TSeq(16, 48, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))
        IO += ['conv_out', Out(ST_TSeq(32, 32, ST_SSeq(2, ST_TSeq(3, 0, ST_TSeq(3, 0, ST_Int())))).magma_repr())]
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineFIFO(ST_TSeq(32, 32, ST_SSeq(2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))), 1, has_valid=True)()
            wire(cls.hi, n1.I)
            wire(cls.valid_up, n1.valid_up)
            n2 = DefineShift_TS(32, 32, 2, 8, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), has_valid=True)()
            wire(n1.O, n2.I)
            wire(n1.valid_down, n2.valid_up)
            n3 = DefineShift_TS(32, 32, 2, 8, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), has_valid=True)()
            wire(n2.O, n3.I)
            wire(n2.valid_down, n3.valid_up)
            n4 = DefineShift_TS(32, 32, 2, 1, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), has_valid=True)()
            wire(n3.O, n4.I)
            wire(n3.valid_down, n4.valid_up)
            n5 = DefineShift_TS(32, 32, 2, 1, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), has_valid=True)()
            wire(n4.O, n5.I)
            wire(n4.valid_down, n5.valid_up)
            n6 = DefineMap2_T(32, 32, DefineMap2_S(2, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(), has_valid=True))),True))()
            wire(n5.O, n6.I0)
            wire(n4.O, n6.I1)
            wire(n5.valid_down & n4.valid_down, n6.valid_up)
            n19 = DefineMap2_T(32, 32, DefineMap2_S(2, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True))),True))()
            wire(n6.O, n19.I0)
            wire(n3.O, n19.I1)
            wire(n6.valid_down & n3.valid_down, n19.valid_up)
            n38 = DefineMap_T(32, 32, DefineMap_S(2, DefineMap_T(1, 2, DefineSerialize(3, 0, ST_Int())),True))()
            wire(n19.O, n38.I)
            wire(n19.valid_down, n38.valid_up)
            n39 = DefineShift_TS(32, 32, 2, 1, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), has_valid=True)()
            wire(n2.O, n39.I)
            wire(n2.valid_down, n39.valid_up)
            n40 = DefineShift_TS(32, 32, 2, 1, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), has_valid=True)()
            wire(n39.O, n40.I)
            wire(n39.valid_down, n40.valid_up)
            n41 = DefineMap2_T(32, 32, DefineMap2_S(2, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(), has_valid=True))),True))()
            wire(n40.O, n41.I0)
            wire(n39.O, n41.I1)
            wire(n40.valid_down & n39.valid_down, n41.valid_up)
            n54 = DefineMap2_T(32, 32, DefineMap2_S(2, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True))),True))()
            wire(n41.O, n54.I0)
            wire(n2.O, n54.I1)
            wire(n41.valid_down & n2.valid_down, n54.valid_up)
            n73 = DefineMap_T(32, 32, DefineMap_S(2, DefineMap_T(1, 2, DefineSerialize(3, 0, ST_Int())),True))()
            wire(n54.O, n73.I)
            wire(n54.valid_down, n73.valid_up)
            n74 = DefineMap2_T(32, 32, DefineMap2_S(2, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_TSeq(3, 0, ST_Int()), has_valid=True)),True))()
            wire(n38.O, n74.I0)
            wire(n73.O, n74.I1)
            wire(n38.valid_down & n73.valid_down, n74.valid_up)
            n84 = DefineShift_TS(32, 32, 2, 1, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), has_valid=True)()
            wire(n1.O, n84.I)
            wire(n1.valid_down, n84.valid_up)
            n85 = DefineShift_TS(32, 32, 2, 1, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), has_valid=True)()
            wire(n84.O, n85.I)
            wire(n84.valid_down, n85.valid_up)
            n86 = DefineMap2_T(32, 32, DefineMap2_S(2, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(), has_valid=True))),True))()
            wire(n85.O, n86.I0)
            wire(n84.O, n86.I1)
            wire(n85.valid_down & n84.valid_down, n86.valid_up)
            n99 = DefineMap2_T(32, 32, DefineMap2_S(2, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True))),True))()
            wire(n86.O, n99.I0)
            wire(n1.O, n99.I1)
            wire(n86.valid_down & n1.valid_down, n99.valid_up)
            n118 = DefineMap_T(32, 32, DefineMap_S(2, DefineMap_T(1, 2, DefineSerialize(3, 0, ST_Int())),True))()
            wire(n99.O, n118.I)
            wire(n99.valid_down, n118.valid_up)
            n119 = DefineMap2_T(32, 32, DefineMap2_S(2, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_TSeq(3, 0, ST_Int()), 2, has_valid=True)),True))()
            wire(n74.O, n119.I0)
            wire(n118.O, n119.I1)
            wire(n74.valid_down & n118.valid_down, n119.valid_up)
            n133 = DefineMap_T(32, 32, DefineMap_S(2, DefineSerialize(3, 0, ST_TSeq(3, 0, ST_Int())),True))()
            wire(n119.O, n133.I)
            wire(n119.valid_down, n133.valid_up)
            n175 = DefineMap_T(32, 32, DefineMap_S(2, Module_0(),True))()
            wire(n133.O, n175.I)
            wire(n133.O, cls.conv_out)
            wire(n133.valid_down, n175.valid_up)
            n178 = DefineMap_T(32, 32, DefineDown_S(2, 1, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), has_valid=True))()
            wire(n175.O, n178.I)
            wire(n175.valid_down, n178.valid_up)
            n179 = DefineReshape_ST(ST_TSeq(32, 32, ST_SSeq(1, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))), ST_TSeq(16, 48, ST_SSeq(2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))), has_valid=True)()
            wire(n178.O, n179.I)
            wire(n178.valid_down, n179.valid_up)
            n180 = DefineReshape_ST(ST_TSeq(16, 48, ST_SSeq(2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))), ST_TSeq(4, 12, ST_SSeq(2, ST_TSeq(4, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))))), has_valid=True)()
            wire(n179.O, n180.I)
            wire(n179.valid_down, n180.valid_up)
            n183 = DefineMap_T(4, 12, DefineDown_S(2, 1, ST_TSeq(4, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))), has_valid=True))()
            wire(n180.O, n183.I)
            wire(n180.valid_down, n183.valid_up)
            n184 = DefineReshape_ST(ST_TSeq(4, 12, ST_SSeq(1, ST_TSeq(4, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))))), ST_TSeq(4, 12, ST_TSeq(4, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))), has_valid=True)()
            wire(n183.O, n184.I)
            wire(n183.valid_down, n184.valid_up)
            n185 = DefineReshape_ST(ST_TSeq(4, 12, ST_TSeq(4, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))), ST_TSeq(16, 48, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))), has_valid=True)()
            wire(n184.O, n185.I)
            wire(n184.valid_down, n185.valid_up)
            n186 = DefineFIFO(ST_TSeq(16, 48, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))), 1, has_valid=True)()
            wire(n185.O, n186.I)
            wire(n185.valid_down, n186.valid_up)
            n187 = DefineFIFO(ST_TSeq(16, 48, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))), 1, has_valid=True)()
            wire(n186.O, n187.I)
            wire(n186.valid_down, n187.valid_up)
            wire(n187.O, cls.O)
            wire(n187.valid_down, cls.valid_down)
    return _Module_1

Main = Module_1
fault_inputs0 = [[1,2],[2,3],[3,4],[2,3],[3,4],[4,5],[3,4],[4,5],[5,6],[3,4],[4,5],[5,6],[4,5],[5,6],[6,7],[5,6],[6,7],[7,8],[5,6],[6,7],[7,8],[6,7],[7,8],[8,9],[7,8],[8,9],[9,10],[7,8],[8,9],[9,10],[8,9],[9,10],[10,11],[9,10],[10,11],[11,12],[9,10],[10,11],[11,12],[10,11],[11,12],[12,13],[11,12],[12,13],[13,14],[11,12],[12,13],[13,14],[12,13],[13,14],[14,15],[13,14],[14,15],[15,16],[13,14],[14,15],[15,16],[14,15],[15,16],[16,17],[15,16],[16,17],[17,18],[15,16],[16,17],[17,18],[16,17],[17,18],[18,19],[17,18],[18,19],[19,20],[17,18],[18,19],[19,20],[18,19],[19,20],[20,21],[19,20],[20,21],[21,22],[19,20],[20,21],[21,22],[20,21],[21,22],[22,23],[21,22],[22,23],[23,24],[21,22],[22,23],[23,24],[22,23],[23,24],[24,25],[23,24],[24,25],[25,26],[23,24],[24,25],[25,26],[24,25],[25,26],[26,27],[25,26],[26,27],[27,28],[25,26],[26,27],[27,28],[26,27],[27,28],[28,29],[27,28],[28,29],[29,30],[27,28],[28,29],[29,30],[28,29],[29,30],[30,31],[29,30],[30,31],[31,32],[29,30],[30,31],[31,32],[30,31],[31,32],[32,33],[31,32],[32,33],[33,34],[31,32],[32,33],[33,34],[32,33],[33,34],[34,35],[33,34],[34,35],[35,36],[33,34],[34,35],[35,36],[34,35],[35,36],[36,37],[35,36],[36,37],[37,38],[35,36],[36,37],[37,38],[36,37],[37,38],[38,39],[37,38],[38,39],[39,40],[37,38],[38,39],[39,40],[38,39],[39,40],[40,41],[39,40],[40,41],[41,42],[39,40],[40,41],[41,42],[40,41],[41,42],[42,43],[41,42],[42,43],[43,44],[41,42],[42,43],[43,44],[42,43],[43,44],[44,45],[43,44],[44,45],[45,46],[43,44],[44,45],[45,46],[44,45],[45,46],[46,47],[45,46],[46,47],[47,48],[45,46],[46,47],[47,48],[46,47],[47,48],[48,49],[47,48],[48,49],[49,50],[47,48],[48,49],[49,50],[48,49],[49,50],[50,51],[49,50],[50,51],[51,52],[49,50],[50,51],[51,52],[50,51],[51,52],[52,53],[51,52],[52,53],[53,54],[51,52],[52,53],[53,54],[52,53],[53,54],[54,55],[53,54],[54,55],[55,56],[53,54],[54,55],[55,56],[54,55],[55,56],[56,57],[55,56],[56,57],[57,58],[55,56],[56,57],[57,58],[56,57],[57,58],[58,59],[57,58],[58,59],[59,60],[57,58],[58,59],[59,60],[58,59],[59,60],[60,61],[59,60],[60,61],[61,62],[59,60],[60,61],[61,62],[60,61],[61,62],[62,63],[61,62],[62,63],[63,64],[61,62],[62,63],[63,64],[62,63],[63,64],[64,0],[63,64],[64,0],[0,0],[63,64],[64,0],[0,0],[64,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]]
fault_inputs0_valid = [True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]
fault_output = [253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,3,253,253,253,253,253,3,253,3,5,253,253,3,253,3,5,3,5,7,253,3,5,3,5,7,5,7,253,3,5,7,5,7,253,7,253,3,5,7,253,7,253,3,253,3,5,7,253,3,253,3,5,3,5,7,253,3,5,3,5,7,5,7,253,3,5,7,5,7,253,7,253,3,5,7,253,7,253,3,253,3,5,7,253,3,253,3,5,3,5,7,253,3,5,3,5,7,5,7,0,3,5,7,5,7,0,7,0,0,5,7,0,7,0,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
fault_output_valid = [True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, clock(mod.CLK))
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(576 + 188):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 576 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.hi, fault_inputs0[f_clk], num_nested_space_layers(ST_TSeq(32, 32, ST_SSeq(2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))))), 0)
            tester.print("hi: ")
            fault_helpers.print_nested_port(tester, tester.circuit.hi, num_nested_space_layers(ST_TSeq(32, 32, ST_SSeq(2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))))))
            tester.print("\n")
        tester.eval()
        if f_clk > 188:
            output_counter += 1
        tester.print("O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_TSeq(16, 48, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))))
        tester.print("\n")
        tester.print("conv out: ")
        fault_helpers.print_nested_port(tester, tester.circuit.conv_out, num_nested_space_layers(ST_TSeq(32, 32, ST_SSeq(2, ST_TSeq(3, 0, ST_TSeq(3, 0, ST_Int()))))))
        tester.print("\n")
        if f_clk >= 188:
            tester.circuit.valid_down.expect(1)
        if f_clk >= 188 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_TSeq(16, 48, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))), 0)
        tester.step(2)
    tester.circuit.valid_down.expect(0)
    fault_helpers.compile_and_run(tester)
