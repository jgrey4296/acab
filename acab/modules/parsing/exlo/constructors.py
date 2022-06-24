"""
Constructors for converting parse results -> Acab data
"""
import acab.core.defaults.value_keys as DS
import acab.interfaces.value as VI
from acab import AcabConfig
from acab.core.defaults import parse_keys as PDS
from acab.core.defaults.value_keys import SEMANTIC_HINT
from acab.core.parsing.annotation import ValueAnnotation, ValueRepeatAnnotation
from acab.core.util.sentences import ProductionComponent
from acab.core.value.instruction import (ProductionContainer,
                                         ProductionStructure)
from acab.core.value.sentence import Sentence
from acab.interfaces.value import ValueFactory as VF
from acab.modules.parsing.exlo import util as EXu

config   = AcabConfig()
SIGNAL = DS.SEMANTIC_HINT

comp_sen      = VF.sen()      << DS.SENTENCE_PRIM << DS.COMPONENT_PRIM
query_comp    = comp_sen      << EXu.QUERY_COMPONENT
trans_comp    = comp_sen      << EXu.TRANSFORM_COMPONENT
act_comp      = comp_sen      << EXu.ACTION_COMPONENT

container_sen = VF.sen()      << EXu.INSTR << EXu.CONTAINER
query_contain = container_sen << EXu.QUERY_COMPONENT
trans_contain = container_sen << EXu.TRANSFORM_COMPONENT
act_contain   = container_sen << EXu.ACTION_COMPONENT

struct_sen    = VF.sen()      << EXu.INSTR << EXu.STRUCT
rule_struct   = struct_sen    << EXu.RULE_PRIM


DEFAULT_ACT   = VF.sen(data={DS.TYPE_INSTANCE: DS.OPERATOR}) << EXu.DEFAULT_ACTION_S

def build_query_component(s, loc, toks):
    """ Build a comparison """
    op = toks[EXu.OPERATOR_S]
    if not isinstance(op, VI.Sentence_i):
        op = op[0]

    params = [VF.sen(["node"])]
    if EXu.VALUE_S in toks:
        params += toks[EXu.VALUE_S][:]

    assert(isinstance(op, VI.Sentence_i)), type(op)
    assert(all([isinstance(x, VI.Sentence_i) for x in params]))
    assert(DS.OPERATOR in op.type)

    type_sen   = VF.sen([DS.CONSTRAINT.name])
    param_sen = VF.sen(data={DS.FLATTEN: False}) << params
    ret_sen = VF.sen(data={DS.FLATTEN: False}) << "returns" << "bool"
    constraint = VF.sen(data={DS.TYPE_INSTANCE: query_comp}) << op << param_sen << ret_sen
    return ValueRepeatAnnotation(DS.CONSTRAINT, constraint)

def build_transform_component(s, loc, toks):
    params = []
    if EXu.LEFT_S in toks:
        params.append(toks[EXu.LEFT_S][0])
    params += toks[EXu.RIGHT_S][:]

    op = toks[EXu.OPERATOR_S][0]
    if isinstance(op, str):
        op = VF.sen() << op

    rebind = toks[EXu.TARGET_S][0]
    param_sen = VF.sen(data={DS.FLATTEN: False}) << (params or "∅")
    ret_sen = VF.sen(data={DS.FLATTEN: False}) << "returns" << rebind
    transform = (VF.sen(data={"sugared": EXu.LEFT_S in toks,
                              DS.TYPE_INSTANCE: trans_comp,
                              DS.FLATTEN: False})
                 << op << param_sen << ret_sen)

    assert(isinstance(op, VI.Sentence_i))
    assert(all([isinstance(x, VI.Sentence_i) for x in params]))
    assert(isinstance(rebind, VI.Value_i))
    assert(DS.OPERATOR in op.type)

    return transform

def build_action_component(s, loc, toks):
    params = []
    if PDS.LEFT in toks:
        params.append(toks[EXu.LEFT_S])
    if PDS.RIGHT in toks:
        params = toks[EXu.RIGHT_S][:]
    op = toks[EXu.OPERATOR_S][0]
    if not isinstance(op, Sentence):
        op = VF.sen() << op

    param_sen = VF.sen(data={DS.FLATTEN: False}) << (params or "∅")
    ret_sen = VF.sen(data={DS.FLATTEN: False}) << "returns" << "unit"
    action = (VF.sen(data={"sugared": EXu.LEFT_S in toks,
                           DS.TYPE_INSTANCE: act_comp,
                           DS.FLATTEN: False})
              << op << param_sen << ret_sen)

    assert(isinstance(op, VI.Sentence_i))
    assert(all([isinstance(x, VI.Sentence_i) for x in params]))
    assert(DS.OPERATOR in op.type)
    return action

#--------------------
def build_query(s, loc, toks):
    clauses = toks[0][:]
    query = ProductionContainer(clauses,
                                data={DS.TYPE_INSTANCE: query_contain})
    return [query]

def build_transform(s, loc, toks):
    clauses = toks[0][:]
    trans = ProductionContainer(clauses,
                                data={DS.TYPE_INSTANCE: trans_contain})
    return [trans]

def build_action(s, loc, toks):
    """
    Unlike build_query/transform,
    action handle's sentences as being the default_action (typically assert)
    if the sentence doesn't have
    """
    clauses = toks[0][:]
    clauses = [x if isinstance(x, Sentence)
               else ProductionComponent(DEFAULT_ACT, params=[x]) for x in clauses]

    act = ProductionContainer(clauses, data={DS.TYPE_INSTANCE: act_contain})

    return [act]


#--------------------
def build_rule(s, loc, toks):
    # Get Conditions
    structure = []

    if EXu.QUERY_S in toks:
        query = toks[EXu.QUERY_S][0]
        assert(isinstance(query, ProductionContainer))
        structure.append(query.copy(name=EXu.QUERY_S))
    else:
        structure.append(EXu.QUERY_S)

    # Get Transform
    if EXu.TRANSFORM_S in toks:
        transform = toks[EXu.TRANSFORM_S][0]
        assert(isinstance(transform, ProductionContainer))
        structure.append(transform.copy(name=EXu.TRANSFORM_S))
    else:
        structure.append(EXu.TRANSFORM_S)

    # Get Action
    if EXu.ACTION_S in toks:
        action = toks[EXu.ACTION_S][0]
        assert(isinstance(action, ProductionContainer))
        structure.append(action.copy(name=EXu.ACTION_S))
    else:
        structure.append(EXu.ACTION_S)

    rule = ProductionStructure(structure,
                               data={DS.TYPE_INSTANCE: rule_struct})
    return rule

def build_constraint_list(s, loc, toks):
    """ Build a constraint list """
    return [x for x in toks[:] if isinstance(x, ValueAnnotation)]
