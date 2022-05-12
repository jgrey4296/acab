"""
Constructors for converting parse results -> Acab data
"""
import acab.interfaces.value as VI
from acab import AcabConfig
from acab.core.parsing import default_keys as PDS
from acab.core.parsing.annotation import ValueAnnotation, ValueRepeatAnnotation
from acab.core.value import default_structure as DS
from acab.core.value.default_structure import SEMANTIC_HINT
from acab.core.value.instruction import (ProductionComponent,
                                         ProductionContainer,
                                         ProductionStructure)
from acab.core.value.sentence import Sentence
from acab.interfaces.value import ValueFactory as VF
from acab.modules.parsing.exlo import util as EXu

config   = AcabConfig()
SIGNAL = DS.SEMANTIC_HINT

def build_query_component(s, loc, toks):
    """ Build a comparison """
    op = toks[EXu.OPERATOR_S]
    if not isinstance(op, VI.Sentence_i):
        op = op[0]

    params = []
    if EXu.VALUE_S in toks:
        params = toks[EXu.VALUE_S][:]

    params = [x[0] if len(x) == 1 else x for x in params]

    assert(isinstance(op, VI.Sentence_i)), type(op)
    assert(all([isinstance(x, VI.Value_i) for x in params]))
    assert(DS.OPERATOR in op.type)
    return ValueRepeatAnnotation(DS.CONSTRAINT,
                                 ProductionComponent(op, params=params))

def build_transform_component(s, loc, toks):
    params = []
    if EXu.LEFT_S in toks:
        params.append(toks[EXu.LEFT_S][0])
    params += toks[EXu.RIGHT_S][:]

    op = toks[EXu.OPERATOR_S][0]
    if isinstance(op, str):
        op = VF.sen() << op

    rebind = toks[EXu.TARGET_S][0]
    params = [x[0] if len(x) == 1 else x for x in params]

    assert(isinstance(op, VI.Sentence_i))
    assert(all([isinstance(x, VI.Value_i) for x in params]))
    assert(isinstance(rebind, VI.Value_i))
    assert(DS.OPERATOR in op.type)
    return ProductionComponent(op,
                               params=params,
                               rebind=rebind,
                               sugared=EXu.LEFT_S in toks)

def build_action_component(s, loc, toks):
    params = []
    if PDS.LEFT in toks:
        params.append(toks[EXu.LEFT_S])
    if PDS.RIGHT in toks:
        params = toks[EXu.RIGHT_S][:]
    op = toks[EXu.OPERATOR_S][0]
    if not isinstance(op, Sentence):
        op = VF.sen() << op
    # params = [x[0] if len(x) == 1 else x for x in params]

    assert(isinstance(op, VI.Sentence_i))
    assert(all([isinstance(x, VI.Value_i) for x in params]))
    assert(DS.OPERATOR in op.type)
    return ProductionComponent(op,
                               params=params,
                               sugared=EXu.LEFT_S in toks)



#--------------------
def build_query(s, loc, toks):
    clauses = toks[0][:]
    query = ProductionContainer(clauses,
                                data={SEMANTIC_HINT: EXu.QUERY_SIGNAL})
    return [query]

def build_transform(s, loc, toks):
    clauses = toks[0][:]
    trans = ProductionContainer(clauses,
                                data={SEMANTIC_HINT: EXu.TRANSFORM_SIGNAL})
    return [trans]

def build_action(s, loc, toks):
    """
    Unlike build_query/transform,
    action handle's sentences as being the default_action (typically assert)
    if the sentence doesn't have
    """
    clauses = toks[0][:]
    clauses = [x if (isinstance(x, ProductionComponent) or
                     isinstance(x, Sentence) and SIGNAL in x.data)
               else ProductionComponent(VF.sen([EXu.DEFAULT_ACTION_S], data={DS.TYPE_INSTANCE: DS.OPERATOR}),
                                        params=[x]) for x in clauses]

    act = ProductionContainer(clauses,
                              data={SEMANTIC_HINT: EXu.ACTION_SIGNAL})

    return [act]


#--------------------
def build_rule(s, loc, toks, signal=None):
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

    if signal is None:
        signal = EXu.RULE_SIGNAL

    rule = ProductionStructure(structure,
                               data={SEMANTIC_HINT: signal,
                                     DS.TYPE_INSTANCE: EXu.RULE_PRIM})
    return rule

def build_constraint_list(s, loc, toks):
    """ Build a constraint list """
    return [x for x in toks[:] if isinstance(x, ValueAnnotation)]
