"""
Constructors for converting parse results -> Acab data
"""
from acab.abstract.core.default_structure import SEMANTIC_HINT
from acab.abstract.core.production_abstractions import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionStructure)
from acab.abstract.core.values import Sentence
from acab.abstract.core import default_structure as DS
from acab.abstract.parsing import default_structure as PDS
from acab.modules.parsing.exlo import util as EXu


def build_query_component(s, loc, toks):
    """ Build a comparison """
    op = toks[EXu.OPERATOR_S][0]
    params = []
    if EXu.VALUE_S in toks:
        params = toks[EXu.VALUE_S][0].words


    return (EXu.CONSTRAINT_S, ProductionComponent(value=op, params=params))

def build_transform_component(s, loc, toks):
    params = []
    if EXu.LEFT_S in toks:
        params.append(toks[EXu.LEFT_S][0])
    params += toks[EXu.RIGHT_S][:]

    op = toks[EXu.OPERATOR_S][0]
    if isinstance(op, str):
        op = Sentence.build([op])

    rebind = toks[EXu.TARGET_S][0]
    filtered_params = [x[0] if len(x) == 1 else x for x in params]

    return ProductionComponent(value=op,
                               params=filtered_params,
                               rebind=rebind,
                               sugared=EXu.LEFT_S in toks)

def build_action_component(s, loc, toks):
    params = []
    if PDS.LEFT in toks:
        params.append(toks[EXu.LEFT_S])
    if PDS.RIGHT in toks:
        params = toks[EXu.RIGHT_S][:]
    op = toks[EXu.OPERATOR_S][0]
    filtered_params = [x[0] if len(x) == 1 else x for x in params]
    return ProductionComponent(value=op, params=filtered_params, sugared=EXu.LEFT_S in toks)



#--------------------
def build_query(s, loc, toks):
    query = ProductionContainer(value=toks[:],
                                data={SEMANTIC_HINT: EXu.QUERY_SEM_HINT})
    return (PDS.QUERY, query)

def build_transform(s, loc, toks):
    trans = ProductionContainer(value=toks[:],
                                data={SEMANTIC_HINT : EXu.TRANSFORM_SEM_HINT})
    return (EXu.TRANSFORM_S, trans)

def build_action(s, loc, toks):
    # TODO: check this
    clauses = [x if isinstance(x, ProductionComponent)
               else ProductionComponent(value=Sentence.build([EXu.DEFAULT_ACTION_S]), params=[x]) for x in toks]

    act = ProductionContainer(value=clauses,
                              data={SEMANTIC_HINT : EXu.ACTION_SEM_HINT})

    return (EXu.ACTION_S, act)


#--------------------
def build_rule(s, loc, toks, sem_hint=None):
    # Get Conditions
    if EXu.QUERY_S in toks:
        c = toks[EXu.QUERY_S][0][1]
        assert(isinstance(c, ProductionContainer))
    else:
        c = None

    # Get Transform
    if EXu.TRANSFORM_S in toks:
        t = toks[EXu.TRANSFORM_S][0][1]
        assert(isinstance(t, ProductionContainer))
    else:
        t = None

    # Get Action
    if EXu.ACTION_S in toks:
        a = toks[EXu.ACTION_S][0][1]
        assert(isinstance(a, ProductionContainer))
    else:
        a = None

    structure = {
        EXu.QUERY_COMPONENT     : c,
        EXu.TRANSFORM_COMPONENT : t,
        EXu.ACTION_COMPONENT    : a
        }

    if sem_hint is None:
        sem_hint = EXu.RULE_SEM_HINT


    rule = ProductionStructure(structure=structure,
                               data={SEMANTIC_HINT: sem_hint})
    rule.data[DS.TYPE_INSTANCE] = EXu.RULE_PRIM
    return (rule.type, rule)

def build_constraint_list(s, loc, toks):
    """ Build a constraint list """
    return (EXu.CONSTRAINT_S, [x[1] for x in toks[:]])
