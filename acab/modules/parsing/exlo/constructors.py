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
from acab.abstract.parsing.annotation import ValueAnnotation, ValueRepeatAnnotation

def build_query_component(s, loc, toks):
    """ Build a comparison """
    op = toks[EXu.OPERATOR_S][0]
    params = []
    if EXu.VALUE_S in toks:
        params = toks[EXu.VALUE_S][:]

    params = [x[0] if len(x) == 1 else x for x in params]

    return ValueRepeatAnnotation(DS.CONSTRAINT,
                                 ProductionComponent(value=op, params=params))

def build_transform_component(s, loc, toks):
    params = []
    if EXu.LEFT_S in toks:
        params.append(toks[EXu.LEFT_S][0])
    params += toks[EXu.RIGHT_S][:]

    op = toks[EXu.OPERATOR_S][0]
    if isinstance(op, str):
        op = Sentence.build([op])

    rebind = toks[EXu.TARGET_S][0]
    params = [x[0] if len(x) == 1 else x for x in params]

    return ProductionComponent(value=op,
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
    params = [x[0] if len(x) == 1 else x for x in params]
    return ProductionComponent(value=op,
                               params=params,
                               sugared=EXu.LEFT_S in toks)



#--------------------
def build_query(s, loc, toks):
    clauses = toks[:]
    query = ProductionContainer(value=clauses,
                                data={SEMANTIC_HINT: EXu.QUERY_SEM_HINT})
    return query

def build_transform(s, loc, toks):
    clauses = toks[:]
    trans = ProductionContainer(value=clauses,
                                data={SEMANTIC_HINT: EXu.TRANSFORM_SEM_HINT})
    return trans

def build_action(s, loc, toks):
    clauses = toks[:]
    clauses = [x if isinstance(x, ProductionComponent)
               else ProductionComponent(value=Sentence.build([EXu.DEFAULT_ACTION_S]),
                                        params=[x]) for x in clauses]

    act = ProductionContainer(value=clauses,
                              data={SEMANTIC_HINT: EXu.ACTION_SEM_HINT})

    return act


#--------------------
def build_rule(s, loc, toks, sem_hint=None):
    # Get Conditions
    if EXu.QUERY_S in toks:
        query = toks[EXu.QUERY_S]
        assert(isinstance(query, ProductionContainer))
    else:
        query = None

    # Get Transform
    if EXu.TRANSFORM_S in toks:
        transform = toks[EXu.TRANSFORM_S]
        assert(isinstance(transform, ProductionContainer))
    else:
        transform = None

    # Get Action
    if EXu.ACTION_S in toks:
        action = toks[EXu.ACTION_S]
        assert(isinstance(action, ProductionContainer))
    else:
        action = None

    structure = {
        EXu.QUERY_COMPONENT     : query,
        EXu.TRANSFORM_COMPONENT : transform,
        EXu.ACTION_COMPONENT    : action
        }

    if sem_hint is None:
        sem_hint = EXu.RULE_SEM_HINT


    rule = ProductionStructure(structure=structure,
                               data={SEMANTIC_HINT: sem_hint,
                                     DS.TYPE_INSTANCE: EXu.RULE_PRIM})
    return rule

def build_constraint_list(s, loc, toks):
    """ Build a constraint list """
    return [x for x in toks[:]]
