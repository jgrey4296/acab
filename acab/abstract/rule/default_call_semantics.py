#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

CTXS = List[Dict[Any, Any]]

def __default_production_call(ProdSem: 'ProductionSemantics',
                              prodution: 'AcabValue',
                              ctxs: CTXS,
                              engine: 'Engine') -> CTXS:
    pass

def rule__call__(ProdSem, rule, ctxs: List[Dict[Any, Any]]=None, engine: 'Engine'=None) -> List[Dict[Any, Any]]:
    """ Rule Logic, returns action proposals """
    query_result = ProdSem._initial_ctx_construction(ctxs)

    # Run the query
    if ProdSem._query is not None:
        query_result = ProdSem.run(rule[PConst.QUERY_V], ctxs, engine)
        if not bool(query_result):
            logging.info("Rule {} Failed".format(rule.name))
            return []

    # Run any transforms
    # This is *not* an unnecessary comprehension
    # because of how query results work
    transformed = query_result[:]
    if PConst.TRANSFORM_V in rule:
        transformed = ProdSem.run(rule[PConst.TRANSFORM_V], ctxs=transformed, engine=engine)

    # *DELAYED* action results
    # return final passing dictionaries
    results = []
    for data in transformed:
        results.append((data, rule))

    return results

def layer__call__(ProdSem, layer, ctxs=None, engine=None):
    """ Run a layer, returning actions to perform """
    # rule returns [(data,ProdSem)]
    results = ProdSem.run(layer, ctxs=ctxs, engine=engine, override=PConst.RULE_V)

    logging.warning("Layer results: {}".format(len(results) < 1))
    # Run layer actions
    contexts = []
    if bool(results):
        contexts.append(results[0][0])

    action_results = ProdSem.run(layer[PConst.ACTION_V], ctxs=contexts, engine=engine)
    return action_results

def pipeline__call__(ProdSem, pipeline, ctxs=None, engine=None):
    """ Run this pipeline on the given engine for a tick """
    results = ProdSem.run(pipeline, engine=engine, override=PConst.RULE_V)
    # TODO extract semantics
    # Run pipeline actions
    output = []

    return output

def agenda__call__(ProdSem, agenda, ctxs=None, engine=None):
    """ Runs an agenda rule on activated rules """
    assert(isinstance(ctxs, list))
    agenda_settings = ProdSem.run(agenda, ctxs=ctxs, engine=engine, override=PConst.RULE_V)

    # TODO extract semantics
    assert(len(agenda_settings) == 1)
    settings = agenda_settings[0][0]

    # Enact agenda
    resulting_ctxs = ProdSem.run(agenda[PConst.ACTION_V], ctxs=ctxs, engine=engine)
    return resulting_ctxs[0][Agenda.RETURN_NAME_S]

def component_call(ProdSem, component, ctxs=None, engine=None):
    """ Verify the Component, retrieving the operator from the engine
    if necessary """
    ctxs = ProdSem._param_ctx_filter(component, ctxs)

    # for each context:

    # $op -> retrieve from ctx
    op = component.op
    if len(op) == 1 and op[0].is_var and ctx is not None:
        op = ctx[op.value]

    if isinstance(op, Sentence) and engine is not None:
        op = engine.get_operator(op)

    # get values from data
    values = ProdSem._get_params(component, ctx)
    # TODO: op's should be able to be Components and Containers as well?
    assert(isinstance(op, (ProductionOperator, ProductionComponent, ProducionContainer, ProductionStructure)))
    result = ProdSem.run(op, [ctx], engine=engine)

    if x._rebind is None and isinstance(result, dict):
        ctx_singular.update(result)
    if x._rebind is not None:
        ctx_singular[x._rebind.value] = AcabValue.safe_make(result)



def container_call(ProdSem, container, ctxs=None, engine=None):
    """ Apply the clauses in one move """
    ctxs = ProdSem._initial_ctx_construction(ctxs)

    for x in container.clauses:
        ctxs = ProdSem.run(x, ctxs, engine)

    return ctxs

