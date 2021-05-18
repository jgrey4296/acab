#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
import logging as root_logger
logging = root_logger.getLogger(__name__)


from acab.abstract.interfaces import semantic_interfaces as SI
from acab.abstract.parsing import consts as PConst
from acab.modules.semantics.context_container import MutableContextInstance

CtxIns = 'ContextInstance'

# Primary Abstractions:
class QueryAbstraction(SI.AbstractionSemantics):
    """
    Very simply accumulate results of multiple sentences of queries
    """
    def __call__(self, instruction, ctxCon, semSys, data=None):
        query = instruction
        for clause in query.clauses:
            # TODO ensure system selects the dep sems and struct
            semSys(clause, data=data, ctxs=ctxCon)


class TransformAbstraction(SI.AbstractionSemantics):
    """ Takes a context, returns a changed context """
    def __call__(self, instruction, ctxCon, semSys, data=None):
        # Note: run *all* the transform clauses at once,
        # To minimise redundent new ctxs
        # runs on a single active ctx

        # TODO: operators might actually come from semSys
        operators   = ctxCon._operators
        transform   = instruction
        for ctxIns in ctxCon.active_list(clear=True):
            # TODO make this a context manager?
            mutx = MutableContextInstance.build(ctxIns)
            for clause in transform.clauses:
                op     = operators[clause.op]
                params = [mutx[x] for x in clause.params]
                result = op(*params, data=clause.data)
                mutx[clause.rebind] = result

            # bind ctx with results
            # TODO move this inside mutablecontextinstance
            ctxCon.push(mutx.finish())

class ActionAbstraction(SI.AbstractionSemantics):
    """ *Consumes* a context, performing all actions in it """
    def __call__(self, instruction, ctxCon, semSys, data=None):
        operators = ctxCon._operators
        action    = instruction

        for ctx in ctxCon.active_list(clear=True):
            for clause in action.clauses:
                op     = operators[clause.op]
                params = [ctx[x] for x in clause.params]
                result = op(*params, data=clause.data, semSystem=semSys)




class AtomicRuleAbstraction(SI.AbstractionSemantics):
    """ Run a rule in a single semantic call """

    def __call__(self, instruction, ctxCon, semMap, data=None):
        """ Rule Logic, returns action proposals """
        # TODO Possibly setup a temp ctxCon

        rule = instruction
        # Run the query
        if PConst.QUERY_S in rule:
            semMap(rule[PConst.QUERY_S], data=data, ctxs=ctxCon)

        if not bool(ctxCon):
            return

        # TODO needs to be applied to all actives
        if PConst.TRANSFORM_S in rule:
            semMap(rule[PConst.TRANSFORM_S], data=data, ctxs=ctxCon)

        if PConst.ACTION_S in rule:
            semMap(rule[PConst.ACTION_S], data=data, ctxs=ctxCon)

class SteppedRuleAbstraction(SI.AbstractionSemantics):
    """ Run a rules queries, then return ctxs bound
    with transform+action continuation """

    def __call__(self, instruction, ctxCon, semMap, data=None):
        """ Rule Logic, returns action proposals """
        rule = instruction
        # TODO Possibly setup a temp ctxCon

        # Run the query
        if PConst.QUERY_S in rule:
            semMap(rule[PConst.QUERY_S], data=data, ctxs=ctxCon)

        if not bool(ctxCon):
            return

        # TODO bind transform and actions to instances
        for ctx in ctxCon.active_list:
            ctx.set_continuation(instruction)


    def run_continuation(self, instruction, ctxCon, semMap, data=None):
        # TODO this might be _continuation of each ctx
        limited_container = ContextContainer.build()
        limited_container.pop()
        for ctx in ctxCon.active_list():
            limited_container.push(ctx)
            cont = ctx.continuation

            # TODO will need to handle each individual ctx
            if PConst.TRANSFORM_S in cont:
                transformed = semMap(cont[PConst.TRANSFORM_S],
                                     data=data,
                                     ctxs=limited_container)

            if PConst.ACTION_S in rule:
                semMap.run(cont[PConst.ACTION_S],
                           data=data,
                           ctxs=limited_container)

            ctxCon.merge(limited_container)
            ctxCon.clear()


# Secondary Abstractions:
class LayerAbstraction(SI.AbstractionSemantics):
    """ A Layer of rules.
    ie: Query for rules.
    Select rules to run.
    run selection of rules.
    select passing rules to complete.
    run passing selection.
    """
    def __call__(self, instruction, ctxCon, semMap, data=None):
        """ Run a layer, returning actions to perform """
        layer = instruction

        if PConst.QUERY_S in layer:
            semMap(layer[PConst.QUERY_S], data=data, ctxs=ctxCon)

        if not bool(ctxCon):
            return

        # TODO needs to be applied to all actives
        if PConst.TRANSFORM_S in layer:
            semMap.run(layer[PConst.TRANSFORM_S], data=data, ctxs=ctxCon)

        if PConst.ACTION_S in layer:
            semMap.run(layer[PConst.ACTION_S], data=data, ctxs=ctxCon)

class AgendaAbstraction(SI.AbstractionSemantics):
    """ A Layer-specific transform, to run operators on ctxs """
    def __call__(self, instruction, ctxCon, semMap, data=None):
        """ Runs an agenda rule on activated rules """
        # setup

        # run limited query

        # run transform on ctxCon
        #
        raise NotImplementedError()



class AtomicPipelineAbstraction(SI.AbstractionSemantics):
    """ A Means of sequencing layers, run all layers per tick """

    def __call__(self, instruction, ctxCon, semMap, data=None):
        """ Run this pipeline on the given engine for a tick """
        # Setup
        pipeline = instruction

        for layer in pipeline:
            # Run the layer
            continue

        raise NotImplementedError()

class TemporalPipelineAbstraction(SI.AbstractionSemantics):
    """ A Means of sequencing layers, one layer per tick """

    def __call__(self, instruction, ctxCon, semMap, data=None):
        """ Run this pipeline on the given engine for a tick """
        # Setup
        pipeline = instruction
        # Determine layer to run
        layer = None
        # run it

        raise NotImplementedError()

class ContainerAbstraction(SI.AbstractionSemantics):
    def __call__(self, instruction, ctxCon, semMap, data=None):
        """ Apply the clauses in one move """
        for x in instruction.clauses:
            ctxs = semMap(x, ctxs=ctxCon, data=data)

        return ctxs
