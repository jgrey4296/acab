#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)


from acab.abstract.core import default_structure as DS
from acab.abstract.interfaces import semantic as SI
from acab.modules.semantics.context_container import MutableContextInstance
from acab.modules.semantics.util import SemanticBreakpointDecorator

CtxIns = 'ContextInstance'

# Primary Abstractions:
class QueryAbstraction(SI.AbstractionSemantics_i):
    """
    Very simply accumulate results of multiple sentences of queries
    """
    @SemanticBreakpointDecorator
    def __call__(self, instruction, semSys, ctxs=None, data=None):
        query = instruction
        # Get the default dependent semantics
        sem, struct = semSys.lookup(None)
        for clause in query.clauses:
            # TODO ensure system selects the dep sems and struct
            sem.query(struct, clause, data=data, ctxs=ctxs)


class TransformAbstraction(SI.AbstractionSemantics_i):
    """ Takes a context, returns a changed context """
    @SemanticBreakpointDecorator
    def __call__(self, instruction, semSys, ctxs=None, data=None):
        # Note: run *all* the transform clauses at once,
        # To minimise redundent new ctxs
        # runs on a single active ctx

        # TODO: operators might actually come from semSys
        operators   = ctxs._operators
        transform   = instruction
        for ctxIns in ctxs.active_list(clear=True):
            # TODO make this a context manager?
            mutx = MutableContextInstance.build(ctxIns)
            for clause in transform.clauses:
                op     = operators[clause.op]
                params = [mutx[x] for x in clause.params]
                result = op(*params, data=ctxIns.data)
                mutx[clause.rebind] = result

            # bind ctx with results
            # TODO move this inside mutablecontextinstance
            ctxs.push(mutx.finish())


class ActionAbstraction(SI.AbstractionSemantics_i):
    """ *Consumes* a context, performing all actions in it """

    @SemanticBreakpointDecorator
    def __call__(self, instruction, semSys, ctxs=None, data=None):
        operators = ctxs._operators
        action    = instruction

        for ctx in ctxs.active_list(clear=True):
            for clause in action.clauses:
                op     = operators[clause.op]
                params = [ctx[x] for x in clause.params]
                result = op(*params, data=clause.data, semSystem=semSys)



class AtomicRuleAbstraction(SI.AbstractionSemantics_i):
    """ Run a rule in a single semantic call """

    @SemanticBreakpointDecorator
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Rule Logic, returns action proposals """
        # TODO Possibly setup a temp ctxs

        rule = instruction
        # Run the query
        if DS.QUERY_COMPONENT in rule:
            semsys(rule[DS.QUERY_COMPONENT], ctxs=ctxs)

        if not bool(ctxs):
            return

        # TODO needs to be applied to all actives
        if DS.TRANSFORM_COMPONENT in rule:
            semsys(rule[DS.TRANSFORM_COMPONENT], data=data, ctxs=ctxs)

        if DS.ACTION_COMPONENT in rule:
            semsys(rule[DS.ACTION_COMPONENT], data=data, ctxs=ctxs)

class ProxyRuleAbstraction(SI.AbstractionSemantics_i):
    """ Run a rules queries, then return ctxs bound
    with transform+action continuation """

    @SemanticBreakpointDecorator
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Rule Logic, returns action proposals """
        # TODO: if all contexts in ctxs have a continuation,
        # self.run_continuations

        # else:
        rule = instruction
        # TODO Possibly setup a temp ctxs

        # Run the query
        if DS.QUERY_COMPONENT in rule:
            semsys(rule[DS.QUERY_COMPONENT], data=data, ctxs=ctxs)

        if not bool(ctxs):
            return

        # TODO bind transform and actions to instances
        for ctx in ctxs.active_list():
            ctx.set_continuation(instruction)


    def run_continuations(self, instruction, ctxs, semsys, data=None):
        # TODO this might be _continuation of each ctx
        limited_container = ContextContainer.build(ctxs._operators)
        limited_container.pop()
        for ctx in ctxs.active_list():
            limited_container.push(ctx)
            cont = ctx.continuation

            # TODO will need to handle each individual ctx
            if DS.TRANSFORM_COMPONENT in cont:
                transformed = semsys(cont[DS.TRANSFORM_COMPONENT],
                                     data=data,
                                     ctxs=limited_container)

            if DS.ACTION_COMPONENT in rule:
                semsys.run(cont[DS.ACTION_COMPONENT],
                           data=data,
                           ctxs=limited_container)

            ctxs.merge(limited_container)
            ctxs.clear()


# Secondary Abstractions:
class LayerAbstraction(SI.AbstractionSemantics_i):
    """ A Layer of rules.
    ie: Query for rules.
    Select rules to run.
    run selection of rules.
    select passing rules to complete.
    run passing selection.
    """
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Run a layer, returning actions to perform """
        layer = instruction

        if DS.QUERY_COMPONENT in layer:
            semsys(layer[DS.QUERY_COMPONENT], data=data, ctxs=ctxs)

        if not bool(ctxs):
            return

        # TODO needs to be applied to all actives
        if DS.TRANSFORM_COMPONENT in layer:
            semsys.run(layer[DS.TRANSFORM_COMPONENT], data=data, ctxs=ctxs)

        if DS.ACTION_COMPONENT in layer:
            semsys.run(layer[DS.ACTION_COMPONENT], data=data, ctxs=ctxs)

class AgendaAbstraction(SI.AbstractionSemantics_i):
    """ A Layer-specific transform, to run operators on ctxs """
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Runs an agenda rule on activated rules """
        # setup

        # run limited query

        # run transform on ctxs
        #
        raise NotImplementedError()



class AtomicPipelineAbstraction(SI.AbstractionSemantics_i):
    """ A Means of sequencing layers, run all layers per tick """

    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Run this pipeline on the given engine for a tick """
        # Setup
        pipeline = instruction

        for layer in pipeline:
            # Run the layer
            continue

        raise NotImplementedError()

class TemporalPipelineAbstraction(SI.AbstractionSemantics_i):
    """ A Means of sequencing layers, one layer per tick """

    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Run this pipeline on the given engine for a tick """
        # Setup
        pipeline = instruction
        # Determine layer to run
        layer = None
        # run it

        raise NotImplementedError()

class ContainerAbstraction(SI.AbstractionSemantics_i):
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Apply the clauses in one move """
        for x in instruction.clauses:
            ctxs = semsys(x, ctxs=ctxs)

        return ctxs
