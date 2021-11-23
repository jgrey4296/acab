#!/usr/bin/env python3
import logging as root_logger
from collections import defaultdict
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

from acab import types as AT
from acab.core.data import default_structure as DS
from acab.core.data.production_abstractions import ProductionOperator
from acab.core.decorators.semantic import RunInSubCtxSet
from acab.interfaces import semantic as SI
from acab.error.semantic_exception import AcabSemanticException
from acab.modules.context.context_set import ContextSet, MutableContextInstance
from acab.modules.semantics.util import SemanticBreakpointDecorator

CtxIns = AT.CtxIns

# Primary Abstractions:
class QueryAbstraction(SI.StatementSemantics_i):
    """
    Very simply accumulate results of multiple sentences of queries
    """
    @SemanticBreakpointDecorator
    def __call__(self, instruction, semSys, ctxs=None, data=None):
        query = instruction
        # Get the default dependent semantics
        sem, struct = semSys.lookup()
        for clause in query.clauses:
            sem.query(clause, struct, data=data, ctxs=ctxs)

class QueryPlusAbstraction(SI.StatementSemantics_i):
    """ A Query abstraction that can handle expanded query semantics.
    Eg: Walkers
    """
    @SemanticBreakpointDecorator
    def __call__(self, instruction, semSys, ctxs=None, data=None):
        query = instruction
        for clause in query.clauses:
            sem, struct = semSys.lookup(clause)
            if struct is None:
                struct = semSys

            sem(clause, struct, data=data, ctxs=ctxs)


class TransformAbstraction(SI.StatementSemantics_i):
    """ Takes a context, returns a changed context """
    @SemanticBreakpointDecorator
    def __call__(self, instruction, semSys, ctxs=None, data=None):
        # Note: run *all* the transform clauses at once
        operators = ctxs._operators
        transform =  instruction
        for ctxIns in ctxs.active_list(clear=True):
            with MutableContextInstance(ctxs, ctxIns) as mutx:
                for clause in transform.clauses:
                    # TODO this should all be replaceable with just: op = mutx[clause.op]
                    if clause.op in mutx:
                        op = mutx[clause.op]
                    elif isinstance(clause.op, ProductionOperator):
                        op = clause.op
                    else:
                        op = operators[clause.op]

                    params              = [mutx[x] for x in clause.params]
                    result              = op(*params, data=mutx.data)
                    mutx[clause.rebind] = result

class ActionAbstraction(SI.StatementSemantics_i):
    """ *Consumes* a context, performing all actions in it """

    @SemanticBreakpointDecorator
    def __call__(self, instruction, semSys, ctxs=None, data=None):
        operators = ctxs._operators
        action    = instruction

        for ctx in ctxs.active_list(clear=True):
            for clause in action.clauses:
                if clause.op in ctx:
                    op = ctx[clause.op]
                elif isinstance(clause.op, ProductionOperator):
                    op = clause.op
                else:
                    op     = operators[clause.op]

                params = [ctx[x] for x in clause.params]
                result = op(*params, data=clause.data, semSystem=semSys)



class AtomicRuleAbstraction(SI.StatementSemantics_i):
    """ Run a rule in a single semantic call """

    @RunInSubCtxSet
    @SemanticBreakpointDecorator
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Rule Logic, returns action proposals """
        # TODO handle rule arguments
        rule = instruction
        # Run the query
        if DS.QUERY_COMPONENT in rule:
            semsys(rule[DS.QUERY_COMPONENT], ctxs=ctxs)

        if not bool(ctxs):
            return

        if DS.TRANSFORM_COMPONENT in rule:
            semsys(rule[DS.TRANSFORM_COMPONENT], ctxs=ctxs)

        if DS.ACTION_COMPONENT in rule:
            semsys(rule[DS.ACTION_COMPONENT], ctxs=ctxs)

class ProxyRuleAbstraction(SI.StatementSemantics_i):
    """ Run a rules queries, then return ctxs bound
    with transform+action continuation """

    @SemanticBreakpointDecorator
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        if instruction in ctxs._named_sets:
            subctx = ctxs[instruction]
            self.run_continuations(instruction, semsys, ctxs=subctx)
        else:
            self.run_query(instruction, semsys, ctxs=ctxs, data=data)



    @RunInSubCtxSet
    def run_query(self, instruction, semsys, ctxs=None, data=None):
        logging.debug("Running Proxy Rule Queries")
        rule = instruction

        # Run the query
        if DS.QUERY_COMPONENT in rule:
            semsys(rule[DS.QUERY_COMPONENT], ctxs=ctxs)

        if not bool(ctxs):
            return

        ctxs.build_named_set(instruction, [x.uuid for x in ctxs.active_list()])

    def run_continuations(self, instruction, semsys, ctxs=None, data=None):
        logging.debug("Running Proxy Rule Continuations")

        if DS.TRANSFORM_COMPONENT in instruction:
            semsys(instruction[DS.TRANSFORM_COMPONENT],
                   ctxs=ctxs)

        if DS.ACTION_COMPONENT in instruction:
            semsys(instruction[DS.ACTION_COMPONENT],
                   ctxs=ctxs)


# Secondary Abstractions:
class LayerAbstraction(SI.StatementSemantics_i):
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
            semsys(layer[DS.QUERY_COMPONENT], ctxs=ctxs)

        if not bool(ctxs):
            return

        # TODO needs to be applied to all actives
        if DS.TRANSFORM_COMPONENT in layer:
            semsys.run(layer[DS.TRANSFORM_COMPONENT], ctxs=ctxs)

        if DS.ACTION_COMPONENT in layer:
            semsys.run(layer[DS.ACTION_COMPONENT], ctxs=ctxs)

class AgendaAbstraction(SI.StatementSemantics_i):
    """ A Layer-specific transform, to run operators on ctxs """
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Runs an agenda rule on activated rules """
        # setup

        # run limited query

        # run transform on ctxs
        #
        raise NotImplementedError()



class AtomicPipelineAbstraction(SI.StatementSemantics_i):
    """ A Means of sequencing layers, run all layers per tick """

    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Run this pipeline on the given engine for a tick """
        # Setup
        pipeline = instruction

        for layer in pipeline:
            # Run the layer
            continue

        raise NotImplementedError()

class TemporalPipelineAbstraction(SI.StatementSemantics_i):
    """ A Means of sequencing layers, one layer per tick """

    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Run this pipeline on the given engine for a tick """
        # Setup
        pipeline = instruction
        # Determine layer to run
        layer = None
        # run it

        raise NotImplementedError()

class ContainerAbstraction(SI.StatementSemantics_i):
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Apply the clauses in one move """
        for x in instruction.clauses:
            ctxs = semsys(x, ctxs=ctxs)

        return ctxs

