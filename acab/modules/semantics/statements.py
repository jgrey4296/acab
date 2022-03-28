#!/usr/bin/env python3
import logging as root_logger
from collections import defaultdict
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

from acab import types as AT
from acab.core.data import default_structure as DS
from acab.core.data.instruction import ProductionOperator, ProductionStructure, ProductionContainer
from acab.core.decorators.semantic import RunInSubCtxSet
from acab.interfaces import semantic as SI
from acab.interfaces import value as VI
import acab.core.data.instruction as Instr
from acab.error.semantic import AcabSemanticException
from acab.modules.context.context_set import ContextSet
from acab.modules.context.context_instance import MutableContextInstance
from acab.modules.values.binding.binding import bind

from acab.core.semantics import basic

CtxIns = AT.CtxIns

# Primary Statements:
class QueryAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):
    """
    Very simply accumulate results of multiple sentences of queries
    """
    def verify(self, instruction) -> bool:
        return (isinstance(instruction, Instr.ProductionContainer) and
                all([isinstance(x, VI.Sentence_i) for x in query.clauses]) and
                all([DS.QUERY in x[-1].data for x in instruction.clauses]))

    def __call__(self, instruction, semSys, *, ctxs=None, data=None):
        query = instruction
        # Get the default semantics for use with all clauses
        spec = semSys.lookup()
        for clause in query.clauses:
            spec(clause, data=data, ctxs=ctxs)

class QueryPlusAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):
    """ A Query abstraction that can handle expanded query semantics.
    Eg: Walkers
    """

    def verify(self, instruction) -> bool:
        return (isinstance(instruction, Instr.ProductionContainer) and
                all([DS.QUERY in x[-1].data for x in instruction.clauses]))

    def __call__(self, instruction, semSys, *, ctxs=None, data=None):
        query = instruction
        for clause in query.clauses:
            # Get a different semantics for each clause
            spec   = semSys.lookup(clause)
            struct = spec.struct
            if struct is None:
                struct = semSys

            spec(clause, struct, data=data, ctxs=ctxs)


class TransformAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):
    """ Takes a context, returns a changed context """

    def verify(self, instruction) -> bool:
        return (isinstance(instruction, Instr.ProductionContainer) and
                all([isinstance(x, Instr.ProductionComponent) for x in instruction.clauses]))

    def __call__(self, instruction, semSys, *, ctxs=None, data=None):
        # Note: run *all* the transform clauses at once
        operators = ctxs._operators
        transform =  instruction
        for ctxIns in ctxs.active_list(clear=True):
            with MutableContextInstance(ctxs, ctxIns) as mutx:
                for clause in transform.clauses:
                    # TODO this should all be replaceable with just: op = mutx[clause.op]
                    if clause.op in operators:
                        op = operators[clause.op]
                    else:
                        op = mutx[clause.op]

                    params              = [bind(x, mutx) for x in clause.params]
                    result              = op(*params, data=mutx.data)
                    mutx[clause.rebind] = result

class ActionAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):
    """ *Consumes* a context, performing all actions in it """

    def verify(self, instruction) -> bool:
        return (isinstance(instruction, Instr.ProductionContainer) and
                all([isinstance(x, Instr.ProductionComponent) for x in instruction.clauses]))

    def __call__(self, instruction, semSys, *, ctxs=None, data=None):
        operators = ctxs._operators
        action    = instruction

        for ctx in ctxs.active_list(clear=True):
            for clause in action.clauses:
                # TODO handle statements to, like DFS
                if clause.op in operators:
                    op = operators[clause.op]
                else:
                    op = ctx[clause.op]


                params = [bind(x, ctx) for x in clause.params]
                result = op(*params, data=clause.data, semSystem=semSys)



class AtomicRuleAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):
    """ Run a rule in a single semantic call """

    def verify(self, instruction) -> bool:
        return (isinstance(instruction, Instr.ProductionStructure) and
                DS.QUERY_COMPONENT in instruction or
                DS.ACTION_COMPONENT in instruction)


    @RunInSubCtxSet
    def __call__(self, instruction, semsys, *, ctxs=None, data=None):
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

class ProxyRuleAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):
    """ Run a rules queries, then return ctxs bound
    with transform+action continuation """

    def verify(self, instruction) -> bool:
        return (isinstance(instruction, Instr.ProductionStructure) and
                DS.QUERY_COMPONENT in instruction and
                DS.ACTION_COMPONENT in instruction)

    def __call__(self, instruction, semsys, *, ctxs=None, data=None):
        if instruction in ctxs._named_sets:
            subctx = ctxs[instruction]
            self.run_continuations(instruction, semsys, ctxs=subctx)
        else:
            self.run_query(instruction, semsys, ctxs=ctxs, data=data)



    @RunInSubCtxSet
    def run_query(self, instruction, semsys, *, ctxs=None, data=None):
        logging.debug("Running Proxy Rule Queries")
        rule = instruction

        # Run the query
        if DS.QUERY_COMPONENT in rule:
            semsys(rule[DS.QUERY_COMPONENT], ctxs=ctxs)

        if not bool(ctxs):
            return

        ctxs.build_named_set(instruction, [x.uuid for x in ctxs.active_list()])

    def run_continuations(self, instruction, semsys, *, ctxs=None, data=None):
        logging.debug("Running Proxy Rule Continuations")

        if DS.TRANSFORM_COMPONENT in instruction:
            semsys(instruction[DS.TRANSFORM_COMPONENT],
                   ctxs=ctxs)

        if DS.ACTION_COMPONENT in instruction:
            semsys(instruction[DS.ACTION_COMPONENT],
                   ctxs=ctxs)


class ContainerAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):

    def verify(self, instruction) -> bool:
        return isinstance(instruction, Instr.ProductionContainer)


    def __call__(self, instruction, semsys, *, ctxs=None, data=None):
        """ Apply the clauses in one move """
        for x in instruction.clauses:
            ctxs = semsys(x, ctxs=ctxs)

        return ctxs
