#!/usr/bin/env python3
##-- imports
from __future__ import annotations
import logging as logmod
from collections import defaultdict
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

import acab.core.defaults.value_keys as DS
import acab.core.value.instruction as Instr
import acab
from acab import types as AT
from acab.core.semantics import basic
from acab.core.util.decorators.semantic import RunInSubCtxSet
from acab.core.util.sentences import ProductionComponent
from acab.core.value.instruction import (ProductionContainer,
                                         ProductionOperator,
                                         ProductionStructure)
from acab.error.semantic import AcabSemanticException
from acab.interfaces import semantic as SI
from acab.interfaces import value as VI
from acab.interfaces.bind import Bind_i
from acab.interfaces.context import ContextSet_i
from acab.modules.context.context_instance import MutableContextInstance

##-- end imports

CtxIns     = AT.CtxIns

config     = acab.config
# TODO import
ContextSet = config.imports.specific.context
Bind       = config.imports.specific.bind

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
            struct = spec.struct or semSys
            spec(clause, struct, data=data, ctxs=ctxs)


class TransformAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):
    """ Takes a context, returns a changed context """

    def verify(self, instruction) -> bool:
        return (isinstance(instruction, Instr.ProductionContainer) and
                all([x.type == "_:SENTENCE.COMPONENT.TRANSFORM" for x in instruction.clauses]))

    def __call__(self, instruction, semSys, *, ctxs=None, data=None):
        # Note: run *all* the transform clauses at once
        operators = ctxs._operators
        transform =  instruction
        for ctxIns in ctxs.active_list(clear=True):
            with MutableContextInstance(ctxIns, parent_set=ctxs) as mutx:
                for clause in transform.clauses:
                    assert(len(clause) == 3)
                    # TODO replace this with bind
                    if clause[0] in operators:
                        op = Bind.bind(clause[0], operators)
                    else:
                        op = Bind.bind(clause[0], mutx)

                    params = []
                    if clause[1][0] != "returns":
                        params = Bind.bind(clause[1], mutx)

                    result              = op(*params, data=mutx.data)
                    mutx[clause[-1][1].key()]    = result

class TransformPlusAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):
    """ Takes a context, returns a changed context """

    def verify(self, instruction) -> bool:
        return (isinstance(instruction, Instr.ProductionContainer) and
                all([x.type == "_:SENTENCE.COMPONENT.TRANSFORM" for x in instruction.clauses]))

    def __call__(self, instruction, semSys, *, ctxs=None, data=None):
        # Note: run *all* the transform clauses at once
        operators = ctxs._operators
        transform =  instruction
        for ctxIns in ctxs.active_list(clear=True):
            with MutableContextInstance(ctxIns, parent_set=ctxs) as mutx:
                for clause in transform.clauses:
                    if clause.type == "_:SENTENCE.COMPONENT.TRANSFORM":
                        self.basic_transform(clause, mutx)
                    else:
                        logging.info("Running Transform+ Semantics: {} : {}", clause.data[DS.SEMANTIC_HINT], clause)
                        semSys(clause, ctxs=[ctxIns])
                        # spec = semSys.lookup(clause)
                        # spec(clause, semSys, ctxs=[mutx])

    def basic_transform(self, clause, mutx):
        assert(len(clause) == 3)
        # TODO replace this with bind
        if clause[0] in operators:
            op = Bind.bind(clause[0], operators)
        else:
            op = Bind.bind(clause[0], mutx)

        params = []
        if clause[1] != "returns":
            params = Bind.bind(clause[1], mutx)

        result              = op(*params, data=mutx.data)
        mutx[clause[-1][1].key()]    = result


class ActionAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):
    """ *Consumes* a context, performing all actions in it """

    def verify(self, instruction) -> bool:
        return (isinstance(instruction, Instr.ProductionContainer) and
                all([x.type == "_:SENTENCE.COMPONENT" for x in instruction.clauses]))

    def __call__(self, instruction, semSys, *, ctxs=None, data=None):
        operators = ctxs._operators
        action    = instruction

        for ctx in ctxs.active_list(clear=True):
            for clause in action.clauses:
                # TODO handle statements to, like DFS
                if clause[0] in operators:
                    op = Bind.bind(clause[0], operators)
                else:
                    op = Bind.bind(clause[0], ctx)

                params = []
                if clause[1][0] != "returns":
                    params = Bind.bind(clause[1], ctx)

                try:
                    result = op(*params, data=clause.data, semSystem=semSys)
                except TypeError as err:
                    breakpoint()
                    logging.warning(err)



class ActionPlusAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):
    """ *Consumes* a context, performing all actions in it """

    def verify(self, instruction) -> bool:
        return (isinstance(instruction, Instr.ProductionContainer) and
                all([x.type == "_:SENTENCE.COMPONENT" for x in instruction.clauses]))

    def __call__(self, instruction, semSys, *, ctxs=None, data=None):
        operators = ctxs._operators
        action    = instruction

        for ctx in ctxs.active_list(clear=True):
            for clause in action.clauses:
                if clause.type == "_:SENTENCE.COMPONENT.ACTION":
                    self.basic_action(clause, ctx, operators, semSys)
                else:
                    # For sentences with semantics, like DFS
                    assert(DS.SEMANTIC_HINT in clause.data), breakpoint()
                    logging.info("Running Action+ Semantics: {} : {}", clause.data[DS.SEMANTIC_HINT], clause)
                    # TODO this is a hack, rewrite
                    spec = semSys.lookup(clause)
                    struct = spec.struct or semSys
                    subctx = ContextSet(ctxs._operators, _parent=ctxs)
                    subctx.pop()
                    subctx.push(ctx)
                    spec(clause, struct, data=data, ctxs=subctx)


    def basic_action(self, clause, ctx, operators, semSys):
        assert(clause.type == "_:SENTENCE.COMPONENT.ACTION")
        if clause[0] in operators:
            op = operators[clause[0]]
        else:
            op = ctx[clause[0]]

        params = []
        if clause[1][0] != "returns":
            params = Bind.bind(clause[1], ctx)

        assert(clause[-1] == "_:returns.unit")
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
        if DS.QUERY_COMPONENT in rule and bool(rule[DS.QUERY_COMPONENT]):
            semsys(rule[DS.QUERY_COMPONENT], ctxs=ctxs)

        if not bool(ctxs):
            return

        if DS.TRANSFORM_COMPONENT in rule and bool(rule[DS.TRANSFORM_COMPONENT]):
            semsys(rule[DS.TRANSFORM_COMPONENT], ctxs=ctxs)

        if DS.ACTION_COMPONENT in rule and bool(rule[DS.ACTION_COMPONENT]):
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

        # TODO invalidate the named set?


class ContainerAbstraction(basic.StatementSemantics, SI.StatementSemantics_i):

    def verify(self, instruction) -> bool:
        return isinstance(instruction, Instr.ProductionContainer)


    def __call__(self, instruction, semsys, *, ctxs=None, data=None):
        """ Apply the clauses in one move """
        for x in instruction.clauses:
            ctxs = semsys(x, ctxs=ctxs)

        return ctxs
