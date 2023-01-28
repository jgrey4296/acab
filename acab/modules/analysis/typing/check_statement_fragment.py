#!/usr/bin/env python3
"""

"""
##-- imports
from __future__ import annotations

import logging as root_logger
from dataclasses import dataclass, field
from typing import Any, Callable

import acab.core.defaults.value_keys as DS
import acab.modules.values.binding.variable_control as VC
import pyparsing as pp
from acab import AcabConfig
from acab import types as AT
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.util.fragments import (DSL_Fragment, PrinterFragment,
                                      Semantic_Fragment)
from acab.core.util.part_implementations.handler_system import HandlerSpec
from acab.core.value import instruction as Ins
from acab.interfaces import data as DI
from acab.interfaces import fragments as FI
from acab.interfaces import handler_system as HS
from acab.interfaces import value as VI
from acab.interfaces.bind import Bind_i
from acab.interfaces.fragments import UnifiedFragment_p
from acab.interfaces.value import ValueFactory as VF

from .unify import type_unify_fns as tuf
from .unify.util import gen_f

##-- end imports

logging = root_logger.getLogger(__name__)
config = AcabConfig()
Handler = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": HS.Handler_i})()
Bind    = config.prepare("Imports.Targeted", "bind", actions=[config.actions_e.IMCLASS], args={"interface": Bind_i})()

DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

##-- signals
TYPE_INSTANCE = config.attr.Print.Signals.TYPE_INSTANCE
TYPE_DEF      = config.attr.Print.Signals.TYPE_DEF
SUM_TYPE      = config.attr.Print.Signals.SUM_TYPE
OP_DEF        = config.attr.Print.Signals.OP_DEF
TYPE_CLASS    = config.attr.Print.Signals.TYPE_CLASS
##-- end signals

comp_sen      = VF.sen()      << DS.SENTENCE_PRIM << DS.COMPONENT_PRIM
trans_comp    = comp_sen      << DS.TRANSFORM_COMPONENT

LinkSignalTo = lambda x, y: DSL_Spec(x, struct=y, flags=[DSL_Spec.flag_e.COLLECT])

@dataclass
class CheckStatementFragment(UnifiedFragment_p):
    """
    Prototype TypeCheckSemantics which provides the parser, printer,
    semantic call, and means to hook into acab
    """
    signal      : str                     = field(default="TYPE_CHECK")
    constructor : None|Callable[..., Any] = field(default=None)
    printer     : None|Callable[..., Any] = field(default=None)
    semantic    : None|Callable[..., Any] = field(default=None)

    def build_dsl(self) -> FI.DSL_Fragment_i:
        """
        return the DSL_Fragment describing this extension's parsing requirements and capabilities
        """
        constructor = self.constructor or self._build_instruction

        HOTLOAD_SEN = pp.Forward()

        TypingStatement= (pp.Keyword("⊢").suppress() + HOTLOAD_SEN("loc")
                          + pp.Optional(pp.Keyword("∈").suppress() + HOTLOAD_SEN("def")))
        TypingStatement.set_name("TypingStatment")
        TypingStatement.set_parse_action(constructor)
        TypingStatement.streamline()


        dsl_fragment = DSL_Fragment(specs=[LinkSignalTo("sentence", HOTLOAD_SEN)],
                                    handlers=[ppDSL.PyParse_Handler("transform.statement", func=TypingStatement)])

        return dsl_fragment

    def build_printers(self) -> FI.Printer_Fragment_i:
        """
        return the PrinterFragment describing this extension's printing requirements and capabilities
        """
        printer = self.printer or self.basic_printer

        print_fragment = PrinterFragment(specs=[HandlerSpec(self.signal)],
                                         handlers=[Handler(signal=self.signal, func=printer)])


        return print_fragment

    def build_semantics(self) -> FI.Semantic_Fragment_i:
        """
        return the Semantic_Fragment describing this extension's semantic handlers
        """
        sem_call = self.semantic or self.sem_call
        sem_frag = Semantic_Fragment(specs=[HandlerSpec(self.signal)],
                                     handlers=[Handler(signal=self.signal, func=sem_call)])
        return sem_frag


    def _build_instruction(self, toks) -> list[VI.Sentence_i]:
        """
        The default constructor for the extension's parser -> internal acab data
        """
        instr = VF.sen(data={DS.TYPE_INSTANCE: trans_comp << self.signal,
                             DS.SEMANTIC_HINT: self.signal}) << toks['loc'].copy(name="loc")
        if 'def' in toks:
            instr = instr << toks['def'].copy(name="def")
        return [instr]

    def basic_printer(self, value, top=None, data=None):
        """
        The default printing routine for the extension's internal acab data -> str
        """
        ret_list = ["⊢ ", value['loc'], " ∈ ", value['def']]
        return ret_list



    def sem_call(self, instruction:AT.Instruction, semSys:AT.SemanticSystem, *, ctxs:None|AT.CtxSet=None, data:None|dict[str,Any]=None) -> CtxSet:
        """
        The default semantic implementation for the extension
        Either:
        1) Checks the given node/sentence against the given type definition
        or
        2) Checks the given sentence container's contents against the system
        (whic will result in 1.)

         ⊢ [loc] : [def]
        """
        logging.info("Type Checking Entry")
        loc       = Bind.bind(instruction['loc'], ctxs[0])

        match loc.value:
            case VI.Sentence_i():
                # TODO either structural check two sentences, or check a sentence against the wm?
                raise NotImplementedError()
            case VI.Instruction_i():
                logging.info("Checking an Instruction: {!r}", loc.value)
                # Check each clause in the instruction
                assert('def' not in instruction)
                assert(isinstance(loc, Ins.Container))
                assert(hasattr(loc.value, "clauses"))

                for clause in loc.value.clauses:
                    match clause:
                        case VI.Sentence_i():
                            self.sen_check(clause, semSys, ctxs=ctxs, data=data)
                        case _:
                            self.op_check(clause, semSys, ctxs=ctxs, data=data)
                return ctxs
            case VI.Value_i():
                assert('def' in instruction)
                the_type  : TypeDefinition = Bind.bind(instruction['def'], ctxs[0])
                logging.info("Checking a binding: {!r} : {!r}", loc.value, the_type)
                # use a fresh and vars to avoid conflicts
                new_var   = gen_f()
                type_sens = [(new_var << x) for x in the_type] if bool(the_type) else [new_var]
                # TODO get to_check down to depth of max(type_sens)
                to_check  : list[VI.Sentence_i] = [VF.sen() << loc]
                return self.structural_check(to_check, type_sens, semSys, ctxs=ctxs, data=data)
            case _:
                raise TypeError("Unexpected Type provided", loc)


    def sen_check(self, clause, semSys:SemanticSystem, *, ctxs:None|CtxSet=None, data:None|dict[str,Any]=None) -> CtxSet:
        logging.debug("Checking a sentence")
        # check the sentence structure against known types in the WM

        # get any operator constraints in the sentence
        constraints = [(x, x.data[DS.CONSTRAINT]) for x in clause if DS.CONSTRAINT in x.data]
        # insert the source value into the tests

        # and op_check them
        for constraint in constraints:
            self.op_check(constraint, semSys, ctxs=ctxs, data=data)

    def op_check(self, clause, semSys:SemanticSystem, *, ctxs:None|CtxSet=None, data:None|dict[str,Any]=None) -> CtxSet:
        """ Usage check for Query/Transform/Action clauses
        query        : a.$b(cond, cond $x).c?
        query struct : a, a.$b?,  a.$b.c?
        query cond   : λcond $b -> bool, λcond $b $x -> bool
        transform    : λa.b.c $x $y -> $z
        action       : λa.b.c $x $y
        """
        assert(clause.type[:2] == "_:SENTENCE.COMPONENT")
        logging.debug("Checking an operator usage")
        # Get the operator
        sub_ctx = semSys(clause[0].copy(data={DS.QUERY: True}))
        assert(bool(sub_ctx))
        # coerce the operator definition,
        op_sens = sub_ctx[0].current_node.value[:]
        # and usage,
        use_sen = clause[1:]
        # the same format
        # then unify
        return self.structural_check([use_sen], op_sens, semSys, ctxs=ctxs, data=data)

    def structural_check(self,
                         to_check:list[VI.Sentence_i],
                         type_sens:list[VI.Sentence_i],
                         semSys:SemanticSystem,
                         *, ctxs:None|CtxSet=None,
                         data:None|dict[str,Any]=None) -> CtxIns:
        """
        unify a list of sentences against a list of applicable definition sentences
        """
        name_suff = data['suffix'] if data and 'suffix' in data else "_right"

        clean_var_names                       = {}
        clean_type_sens : list[VI.Sentence_i] = [VC.rectx(x, ctx=clean_var_names, name_suff=name_suff) for x in type_sens]
        unified                               = tuf.type_unify.repeat(to_check, clean_type_sens, ctxs[0])
        return unified
