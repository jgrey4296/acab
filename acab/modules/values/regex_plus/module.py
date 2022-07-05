#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.defaults.value_keys as DS
import pyparsing as pp
from acab import AcabConfig
from acab import types as AT
from acab.core.defaults import value_keys as CDS
from acab.core.parsing import consts as PConst
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.util.annotation import ModalAnnotation, ValueAnnotation
from acab.core.parsing.consts import (CPAR, DBLCOLON, NG, OPAR, TAG, N,
                                      component_gap, emptyLine, gap, ln, op,
                                      opLn, orm, s, s_key, s_lit, zrm)
from acab.core.util.decorators.semantic import (OperatorArgUnWrap,
                                                OperatorDataUnWrap,
                                                OperatorResultWrap,
                                                OperatorSugar)
from acab.core.util.fragments import (DSL_Fragment, PrinterFragment,
                                      Semantic_Fragment)
from acab.core.util.part_implementations.handler_system import HandlerSpec
from acab.core.value import instruction as Ins
from acab.core.value.instruction import ProductionOperator
from acab.core.value.sentence import Sentence
from acab.interfaces import data as DI
from acab.interfaces import fragments as FI
from acab.interfaces import handler_system as HS
from acab.interfaces import value as VI
from acab.interfaces.bind import Bind_i
from acab.interfaces.fragments import UnifiedFragment_p
from acab.interfaces.value import ValueFactory
from acab.interfaces.value import ValueFactory as VF

logging = logmod.getLogger(__name__)
config = AcabConfig()


config = AcabConfig()
LinkSignalTo = lambda x, y: DSL_Spec(x, struct=y, flags=[DSL_Spec.flag_e.COLLECT])
Handler = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": HS.Handler_i})()

@dataclass
class RegexPlusFragment(UnifiedFragment_p):
    """
    An improved regex operator over the core version.
    handles matching and substitution
    and global/case insensitive options


    """
    signal      : str                     = field(default=VF.sen(["REGEX", "PLUS"]))
    constructor : None|Callable[..., Any] = field(default=None)
    printer     : None|Callable[..., Any] = field(default=None)
    semantic    : None|Callable[..., Any] = field(default=None)


    @dataclass(repr=False)
    class RegexPlusInternal:
        match : str
        sub   : str       = field(default=None)
        opts   : list[str] = field(default_factory=list)

        def __repr__(self):
            return f"/{self.match}/{self.sub}/{self.opts}"

    #endof regexplusinternal
    def __post_init__(self):
        VI.Value_i.extend_core(self.RegexPlusInternal)


    def build_dsl(self) -> FI.DSL_Fragment_i:
        """
        return the DSL_Fragment describing this extension's parsing requirements and capabilities
        """
        constructor = self.constructor or self._build_instruction

        REGEX_plus_parser = pp.Regex(r'/.+?/')("match") + op(pp.Regex(r'.+?/')("sub") + op(pp.Word("gi")('opts')))
        REGEX_plus_parser.set_parse_action(constructor)

        dsl_fragment = DSL_Fragment(handlers=[ppDSL.PyParse_Handler("word.value", func=REGEX_plus_parser)])

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




    def _build_instruction(self, s, l, toks):
        type_sen = self.signal
        value = self.RegexPlusInternal(re.compile(toks.match[1:-1]),
                                       toks.sub[:-1] or None,
                                       toks.opts or [])

        return (type_sen, value)

    def basic_printer(self, value, top=None, data=None):
        """
        The default printing routine for the extension's internal acab data -> str
        """
        ret_list = ["/"]
        ret_list.append(value.value.match.pattern)
        if value.value.sub is not None:
            ret_list += [ "/", value.value.sub, "/" ]
            if bool(value.value.opts):
                ret_list += [ value.value.opts ]
        else:
             ret_list.append("/")



        return ret_list



    def sem_call(self, instruction:AT.Instruction, semSys:AT.SemanticSystem, *, ctxs:None|AT.CtxSet=None, data:None|dict[str,Any]=None) -> CtxSet:
        pass



@OperatorSugar(config.attr.Operator.Sugar.REGEX_MATCH)
class RegMatch(ProductionOperator):

    # TODO implement sub-binds
    # currently they are ignored
    def __call__(self, a:Value, b:Value[RegexPlusInternal], *, data=None, ctx=None):
        result = re.search(b.value.match, a.value)
        if result is not None:
            result = result.groupdict()
        if result is not None and not bool(result):
            result = True
        return result

@OperatorSugar(config.attr.Operator.Sugar.REGEX_TRANSFORM)
class RegexOp(ProductionOperator):

    def __call__(self, value, pattern, *, data=None, ctx=None):
        """ Substitute value pattern with value value from passed in data
        value : the replacement
        pattern: the pattern

        sub(pattern, replacement, string, count, flags)
        """
        match = pattern.value.match
        replacement = pattern.value.sub
        match value:
            case VI.Sentence_i():
                words = [re.sub(match, replacement, x.value) for x in value.words]
                return value.copy(value=words)
            case VI.Value_i():
                subbed = re.sub(match, replacement, value.value)
                return value.copy(value=subbed, name=subbed)
            case _:
                raise TypeError("Unrecognized type to RegexOp", value)
