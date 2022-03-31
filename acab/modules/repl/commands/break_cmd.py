#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

import pyparsing as pp

logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass


from acab.modules.repl.repl_commander import register_class
from acab.modules.repl.ReplParser import rst
from acab.modules.repl.util import ConfigBasedLoad


@register_class("break")
class BreakCmd:

    def __init__(self):
        self._parser = self._gen_parser()

    def _gen_parser(self):
        filename = pp.Word(pp.alphas + "/_") + pp.Literal(".py")
        filename.set_parse_action(lambda s,l,t: "".join(t[:]))

        linenum  = pp.Word(pp.nums)
        linenum.set_parse_action(lambda s,l,t: int(t.line))

        basic_bp     = filename("file") + pp.Suppress(pp.Keyword(":")) + linenum("line")

        semantic_bp  = pp.Keyword("semantic") + pp.Regex(".+?\?")("semantic")

        parser_bp    = pp.Keyword("parser") + rst("parser")

        break_parser = parser_bp | semantic_bp("semantic") | basic_bp("basic") | rst

        return break_parser


    @ConfigBasedLoad
    def __call__(self, line):
        """
        Control a bdb customization.
        Specified in Config [Module.Debug].

        break file line maybe_funcname
        break rule.name?

        break delete num
        break list

        # To break on semantic execution:

        """
        ctxs = self._parser.parse_string(line)
        # TODO refactor the basic/semantic logic into the debugger
        if "basic" in ctxs:
            bp_result = self._cmd.state.debugger.set_break(ctxs.file, ctxs.line)
            if bp_result is None:
                print(f"Breakpoint Set: {ctxs.file} : {ctxs.line}")
            else:
                print(f"Breakpoint NOT Set: {bp_result}")

        elif "semantic" in ctxs:
            # TODO breakpoint a semantic handler by name,
            # TODO breakpoint (in)dependent semantic function
            # TODO breakpoint a node by sentence path
            # TODO breakpoint an operator/action
            # TODO breakpoint a variable
            # run query
            self._cmd.state.ctxs = self._cmd.state.engine(ctxs.semantic)
            # attach semantic breakpoints to each prod_abstraction
            if len(self._cmd.state.ctxs) == 1 and isinstance(self._cmd.state.ctxs[0]._current.value, Instruction_i):
                curr = self._cmd.state.ctxs[0]._current.value
                curr.do_break()
                if curr.should_break:
                    print(f"Breakpoint Set: {repr(curr)} : {curr.uuid}")
                else:
                    print(f"Breakpoint Unset: {repr(curr)} : {curr.uuid}")
            elif bool(self._cmd.state.ctxs):
                count = 0
                for inst in self._cmd.state.ctxs:
                    for bind in inst.data.items():
                        if isinstance(bind, Instruction_i):
                            bind.do_break()
                            count += 1

                print(f"{count} Breakpoints Set: {ctxs.semantic}")

        elif "parser" in ctxs:
            # TODO add debug breakpoint to a parser
            # Either import and inspect, print all parser elements, if query
            if ctxs['parser'][-1] == "?":
                module = importlib.import_module(ctxs['parser'].strip()[:-1])
                parsers = [x for x in dir(module) if isinstance(getattr(module, x), pp.ParserElement)]

                if not bool(parsers):
                    return print("No Available Parsers to Breakpoint on.")

                print(f"Available Parsers in {ctxs['parser'][:-1]}")
                for x in parsers:
                    print(f"-- {x}")
                return

            # When specific, flip flop:
            mod_str, p_str = splitext(ctxs['parser'].strip())
            module = importlib.import_module(mod_str)
            if not hasattr(module, p_str[1:]) or not isinstance(getattr(module, p_str[1:]), pp.ParserElement):
                print(f"No parser named {p_str[1:]} in {mod_str}.")
                print("Try quering the  with a trailing '?'")
                return

            parser : pp.ParserElement = getattr(module, p_str[1:])
            turn_off = hasattr(parser._parse, "_originalParseMethod")
            if turn_off:
                print(f"Turning Breakpoint off for: {ctxs['parser']}")
                parser.set_break(False)
            else:
                print(f"Turning Breakpoint on for: {ctxs['parser']}")
                parser.set_break(True)
        else:
            print("""
            Shunting to Python debugger.
            Explore using: self._cmd.state, self._cmd.state.engine
            self is the break cmd,
            self._cmd is the repl,
            self._cmd.state is data the repl tracks,
            self._cmd.state.engine is the active ACAB engine,
            self._cmd.state.ctxs is the current context set
            """)
            self._cmd.state.debugger.set_trace()
