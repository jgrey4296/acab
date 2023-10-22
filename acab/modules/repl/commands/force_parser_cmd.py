#!/usr/bin/env python3
##-- imports
from __future__ import annotations

import abc
import logging as logmod
import traceback
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass

import pyparsing as pp
from acab.modules.repl import ReplParser as RP
from acab.modules.repl.repl_commander import register_class
from acab.interfaces.handler_system import HandlerSpec_i
from acab.core.parsing.debug_funcs import dfs_activate

##-- end imports

rst = RP.rst

@register_class("forcep")
class ForceParserCmd:
    """ Force Parser:
    Query the bootstrap parser,
    and if supplied text, parse it and try to run it
    """

    def __init__(self):
        self._parser = self._gen_parser()

    def _gen_parser(self):
        query          = pp.Word(pp.alphas + ".")("query") + pp.Suppress(pp.Literal("?"))
        send_to_parser = rst("send")
        force_parser = query + pp.Optional(send_to_parser)
        return force_parser

    def __call__(self, line):
        try:
            # parse the line
            params = self._parser.parse_string(line, parse_all=True)
        except pp.ParseException as err:
            print("Unrecognised Option: ", err.pstr)
            print(self.__doc__)
            return

        try:
            # Get the handler for the specified signal
            parser = self._repl.state.engine._dsl[params.query]
            print(f"Retrieved: {parser}\n")
            if not bool(params.send):
                print("Nothing sent to parser")
                return

            print(f"Trying Parser on: {params.send}")
            # if exists, parse, then call engine on it
            if isinstance(parser, HandlerSpec_i):
                built = parser.build()
            else:
                assert(hasattr(parser.func, "parse_string"))
                built = parser.func

            dfs_activate(built)
            forced_result = built.parse_string(params.send.strip(), parseAll=True)[:]
            dfs_activate(built, remove=True)

            self._repl.state.debug_data = forced_result
            print(f"----- Forced Parse Result: {forced_result}\n")

            if isinstance(forced_result, tuple):
                forced_result = forced_result[1]

            answer = input("Execute result? Y/* ")
            if answer == "Y":
                print("Attempting to execute result:")
                self._repl.state.ctxs = self._repl.state.engine(forced_result,
                                                              ctxset=self._repl.state.ctxs)
            else:
                print("Not Executing result")

        except pp.ParseException as err:
            traceback.print_tb(err.__traceback__)
            logging.warning(f"Parse Failure: {err.markInputline()}")
            logging.warning(err)
        except Exception as err:
            print("\n")
            traceback.print_tb(err.__traceback__)
            print("\n")
            logging.warning(f"Force Failure: {err}")
