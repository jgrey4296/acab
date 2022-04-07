#!/usr/bin/env python3
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

rst = RP.rst

@register_class("forcep")
class ForceParserCmd:

    def __init__(self):
        self._parser = self._gen_parser()

    def _gen_parser(self):
        query          = pp.Word(pp.alphas + ".")("query") + pp.Suppress(pp.Literal("?"))
        send_to_parser = rst("send")
        force_parser = query + pp.Optional(send_to_parser)
        return force_parser

    
    def __call__(self, line):
        """ Force Parser:
        Query the bootstrap parser,
        and if supplied text, parse it and try to run it
        """
        try:
            # parse the line
            params = self._parser.parse_string(line)
            # Get the handler for the specified signal
            parser = self._cmd.state.engine._dsl[params.query]
            print(f"Retrieved: {parser}\n")
            if not bool(params.send):
                print("Nothing sent to parser")
                return

            print(f"Trying Parser on: {params.send}")
            # if exists, parse, then call engine on it
            built = parser.build()
            built.set_debug(True)
            forced_result = built.parse_string(params.send.strip(), parseAll=True)[:]
            built.set_debug(False)
            self._cmd.state.debug_data = forced_result
            print(f"----- Forced Parse Result: {forced_result}\n")

            if isinstance(forced_result, tuple):
                forced_result = forced_result[1]

            print("Attempting to execute result:")
            self._cmd.state.ctxs = self._cmd.state.engine(forced_result,
                                                          ctxset=self._cmd.state.ctxs)

        except pp.ParseException as err:
            traceback.print_tb(err.__traceback__)
            logging.warning(f"Parse Failure: {err.markInputline()}")
            logging.warning(err)
        except Exception as err:
            print("\n")
            traceback.print_tb(err.__traceback__)
            print("\n")
            logging.warning(f"Force Failure: {err}")
