"""
Commands for the REPL

"""
import importlib
import logging as root_logger
import re
import traceback
from datetime import datetime
from enum import Enum
from os.path import abspath, exists, expanduser, split, splitext

import acab
import pyparsing as pp

config = acab.setup()

from acab.abstract.core.production_abstractions import (ProductionOperator,
                                                        ProductionStructure)
from acab.abstract.interfaces.value import Statement_i
from acab.modules.repl import ReplParser as RP
from acab.modules.repl.repl_commander import register
from acab.modules.repl.util import ConfigBasedLoad
from acab.modules.semantics.context_set import ContextSet

logging = root_logger.getLogger(__name__)


@register
def do_prompt(self, line):
    """
    Change the prompt of the repl
    """
    self.state.prompt = line.strip()

@register
def do_multi(self, line):
    """
    Activate multi-line collation
    """
    if not self.state.in_multi_line:
        # Start
        logging.info("Activating multi line")
        self.state.in_multi_line = True
        self.state.collect_str = []
        self.state.prompt_bkup = self.state.prompt
        self.state.prompt = self.state.prompt_ml
    else:
        logging.info("Deactivating multi line")
        collected = "\n".join(self.state.collect_str)
        self.state.in_multi_line = False
        self.state.prompt = self.state.prompt_bkup
        logging.info(f"Collected: {collected}")
        if bool(line):
            self.onecmd(line + " " + collected)
        else:
            self.onecmd(collected)


@register
def do_pop(self, line):
    """
    Pop off the last string added in multi-line mode,
    for when an error was made
    """
    self.state.collect_str.pop()
    logging.info(f"Collecting: {self.state.collect_str}")

@register
def do_collect(self, line):
    """ Add a line to the multi line collection,
    ready to be used as one statement when multi line is closed """
    assert(self.state.in_multi_line)
    self.state.collect_str.append(line)
    logging.info("Collecting: {}".format(self.state.collect_str))

@register
def do_echo(self, line):
    """
    Toggle echoing of working memory state
    """
    self.state.echo = not self.state.echo

@register
@ConfigBasedLoad
def do_break(self, line):
    """
    Control a bdb customization.
    Specified in Config [Module.Debug].

    break file line maybe_funcname
    break rule.name?

    break delete num
    break list

    # To break on semantic execution:

    """
    ctxs = RP.break_parser.parseString(line)
    # TODO refactor the basic/semantic logic into the debugger
    if "basic" in ctxs:
        bp_result = self.state.debugger.set_break(ctxs.file, ctxs.line)
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
        self.state.ctxs = self.state.engine(ctxs.semantic)
        # attach semantic breakpoints to each prod_abstraction
        if len(self.state.ctxs) == 1 and isinstance(self.state.ctxs[0]._current.value, Statement_i):
            curr = self.state.ctxs[0]._current.value
            curr.do_break()
            if curr.should_break:
                print(f"Breakpoint Set: {repr(curr)} : {curr.uuid}")
            else:
                print(f"Breakpoint Unset: {repr(curr)} : {curr.uuid}")
        elif bool(self.state.ctxs):
            count = 0
            for inst in self.state.ctxs:
                for bind in inst.data.items():
                    if isinstance(bind, Statement_i):
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
            parser.setBreak(False)
        else:
            print(f"Turning Breakpoint on for: {ctxs['parser']}")
            parser.setBreak(True)
    else:
        print("""
        Shunting to Python debugger.
        Explore using: self.state, self.state.engine
        self is the repl,
        self.state is data the repl tracks,
        self.state.engine is the active ACAB engine,
        self.state.ctxs is the current context set
        """)
        self.state.debugger.set_trace()


@register
def do_ctx(self, line):
    """ Control contexts.
    Select a subset using a slice '[1:-2]'
    Clear the context with '-'
    """
    try:
        params = RP.ctx_select_parser.parseString(line)

        if "subset" in params:
            result = self.state.ctxs[params.subset]
            if not isinstance(result, ContextSet):
                self.state.ctxs = self.state.ctxs.subctx([result])

        elif "clear" in params:
            print("Clearing Context")
            self.state.ctxs = None
        else:
            self.onecmd("print_ctx")

    except pp.ParseException as err:
        logging.error(f"Failed to select context: {err.markInputline()}")


@register
def do_forcep(self, line):
    """ Force Parser:
    Query the bootstrap parser,
    and if supplied text, parse it and try to run it
    """
    try:
        # parse the line
        params = RP.force_parser.parseString(line)
        # query the bootstrapper for the parser
        parser = self.state.engine._dsl_builder._bootstrap_parser.query(params.query)
        print(f"Retrieved: {parser}\n")
        if not bool(params.send):
            print("Nothing sent to parser")
            return

        print(f"Trying Parser on: {params.send}")
        # if exists, parse, then call engine on it
        parser.setDebug(True)
        forced_result = parser.parseString(params.send.strip(), parseAll=True)[:]
        parser.setDebug(False)
        self.state.debug_data = forced_result
        print(f"Forced Parse: {forced_result}\n")

        if isinstance(forced_result, tuple):
            forced_result = forced_result[1]

        self.state.ctxs = self.state.engine(forced_result,
                                            ctxset=self.state.ctxs)

    except pp.ParseException as err:
        traceback.print_tb(err.__traceback__)
        logging.warning(f"Parse Failure: {err.markInputline()}")
        logging.warning(err)
    except Exception as err:
        print("\n")
        traceback.print_tb(err.__traceback__)
        print("\n")
        logging.warning(f"Force Failure: {err}")
