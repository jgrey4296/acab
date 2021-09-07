"""
Commands for the REPL

"""
from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import re
import pyparsing as pp
import traceback
import acab
config = acab.setup()

from acab.modules.repl.repl_commander import register
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure
from acab.modules.repl import ReplParser as RP
from acab.abstract.interfaces.value import Statement_i
from acab.modules.repl.util import ConfigBasedLoad

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
    result = RP.break_parser.parseString(line)
    # TODO refactor the basic/semantic logic into the debugger
    if "basic" in result:
        bp_result = self.state.debugger.set_break(result.file, result.line)
        if bp_result is None:
            print(f"Breakpoint Set: {result.file} : {result.line}")
        else:
            print(f"Breakpoint NOT Set: {bp_result}")

    elif "semantic" in result:
        # TODO breakpoint a semantic handler by name,
        # TODO breakpoint (in)dependent semantic function
        # TODO breakpoint a node by sentence path
        # TODO breakpoint an operator/action
        # TODO breakpoint a variable
        # run query
        self.state.result = self.state.engine(result.semantic)
        # attach semantic breakpoints to each prod_abstraction
        if len(self.state.result) == 1 and isinstance(self.state.result[0]._current.value, Statement_i):
            curr = self.state.result[0]._current.value
            curr.do_break()
            if curr.should_break:
                print(f"Breakpoint Set: {repr(curr)} : {curr.uuid}")
            else:
                print(f"Breakpoint Unset: {repr(curr)} : {curr.uuid}")
        elif bool(self.state.result):
            count = 0
            for inst in self.state.result:
                for bind in inst.data.items():
                    if isinstance(bind, Statement_i):
                        bind.do_break()
                        count += 1

            print(f"{count} Breakpoints Set: {result.semantic}")

    elif "parser" in result:
        # TODO add debug breakpoint to a parser
        pass
    else:
        print("""
        Shunting to Python debugger.
        Explore using: self.state, self.state.engine
        self is the repl,
        self.state is data the repl tracks,
        self.state.engine is the active ACAB engine,
        self.state.result is the current context container
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
            self.state.result = self.state.result.__getitem__(params.subset, wrap=True)
        elif "clear" in params:
            print("Clearing Context")
            self.state.result = None
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

        self.state.result = self.state.engine(forced_result,
                                        bindings=self.state.result)

    except pp.ParseException as err:
        traceback.print_tb(err.__traceback__)
        logging.warning(f"Parse Failure: {err.markInputline()}")
        logging.warning(err)
    except Exception as err:
        print("\n")
        traceback.print_tb(err.__traceback__)
        print("\n")
        logging.warning(f"Force Failure: {err}")
