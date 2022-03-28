from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as logmod
import pyparsing as pp
import re
from collections import defaultdict
import acab
config = acab.GET()

from acab.modules.repl.repl_commander import register
from acab.modules.repl import ReplParser as RP
from acab.core.data.instruction import ProductionOperator, ProductionStructure
from acab.modules.repl.util import print_contexts

logging = logmod.getLogger(__name__)


@register
def do_listen(self, line):
    """ Listeners:
    Listen for specific assertions / rule firings / queries,
    and pause on them
    """
    # TODO use the same strategy as semantic breakpoints
    logging.info(f"Listening: {line}")
    params = RP.listen_parser.parse_string(line)[:]
    words = [y for x in params for y in SPLIT_RE.split(x)]
    if params['type'] == "add":
        self.state.engine.add_listeners(*words)
    elif params['type'] == "remove":
        self.state.engine.remove_listeners(*words)
    elif params['type'] == "list":
        ctxs = []
        ctxs.append(" ".join(self.state.engine.get_listeners()))
        ctxs.append("Threshold: {}".format(self.state.engine.get_listener_threshold()))
        params["ctxs"] = "\n".join(ctxs)
    elif params['type'] == "threshold":
        params = SPLIT_RE.split((params[0]))
        self.state.engine.set_listener_threshold(int(params[0]), int(params[1]))



@register
def do_check(self, line):
    """
    Trigger an analysis action.
    eg: type checking
    """
    params = line
    logging.info(f"Checking: {line}")
    # TODO
    # If single statement, run analysis layer with statement inserted,
    # return types

    # else everything: run analysis layer




@register
def do_decompose(self, line):
    """
    Decompose binding into components.
    eg: rule -> queries, transforms, actions
    """
    params = line
    # TODO : split objects into tries
    # run query
    # split ctxs into bindings



@register
def do_step(self, line):
    """
    Control the engine state,
    Enabling rewinding to last saved rng position (ie: start of this pipeline loop)
    and stepping forwards by rule/layer/pipeline
    """
    logging.info(f"Stepping {line}")
    params = RP.step_parser.parse_string(line)[:]
    # TODO
    try:
        ctxs = []
        if "back" in params[0]:
            print("back")
        elif "rule" in params[0]:
            print(f"rule: {params[1]}")
        elif "layer" in params[0]:
            print("layer")
        elif "pipeline" in params[0]:
            print("pipeline")

        self.state.ctxs = "\n".join(ctxs)
    except Exception as err:
        logging.error(f"Failed to step: {line}")
        logging.error(f"{err}")
