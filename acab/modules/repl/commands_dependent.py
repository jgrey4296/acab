from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import pyparsing as pp
import re
from collections import defaultdict
import acab
config = acab.setup()

from acab.modules.repl.repl_commander import register
from acab.modules.repl import ReplParser as RP
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure
from acab.modules.repl.util import print_contexts

logging = root_logger.getLogger(__name__)


@register
def do_listen(self, line):
    """ Listeners:
    Listen for specific assertions / rule firings / queries,
    and pause on them
    """
    # TODO use the same strategy as semantic breakpoints
    logging.info(f"Listening: {line}")
    params = RP.listen_parser.parseString(line)[:]
    words = [y for x in params for y in SPLIT_RE.split(x)]
    if params['type'] == "add":
        self.state.engine.add_listeners(*words)
    elif params['type'] == "remove":
        self.state.engine.remove_listeners(*words)
    elif params['type'] == "list":
        result = []
        result.append(" ".join(self.state.engine.get_listeners()))
        result.append("Threshold: {}".format(self.state.engine.get_listener_threshold()))
        params["result"] = "\n".join(result)
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
    # split result into bindings



@register
def do_step(self, line):
    """
    Control the engine state,
    Enabling rewinding to last saved rng position (ie: start of this pipeline loop)
    and stepping forwards by rule/layer/pipeline
    """
    logging.info(f"Stepping {line}")
    params = RP.step_parser.parseString(line)[:]
    # TODO
    try:
        result = []
        if "back" in params[0]:
            print("back")
        elif "rule" in params[0]:
            print(f"rule: {params[1]}")
        elif "layer" in params[0]:
            print("layer")
        elif "pipeline" in params[0]:
            print("pipeline")

        self.state.result = "\n".join(result)
    except Exception as err:
        logging.error(f"Failed to step: {line}")
        logging.error(f"{err}")
