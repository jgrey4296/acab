"""
Utility functions for the REPL
"""
from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger

from py_rule.abstract.agenda import Agenda
from py_rule.agendas import *
from py_rule.abstract.action import ActionOp

logging = root_logger.getLogger(__name__)

ReplE = Enum("Repl Commands", "INIT LOAD SAVE PRINT RUN PASS STEP ACT DECOMPOSE LISTEN CHECK STATS HELP EXIT MULTILINE MODULE PROMPT")
repl_commands = {}

# utility functions
def register(cmd, fn, override=False):
    assert(isinstance(cmd, ReplE))
    assert(override or cmd not in repl_commands)
    repl_commands[cmd] = fn

def get(cmd):
    assert(isinstance(cmd, ReplE))
    return repl_commands[cmd]

#--------------------

def engine_init(engine, params):
    logging.info("Initialising: {}".format(params))
    init_module = importlib.import_module(splitext(params[0])[0])
    # build engine
    engine = eval('init_module{}'.format(splitext(params[0])[1]))()
    return engine, None

register(ReplE.INIT, engine_init)

def engine_module(engine, params):
    logging.info("Loading Module: {}".format(params))
    temp_module = importlib.import_module(params[0])
    engine.load_modules(temp_module.MODULE_SPEC)
    return engine, None

register(ReplE.MODULE, engine_module)

def engine_load(engine, params):
    logging.info("Loading: {}".format(params))
    filename = abspath(expanduser(params[0]))
    assert(exists(filename))
    engine.load_file(filename)
    return engine, None

register(ReplE.LOAD, engine_load)

def engine_save(engine, params):
    logging.info("Saving State to: {}".format(params))
    filename = abspath(expanduser(params[0]))
    assert(exists(filename))
    engine.save_file(filename)
    return engine, None

register(ReplE.SAVE, engine_save)

def engine_print(engine, params):
    logging.info("Printing: {}".format(params))
    result = []
    if "wm" in params:
        result.append("WM:")
        result.append(str(engine._working_memory))
    elif "module" in params:
        result.append("Module: {}".format(params[1]))
        result.append(str(engine._loaded_modules[params[1]]))
    elif "layer" in params:
        result.append("Layer: {}".format(params[1]))
        result.append(str(engine._pipeline[params[1]]))
    elif "pipeline" in params:
        result.append("Pipeline: {}")
        result.append(str(engine._pipeline))
    elif "bindings" in params:
        result.append("Bindings: {}")
        result.append(str(engine._cached_bindings[params[1]]))
    else:
        result.append("Querying: {}")

    return engine, "\n".join(result)

register(ReplE.PRINT, engine_print)

def engine_run(engine, params):
    logging.info("Running: {}".format(params))
    # TODO
    # query

    # determine type

    # run type (rule, layer, pipeline)
    result = engine.run_layer(params)
    return engine, result

register(ReplE.RUN, engine_run)

def engine_pass(engine, params):
    logging.info("Passing sentences through: {}".format(params))
    # Determine query / assertion
    # FIXME: Simple hack for now
    if params[0].strip()[-1] == "?":
        result = engine.query(*params)
    else:
        result = engine.add(*params)
    return engine, result

register(ReplE.PASS, engine_pass)

def engine_decompose(engine, params):
    # TODO
    # run query
    # split result into bindings
    return engine, None

register(ReplE.DECOMPOSE, engine_decompose)

def engine_step(engine, params):
    logging.info("Stepping {}".format(params))
    # TODO
    result = []
    if "back" in params[0]:
        print("back")
    elif "rule" in params[0]:
        print("rule")
    elif "layer" in params[0]:
        print("layer")
    elif "pipeline" in params[0]:
        print("pipeline")

    return engine, "\n".join(result)

register(ReplE.STEP, engine_step)

def engine_act(engine, params):
    logging.info("Manually acting: {}".format(params))
    # TODO
    result = []
    # Parse the act / retrieve the act

    # combine with a binding

    # call act

    return engine, "\n".join(result)

register(ReplE.ACT, engine_act)

def engine_listen(engine, params):
    """ Listeners:
    Listen for specific assertions / rule firings / queries,
    and pause on them
    """
    logging.info("Listener: {}".format(params))
    # TODO
    # if add listener:

    # if remove listener

    # if list listeners

    return engine, None

register(ReplE.LISTEN, engine_listen)

def engine_type_check(engine, params):
    logging.info("Type Checking: {}".format(params))
    # TODO
    # If single statement, run analysis layer with statement inserted, return types



    # else everything: run analysis layer

    return engine, None

register(ReplE.CHECK, engine_type_check)

def engine_stats(engine, params):
    logging.info("Getting Stats: {}".format(params))
    allow_all = not bool(params)
    result = []
    # actions
    if allow_all or "action" in params:
        result.append("Action Stats: ")
        result.append("\t{}".format("\n\t".join([str(x) for x in ActionOp.op_list])))

    # agendas
    if allow_all or "agenda" in params:
        result.append("--------------------")
        result.append("Agenda Stats: ")
        result.append("\t{}".format("\n\t".join([str(x) for x in Agenda.agenda_list])))

    # rules
    if allow_all or "rule" in params:
        result.append("--------------------")
        result.append("Rule Stats: ")


    # pipeline
    if allow_all or "pipeline" in params:
        result.append("--------------------")
        result.append("Pipeline Stats: ")
        result.append(str(engine._pipeline))

    # layers
    if (allow_all or "layer" in params) and engine._pipeline is not None:
        result.append("--------------------")
        result.append("Layer Stats: ")
        result.append("\t{}".format("\n\t".join([str(x) for x in engine._pipeline._layers])))

    # modules
    if allow_all or "module" in params:
        result.append("--------------------")
        result.append("Module Stats: ")
        result.append("\t{}".format("\n\t".join([x for x in engine._loaded_modules])))

    # WM stats
    if allow_all or "wm" in params:
        result.append("--------------------")
        result.append("WM Stats: ")
        result.append(str(engine._working_memory))

    # Bindings
    if allow_all or "binding" in params:
        result.append("--------------------")
        result.append("Binding Stats: ")
        # TODO possibly filter out at binds
        bind_groups = engine._cached_bindings[:]
        result.append("\t{}".format("\n\t".join([str(x) for x in bind_groups])))

    result.append("")
    return engine, "\n".join(result)

register(ReplE.STATS, engine_stats)

def engine_help(engine, params):
    logging.info("Printing Help")
    # TODO
    result = []
    # Print commands
    result.append("-------------------- PyRule Help:")
    result.append("---------- Args:")
    result.append("--engine                          : Specify a load engine.")
    result.append("                                    Defaults to py_rule.engines.trie_engine.TrieEngine")
    result.append("-v {LEVEL} | --verbose {LEVEL}    : Specify Log level. File Level is one lower")

    result.append("")
    result.append("---------- Commands:")
    result.append("\tany.valid.sentence     : Assert")
    result.append("\tany.value.sentence?    : Query")

    result.append("\tquit                   : Quit the interpreter")
    result.append("\tstats                  : Print interpreter stats")

    result.append("\tinit {a.class}         : Initialise a new engine")
    result.append("\tmodule {a.module}      : Add a new module to the engine")

    result.append("\tload {file}            : Load a file into the engine")
    result.append("\tsave {file}            : Save a file to the engine")

    result.append("\tprint                  : TODO print")
    result.append("\trun                    : TODO run")
    result.append("\tact                    : TODO act")
    result.append("\tlisten et al           : TODO listen")
    result.append("\tcheck                  : TODO check")

    result.append("\thelp                   : Print this help")

    return engine, "\n".join(result)

register(ReplE.HELP, engine_help)

def engine_exit(engine, params):
    logging.info("Quit Command")
    filename = "{}_{}.auto".format(engine.__class__.__name__,
                                   datetime.now().strftime("%Y_%m-%d_%H_%M"))
    engine.save_file(filename)
    return engine, None

register(ReplE.EXIT, engine_exit)

#---------------------
