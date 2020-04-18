"""
Utility functions for the REPL
"""
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
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
    # TODO
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
    # TODO
    assert(exists(filename))
    engine.save_file(filename)
    return engine, None

register(ReplE.SAVE, engine_save)

def engine_print(engine, params):
    logging.info("Printing: {}".format(params))
    # TODO
    # if kb

    # if bindings

    # modules/layers/pipeline

    # else sentence/query

    result = None
    return engine, result

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
    # TODO
    # Determine query / assertion
    engine.add(*params)
    return engine, None

register(ReplE.PASS, engine_pass)

def engine_step(engine, params):
    logging.info("Stepping {}".format(params))
    # Step back layer
    # TODO

    # step rule forward

    # step layer foward

    # step pipeline forward

    return engine, None

register(ReplE.STEP, engine_step)

def engine_act(engine, params):
    logging.info("Manually acting: {}".format(params))
    # TODO
    # Get act

    # populate act with bindings

    # call act
    return engine, None

register(ReplE.ACT, engine_act)

def engine_listen(engine, params):
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
    # If single statement

    # else everything

    return engine, None

register(ReplE.CHECK, engine_type_check)

def engine_stats(engine, params):
    logging.info("Getting Stats: {}".format(params))
    # TODO
    # KB stats

    # actions

    # rules

    # layers

    # modules

    return engine, None

register(ReplE.STATS, engine_stats)

def engine_help(engine, params):
    logging.info("Printing Help")
    # TODO
    # Print commands
    return engine, None

register(ReplE.HELP, engine_help)

def engine_exit(engine, params):
    logging.info("Quit Command")
    # TODO
    # auto save
    return engine, None

register(ReplE.EXIT, engine_exit)

#---------------------
