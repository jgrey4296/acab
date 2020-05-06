"""
Utility functions for the REPL
"""
from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger

from py_rule.abstract.production_operator import ProductionOperator
from py_rule.abstract.agenda import Agenda
from py_rule.abstract.action import ActionOp
from py_rule.abstract.rule import Rule
from py_rule.abstract.layer import Layer

logging = root_logger.getLogger(__name__)

ReplE = Enum("Repl Commands", "NOP INIT LOAD SAVE PRINT RUN PASS STEP ACT DECOMPOSE LISTEN CHECK STATS HELP EXIT MULTILINE POP MODULE PROMPT")
repl_commands = {}

# utility functions
def register(cmd, fn, override=False):
    assert(isinstance(cmd, ReplE))
    assert(override or cmd not in repl_commands)
    repl_commands[cmd] = fn

def get(cmd):
    assert(isinstance(cmd, ReplE))
    return repl_commands[cmd]


def build_command(cmd_e, **kwargs):
    command_dict = {'command' : cmd_e}
    command_dict.update(kwargs)
    return command_dict


#--------------------

def engine_init(engine, data):
    params = data['params']
    logging.info("Initialising: {}".format(params))
    if not bool(params) or params[0] == "":
        params = ["py_rule.engines.trie_engine.TrieEngine"]
    init_module = importlib.import_module(splitext(params[0])[0])
    # build engine
    engine = eval('init_module{}'.format(splitext(params[0])[1]))()
    return engine, None

register(ReplE.INIT, engine_init)

def engine_module(engine, data):
    params = data['params']
    logging.info("Loading Module: {}".format(params))
    temp_module = importlib.import_module(params[0])
    engine.load_modules(temp_module.MODULE_SPEC)
    engine.reload_all_modules()
    return engine, None

register(ReplE.MODULE, engine_module)

def engine_load(engine, data):
    params = data['params']
    logging.info("Loading: {}".format(params))
    filename = abspath(expanduser(params[0]))
    assert(exists(filename))
    engine.load_file(filename)
    return engine, None

register(ReplE.LOAD, engine_load)

def engine_save(engine, data):
    params = data['params']
    logging.info("Saving State to: {}".format(params))
    filename = abspath(expanduser(params[0]))
    assert(exists(split(filename)[0]))
    engine.save_file(filename)
    return engine, None

register(ReplE.SAVE, engine_save)

def engine_print(engine, data):
    params = data['params']
    logging.info("Printing: {}".format(params))
    result = []
    if "wm" in params[0]:
        result.append("WM:")
        result.append(str(engine._working_memory))
    elif "bootstrap" in params[0]:
        result.append("Bootstrap Parser:")
        result.append(engine._working_memory._bootstrap_parser.print_trie())

    elif "module" in params[0]:
        result.append("Module: {}".format(params[1]))
        result.append(str(engine._loaded_modules[params[1]]))
    elif "layer" in params[0]:
        result.append("Layer: {}".format(params[1]))
        result.append(str(engine._pipeline[params[1]]))
    elif "pipeline" in params[0]:
        result.append("Pipeline: {}".format(params[1]))
        result.append(str(engine._pipeline))
    elif "binding" in params[0]:
        if len(params) > 1:
            if isinstance(params[1], int) and len(engine._cached_bindings) <= params[1]:
                result.append("Bindings: {} Out of Bounds".format(params[1]))
            else:
                result.append("Bindings: {}".format(params[1]))
                result.append(str(engine._cached_bindings[params[1]]))
        else:
            result.append("Bindings: ")
            result.append("\n".join([str(x) for x in engine._cached_bindings]))
    else:
        result.append("Querying: {}")

    data['result' ] = "\n".join(result)
    return engine, data

register(ReplE.PRINT, engine_print)

def engine_run(engine, data):
    params = data['params']
    result = None
    logging.info("Running: {}".format(params))
    # query
    if not bool(params):
        result = engine.tick()
        data['result'] = result
        return engine, data

    query_result = engine.query(params[-1])
    assert(len(query_result) == 1)

    # Get var
    value = query_result[0][params[-2][1]]

    result = engine.run_thing(value)

    data['result'] = result
    return engine, data

register(ReplE.RUN, engine_run)

def engine_pass(engine, data):
    params = data['params']
    logging.info("Passing sentences through: {}".format(params))
    # Determine query / assertion
    # FIXME: Simple hack for now
    if params[0].strip()[-1] == "?":
        result = engine.query(*params)
    else:
        result = engine.add(*params)

    data['result'] = result
    return engine, data

register(ReplE.PASS, engine_pass)

def engine_decompose(engine, data):
    params = data['params']
    # TODO
    # run query
    # split result into bindings
    return engine, None

register(ReplE.DECOMPOSE, engine_decompose)

def engine_step(engine, data):
    params = data['params']
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

    data['result'] = "\n".join(result)
    return engine, data

register(ReplE.STEP, engine_step)

def engine_act(engine, data):
    params = data['params']
    logging.info("Manually acting: {}".format(params))
    # TODO
    result = []
    # Parse the act / retrieve the act

    # combine with a binding

    # call act

    data['result'] = "\n".join(result)
    return engine, data

register(ReplE.ACT, engine_act)

def engine_listen(engine, data):
    params = data['params']
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

def engine_type_check(engine, data):
    params = data['params']
    logging.info("Type Checking: {}".format(params))
    # TODO
    # If single statement, run analysis layer with statement inserted, return types



    # else everything: run analysis layer

    return engine, None

register(ReplE.CHECK, engine_type_check)

def engine_stats(engine, data):
    params = data['params']
    logging.info("Getting Stats: {}".format(params))
    allow_all = not bool(params)
    result = []
    # Operators
    if allow_all or "operator" in params:
        result.append("Operator Stats: ")
        result.append("\t{}".format("\n\t".join([str(x) for x in ProductionOperator.op_dict])))

    # agendas
    if allow_all or "agenda" in params:
        result.append("--------------------")
        result.append("Agenda Stats: ")
        # TODO: need to query for agendas

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

    # TODO add parser stats/trie

    result.append("")
    data['result'] = "\n".join(result)
    return engine, data

register(ReplE.STATS, engine_stats)

def engine_help(engine, data):
    params = data['params']
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

    data['result'] = "\n".join(result)
    return engine, data

register(ReplE.HELP, engine_help)

def engine_prompt(engine, data):
    data['prompt'] = data['params'][0]

    return engine, data

register(ReplE.PROMPT, engine_prompt)

def engine_exit(engine, data):
    logging.info("Quit Command")
    filename = "{}_{}.auto".format(engine.__class__.__name__,
                                   datetime.now().strftime("%Y_%m-%d_%H_%M"))
    engine.save_file(filename)
    return engine, None

register(ReplE.EXIT, engine_exit)

def engine_multi(engine, data):
    param = data['params'][0]
    if param:
        # Start
        logging.info("Activating multi line")
        data['in_multi_line'] = True
        data['prompt_bkup'] = data['prompt']
        data['prompt'] = data['prompt_ml']
        return engine, data
    else:
        logging.info("Deactivating multi line")
        data['in_multi_line'] = False
        data['params'] = ["\n".join(data['collect_str'])]
        data['collect_str'] = []
        data['current_str'] = ""
        data['prompt'] = data['prompt_bkup']
        return engine_pass(engine, data)

register(ReplE.MULTILINE, engine_multi)

def engine_multi_pop(engine, data):
    data['collect_str'].pop()
    return engine, data

register(ReplE.POP, engine_multi_pop)

def engine_nop(engine, data):
    if data['in_multi_line']:
        data['collect_str'].append(data['current_str'])
        logging.info("Collecting: {}".format(data['collect_str']))

    return engine, data

register(ReplE.NOP, engine_nop)

#---------------------
