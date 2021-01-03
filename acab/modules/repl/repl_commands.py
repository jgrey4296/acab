"""
Commands for the REPL

"""
from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import re

from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure

logging = root_logger.getLogger(__name__)

ReplE = Enum("Repl Commands", "NOP INIT LOAD SAVE PRINT RUN PASS STEP ACT DECOMPOSE LISTEN CHECK STATS HELP EXIT MULTILINE POP MODULE PROMPT ECHO BREAK")

SPLIT_RE = re.compile("[ .!?/]")

repl_commands = {}

# utility functions
def register(cmd, fn, override=False):
    """
    Register a function with its ReplE, so the repl parser can find it
    """
    assert(isinstance(cmd, ReplE))
    assert(override or cmd not in repl_commands)
    repl_commands[cmd] = fn

def get(cmd):
    """
    Retrieve a repl command for use
    """
    assert(isinstance(cmd, ReplE))
    return repl_commands[cmd]


def build_command(cmd_e, **kwargs):
    """
    Package parameters up for use in a command
    """
    assert(isinstance(cmd_e, ReplE))
    command_dict = {'command' : cmd_e}
    command_dict.update(kwargs)
    return command_dict


#--------------------

def engine_init(engine, data):
    """
    Specify the Engine to initialise.
    Imports the module, and uses the final component as the Engine Class.
    eg: acab.engines.trie_engine.TrieEngine -> TrieEngine
    """
    params = data['params']
    logging.info("Initialising: {}".format(params))
    if not bool(params) or params[0] == "":
        params = ["acab.engines.trie_engine.TrieEngine"]

    init_module = importlib.import_module(splitext(params[0])[0])
    # build engine
    engine = eval('init_module{}'.format(splitext(params[0])[1]))()
    engine.build_DSL()
    return engine, None

register(ReplE.INIT, engine_init)

def engine_module(engine, data):
    """
    Load an acab compliant python module into the engine
    Rebuilds the DSL after import.
    """
    params = data['params']
    logging.info("Loading Module: {}".format(params))
    engine.load_modules(*params)
    engine.build_DSL()
    return engine, None

register(ReplE.MODULE, engine_module)

def engine_load(engine, data):
    """
    Load a dsl file into the engine
    """
    params = data['params']
    logging.info("Loading: {}".format(params))
    filename = abspath(expanduser(params[0])).strip()
    assert(exists(filename)), filename
    engine.load_file(filename)
    return engine, None

register(ReplE.LOAD, engine_load)

def engine_save(engine, data):
    """
    Save the state of the working memory into a file
    """
    params = data['params']
    logging.info("Saving State to: {}".format(params))
    filename = abspath(expanduser(params[0]))
    assert(exists(split(filename)[0]))
    engine.save_file(filename)
    return engine, None

register(ReplE.SAVE, engine_save)

def engine_print(engine, data):
    """
    Print out information
    """
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
    # TODO print keywords if passed nothing

    return engine, data

register(ReplE.PRINT, engine_print)

def engine_run(engine, data):
    """
    Take a binding from a query, and run it.
    Used for running rules, queries, actions, layers, pipelines...
    """
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

    result = engine(value)

    data['result'] = result
    return engine, data

register(ReplE.RUN, engine_run)

def engine_pass(engine, data):
    """
    Pass strings through to the engine/working memory
    """
    params = data['params']
    logging.info("Passing sentences through: {}".format(params))
    # Determine query / assertion
    # FIXME: Simple hack for now
    if not bool(params[0]):
        result = ""
    elif params[0].strip()[-1] == "?":
        result = engine.query(*params)
    else:
        result = engine.add(*params)

    data['result'] = result
    return engine, data

register(ReplE.PASS, engine_pass)

def engine_decompose(engine, data):
    """
    Decompose an object into a trie of its components.
    Useful for decomposing rules
    """
    params = data['params']
    # TODO : split objects into tries
    # run query
    # split result into bindings
    return engine, None

register(ReplE.DECOMPOSE, engine_decompose)

def engine_step(engine, data):
    """
    Control the engine state,
    Enabling rewinding to last saved rng position (ie: start of this pipeline loop)
    and stepping forwards by rule/layer/pipeline
    """
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
    """
    Manually perform an output action
    """
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
    """ Listeners:
    Listen for specific assertions / rule firings / queries,
    and pause on them
    """
    params = data['params']
    logging.info("Listener ({}): {}".format(data["type"], params))
    words = [y for x in params for y in SPLIT_RE.split(x)]
    if data['type'] == "add":
        engine.add_listeners(*words)
    elif data['type'] == "remove":
        engine.remove_listeners(*words)
    elif data['type'] == "list":
        result = []
        result.append(" ".join(engine.get_listeners()))
        result.append("Threshold: {}".format(engine.get_listener_threshold()))
        data["result"] = "\n".join(result)
    elif data['type'] == "threshold":
        params = SPLIT_RE.split((params[0]))
        engine.set_listener_threshold(int(params[0]), int(params[1]))

    return engine, data

register(ReplE.LISTEN, engine_listen)

def engine_type_check(engine, data):
    """
    Trigger the type checking of the working memory state
    """
    params = data['params']
    logging.info("Type Checking: {}".format(params))
    # TODO
    # If single statement, run analysis layer with statement inserted, return types



    # else everything: run analysis layer

    return engine, None

register(ReplE.CHECK, engine_type_check)

def engine_stats(engine, data):
    """
    Print Stats about the engine.
    ie: Modules/Operators/Pipelines/Layers/Rules....
    """
    params = data['params']
    logging.info("Getting Stats: {}".format(params))
    allow_all = not bool(params)
    result = []
    # Operators
    if allow_all or "operator" in params:
        result.append("Operator Stats: ")
        result.append(str(engine._operators))

    # pipeline
    if allow_all or "pipeline" in params:
        result.append("--------------------")
        result.append("Pipeline Stats: ")
        # TODO pipeline stats

    # layers
    if (allow_all or "layer" in params):
        result.append("--------------------")
        result.append("Layer Stats: ")
        # result.append("\t{}".format("\n\t".join([str(x) for x in engine._pipeline._layers])))
        # TODO layer stats

    # agendas
    if allow_all or "agenda" in params:
        result.append("--------------------")
        result.append("Agenda Stats: ")
        # TODO: need to query for agendas

    # rules
    if allow_all or "rule" in params:
        result.append("--------------------")
        result.append("ProductionStructure Stats: ")
        # TODO rule stats

    # modules
    if allow_all or "module" in params:
        result.append("--------------------")
        result.append("Module Stats: ")
        result.append("\t{}".format("\n\t".join([x for x in engine._loaded_modules])))

    # WM stats
    if allow_all or "wm" in params:
        result.append("--------------------")
        result.append("WM Stats: ")
        # TODO
        result.append(str(engine._working_memory))

    # Bindings
    if allow_all or "binding" in params:
        result.append("--------------------")
        result.append("Binding Stats: ")
        # TODO pretty print bindings
        bind_groups = engine._cached_bindings[:]
        result.append("\t{}".format("\n\t".join([str(x) for x in bind_groups])))

    # TODO add parser stats

    # Print Keywords
    if allow_all or "keywords" in params:
        result.append("--------------------")
        result.append("Keywords: ")
        result.append("\t{}".format(" ".join(x for x in ["operator", "agenda", "rule", "pipeline",
                                                         "layer", "module", "wm", "binding", "keywords"])))


    result.append("")
    data['result'] = "\n".join(result)
    return engine, data

register(ReplE.STATS, engine_stats)

def engine_help(engine, data):
    """
    Get reminders of use
    """
    params = data['params']
    logging.info("Printing Help")
    # TODO update this
    result = []
    # Print commands
    result.append("-------------------- Acab Help: (outdated)")
    result.append("---------- Args:")
    result.append("--engine                          : Specify a load engine.")
    result.append("                                    Defaults to acab.engines.trie_engine.TrieEngine")
    result.append("-v {LEVEL} | --verbose {LEVEL}    : Specify Log level. File Level is one lower")

    result.append("")
    result.append("---------- Commands:")
    result.append("\tany.valid.sentence     : Assert")
    result.append("\tany.value.sentence?    : Query")
    result.append("")

    result.append("\t:quit                   : Quit the interpreter")
    result.append("\t:stats                  : Print interpreter stats")

    result.append("\t:init {a.class}         : Initialise a new engine")
    result.append("\t:module {a.module}      : Add a new module to the engine")

    result.append("\t:load {file}            : Load a file into the engine")
    result.append("\t:save {file}            : Save a file to the engine")

    result.append("\t:print                  : TODO print (binding)")
    result.append("\t:run                    : TODO run")
    result.append("\t:act                    : TODO act")
    result.append("\t:listen et al           : listen (for {xs}) (remove {xs}) (threshold x/y) ")
    result.append("\t:check                  : TODO check")
    result.append("\t:echo                   : TODO echo")

    result.append("\t:help                   : Print this help")

    data['result'] = "\n".join(result)
    return engine, data

register(ReplE.HELP, engine_help)

def engine_prompt(engine, data):
    """
    Change the prompt of the repl
    """
    data['prompt'] = data['params'][0]

    return engine, data

register(ReplE.PROMPT, engine_prompt)

def engine_exit(engine, data):
    """
    Exit the repl, automatically saving the engine state
    """
    logging.info("Quit Command")
    filename = "{}_{}.auto".format(engine.__class__.__name__,
                                   datetime.now().strftime("%Y_%m-%d_%H_%M"))
    engine.save_file(filename)
    return engine, None

register(ReplE.EXIT, engine_exit)

def engine_multi(engine, data):
    """
    Activate multi-line collation
    """
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
    """
    Pop off the last string added in multi-line mode,
    for when an error was made
    """
    data['collect_str'].pop()
    return engine, data

register(ReplE.POP, engine_multi_pop)

def engine_nop(engine, data):
    """
    A null command used to collect strings in multi-line mode
    without making any changes to the engine state
    """
    if data['in_multi_line']:
        data['collect_str'].append(data['current_str'])
        logging.info("Collecting: {}".format(data['collect_str']))

    return engine, data

register(ReplE.NOP, engine_nop)

def engine_echo(engine, data):
    """
    Toggle echoing of working memory state
    """
    if data['echo']:
        data['echo'] = False
    else:
        data['echo'] = True
    return engine, data

register(ReplE.ECHO, engine_echo)

def engine_break(engine, data):
    """
    Manually switch to PDB for debugging
    """
    if "toggle" not in data['current_str']:
        raise Exception("Triggering breakpoint")

    data['stack'] = not data['stack']
    return engine, data


register(ReplE.BREAK, engine_break)

#---------------------
