"""
Utility functions for the REPL
"""
from enum import Enum

ReplE = Enum("Repl Commands", "INIT LOAD SAVE PRINT RUN PASS STEP ACT DECOMPOSE LISTEN CHECK STATS HELP EXIT MULTILINE")
repl_commands = {}

def register(cmd, fn, override=False):
    assert(isinstance(cmd, ReplE))
    assert(override or cmd not in repl_commands)
    repl_commands[cmd] = fnd

def get(cmd):
    assert(isinstance(cmd, ReplE))
    return repl_commands[cmd]

#--------------------

def engine_init(engine, string):
    return engine, None

def engine_load(engine, string):
    return engine, None

def engine_save(engine, string):
    return engine, None

def engine_print(engine, params):
    result = None
    return engine, result

def engine_run(engine, params):
    result = engine.run_layer(params)
    return engine, result

def engine_pass(engine, string):
    engine.add(string)
    return engine, None

def engine_step(engine, string):
    return engine, None

def engine_act(engine, string):
    return engine, None

def engine_listen(engine, string):
    return engine, None

def engine_type_check(engine, params):
    return engine, None

def engine_stats(engine, string):
    return engine, None

def engine_help(engine, string):
    return engine, None

def engine_exit(engine, string):
    return engine, None



#---------------------
# Utility functions
