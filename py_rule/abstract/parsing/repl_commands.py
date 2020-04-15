"""
Utility functions for the REPL
"""
from enum import Enum

ReplE = Enum("Repl Commands", "INIT LOAD SAVE PRINT QUERY ASSERT HELP EXIT MULTI_LINE")
repl_commands = {}

def register(cmd, fn, override=False):
    assert(isinstance(cmd, ReplE))
    assert(override or cmd not in repl_commands)
    repl_commands[cmd] = fnd

def get(cmd):
    assert(isinstance(cmd, ReplE))
    return repl_commands[cmd]


def engine_add(engine, string):
    engine.add(string)
    return engine, None

def engine_query(engine, string):
    result = engine.query(string)
    return engine, result

def engine_run(engine, params):
    result = engine.run_layer(params)
    return engine, result

def engine_print(engine, params):
    result = None
    return engine, result

def engine_state(engine, params):
    result = None
    return engine, result

def engine_module(engine, params):
    result = None
    return engine, result

def engine_register_pause(engine, params):
    return engine, None

def engine_type_check(engine, params):
    return engine, None
