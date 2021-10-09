#!/usr/bin/env python3
"""
Standardized Actions for use in Config.prepare

"""
import importlib
from enum import Enum
from acab.core.decorators.util import mapToEnum

actions_e = Enum("Config Actions", "STRIPQUOTE KEYWORD LITERAL DICT LIST UNESCAPE SPLIT PSEUDOSEN BOOL IMPORT")

DEFAULT_ACTIONS = {}


@mapToEnum(DEFAULT_ACTIONS, actions_e.STRIPQUOTE)
def stripquote(x):
    return x.strip("\"'")

@mapToEnum(DEFAULT_ACTIONS, actions_e.LIST)
def split_lines(x):
    return x.split("\n")

@mapToEnum(DEFAULT_ACTIONS, actions_e.UNESCAPE)
def unescape(x):
    return x.encode().decode("unicode_escape")

@mapToEnum(DEFAULT_ACTIONS, actions_e.SPLIT)
def split(x):
    return x.split(" ")

@mapToEnum(DEFAULT_ACTIONS, actions_e.PSEUDOSEN)
def pseudosen(x):
    return "_:{}".format(x)

@mapToEnum(DEFAULT_ACTIONS, actions_e.BOOL)
def is_bool(x):
    return True if x == "True" else False

@mapToEnum(DEFAULT_ACTIONS, actions_e.IMPORT)
def import_mod(x):
    return importlib.import_module(x)
