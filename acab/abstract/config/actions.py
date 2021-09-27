#!/usr/bin/env python3
import importlib
from enum import Enum

actions_e = Enum("Config Actions", "STRIPQUOTE KEYWORD LITERAL DICT LIST UNESCAPE SPLIT PSEUDOSEN BOOL IMPORT")

DEFAULT_ACTIONS = {actions_e.STRIPQUOTE : lambda x: x.strip("\"'"),
                   actions_e.LIST       : lambda x: x.split("\n"),
                   actions_e.UNESCAPE   : lambda x: x.encode().decode("unicode_escape"),
                   actions_e.SPLIT      : lambda x: x.split(" "),
                   actions_e.PSEUDOSEN  : lambda x: "_:{}".format(x),
                   actions_e.BOOL       : lambda x: True if x == "True" else False,
                   actions_e.IMPORT     : lambda x: importlib.import_module(x)}
