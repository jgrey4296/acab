from __future__ import annotations

import builtins
import datetime
import importlib
import logging as logmod
from functools import wraps
from types import FunctionType, ModuleType
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Dict, Generic,
                    Iterable, Iterator, List, Mapping, Match, MutableMapping,
                    Optional, Sequence, Set, Tuple, TypeVar, Union, cast)
from os.path import splitext

from acab.interfaces.debugger import AcabDebugger_i
from acab.interfaces.engine import AcabEngine_i

if TYPE_CHECKING:
    from acab.interfaces.fragments import ModuleFragment

logging = logmod.getLogger(__name__)

def build_slice(s, l, toks):
    first  = None
    second = None
    if 'first' in toks:
        first = toks['first']

    if 'second' in toks:
        second = toks['second']

    return slice(first, second)

def init_inspect(mod_str):
    """
    Import and Inspect the passed in module for potential constructor functions
    to init with
    """
    mod = importlib.import_module(mod_str)
    try:
        not_dunders    = [getattr(mod, x) for x in dir(mod) if "__" not in x]
        not_modules    = [x for x in not_dunders if not isinstance(x, ModuleType)]
        correct_module = [x for x in not_modules if mod_str in x.__module__]
        funcs = [x for x in correct_module if isinstance(x, FunctionType)]
        engines        = [x for x in correct_module if isinstance(x, type) and issubclass(x, AcabEngine_i)]
        total = funcs + engines

        if not bool(total):
            return print(f"No Available Constructors in {mod_str}")

        print(f"Potential Constructors in {mod_str}:")
        for x in total:
            print(f"-- {x.__name__}")
    except:
        breakpoint()

# def build_rebind_instruction(value:str):
#     """ Manually construct a startup rebind instruction """
#     # from acab.core.value.sentence import Sentence
#     import acab.core.defaults.value_keys as DS
#     from acab.core.util.sentences import ProductionComponent
#     from acab.core.value.instruction import ProductionContainer
#     from acab.interfaces.value import ValueFactory as VF


#     action_sem_hint = VF.sen() << config.attr.Semantic.Signals.ACTION
#     op_sen = VF.sen() << "acab.modules.operators.action.RebindOperator".split(".")
#     section_sen = VF.sen() << "§"

#     inst = ProductionComponent(op_sen, params=[section_sen, op_sen])

#     act = ProductionContainer(value=[inst],
#                               data={DS.SEMANTIC_HINT: action_sem_hint})


#     return act
