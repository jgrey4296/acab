import importlib
import logging as logmod
from types import FunctionType, ModuleType
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from functools import wraps
import datetime
import builtins
from types import FunctionType

import acab
import acab.core.value.default_structure as DS
from acab import AcabConfig
from acab.interfaces.debugger import AcabDebugger_i
from acab.interfaces.engine import AcabEngine_i

logging = logmod.getLogger(__name__)
config  = AcabConfig()

action_sem_spec = config.prepare("Semantic.Signals", "ACTION")
SEM_HINT = DS.SEMANTIC_HINT

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

def build_rebind_instruction(value:str):
    """ Manually construct a startup rebind instruction """
    from acab.core.value.instruction import ProductionComponent, ProductionContainer
    from acab.core.value.sentence import Sentence

    action_sem_hint = Sentence([action_sem_hint()])

    inst = ProductionComponent(value=Sentence([ "acab.modules.operators.action.RebindOperator" ]),
                               params=[Sentence([ "§" ]),
                                       Sentence([ "acab.modules.operators.action.RebindOperator" ])])

    act = ProductionContainer(value=[inst],
                              data={SEM_HINT: action_sem_hint})

    return act


def capture_printing():
    """
    Setup a file handler for a separate logger,
    to keep a trace of anything printed
    """
    oldprint = builtins.print
    # start_time = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M")
    TRACE_FILE_NAME = "trace.repl"
    input_trace_handler = logmod.FileHandler(TRACE_FILE_NAME, mode='w')
    input_trace_handler.setLevel(logmod.INFO)

    trace_logger = logmod.getLogger('acab.repl.trace')
    trace_logger.setLevel(logmod.INFO)
    trace_logger.addHandler(input_trace_handler)
    trace_logger.propagate = False

    @wraps(oldprint)
    def intercepted(*args, **kwargs):
        """ Wraps print to also log to a separate file """
        oldprint(*args, **kwargs)
        if bool(args):
            trace_logger.warning(args[0])

    builtins.print = intercepted
