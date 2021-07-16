"""
Utility decorators
"""
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

Structure= 'AcabStructure'
Sentence = 'Sentence'

def ForceListArgs(f):
    """ Force the first arg to be a list """
    def wrapped(self, *the_args, **the_kwargs):
        forced = []
        if isinstance(the_args[0], list):
            forced = the_args
        else:
            forced.append([the_args[0]])
            forced += the_args[1:]

        return f(self, *forced, **the_kwargs)

    wrapped.__name__ = f"FLA({f})"
    return wrapped

#--------------------
def default_key(node:Any, data:Dict[Any,Any]=None) -> str:
    return str(node.value)

def default_failure(semantics, struct, instruction, ctxs, data, err):
    logging.warning("Default Failure: {}".format(err))

def example_hook(semSystem, semantics, struct: Structure, instruction: Sentence, ctxs, data=None):
    pass
