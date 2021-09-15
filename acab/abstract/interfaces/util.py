"""
Utility decorators
"""
import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

Structure= 'AcabStructure'
Sentence = 'Sentence'

#--------------------
def default_key(node:Any, data:Dict[Any,Any]=None) -> str:
    return str(node.value)

def default_failure(semantics, struct, instruction, ctxs, data, err):
    logging.warning("Default Failure: {}".format(err))

def example_hook(semSystem, semantics, struct: Structure, instruction: Sentence, ctxs, data=None):
    pass
