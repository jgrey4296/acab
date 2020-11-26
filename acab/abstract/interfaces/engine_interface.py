#!/usr/bin/env python3
import abc
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic


class EngineInterface(metaclass=abc.ABCMeta):
    """ """

    @abc.abstractmethod
    def extract_from_module(self, module):
        pass

    @abc.abstractmethod
    def reload_all_modules(self):
        pass


    @abc.abstractmethod
    def to_sentences(self) -> List['Sentence']:
        pass


    @abc.abstractmethod
    def __call__(self, thing, bindings=None):
        pass


    @abc.abstractmethod
    def get_operator(self, op_name):
        """ Get an operator from the operator wm """
        pass

    @abc.abstractmethod
    def alias_module(self, mod_name, alias_name):
        """
        Assert an alias of a module into the operator wm
        """
        pass


    @abc.abstractmethod
    def register_ops(self, sentences):
        """
        Assert sentences into the operator working memory
        """
        pass


    @abc.abstractmethod
    def build_DSL(self):
        """
        Using currently loaded modules, rebuild the usable DSL parser from fragments
        """
        pass


    @abc.abstractmethod
    def load_modules(self, *modules):
        """ Given ModuleInterface objects,
        store them then tell the working memory to load them
        return a list of dictionaries
        """
        pass
