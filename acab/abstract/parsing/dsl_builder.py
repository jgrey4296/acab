import abc
from dataclasses import dataclass, field
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.interfaces.dsl_interface import (DSL_Interface,
                                                    DSLBuilder_Interface)
from acab.abstract.parsing.trie_bootstrapper import TrieBootstrapper

Bootstrapper = 'Bootstrapper'

# Decorator for Engine:
def EnsureDSLInitialised(method):
    def fn(self, *args, **kwargs):
        if not self.initialised:
            raise AcabBaseException("DSL Not Initialised")

        return method(self, *args, **kwargs)

    fn.__name__ = method.__name__
    return fn
#--------------------
@dataclass
class DSLBuilder(DSLBuilder_Interface):
    """ describes engine assembly of a parser from DSL Fragments """

    _bootstrap_parser: Bootstrapper = field(default_factory=TrieBootstrapper)

    def construct_parsers_from_fragments(self, fragments:List[DSL_Interface]):
        """ Assemble parsers from the fragments of the wm and loaded modules """
        assert(all([isinstance(x, DSL_Interface) for x in fragments]))

        # assert base parser
        self.root_fragment.assert_parsers(self._bootstrap_parser)

        for x in fragments:
            #Populate the trie
            x.assert_parsers(self._bootstrap_parser)

        for x in fragments:
            # Now query and populate the modules
            x.query_parsers(self._bootstrap_parser)

        # then assign main and query parsers from the base parser
        main_p, query_p = self.root_fragment.query_parsers(self._bootstrap_parser)
        self._main_parser = main_p
        self._query_parser = query_p

        # TODO: register the loaded fragments
        # fragments = [y for x in self._loaded_DSL_fragments.values() for y in x]
