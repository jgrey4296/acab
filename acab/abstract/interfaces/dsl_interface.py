"""
A DSL interface for the system, which

"""
import abc
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
from dataclasses import dataclass, field


Parser = "Parser"

class Bootstrapper(metaclass=abc.ABCMeta):
    """ A Utility class for registering and retrieving
    interacting parsers """

    @abc.abstractmethod
    def add(self, *inputs: List[Union[str, Parser]]):
        """ for each pair in inputs (a,b):
        register parser b at location a
        """
        pass

    @abc.abstractmethod
    def query(self, *queries: List[str]) -> Parser:
        """ Get all parsers registered at the string locations
        and return an aggregate parser of them
        """
        pass

class DSL_Interface(metaclass=abc.ABCMeta):
    """ """

    def parse_string(self, string):
        """ Takes a String, parses it into Data format """
        pass

    def parse_file(self, file):
        pass

    def init_strings(self):
        """ Return any strings to parse as
        part of the modules initialisation.
        Defining values etc that can now be parsed by
        the hotloaded value and statement parsers """
        pass

    @abc.abstractmethod
    def assert_parsers(self, bootstrapper: Bootstrapper):
        """
        Assert parsers from this module for integration later
        ie: values.number <= number_parser
        values.time      <= time_parser
        operators.set.add <=  set_add_op
        hotloads.value    <= HOTLOAD_VALUES
        """
        pass

    @abc.abstractmethod
    def query_parsers(self, bootstrapper: Bootstrapper):
        """
        Query the now complete parser trie for hotloads
        values.$xs?
        hotloads.values!$p(~= /values/)?

        parser.or($xs) -> $y
        parser.assign $p $y

        """
        pass



#--------------------------------------------------
@dataclass
class DSLBuilderInterface(metaclass=abc.ABCMeta):
    """ describes engine assembly of a parser from DSL Fragments """
    root_fragment        : DSL_Interface   = field()

    _bootstrap_parser    : Bootstrapper    = field(init=False)
    _main_parser         : Parser          = field(init=False)
    _query_parser        : Parser          = field(init=False)
    _parsers_initialised : bool            = field(init=False, default=False)
    _loaded_DSL_fragments : Dict[Any, Any] = field(init=False, default_factory=dict)


    def build_DSL(self):
        """
        Using currently loaded modules, rebuild the usable DSL parser from fragments
        """
        self.clear_bootstrap()
        self.construct_parsers_from_fragments()
        self.initialised = True

    def construct_parsers_from_fragments(self):
        """ Assemble parsers from the fragments of the wm and loaded modules """
        # assert base parser
        self.root_fragment.assert_parsers(self._bootstrap_parser)

        fragments = [y for x in self._loaded_DSL_fragments.values() for y in x]
        assert(all([isinstance(x, DSL_Interface) for x in fragments]))

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

    def clear_bootstrap(self):
        self._bootstrap_parser = self._bootstrap_parser.__class__()
