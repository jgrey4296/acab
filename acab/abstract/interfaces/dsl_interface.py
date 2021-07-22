"""
A DSL interface for the system, which

"""
import abc
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
from dataclasses import dataclass, field
from acab.error.acab_base_exception import AcabBaseException

Parser           = "Parser"
Sentence         = 'Sentence'
Query            = 'ProductionContainer'
ModuleComponents = "ModuleComponents"
File             = 'FileObj'

# Decorator for DSL Builder:
def EnsureInitialised(method):
    def fn(self, *args, **kwargs):
        if not self._parsers_initialised:
            raise AcabBaseException("DSL Not Initialised")

        return method(self, *args, **kwargs)

    fn.__name__ = method.__name__
    return fn
#----------------------------------------
class Bootstrapper_i(metaclass=abc.ABCMeta):
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

#----------------------------------------
# TODO refactor to DSL_Fragment_i
class DSL_Fragment_i(metaclass=abc.ABCMeta):
    """ """

    def parse_string(self, string):
        """ Takes a String, parses it into Data format """
        pass

    def parse_file(self, file):
        pass

    @abc.abstractmethod
    def assert_parsers(self, bootstrapper: Bootstrapper_i):
        """
        Assert parsers from this module for integration later
        ie: values.number <= number_parser
        values.time      <= time_parser
        operators.set.add <=  set_add_op
        hotloads.value    <= HOTLOAD_VALUES
        """
        pass

    @abc.abstractmethod
    def query_parsers(self, bootstrapper: Bootstrapper_i):
        """
        Query the now complete parser trie for hotloads
        values.$xs?
        hotloads.values!$p(~= /values/)?

        parser.or($xs) -> $y
        parser.assign $p $y

        """
        pass



#----------------------------------------
# TODO refactor to DSL_Builder_i
@dataclass
class DSLBuilder_i(metaclass=abc.ABCMeta):
    root_fragment        : DSL_Fragment_i   = field()

    _bootstrap_parser    : Bootstrapper_i     = field(init=False)
    _main_parser         : Parser           = field(init=False)
    _query_parser        : Parser           = field(init=False)
    _parsers_initialised : bool             = field(init=False, default=False)
    _loaded_DSL_fragments : Dict[Any, Any]  = field(init=False, default_factory=dict)

    def build_DSL(self, modules: List[ModuleComponents]):
        """
        Using currently loaded modules, rebuild the usable DSL parser from fragments
        """
        self.clear_bootstrap()
        fragments = [y for x in modules for y in x.dsl_fragments]
        self.construct_parsers_from_fragments(fragments)
        self._parsers_initialised = True

    def clear_bootstrap(self):
        self._bootstrap_parser = self._bootstrap_parser.__class__()

    @EnsureInitialised
    def parse(self, s:str) -> List[Sentence]:
        return self._main_parser.parseString(s)[:]

    @EnsureInitialised
    def parseFile(self, f:File) -> List[Sentence]:
        return self._main_parser.parseFile(f)[:]

    @EnsureInitialised
    def query_parse(self, s:str) -> Query:
        return self._query_parser.parseString(s)[0][1]
    @abc.abstractmethod
    def construct_parsers_from_fragments(self, fragments: List[DSL_Fragment_i]):
        """ Assemble parsers from the fragments of the wm and loaded modules """
        pass
