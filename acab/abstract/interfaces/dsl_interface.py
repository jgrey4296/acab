"""
A DSL interface for the system, which

"""

import abc
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

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

