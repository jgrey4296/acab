#!/usr/bin/env python3

import abc
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic


class DSL_Interface(metaclass=abc.ABCMeta):
    """ """

    def parse_string(self, string):
        """ Takes a String, parses it into Data format """
        raise NotImplemented()

    def init_strings(self):
        """ Return any strings to parse as
        part of the modules initialisation.
        Defining values etc that can now be parsed by
        the hotloaded value and statement parsers """
        raise NotImplemented()

    @abc.abstractmethod
    def assert_parsers(self, parser_trie):
        """
        Assert parsers from this module for integration later
        ie: values.number <= number_parser
        values.time      <= time_parser
        operators.set.add <=  set_add_op
        hotloads.value    <= HOTLOAD_VALUES
        """
        pass

    @abc.abstractmethod
    def query_parsers(self, parser_trie):
        """
        Query the now complete parser trie for hotloads
        values.$xs?
        hotloads.values!$p(~= /values/)?

        parser.or($xs) -> $y
        parser.assign $p $y

        """
        pass

