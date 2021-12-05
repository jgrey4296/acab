"""
A DSL interface for the system, which

"""
import abc
import collections.abc as cABC
import pyparsing as pp
import traceback
import logging as root_logger
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

from acab import types as AT
from acab.core.decorators.dsl import EnsureDSLInitialised
from acab.error.acab_exception import AcabException

Parser           = "pp.ParserElement"
Sentence         = AT.Sentence
Query            = AT.Container
ModuleComponents = AT.ModuleComponents
File             = 'FileObj'

#----------------------------------------
    """ """

    def set_word_exclusions(self, *words):
        pass

    def parse_string(self, string):
        """ Takes a String, parses it into Data format """
        raise NotImplementedError()

    def parse_file(self, file):
        raise NotImplementedError()



        parser.or($xs) -> $y
        parser.assign $p $y

        """
        pass



    def __delitem__(self, key):
        raise NotImplementedError("DSL Fragment's shouldn't delete parsers")

    def __getitem__(self, key):
        raise NotImplementedError("DSL Fragment's should get parsers using `query_parsers`")

    def __setitem__(self, key, value):
        raise NotImplementedError("DSL Fragment's should add parsers using `assert_parsers`")

    def __iter__(self):
        raise NotImplementedError("Iteration through DSL Fragment's is nonsensical")

    def __len__(self):
        raise NotImplementedError("Length of a DSL Fragment is nonsensical")

#----------------------------------------
@dataclass
class DSLBuilder_i(metaclass=abc.ABCMeta):
    root_fragment         : DSL_Fragment_i = field()

    _parsers_initialised  : bool           = field(init=False, default=False)
    _loaded_DSL_fragments : Dict[Any, Any] = field(init=False, default_factory=dict)


    def build_DSL(self, modules: List[ModuleComponents]):
        """
        Using currently loaded modules, rebuild the usable DSL parser from fragments
        """
        logging.info("Building DSL")
        fragments = [y for x in modules for y in x.dsl_fragments]
        self.construct_parsers_from_fragments(fragments)
        self._parsers_initialised = True


    @EnsureDSLInitialised
    def parse(self, s:str) -> List[Sentence]:
        try:
            return self._main_parser.parseString(s, parseAll=True)[:]
        except pp.ParseException as exp:
            logging.warning("\n--------------------\nParse Failure:\n")
            traceback.print_tb(exp.__traceback__)
            logging.warning(f"\n{exp.args[-1]}\n")
            logging.warning(f"Parser: {exp.parserElement}\n")
            logging.warning("Line: {}, Col: {} : {}".format(exp.lineno, exp.col, exp.markInputline()))
            return []

    @EnsureDSLInitialised
    def parseFile(self, f:File) -> List[Sentence]:
        logging.debug(f"Loading File: {f}")
        text = ""
        with open(f, "r") as the_file:
            text = the_file.read()

        logging.info(f"Loading File Text:\n{text}")
        return self.parse(text)

    @abc.abstractmethod
    def construct_parsers_from_fragments(self, fragments: List[DSL_Fragment_i]):
        """ Assemble parsers from the fragments of the wm and loaded modules """
        pass
