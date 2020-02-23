"""
The Module Interface definition that modules need to enact.
Comes in two parts: The Parser, and the data

"""
from .value import PyRuleValue


class ModuleSpecification:

    def __init__(self, types=None, funcs=None):

        # A Parser has to provide a parser combinator to
        # integrate into the Trie Lanuage.
        # The Combinator *must* return a tuple:
        # ("typestr", data)
        self._value_parsers = []
        # Statement parsers return sentences with values in them
        self._statement_parsers = []
        # The value types the module adds
        self._types = []
        if types is not None:
            assert(all([x in PyRuleValue.__subclasses__() for x in types]))
            self._types += types
        # The functions on the value types the module adds
        self._functions = []
        if funcs is not None:
            self._functions += funcs

    def parse_string(self, string):
        """ Takes a String, parses it into Data format """
        raise NotImplementedError()

    def get_value_parsers(self):
        return self._value_parsers

    def get_statement_parsers(self):
        return self._statement_parsers

    def construct_operators(self):
        """ Use this to call operator constructors """
        raise NotImplementedError()

    def init_strings(self):
        """ Return any strings to parse as
        part of the modules initialisation.
        Defining values etc that can now be parsed by
        the hotloaded value and statement parsers """
        raise NotImplementedError(self)
