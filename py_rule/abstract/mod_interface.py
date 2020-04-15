"""
The Module Interface definition that modules need to enact.
Comes in two parts: The Parser, and the data

"""
from .value import PyRuleValue


class ModuleSpecification:

    def __init__(self, types=None, funcs=None):

        # A Parser has to provide a parser combinator to
        # integrate into the Working Memory Lanuage.
        # (By Default Exclusion Trie).
        # The Combinator *must* return a tuple:
        # ("typestr", data)
        self._value_parsers = []
        # Statement parsers return sentences with values in them
        self._statement_parsers = []
        # Annotation parsers return constraints for a value
        self._annotation_parsers = []
        # TODO The value types the module adds
        self._types = []
        # TODO The functions on the value types the module adds
        self._functions = []

        # TODO add printing lookup dictionaries

        if types is not None:
            assert(all([x in PyRuleValue.__subclasses__() for x in types]))
            self._types += types

        if funcs is not None:
            self._functions += funcs


    @property
    def value_parsers(self):
        return self._value_parsers

    @property
    def statement_parsers(self):
        return self._statement_parsers

    @property
    def annotation_parsers(self):
        return self._annotation_parsers


    def parse_string(self, string):
        """ Takes a String, parses it into Data format """
        raise NotImplementedError()


    def construct_operators(self):
        """ Use this to call operator constructors """
        raise NotImplementedError()

    def init_strings(self):
        """ Return any strings to parse as
        part of the modules initialisation.
        Defining values etc that can now be parsed by
        the hotloaded value and statement parsers """
        raise NotImplementedError()

    def define_layers(self):
        """ Return any layers the module itself defines """
        raise NotImplementedError()
