"""
The Module Interface definition that modules need to enact.
Comes in two parts: The Parser, and the data

"""
from .value import PyRuleValue


class ModuleSpecification:
    """ A Module specification.
    Should be constructed in a module's __init__,
    into a MODULE_SPEC variable.

    Add parsers to:
    ._[value/statement/annotation]_parsers

    Types to     ._types
    Functions to ._functions

    Implement:
    parse_string,
    construct_operators
    init_strings
    define_layers
    """


    def __init__(self, value_ps=None, statement_ps=None, annotate_ps=None):

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

        if value_ps is not None:
            self._value_parsers += value_ps
        if statement_ps is not None:
            self._statement_parsers += statement_ps
        if annotate_ps is not None:
            self._annotation_parsers += annotate_ps

        # TODO add printing lookup dictionaries

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

    def insert_hotloads(self, a_dict):
        """ Working memory calls this, providing the basic
        set of parsers to hotload into the module.
        eg: basic sentences, queries, etc """
        raise NotImplementedError()
