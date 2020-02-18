"""
The Module Interface definition that modules need to enact.
Comes in two parts: The Parser, and the data

"""
from .value import PyRuleValue

class ModuleSpecification:

    def __init__(self, parser=None, types=None, funcs=None):
        """ A Parser has to provide a parser combinator to
        integrate into the Trie Lanuage.
        The Combinator *must* return a tuple:
        ("typestr", data) """
        self._parser = parser
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
        """ Takes a String, parses it into Data format,
        This is used to integrate the module as a value
        in the main language """
        raise Exception("This is an Abstract Method")

    def get_parser(self):
        return self._parser

    def construct_operators(self):
        """ Use this to call operator constructors """
        raise Exception("This is an Abstract Method")
