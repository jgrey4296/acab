"""
The Interface definition that modules need to enact.
Comes in two parts: The Parser, and the data

"""


class ModuleParser:

    def __init__(self, parser):
        """ A Parser has to provide a parser combinator to
        integrate into the Trie Lanuage.
        The Combinator *must* return a tuple:
        ("typestr", data) """
        self._parser = parser

    def parse_string(self, string):
        """ Takes a String, parses it into Data format,
        This is used to integrate the module as a value
        in the main language """
        raise Exception("This is an Abstract Method")

    def get_parser(self):
        return self._parser

class ModuleData:

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise Exception("This is an Abstract Method")

    def copy(self):
        """ Data needs to be able to be copied """
        raise Exception("This is an Abstract Method")

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise Exception("This is an Abstract Method")

    def var_set(self):
        """ Data needs to be able to report internal variables """
        raise Exception("This is an Abstract Method")
