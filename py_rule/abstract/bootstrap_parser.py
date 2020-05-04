"""
Bootstrap dsl

Add:
a.test.location = blah
a.first.location, a.second.location = blah2

Query:
value = a.test.location
values = a.test.locations.*

values = a.test.location, a.second.location
values = a.test.location, a.test.location.*


"""
import logging as root_logger
import pyparsing as pp

from py_rule.abstract.trie.trie import Trie
from py_rule.abstract.production_operator import ProductionOperator

logging = root_logger.getLogger(__name__)



class BootstrapParser:
    """ Manage parsers and allow queries for hotloading,
    used in working memory and module interfaces """


    def __init__(self):
        self._parser_trie = Trie()

    def add(self, *inputs):
        """ Use inputs as a plist,
        alternating between location string, and parser """
        assert(len(inputs) % 2 == 0)
        loc_string = inputs.pop(0)
        parser = inputs.pop(0)

        if isinstance(parser, ProductionOperator):
            parser = pp.Keyword(parser.op_str)

        self._parser_trie.add(loc_string.split("."), data=parser)

    def query(self, *queries):
        """ Given a bunch of query strings, get them and return them """

        # Run queries

        # Or the results if multiple

        return []


    def print_setup(self):
        """ Print the trie of parsers, marking nodes that have been used,
        and the queries that are used """
        return self._parser_trie.print_trie()
