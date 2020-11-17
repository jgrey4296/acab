"""
Bootstrap dsl: A Simple Trie to register and query parser fragments

Add:
a.test.location = blah
a.first.location, a.second.location = blah2

Query:
value = a.test.location
values = a.test.locations.*
values = a.*.locations.*

values = a.test.location, a.second.location
values = a.test.location, a.test.location.*


"""
import logging as root_logger
import pyparsing as pp

from acab.abstract.core.sentence import Sentence
from acab.abstract.core.value import AcabValue
from acab.abstract.data.node import AcabNode
from acab.abstract.rule.production_operator import ProductionOperator
from acab.abstract.parsing.parsers import OPERATOR_SUGAR

from acab.modules.structures.trie.trie import Trie
from acab.modules.structures.trie.trie_semantics import BasicTrieSemantics
from acab.modules.semantics.basic_semantics import BasicNodeSemantics

logging = root_logger.getLogger(__name__)

class BootstrapParser:
    """ Manage parsers and allow queries for hotloading,
    used in working memory and module interfaces """

    def __init__(self, empty=False):
        semantics = BasicTrieSemantics({AcabNode : BasicNodeSemantics()},
                                       {AcabValue : (AcabNode, {})},
                                       sentence_sort=lambda x: str(x))
        self._internal_trie = Trie(semantics)
        if not empty:
            self.add("operator.sugar", OPERATOR_SUGAR)

    def __len__(self):
        return len(self._internal_trie)
    def __bool__(self):
        return bool(self._internal_trie)

    
    def add(self, *inputs):
        """ Use inputs as a plist,
        alternating between location string, and parser """
        assert(len(inputs) % 2 == 0)
        input_list = [x for x in inputs]
        while bool(input_list):
            current = input_list.pop(0)
            loc_string = Sentence.build(current.split('.'))
            parser = input_list.pop(0)

            if parser is None:
                logging.warning("Loc given None: {}".format(loc_string))
                continue
            elif isinstance(parser, str):
                parser = pp.Literal(parser)
            if isinstance(parser, type) and issubclass(parser, ProductionOperator):
                raise DeprecationWarning("Production Operators shouldn't be being built here any more")

            assert(isinstance(parser, pp.ParserElement))
            self._internal_trie.add(loc_string, data={'parser': parser})

    def query(self, *queries):
        """ Given a bunch of query strings, get them and return them """
        # TODO: cache the queries for debugging
        results = []
        # Run queries
        for query in queries:
            node = self._query(query)
            if node is None:
                logging.warning("No parser found in: {}".format(query))
            elif isinstance(node, list):
                results += [x._data['parser'] for x in node if 'parser' in x._data]

        if not bool(results):
            raise Exception("No Parsers found for: {}".format(" | ".join(queries)))
        elif len(results) == 1:
            final_parser = results[0]
        else:
            final_parser = pp.Or(results)

        return final_parser

    def _query(self, query):
        # TODO convert this to semantics
        assert(isinstance(query,str))

        results = []
        queue = [(self._internal_trie.root, query.split("."))]
        while queue:
            curr_node, remaining_path = queue.pop(0)
            if not bool(remaining_path):
                results.append(curr_node)
                continue

            looking_for = remaining_path.pop(0)

            if looking_for == "*":
                queue += [(x, remaining_path[:]) for x in curr_node.children]
            elif curr_node.has_child(looking_for):
                next_node  = curr_node.get_child(looking_for)
                queue.append((next_node, remaining_path[:]))

        return results

    def print_trie(self):
        """ Print the trie of parsers, marking nodes that have been used,
        and the queries that are used """
        return self._internal_trie.print_trie(join_str=".")
