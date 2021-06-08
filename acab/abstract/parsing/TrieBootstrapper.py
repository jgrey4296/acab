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
from acab.abstract.core.node import AcabNode
from acab.abstract.core.production_abstractions import ProductionOperator
from acab.abstract.core.values import AcabValue, Sentence
from acab.abstract.interfaces.dsl_interface import Bootstrapper
from acab.modules.semantics.context_container import ContextContainer
from acab.modules.semantics.dependent import BreadthTrieSemantics
from acab.modules.semantics.independent import BasicNodeSemantics
from acab.modules.structures.trie.trie import Trie

logging = root_logger.getLogger(__name__)

class TrieBootstrapper(Bootstrapper):
    """ Manage parsers and allow queries for hotloading,
    used in working memory and module interfaces """

    @staticmethod
    def build_default():
        return BootstrapParser()

    def __init__(self):
        super().__init__()
        self._semantics = BreadthTrieSemantics(BasicNodeSemantics())
        self._structure = Trie.build_default()

    def __len__(self):
        return len(self._structure)
    def __bool__(self):
        return bool(self._structure)

    
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
                logging.warning("Parse Point mentioned, given None: {}".format(loc_string))
                continue
            elif isinstance(parser, str):
                parser = pp.Literal(parser)
            if isinstance(parser, type) and issubclass(parser, ProductionOperator):
                raise DeprecationWarning("Production Operators shouldn't be being built here any more")

            assert(isinstance(parser, pp.ParserElement))
            self._semantics.insert(self._structure, loc_string, data={'parser': parser})

    def query(self, *queries):
        """ Given a bunch of query strings, get them and return them """
        # TODO: cache the queries for debugging
        result_nodes = []
        # Run queries
        for query in queries:
            ctxs = ContextContainer.build()
            # TODO add BIND
            q_sentence = Sentence.build(current.split('.'))
            self._semantics.query(self._structure, q_sentence, ctxs=ctxs)
            if not bool(ctxs):
                logging.warning("No parser found in: {}".format(query))
            elif isinstance(node, list):
                result_nodes += [x._current for x in ctxs.active_list()]

        # Having found all parsers for the queries, join them and return
        if not bool(result_nodes) or not all(['parser' in x.data for x in result_nodes]):
            raise Exception("No Parsers found for: {}".format(" | ".join(queries)))
        elif len(results) == 1:
            final_parser = result_nodes[0].data['parser']
        else:
            final_parser = pp.Or([x.data['parser'] for x in result_nodes])

        return final_parser
