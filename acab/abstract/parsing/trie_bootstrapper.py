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
from acab.abstract.config.config import GET
from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.abstract.core.node import AcabNode
from acab.abstract.core.production_abstractions import ProductionOperator
from acab.abstract.core.values import AcabValue, Sentence
from acab.abstract.interfaces.dsl_interface import Bootstrapper_i
from acab.modules.semantics.context_container import ContextContainer
from acab.modules.semantics.dependent import BreadthTrieSemantics
from acab.modules.semantics.independent import BasicNodeSemantics

logging = root_logger.getLogger(__name__)
config = GET()

BIND_S    = config.prepare("Value.Structure", "BIND")()

class TrieBootstrapper(Bootstrapper_i):
    """ Manage parsers and allow queries for hotloading,
    used in working memory and module interfaces """

    WILDCARD_STR = "*"

    def __init__(self):
        super().__init__()
        # Trie Semantics, using basic nodes
        self._semantics = BreadthTrieSemantics("_:Trie",
                                            handlers=[],
                                            structs=[],
                                            default=(BasicNodeSemantics("_:Node"),None))

        # And using a standard node struct
        self._structure = BasicNodeStruct.build_default("_:Trie")

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
            new_node = self._semantics.insert(self._structure, loc_string)
            new_node.data.update({'parser': parser})

    def query(self, *queries):
        """ Given a bunch of query strings, get them and return them """
        # TODO: cache the queries for debugging
        results = []
        # Run queries
        for query in queries:
            ctxs = ContextContainer.build()
            q_sentence = Sentence.build(query.split('.'))
            if q_sentence[-1].name == TrieBootstrapper.WILDCARD_STR:
                    q_sentence[-1].data[BIND_S] = True

            self._semantics.query(self._structure, q_sentence, ctxs=ctxs)
            if not bool(ctxs):
                logging.debug(f"Parser Query Empty: {query}")
            else:
                nodes = [x._current for x in ctxs.active_list()]
                parsers = [x.data['parser'] for x in nodes if 'parser' in x.data]
                results += parsers

        # Having found all parsers for the queries, join them and return
        if not bool(results):
            logging.debug("Total Parser Query Empty: {}".format(" | ".join(queries)))
            return pp.NoMatch()
        elif len(results) == 1:
            final_parser = results[0]
        else:
            final_parser = pp.Or(results)

        return final_parser

