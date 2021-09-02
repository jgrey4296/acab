#!/usr/bin/env python3
import logging as root_logger

import acab.abstract.interfaces.semantic as SI
import acab.error.acab_semantic_exception as ASErr
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import Sentence, AcabStatement
from acab.abstract.core.acab_struct import BasicNodeStruct

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

CONSTRAINT_S     = config.prepare("Value.Structure", "CONSTRAINT")()
NEGATION_S       = config.prepare("Value.Structure", "NEGATION")()
QUERY_FALLBACK_S = config.prepare("Value.Structure", "QUERY_FALLBACK")()
DEFAULT_SETUP_S  = config.prepare("Data", "DEFAULT_SETUP_METHOD")()
DEFAULT_UPDATE_S = config.prepare("Data", "DEFAULT_UPDATE_METHOD")()

Node          = 'AcabNode'
Printable     = 'Printable'
Value         = 'AcabValue'
Structure     = 'AcabStruct'
Engine        = 'Engine'
Contexts      = 'Contexts'


# Dependent Semantics
class BreadthTrieSemantics(SI.DependentSemantics_i):
    """
    Trie Semantics which map values -> Nodes
    Searches *Breadth First*
    """
    def compatible(self, struct):
        is_bns = isinstance(struct, BasicNodeStruct)
        has_all_node_comp = "all_nodes" in struct.components
        return is_bns or has_all_node_comp

    def insert(self, struct, sen, data=None, ctxs=None):
        if data is None:
            data = {}

        if NEGATION_S in sen.data and sen.data[NEGATION_S]:
            return self._delete(struct, sen, data)

        # Get the root
        current = self.default[0].up(struct.root)
        for word in sen:
            semantics, _ = self.lookup(current)
            accessible = semantics.access(current, word, data)
            if bool(accessible):
                current = accessible[0]
            else:
                next_semantics, _ = self.lookup(word)
                new_node = next_semantics.make(word, data)
                struct.components['all_nodes'][new_node.uuid] = new_node
                current = semantics.insert(current, new_node, data)

        return current

    def _delete(self, struct, sen, data=None):
        parent = struct.root
        current = struct.root

        for word in sen:
            # Get independent semantics for current
            semantics, _ = self.lookup(current)
            accessed = semantics.access(current, word, data)
            if bool(accessed):
                parent = current
                current = accessed[0]
            else:
                return None

        # At leaf:
        # remove current from parent
        semantics, _ = self.lookup(parent)
        semantics.remove(parent, current.value, data)


    def query(self, struct, sen, data=None, ctxs=None):
        """ Breadth First Search Query """
        if ctxs is None:
            raise ASErr.AcabSemanticException("Ctxs is none to TrieSemantics.query", sen)

        negated_query = False
        if NEGATION_S in sen.data and sen.data[NEGATION_S]:
            negated_query = True

        # TODO get collapse vars from the sentence
        collapse_vars = []
        with ctxs(struct.root, sen, data, collapse_vars, negated_query):
            for word in sen:
                for ctxInst in ctxs.active_list(clear=True):
                    indep, _ = self.lookup(ctxInst._current)
                    search_word = word
                    get_all = False
                    # Handle variable:
                    if word.is_var and word not in ctxInst:
                        get_all = True
                    elif word.is_var and word in ctxInst:
                        # Word is var, but bound, so look for that instead
                        search_word = ctxInst[word]

                    results = indep.access(ctxInst._current,
                                           search_word,
                                           data,
                                           get_all=get_all)
                    if not bool(results):
                        ctxs.fail(ctxInst, word, None)
                    else:
                        ctxs.test(ctxInst, results, word)


    def to_sentences(self, struct, data=None, ctxs=None):
        """ Convert a trie to a list of sentences
        essentially a dfs of the structure
        """
        # TODO if passed a node, use that in place of root
        result_list = []
        # Queue: List[Tuple[List[Value], Node]]
        queue = [([], struct.root)]
        while bool(queue):
            path, current = queue.pop(0)
            updated_path = path + [current.value]
            semantics, _ = self.lookup(current)
            accessible = semantics.access(current, None, data, get_all=True)
            if bool(accessible):
                # branch
                queue += [(updated_path, x) for x in accessible]

            if not bool(accessible) or isinstance(current.value, AcabStatement):
                # Leaves and Statements
                # Always ignore the root node
                words = [x.to_word() if isinstance(x, AcabStatement) else x for x in updated_path[1:-1]]
                words.append(updated_path[-1])
                result_list.append(Sentence.build(words))

        return result_list

