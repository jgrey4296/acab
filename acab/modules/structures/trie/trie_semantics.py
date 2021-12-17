#!/usr/bin/env python3
import logging as root_logger

from acab import types as AT
import acab.interfaces.semantic as SI
import acab.error.semantic_exception as ASErr
from acab.core.config.config import AcabConfig
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.values import AcabStatement, Sentence
from acab.interfaces.value import Sentence_i
from acab.modules.context.context_query_manager import ContextQueryManager

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

NEGATION_S       = config.prepare("Value.Structure", "NEGATION")()

Node          = AT.Node
Value         = AT.Value
Structure     = AT.DataStructure
Engine        = AT.Engine
Contexts      = AT.CtxSet


class BreadthTrieSemantics(SI.StructureSemantics_i):
    """
    Trie Semantics which map values -> Nodes
    Searches *Breadth First*
    """
    def compatible(self, struct):
        is_bns = isinstance(struct, BasicNodeStruct)
        has_all_node_comp = "all_nodes" in struct.components
        return is_bns or has_all_node_comp

    def insert(self, sen, struct, data=None, ctxs=None):
        if data is None:
            data = {}

        if NEGATION_S in sen.data and sen.data[NEGATION_S]:
            self._delete(sen, struct, data)
            return ctxs

        logging.debug(f"Inserting: {sen} into {struct}")
        # Get the root
        current = self.lookup()[0].up(struct.root)
        for word in sen:
            spec = self.lookup(current)
            accessible = spec[0].access(current, word, data)
            if bool(accessible):
                current = accessible[0]
            else:
                next_spec = self.lookup(word)
                new_node = next_spec[0].make(word, data)
                struct.components['all_nodes'][new_node.uuid] = new_node
                current = spec[0].insert(current, new_node, data)

        return current

    def _delete(self, sen, struct, data=None):
        logging.debug(f"Removing: {sen} from {struct}")
        parent  = struct.root
        current = struct.root

        for word in sen:
            # Get independent semantics for current
            spec = self.lookup(current)
            accessed = spec[0].access(current, word, data)

            if bool(accessed):
                parent = current
                current = accessed[0]
            else:
                return None

        # At leaf:
        # remove current from parent
        spec = self.lookup(parent)
        spec[0].remove(parent, current.value, data)

    def query(self, sen, struct, data=None, ctxs=None):
        """ Breadth First Search Query """
        if ctxs is None:
            raise ASErr.AcabSemanticException("Ctxs is none to TrieSemantics.query", sen)

        with ContextQueryManager(sen, struct.root, ctxs) as cqm:
            for source_word in cqm.query:
                if source_word.is_at_var:
                    continue
                for bound_word, ctxInst, current_node in cqm.active:
                    spec = self.lookup(current_node)
                    results = spec[0].access(current_node,
                                             bound_word,
                                             data)

                    cqm.test_and_update(results)

        return ctxs

    def to_sentences(self, struct, data=None, ctxs=None):
        """ Convert a trie to a list of sentences
        essentially a dfs of the structure,
        ensuring only leaves are complex structures.

        structures are converted to words for use within sentences
        """
        # TODO if passed a node, use that in place of root
        result_list = []
        # Queue: List[Tuple[List[Value], Node]]
        queue = [([], struct.root)]
        while bool(queue):
            path, current = queue.pop(0)
            updated_path  = path + [current.value]
            spec          = self.lookup(current)
            accessible    = spec[0].access(current, None, data)
            if bool(accessible):
                # branch
                queue += [(updated_path, x) for x in accessible]

            if not bool(accessible) or isinstance(current.value, AcabStatement):
                # Leaves and Statements
                # Always ignore the root node, so starting index is 1
                words = [x.to_word() if not isinstance(x, Sentence_i) else x for x in updated_path[1:-1]]
                words.append(updated_path[-1])
                result_list.append(Sentence.build(words))

        return result_list




class DepthTrieSemantics(SI.StructureSemantics_i):
    """
    Trie Semantics which map values -> Nodes
    Searches *Depth First*
    """

    def insert(self, struct, sen, data=None, ctxs=None):
        if data is None:
            data = {}

        if NEGATION_S in sen.data and sen.data[NEGATION_S]:
            return self._delete(struct, sen, data)

        # Get the root
        # TODO: Ensure the struct is appropriate
        current = self.default[0].up(struct.root)
        for word in sen:
            spec = self.lookup(current)
            accessible = spec[0].access(current, word, data)
            if bool(accessible):
                current = accessible[0]
            else:
                next_spec = self.lookup(word)
                new_node = next_spec[0].make(word, data)
                current = spec[0].insert(current, new_node, data)

        return current


    def _delete(self, struct, sen, data=None, ctxs=None):
        parent = struct.root
        current = struct.root

        for word in sen:
            # Get value semantics for current
            spec = self.lookup(current)
            accessed = spec[0].access(current, word, data)
            if bool(accessed):
                parent = current
                current = accessed[0]
            else:
                return None

        # At leaf:
        # remove current from parent
        spec = self.lookup(parent)
        spec[0].remove(parent, current.value, data)

        return current


    def query(self, struct, sen, data=None, ctxs=None):
        """ Depth First Search Query """
        if ctxs is None:
            raise ASErr.AcabSemanticException("Ctxs is none to TrieSemantics.query", sen)

        negated_query = False
        if NEGATION_S in sen.data and sen.data[NEGATION_S]:
            negated_query = True

        # TODO get collapse vars from the sentence
        collapse_vars = []
        with ctxs(struct.root, sen, data, collapse_vars, negated_query):
            while ctxs.active:
                currentInst = ctxs.pop(top=True)
                try:
                    remaining_sen = sen.words[:]
                    if currentInst._remaining_query is not None:
                        remaining_sen = currentInst._remaining_query

                    while bool(remaining_sen):
                        word = remaining_sen.pop(0)
                        spec = self.lookup(currentInst._current)
                        search_word = word
                        # Handle variable:
                        if word.is_var and word not in ctxInst:
                            search_word = None
                        elif word.is_var and word in ctxInst:
                            # Word is var, but bound, so look for that instead
                            search_word = currentInst[word]


                        results = spec[0].access(currentInst._current,
                                                 search_word,
                                                 datac)

                        # TODO to make this depth first,
                        # the current ctxInst needs to be updated
                        # as the search progresses
                        if not bool(results):
                            ctxs.fail(currentInst, word, None)
                            raise ASErr.AcabSemanticQueryContextDepletionFailure("Ctx has failed, going to next", None)
                        else:
                            new_active_ctxs = ctxs.test(currentInst, results, word)
                            if not bool(new_active_ctxs):
                                raise ASErr.AcabSemanticQueryContextDepletionFailure("No Successes, switch to next context", None)
                            for ctxInst in new_active_ctxs:
                                ctx_inst._remaining_query = remaining_sen[:]

                            currentInst = new_active_ctxs.pop(0)

                except ASErr.AcabSemanticQueryContextDepletionFailure as err:
                    continue

        return ctxs


    def trigger(self, struct, sen, data=None, ctxs=None):
        pass
