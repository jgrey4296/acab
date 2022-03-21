#!/usr/bin/env python3
"""
Rete semantics for acab:
Queries are asserted into a trie, constructing the Alpha Network.
Leaves point to Beta Network nodes.
Nodes hold CtxSets, for each condition they are part of.
Those CtxSets are the partial matches, and hold the constraints

Bang in a sentence invalidates parallel ctxs

"""
import logging as root_logger

from acab import types as AT
import acab.interfaces.semantic as SI
import acab.error.semantic as ASErr
from acab.core.config.config import AcabConfig
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.instruction import Instruction
from acab.core.data.sentence import Sentence
from acab.interfaces.value import Sentence_i
from acab.modules.context.context_query_manager import ContextQueryManager

logging = root_logger.getLogger(__name__)
config = AcabConfig()

NEGATION_S       = config.prepare("Value.Structure", "NEGATION")()

Node          = AT.Node
Value         = AT.Value
Structure     = AT.DataStructure
Engine        = AT.Engine
Contexts      = AT.CtxSet


class ReteSemantics(SI.StructureSemantics_i):
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
        current = self.default.func.up(struct.root)
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
        parent = struct.root
        current = struct.root

        for word in sen:
            # Get independent semantics for current
            spec     = self.lookup(current)
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
        # Queue: list[Tuple[list[Value], Node]]
        queue = [([], struct.root)]
        while bool(queue):
            path, current = queue.pop(0)
            updated_path  = path + [current.value]
            spec          = self.lookup(current)
            accessible    = spec[0].access(current, None, data)
            if bool(accessible):
                # branch
                queue += [(updated_path, x) for x in accessible]

            if not bool(accessible) or isinstance(current.value, Instruction):
                # Leaves and Statements
                # Always ignore the root node, so starting index is 1
                words = [x.to_word() if not isinstance(x, Sentence_i) else x for x in updated_path[1:-1]]
                words.append(updated_path[-1])
                result_list.append(Sentence.build(words))

        return result_list




