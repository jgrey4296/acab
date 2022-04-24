#!/usr/bin/env python3
import logging as logmod

from acab import types as AT
import acab.interfaces.semantic as SI
import acab.interfaces.data as DI
import acab.error.semantic as ASErr
from acab.core.config.config import AcabConfig
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.interfaces.value import Sentence_i
from acab.modules.context.context_query_manager import ContextQueryManager
from acab.modules.context.flatten_query_manager import FlattenQueryManager
from acab.core.value.default_structure import NEGATION, FLATTEN
from acab.core.semantics import basic
from acab.error.protocol import AcabProtocolError as APE

logging = logmod.getLogger(__name__)
config = AcabConfig()

Node          = AT.Node
Value         = AT.Value
Structure     = AT.DataStructure
Engine        = AT.Engine
Contexts      = AT.CtxSet


@APE.assert_implements(SI.StructureSemantics_i)
class FlattenBreadthTrieSemantics(basic.StructureSemantics, SI.StructureSemantics_i):
    """
    Trie Semantics which map values -> Nodes
    Searches *Breadth First*

    Has sentence-flattening logic for querying.
    """
    def verify(self, instruction) -> bool:
        return isinstance(instruction, Sentence)

    def compatible(self, struct):
        is_bns = isinstance(struct, BasicNodeStruct)
        has_all_node_comp = "all_nodes" in struct.components
        return is_bns or has_all_node_comp

    def insert(self, sen, struct, *, data=None, ctxs=None):
        if data is None:
            data = {}

        if NEGATION in sen.data and sen.data[NEGATION]:
            self._delete(sen, struct, data=data, ctxs=ctxs)
            return ctxs

        logging.debug(f"Inserting: {sen} into {struct}")
        # Get the root
        current = self.lookup()[0].up(struct.root)
        for word in sen:
            spec = self.lookup(current)
            accessible = spec[0].access(current, word, data=data)
            if bool(accessible):
                current = accessible[0]
            else:
                next_spec = self.lookup(word)
                new_node = next_spec[0].make(word, data=data)
                struct.components['all_nodes'][new_node.uuid] = new_node
                current = spec[0].insert(current, new_node, data=data)

        return current

    def _delete(self, sen, struct, *, data=None, ctxs=None):
        logging.debug(f"Removing: {sen} from {struct}")

        pos_sen = sen.copy(data={NEGATION:False})
        results = self.query(pos_sen, struct, data=data, ctxs=ctxs)

        for ctx in results:
            assert(ctx._current is not None)
            assert(ctx._current.parent is not None)
            # At leaf:
            # remove current from parent
            current  = ctx._current
            parent   = current.parent()
            spec     = self.lookup(parent)
            spec[0].remove(parent, current.value, data=data)

    def query(self, sen, struct, *, data=None, ctxs=None):
        """ Breadth First Search Query """
        if ctxs is None:
            raise ASErr.AcabSemanticException("Ctxs is none to TrieSemantics.query", rest=sen)

        clause_flatten = FLATTEN not in sen.data or sen.data[FLATTEN]
        root           = struct.root if isinstance(struct, DI.Structure_i) else struct

        with FlattenQueryManager(sen, root, ctxs) as cqm:
            for source_word in cqm.query:
                for bound_word, ctxInst, current_node in cqm.active:
                    should_flatten = clause_flatten and bound_word and (FLATTEN not in bound_word.data or bound_word.data[FLATTEN])
                    if isinstance(bound_word, Sentence_i) and should_flatten:
                        self.query(bound_word, current_node, data=data, ctxs=ctxs.subctx([ctxInst.uuid]))
                    elif source_word.is_at_var and not bool(cqm._current_constraint):
                        continue
                    elif source_word.is_at_var:
                        cqm.maybe_test([current_node])
                    else:
                        spec = self.lookup(current_node)
                        results = spec[0].access(current_node,
                                                 bound_word,
                                                 data=data)

                        cqm.maybe_test(results)

        return ctxs

    def to_sentences(self, struct, *, data=None, ctxs=None):
        """ Convert a trie to a list of sentences
        essentially a dfs of the structure,
        ensuring only leaves are complex structures.

        structures are converted to words for use within sentences
        """
        # TODO if passed a node, use that in place of root
        result_list = []
        # Queue: list[Tuple[list[Value], Node]]

        match struct:
            case DI.Structure_i():
                root = struct.root
            case DI.Node_i():
                root = struct.Root()
                root.add(struct)
            case _:
                raise TypeError("Unknown struct passed in")

        # TODO add depth limit
        queue = [([], root)]
        while bool(queue):
            path, current = queue.pop(0)
            updated_path  = path + [current.value]
            spec          = self.lookup(current)
            accessible    = spec[0].access(current, None, data=data)
            if bool(accessible):
                # branch
                queue += [(updated_path, x) for x in accessible]

            if not bool(accessible) or isinstance(current.value, Instruction):
                # Leaves and Statements
                # Always ignore the root node, so starting index is 1
                words = [x.to_word() if not isinstance(x, Sentence_i) else x for x in updated_path[1:-1]]
                words.append(updated_path[-1])
                result_list.append(Sentence(words))

        return result_list



    @staticmethod
    def from_sentences(self, sens):
        raise NotImplementedError()

    def to_word(self):
        raise NotImplementedError()


