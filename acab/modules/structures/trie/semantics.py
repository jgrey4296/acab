#!/usr/bin/env python3
import logging as logmod

import acab.core.defaults.value_keys as DS
import acab.error.semantic as ASErr
import acab.interfaces.data as DI
import acab.interfaces.semantic as SI
from acab import types as AT
from acab import AcabConfig
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.semantics import basic
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.interfaces.bind import Bind_i
from acab.interfaces.value import Sentence_i
from acab_config import AcabProtocolError as APE

from .flatten_query_manager import FlattenQueryManager

logging = logmod.getLogger(__name__)
config = AcabConfig()

Node          = DI.StructView
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
        # TODO can insert handle @vars?
        data = data or {}

        data.update({'source_semantics': self})
        logging.info("Inserting: {!r} into {!r}", sen, struct)
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

    def delete(self, sen, struct, *, data=None, ctxs=None):
        logging.debug("Removing: {!r} from {!r}", sen, struct)

        pos_sen = sen.copy(data={DS.NEGATION:False})
        results = self.query(pos_sen, struct, data=data, ctxs=ctxs)

        for ctx in results:
            assert(ctx.current_node is not None)
            assert(ctx.current_node.node.parent is not None)
            # At leaf:
            # remove current from parent
            current  = ctx.current_node.node
            parent   = current.parent()
            spec     = self.lookup(parent)
            spec[0].remove(parent, current.value, data=data)

    def query(self, sen, struct, *, data=None, ctxs=None):
        """ Breadth First Search Query """
        assert(ctxs is not None)
        logging.info("Querying: {!r} : {!r}", sen ,struct)
        clause_flatten = DS.FLATTEN not in sen.data or sen.data[DS.FLATTEN]
        root           = DI.StructView(struct.root, self) if isinstance(struct, DI.Structure_i) else struct
        assert(isinstance(root, Node))

        cqm = FlattenQueryManager(sen, root, ctxs)
        with cqm:
            for source_word in cqm.current:
                for bound_word, ctxInst, view in cqm.active:
                    # This happens here to catch when $x -> a.sub.sentence
                    # in a.larger.query.$x.blah?
                    should_flatten = clause_flatten and bound_word and (DS.FLATTEN not in bound_word.data or bound_word.data[DS.FLATTEN])
                    if isinstance(bound_word, Sentence_i) and should_flatten:
                        # sub query when dealing with a sentence that needs to be flattened
                        subctx = self.query(bound_word, view, data=data, ctxs=ctxs.subctx([ctxInst.uuid]))
                        for sub_inst in subctx.active_list(clear=True):
                            subctx.push(sub_inst.progress(source_word, [sub_inst.current_node]))
                        cqm.queue_ctxs(subctx.active_list())
                    elif source_word.is_at_var and not bool(cqm._current_constraint):
                        continue
                    elif source_word.is_at_var:
                        assert(source_word == sen[0])
                        cqm.maybe_test([view])
                    else:
                        if not isinstance(view, DI.StructView):
                            breakpoint()
                        spec = self.lookup(view.node)
                        results = spec[0].access(view.node,
                                                 bound_word,
                                                 data=data)

                        cqm.maybe_test([DI.StructView(x, self) for x in results])

        return cqm.finished

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
            case DI.StructView():
                root = struct.node.Root()
                root.add(struct.node)
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


