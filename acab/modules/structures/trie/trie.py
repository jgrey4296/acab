"""
A Trie Structure, using AcabNodes
"""
import logging as root_logger
from weakref import WeakValueDictionary, ref, proxy
from re import search
from dataclasses import dataclass, field

from acab.abstract.core.values import Sentence
from acab.abstract.core.values import AcabValue, AcabStatement
from acab.abstract.core.node import AcabNode
from acab.abstract.core.acab_struct import AcabStruct

from acab.error.acab_base_exception import AcabBaseException
from acab.modules.semantics.independent import BasicNodeSemantics
from acab.abstract.interfaces.semantic_interfaces import SemanticSystem
from acab.abstract.interfaces.data_interfaces import StructureInterface
from acab.abstract.core.acab_struct import BasicNodeStruct

from acab.modules.semantics.dependent import BreadthTrieSemantics

from acab.abstract.config.config import AcabConfig
config = AcabConfig.Get()

CONSTRAINT_S = config.value("Value.Structure", "CONSTRAINT")
AT_BIND_S    = config.value("Value.Structure", "AT_BIND")

logging = root_logger.getLogger(__name__)

@dataclass
class Trie(BasicNodeStruct):
    all_nodes : WeakValueDictionary = field(init=False, default_factory=WeakValueDictionary)


    def __str__(self):
        return self.print_trie()

    def __repr__(self):
        return "Trie: {}".format(len(self.get_nodes()))

    def __len__(self):
        return len(self.get_nodes())





    def get_nodes(self, pred=None, explore=None):
        """ Get nodes passing a predicate function,
        exploring by an explore function:

        explore : [node] -> node -> [node]
        """
        assert(pred is None or callable(pred))
        assert(explore is None or callable(explore))
        nodes = []
        queue = list(self.root.children.values())
        visited = set()
        while queue:
            current = queue.pop(0)

            if current in nodes or current in visited:
                continue
            visited.add(current)

            if pred is None or pred(current):
                nodes.append(current)

            if explore is None:
                queue += [x for x in list(current.children.values())
                          if x not in visited]
            else:
                queue = explore(queue, current)

        return nodes


    def to_sentences(self, leaf_predicate=None):
        raise DeprecationWarning("This should be in a semantics")
        # output = []
        # queue = [([], x) for x in self.root]

        # while bool(queue):
        #     curr_path, current_node = queue.pop(0)
        #     total_path = curr_path + [current_node.value]
        #     if not bool(current_node) or isinstance(current_node.value, AcabStatement):
        #         if leaf_predicate is None or leaf_predicate(current_node):
        #             as_sentence = Sentence.build(total_path)
        #             output.append(as_sentence)

        #     if bool(current_node):
        #         queue += [(total_path, x) for x in current_node]

        # return output


    def add(self, sen):
        pass
    def query(self, sen):
        pass
    def contain(self, sen):
        pass
