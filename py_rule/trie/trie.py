"""
A Trie for Types
"""
from .nodes.trie_node import TrieNode
import IPython
import logging as root_logger
logging = root_logger.getLogger(__name__)

class Trie:

    def __init__(self, node_type=TrieNode):
        self._root = node_type.Root()
        self.node_type = node_type

    def __str__(self):
        return self.print_trie()

    def __repr__(self):
        return "Trie: {}".format(len(self.get_nodes()))

    def __len__(self):
        return len(self.get_nodes(lambda x: True))

    def query(self, path):
        current = self._root
        for x in path:
            logging.debug("Searching {} for: {}".format(str(current), x))
            if current.has_child(x):
                current = current.get_child(x)
            else:
                return None
        return current

    def add(self, path, data=None, update=None, u_data=None):
        """ Add the data to the leaf defined by path,
        updating each node along the way using update and u_data
        """
        current = self._root
        current_path = []
        for x in path:
            current_path.append(x)
            if current.has_child(x):
                current = current.get_child(x)
                logging.debug("Trie: Retrieved: {}".format(current))
            else:
                current = current.add_child(self.node_type(x))
                logging.debug("Trie: Added: {}".format(current))
            if update is not None:
                update(current, x, current_path, u_data)

        current.set_data(data)

        return current

    def remove(self, path):
        query = self.query(path[:-1])
        if query is not None:
            if hasattr(path[-1], '_value') and str(path[-1]._value) in query._children:
                del query._children[str(path[-1]._value)]
            elif str(path[-1]) in query._children:
                del query._children[str(path[-1])]

    def get_nodes(self, pred=None):
        nodes = []
        queue = list(self._root._children.values())
        visited = set()
        while queue:
            current = queue.pop(0)
            visited.add(current)
            if (pred is None or pred(current)) and current not in nodes:
                nodes.append(current)
            queue += [x for x in list(current._children.values()) if x not in visited]

        return nodes

    def print_trie(self):

        output = []
        queue = [([], x) for x in self._root]
        processed = set()
        while bool(queue):
            curr_path, current_node = queue.pop(0)
            total_path = curr_path + [current_node]
            if not bool(current_node):
                #if leaf
                output.append("".join([str(x) for x in curr_path] + [current_node.opless_print()]))
            else:
                queue += [(total_path, x) for x in current_node]

        return "\n".join(output)
