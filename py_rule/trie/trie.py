"""
A Trie for Types
"""
import py_rule.type_exceptions as te
from .nodes.trie_node import TrieNode
import IPython
import logging as root_logger
logging = root_logger.getLogger(__name__)


log_messages = {}
log_messages['validate_top'] = "Validating: {} on {} ({})"
log_messages['curr_def'] = "Current Definition to Validate: {} : {} : {}"
log_messages['curr_use_set'] = "Current Usage Set: {}"
log_messages['no_children'] = "Val: No Children, assigning type: {} to {}"
log_messages['match_type_usage'] = "Matching Type {} onto usage set"
log_messages['mult_child'] = "Current Def has multiple children, checking for conflicts in structure"

class Trie:

    def __init__(self, node_type=TrieNode):
        self._root = node_type.Root()
        self.node_type = node_type

    def __str__(self):
        return self._root.root_str()

    def __repr__(self):
        return "Trie: {}".format(len(self.get_nodes()))

    def __len__(self):
        return len(self.get_nodes(lambda x: not bool(x)))

    def query(self, path):
        current = self._root
        for x in path:
            logging.debug("Searching {} for: {}".format(str(current), x))
            if current.has_child(x):
                current = current.get_child(x)
            else:
                return None
        return current

    def add(self, path, data, update=None, u_data=None):
        current = self._root
        current_path = []
        for x in path:
            current_path.append(x)
            if current.has_child(x):
                current = current.get_child(x)
                logging.debug("Trie: Retrieved: {}".format(current))
            else:
                current = current.add_child(self.node_type(x, current_path))
                logging.debug("Trie: Added: {}".format(current))
            if update is not None:
                update(current, x, current_path, u_data)

        current.set_data(data)

        return current

    def remove(self, path):
        query = self.query(path[:-1])
        if query is not None and path[-1].name in query._children:
            del query._children[path[-1].name]

    def get_nodes(self, pred=None):
        nodes = []
        queue = [self._root]
        visited = set()
        while queue:
            current = queue.pop(0)
            visited.add(current)
            if (pred is None or pred(current)) and current not in nodes:
                nodes.append(current)
            queue += [x for x in list(current._children.values()) if x not in visited]

        return nodes

    def print_trie(self):
        leaves = self.get_nodes(lambda x: not bool(x._children))
        leaf_paths = [x.print_path() for x in leaves]
        return "\n".join(leaf_paths)
