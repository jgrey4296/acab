"""
A Trie for Types
"""
import logging as root_logger
from weakref import WeakValueDictionary, ref, proxy

from py_rule.error.pyrule_base_exception import PyRuleBaseException
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.value import PyRuleStatement

from py_rule.abstract.node import PyRuleNode

logging = root_logger.getLogger(__name__)


class Trie:

    def __init__(self, node_type=PyRuleNode):
        self._root = node_type.Root()
        self._node_type = node_type
        # Stores UUIDs -> Nodes
        self._all_nodes = WeakValueDictionary()

    def __str__(self):
        return self.print_trie()

    def __repr__(self):
        return "Trie: {}".format(len(self.get_nodes()))

    def __len__(self):
        return len(self.get_nodes())


    @property
    def root(self):
        return self._root

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
        wrapped_path = [self._node_type(x) for x in path]

        current = self._root
        current_path = []
        for x, y in zip(wrapped_path, path):
            current_path.append(x)
            if current.has_child(x):
                current = current.get_child(x)
                logging.debug("Trie: Retrieved: {}".format(current))
            else:
                current = current.add_child(x)
                logging.debug("Trie: Added: {}".format(current))
                self._all_nodes[current._uuid] = current
            if update is not None:
                update(current, y, current_path, u_data)

        current.set_data(data)

        return current

    def remove(self, path):
        query_result = self.query(path[:-1])
        if query_result is not None:
            query_result.remove_child(path[-1])

    def get_nodes(self, pred=None, explore=None):
        assert(pred is None or callable(pred))
        assert(explore is None or callable(explore))
        nodes = []
        queue = list(self._root._children.values())
        visited = set()
        while queue:
            current = queue.pop(0)

            if current in nodes or current in visited:
                continue
            visited.add(current)

            if pred is None or pred(current):
                nodes.append(current)

            if explore is None:
                queue += [x for x in list(current._children.values())
                          if x not in visited]
            else:
                queue += explore(current)

        return nodes

    def print_trie(self, join_str=None):
        output = []
        queue = [([], x) for x in self._root]
        while bool(queue):
            curr_path, current_node = queue.pop(0)
            total_path = curr_path + [current_node]
            if not bool(current_node) or isinstance(current_node._value, PyRuleStatement):
                # if leaf or statement
                as_sentence = Sentence(total_path)
                output.append(as_sentence.pprint(join_str=join_str))

            if bool(current_node):
                queue += [(total_path, x) for x in current_node]

        return "\n".join(sorted(output))

    def match_as_pattern(self, possible_matches, match_func):
        """ Given a trie/node of possible matches, return only actual matches,
        Treating self as the pattern
        """
        if isinstance(possible_matches, Trie):
            possible_matches = possible_matches.root

        if not isinstance(possible_matches, PyRuleNode):
            raise PyRuleBaseException()

        final_matches = []
        pattern_nodes = list(possible_matches._children.values())
        # (current pattern position, available choices, match state)
        queue = [(x, pattern_nodes, []) for x in self.root._children.values()]

        while bool(queue):
            current, available_nodes, match_state = queue.pop(0)

            matching_nodes = match_func(current, available_nodes)
            for node in matching_nodes:
                next_match_state = match_state + [(current, node)]
                next_available = list(node._children.values())

                if bool(current):
                    next_patterns = list(current._children.values())
                    queue += [(x, next_available, next_match_state) for x in next_patterns]
                else:
                    final_matches.append(next_match_state)

        return final_matches
