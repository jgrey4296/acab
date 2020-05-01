""" Contexts: A Container for all partial matches of a query being run """
import itertools as it
import logging as root_logger

from py_rule.abstract.node import PyRuleNode
from py_rule.util import AT_BIND_S, FALLBACK_S, AT_BIND_S, BIND_S

logging = root_logger.getLogger(__name__)

# Utility functions:
def test_alphas(node, comps):
    """ Run alpha tests against a retrieved value """
    return all([x(node) for x in comps])

def test_betas(node, comps, data):
    """ Run a beta tests against a retrieved value, with supplied bindings """
    return all([x(node, data=data) for x in comps])


class Contexts:
    """ Container of available contexts for a match in the trie
    A list of tuples: ({}, LastAccessedNode)
    """
    def __init__(self, start_node=None, bindings=None):
        """ Setup the initial context of no bindings
        """
        self._bind_groups = []
        self._nodes = []
        self._failures = []
        self._queued_failures = []

        if bindings is not None:
            if not isinstance(bindings, list):
                bindings = [bindings]

            self._bind_groups += bindings

        if start_node is not None:
            self._nodes.append(start_node)


    def __len__(self):
        return len(self._bind_groups)

    def __getitem__(self, key):
        """ Get binding dictionaries """
        if isinstance(key, slice):
            return [x for x in self._bind_groups.__getitem__(key)]

        return self._bind_groups[key]

    def __iter__(self):
        return iter(self._bind_groups)

    def __repr__(self):
        if bool(self):
            return "Context: {}".format(len(self))
        else:
            return "Context: False"

    def __bool__(self):
        return bool(self._bind_groups)



    def append(self, *data):
        """ Add a number of matching possibilities into this set of contexts """
        assert(all([isinstance(x, tuple) for x in data]))
        for x, y in data:
            assert(x is not None)
            self._bind_groups.append(x)
            self._nodes.append(y)

    def fail(self, item=None):
        """ Remove all contexts, as none are suitable """
        if item is not None:
            self._queued_failures.append(item)
        else:
            self.clear_bind_groups()


    def invert(self):
        # swap failures with successes
        temp = self._bind_groups
        self._bind_groups = self._queued_failures
        self._queued_failures = temp

    def promote_failures(self, fallback_pairs):
        while bool(self._queued_failures):
            current = self._queued_failures.pop(0)
            for bind_target, val in clause._data[FALLBACK_S]:
                current[bind_target.value] = val
                self.append((current, None))

    def demote_failures(self):
        self._failures += self._queued_failures
        self._queued_failures = []

    def clear_bind_groups(self):
        self._bind_groups = []
        self._nodes = []

    def force_node_position(self, target=None, binding=None):
        assert (target is not None or binding)
        if not bool(self):
            self._bind_groups = [{}]

        if binding is None:
            self._nodes = [target]
            return

        assert(binding is not None)
        bind_node_name = AT_BIND_S + binding
        bind_groups = self._bind_groups
        self.clear_bind_groups()

        self._bind_groups = [x for x in bind_groups if bind_node_name in x]
        self._nodes = [x[bind_node_name] for x in self._bind_groups]

    def pairs(self):
        return zip(self._bind_groups, it.cycle(self._nodes))


    def depth_apply(self, words):
        raise NotImplementedError()

    def breadth_apply(self, word, test_sequence=None):
        """ Apply a query word, and sequence form of tests, to
        all available contexts in a BFS manner """
        assert(not word._data[BIND_S] == AT_BIND_S)
        pairs  = self.pairs()
        self.clear_bind_groups()

        if test_sequence is None:
            test_sequence = [Contexts.non_bind_value_match,
                             Contexts.existing_bind_match,
                             Contexts.create_new_bindings]

        for (data, last_node) in pairs:
            current_test_sequence = test_sequence[:]
            test_enacted = False
            test_failed = False
            while (not test_enacted) and bool(test_sequence):
                test_type = current_test_sequence.pop(0)

                test_enacted, test_failed = test_type(self, word, last_node, data)


            if (not test_enacted) or test_failed:
                self.fail(data.copy())


    def non_bind_value_match(self, a, b, data):
        """ Compare two values without caring about bindings
        a is the test node, b is the existing node
        """
        logging.info("Running non bind value match: {}{}".format(repr(b), repr(a)))
        assert(isinstance(a, PyRuleNode))
        assert(isinstance(b, PyRuleNode))
        if a.is_var:
            return False, False

        alphas, betas, regexs = a.split_tests()
        new_node, new_data = (None, None)
        logging.info("Not Bind: {}|{}".format(str(a), b._children.keys()))
        if a in b:
            logging.info("Suitable value")
            new_data, new_node = b.get_child(a).test_regexs_for_matching(regexs,
                                                                         data)
        if new_data is not None:
            self.append((new_data, new_node))

        return True, new_data is None

    def existing_bind_match(self, a, b, data):
        """ Compare an existing bound value to the children of b
        a is the test node, b is the existing node
        """
        logging.info("Running existing bind match: {}{}".format(repr(b), repr(a)))
        assert(isinstance(a, PyRuleNode))
        assert(isinstance(b, PyRuleNode))
        alphas, betas, regexs = a.split_tests()
        new_node, new_data = (None, None)
        if a.name not in data:
            return False, False

        if data[a.name] in b._children \
           and test_alphas(data[a.name], alphas) \
           and test_betas(data[a.name], betas, data):
            new_data, new_node = b._children[data[a.name]].test_regexs_for_matching(regexs,
                                                                                    data)
        if new_data is not None:
            self.append((new_data, new_node))

        return True, new_data is None

    def create_new_bindings(self, word, node, data):
        """ Create new bindings for word previously unbound variable.
        word: the clause component,
        node: the node
        """
        logging.info("Creating new bindings: {} {}".format(repr(node),
                                                           repr(word)))
        assert(isinstance(word, PyRuleNode))
        assert(isinstance(node, PyRuleNode))
        alphas, betas, regexs = word.split_tests()

        output = []
        potentials = node._children.values()
        passing = [x for x in potentials if test_alphas(x, alphas)
                   and test_betas(x, betas, data)]
        logging.info("Passing: {}".format(len(passing)))
        for x in passing:
            new_data = data.copy()
            new_data[word._value] = x._value
            new_data[AT_BIND_S + word._value] =  x

            output.append(x.test_regexs_for_matching(regexs, new_data))

        successes = [x for x in output if x[0] is not None]
        self.append(*successes)

        return True, not bool(successes)
