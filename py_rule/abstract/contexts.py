""" Contexts: A Container for all partial matches of word query being run """
import itertools as it
import logging as root_logger

from py_rule.abstract.node import PyRuleNode
from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.query import QueryComponent
from py_rule.util import AT_BIND_S, FALLBACK_S, AT_BIND_S, BIND_S, CONSTRAINT_S

logging = root_logger.getLogger(__name__)

# Utility functions:
def test_alphas(node, comps, data=None, engine=None):
    """ Run alpha tests against word retrieved value """
    return all([x(node, data=data, engine=engine) for x in comps])

def test_betas(node, comps, data=None, engine=None):
    """ Run word beta tests against word retrieved value, with supplied bindings """
    return all([x(node, data=data, engine=engine) for x in comps])


class Contexts:
    """ Container of available contexts for word match in the trie
    Conceptually a list of tuples: ({}, LastAccessedNode)
    And Stores failure state
    """
    @staticmethod
    def rebind_across_contexts(names, values, base):
        assert(isinstance(base, dict))
        assert(isinstance(names, list))
        assert(isinstance(values, tuple))
        new_base = {}
        new_base.update(base)
        for x,y in zip(names, values):
            new_base[x.name] = PyRuleValue.safe_make(y)

        return new_base


    def __init__(self, start_node=None, bindings=None, engine=None):
        """
        Setup the initial context of no bindings
        """
        self._bind_groups = []
        self._nodes = []
        self._failures = []
        self._queued_failures = []
        self._engine = engine
        self._test_sequence = [Contexts.non_bind_value_match,
                               Contexts.existing_bind_match,
                               Contexts.create_new_bindings]


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


    def depth_apply(self, words, test_sequence=None):
        raise NotImplementedError()

    def breadth_apply(self, word, test_sequence=None):
        """ query word, with a sequence of tests,
        in all available bind groups, BFS manner """
        assert(not word.is_at_var)
        if test_sequence is None:
            test_sequence = self._test_sequence[:]

        pairs  = self.pairs()
        self.clear_bind_groups()

        for (data, last_node) in pairs:
            current_test_sequence = test_sequence[:]
            test_enacted = False
            test_failed = False
            while (not test_enacted) and bool(test_sequence):
                test_type = current_test_sequence.pop(0)

                test_enacted, test_failed = test_type(self, word, last_node, data)


            if (not test_enacted) or test_failed:
                self.fail(data.copy())

    # TODO simplify test sequence
    def non_bind_value_match(self, word, node, data):
        """ Compare two values without caring about bindings
        word is the test value, node is the existing node
        """
        logging.info("Running non bind value match: {}{}".format(repr(node), repr(word)))
        assert(isinstance(word, PyRuleValue))
        assert(isinstance(node, PyRuleNode))
        if word.is_var:
            return False, False

        alphas, betas, regexs = split_tests(word)
        # TODO: test alphas and betas
        new_node, new_data = (None, None)
        logging.info("Not Bind: {}|{}".format(str(word), node._children.keys()))
        if word in node:
            logging.info("Suitable value")
            new_data, new_node = node.get_child(word).test_regexs_for_matching(regexs,
                                                                               data)
        if new_data is not None:
            self.append((new_data, new_node))

        return True, new_data is None

    def existing_bind_match(self, word, node, data):
        """ Compare an existing bound value to the children of node
        word is the test node, node is the existing node
        """
        logging.info("Running existing bind match: {}{}".format(repr(node), repr(word)))
        assert(isinstance(word, PyRuleValue))
        assert(isinstance(node, PyRuleNode))
        alphas, betas, regexs = split_tests(word)
        new_node, new_data = (None, None)
        if word.name not in data:
            return False, False

        if data[word.name] in node \
           and test_alphas(data[word.name], alphas, data, self._engine) \
           and test_betas(data[word.name], betas, data, self._engine):
            new_data, new_node = node.get_child(data[word.name]).test_regexs_for_matching(regexs,
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
        assert(isinstance(word, PyRuleValue))
        assert(isinstance(node, PyRuleNode))
        alphas, betas, regexs = split_tests(word)

        output = []
        potentials = node._children.values()
        passing = [x for x in potentials if test_alphas(x, alphas, data, engine=self._engine)
                   and test_betas(x, betas, data, engine=self._engine)]
        logging.info("Passing: {}".format(len(passing)))
        for x in passing:
            new_data = data.copy()
            new_data[word.name] = x.value
            new_data[AT_BIND_S + word.name] =  x

            output.append(x.test_regexs_for_matching(regexs, new_data))

        successes = [x for x in output if x[0] is not None]
        self.append(*successes)

        return True, not bool(successes)



def split_tests(word):
    """ Split tests into (alphas, betas, regexs) """
    if CONSTRAINT_S not in word._data:
        return ([], [], [])

    comps = [x for x in word._data[CONSTRAINT_S] if isinstance(x, QueryComponent)]
    alphas = []
    betas = []
    regexs = []
    for c in comps:
        if c.is_regex_test:
            regexs.append(c)
        elif c.is_alpha_test:
            alphas.append(c)
        else:
            betas.append(c)

    return (alphas, betas, regexs)
