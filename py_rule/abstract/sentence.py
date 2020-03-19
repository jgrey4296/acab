"""
Defines a Sentence of Fact Words, which can be a query, and
have fallback bindings
"""
from py_rule.util import BIND_S, OPERATOR_S, AT_BIND_S
from .value import PyRuleValue
from .node import PyRuleNode

class Sentence(PyRuleValue):
    """
    The Basic Sentence Class: Essentially a List of Words
    """

    def __init__(self, words=None, negated=False,
                 fallback=None, is_query=False):
        self._words = []
        self._negated = negated
        self._fallback = []
        self._is_query = is_query
        if words is not None:
            assert(all([isinstance(x, PyRuleNode) for x in words]))
            assert(not any([AT_BIND_S in x._data for x in words[1:]]))
            self._words += words
        if fallback is not None:
            self._fallback += fallback[:]

    def __str__(self):
        result = "".join([str(x) for x in self._words[:-1]])
        result += self._words[-1].opless_print()
        if self._is_query:
            result += "?"
        negated_str = ""
        fallback_str = ""
        if bool(self._fallback):
            fallback_str = " || " + ", ".join(["${}:{}".format(x[0], x[1])
                                               for x in self._fallback])
        if self._negated:
            negated_str = "~"

        return "{}{}{}".format(negated_str, result, fallback_str)

    def __repr__(self):
        return "Sentence({})".format(str(self))

    def __iter__(self):
        return iter(self._words)

    def __getitem__(self, i):
        return self._words.__getitem__(i)

    def __len__(self):
        return len(self._words)

    def expand_bindings(self, bindings):
        """ Given a dictionary of bindings, reify the sentence,
        using those bindings.
        ie: .a.b.$x with {x: blah} => .a.b.blah
        """
        # TODO update this for AT_BIND
        assert(isinstance(bindings, dict))
        output = []

        for word in self:
            if not (word._data[BIND_S] and word._value in bindings):
                # early expand if a plain node
                output.append(word.copy())
                continue

            retrieved = bindings[word._value]

            if isinstance(retrieved, Sentence):
                output += [y.copy() for y in retrieved]
                output[-1]._data[OPERATOR_S] = word._data[OPERATOR_S]
                continue

            copied_node = word.copy()
            copied_node._value = retrieved
            copied_node._data[BIND_S] = False
            output.append(copied_node)

        return Sentence(output,
                        negated=self._negated,
                        fallback=self._fallback,
                        is_query=self._is_query)

    def copy(self):
        words = self._words[:]
        fallback = self._fallback[:]
        return Sentence(words, self._negated,
                        fallback, self._is_query)

    def add(self, other):
        assert(isinstance(other, PyRuleNode))
        self._words.append(other)
