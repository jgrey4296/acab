"""
Defines a Sentence of Fact Words, which can be a query, and
have fallback bindings
"""
from py_rule.util import BIND_S, OPERATOR_S, AT_BIND_S, NEGATION_S, FALLBACK_S, QUERY_S, SEN_S
from .value import PyRuleValue
from .node import PyRuleNode

class Sentence(PyRuleValue):
    """
    The Basic Sentence Class: Essentially a List of Words
    """

    def __init__(self, words=None, data=None):
        super().__init__(type_str=SEN_S)
        self._words = []
        self._data = {}
        if data is not None:
            self._data.update(data)

        if words is not None:
            assert(all([isinstance(x, PyRuleNode) for x in words]))
            assert(not any([AT_BIND_S in x._data for x in words[1:]]))
            self._words += words

    # TODO update this
    def __str__(self):
        result = "".join([str(x) for x in self._words[:-1]])
        result += self._words[-1].opless_print()
        if QUERY_S in self._data:
            result += "?"
        negated_str = ""
        fallback_str = ""
        if FALLBACK_S in self._data and self._data[FALLBACK_S] is not None:
            fallback_str = " || " + ", ".join(["${}:{}".format(x[0], x[1])
                                               for x in self._data[FALLBACK_S]])
        if NEGATION_S in self._data and self._data[NEGATION_S]:
            negated_str = "~"

        return "{}{}{}".format(negated_str, result, fallback_str)

    def __repr__(self):
        return "Sentence({})".format(str(self))

    def __eq__(self, other):
        return str(self) == str(other)

    def __iter__(self):
        return iter(self._words)

    def __getitem__(self, i):
        if isinstance(i, slice):
            return Sentence(self._words.__getitem__(i))
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

        return Sentence(output, data=self._data)

    def copy(self):
        words = self._words[:]
        return Sentence(words, data=self._data)

    def add(self, *other):
        for word in other:
            assert(isinstance(word, PyRuleNode))
            self._words.append(other)
