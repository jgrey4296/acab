"""
Defines a Sentence of Fact Words, which can be a query, and
have fallback bindings
"""
from py_rule.utils import BIND_S, OPERATOR_S


class Sentence:
    """
    The Basic Sentence Class: Essentially a List of Words
    """

    def __init__(self, words=None, negated=False, fallback=None, is_query=False):
        self._words = []
        self._negated = negated
        self._fallback = []
        self._is_query = is_query
        if words is not None:
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
            fallback_str = " || " + ", ".join(["${}:{}".format(x[0], x[1]) for x in self._fallback])
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
        """ Given a list of fact components, and a dictionary of bindings,
        reify the fact, using those bindings.
        ie: .a.b.$x with {x: blah} => .a.b.blah
        """
        # TODO: respect typing
        assert(isinstance(bindings, dict))
        output = []

        for x in self:
            if not (x._data[BIND_S] and x._value in bindings):
                # early exit if a plain node
                output.append(x.copy())
                continue

            retrieved = bindings[x._value]

            if isinstance(retrieved, Sentence):
                output += [y.copy() for y in retrieved]
                output[-1]._data[OPERATOR_S] = x._data[OPERATOR_S]
                continue

            # if hasattr(retrieved, '_data'):
                # copied_node = x.copy()
                # copied_node._value = retrieved._value
                # output.append(copied_node)
            # else:
            copied_node = x.copy()
            copied_node._value = retrieved
            copied_node._data[BIND_S] = False
            output.append(copied_node)

        return Sentence(output,
                        negated=self._negated,
                        fallback=self._fallback,
                        is_query=self._is_query)
