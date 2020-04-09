"""
Defines a Sentence of Fact Words, which can be a query, and
have fallback bindings
"""
from py_rule.util import BIND_S, OPERATOR_S
from py_rule.util import SEN_S, AT_BIND_S
from py_rule.abstract.printing import util as PrU

from .value import PyRuleValue
from .node import PyRuleNode


class Sentence(PyRuleValue):
    """
    The Basic Sentence Class: Essentially a List of Words
    """

    def __init__(self, words=None, params=None, tags=None, data=None):
        if words is not None:
            assert(all([isinstance(x, PyRuleNode) for x in words])), words
            assert(not any([x.is_at_var for x in words[1:]]))
        else:
            words = []

        super().__init__(words,
                         data=data,
                         params=params,
                         tags=tags,
                         type_str=SEN_S)

    def __hash__(self):
        return super(Sentence, self).__hash__()

    def __eq__(self, other):
        return hash(self.pprint()) == hash(other.pprint())

    def __iter__(self):
        return iter(self.words)

    def __getitem__(self, i):
        if isinstance(i, slice):
            return Sentence(self.words.__getitem__(i))
        return self.words.__getitem__(i)

    def __len__(self):
        return len(self.words)


    @property
    def words(self):
        return self._value

    @property
    def var_set(self):
        obj = super(Sentence, self).var_set
        for w in self.words:
            temp = w.var_set
            obj['in'].update(temp['in'])
            obj['out'].update(temp['out'])

        return obj


    def expand_bindings(self, bindings):
        """ Given a dictionary of bindings, reify the sentence,
        using those bindings.
        ie: .a.b.$x with {x: blah} => .a.b.blah
        """
        assert(isinstance(bindings, dict))
        output = []

        for word in self:
            if not word.is_var:
                # early expand if a plain node
                output.append(word.copy())
                continue

            if word.is_at_var:
                retrieved = bindings[AT_BIND_S + word._value]
            else:
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
                        data=self._data,
                        params=self._vars,
                        tags=self._tags)

    def copy(self):
        words = [x.copy() for x in self.words]
        return Sentence(words,
                        data=self._data,
                        params=self._vars,
                        tags=self._tags)

    def add(self, *other):
        for word in other:
            assert(isinstance(word, PyRuleNode))
            self._value.append(other)

    def pprint(self, **kwargs):
        return PrU.print_sequence(self, **kwargs)
