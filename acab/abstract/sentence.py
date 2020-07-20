"""
Defines a Sentence of Words, which can be a query, and
have fallback bindings
"""
from acab.util import BIND_S, OPERATOR_S
from acab.util import SEN_S, AT_BIND_S
from acab.abstract.printing import util as PrU

from . import type_base as TB

from .value import AcabValue, AcabStatement


class Sentence(AcabValue):
    """
    The Basic Sentence Class: Essentially a List of Words
    """
    @staticmethod
    def build(words):
        safe_words = [AcabValue.safe_make(x) for x in words]
        return Sentence(safe_words)

    def __init__(self, words=None, params=None, tags=None, data=None):
        if words is not None:
            assert(all([isinstance(x, AcabValue) for x in words])), words
            assert(not any([x.is_at_var for x in words[1:]]))
        else:
            words = []

        super().__init__(words, data=data, params=params,
                         tags=tags, _type=TB.SENTENCE)

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


    def bind(self, bindings):
        """ Given a dictionary of bindings, reify the sentence,
        using those bindings.
        ie: .a.b.$x with {x: blah} => .a.b.blah
        """
        assert(isinstance(bindings, dict))
        output = []

        for word in self:
            # early expand if a plain node
            if not word.is_var:
                output.append(word.copy())
                continue

            # Sentence invariant: only word[0] can have an at_bind
            if word.is_at_var:
                retrieved = bindings[AT_BIND_S + word._value]
            else:
                retrieved = bindings[word._value]

            # Fixup the last modal operator if a sentence has been inserted
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

    def add(self, *other):
        """ Return a copy of the sentence, with words added to the end.
        This can flatten entire sentences onto the end
        """
        words = self.words
        for sen in other:
            assert(isinstance(sen, (list, Sentence)))
            words += [x for x in sen]


        new_sen = self.copy()
        new_sen._value = words
        return new_sen

    def attach_statement(self, value):
        """
        Copy the sentence,
        Replace the leaf with the provided statement,
        Name the statement to the name of the former leaf

        """
        assert(isinstance(value, AcabStatement))
        last = self.words[-1]
        value_copy = value.copy()
        sen_copy = self.copy()

        value_copy._name = last.name
        value_copy.set_path(self)
        combined_data = last._data.copy()
        combined_data.update(value._data)
        value_copy._data.update(combined_data)

        sen_copy._value[-1] = value_copy
        return sen_copy

    def detach_statement(self):
        """
        The inverse of attach_statement.
        Copy the sentence,
        Reduce the leaf of a sentence to a simple value
        Return the copy, and the statement
        """
        last = None

        if not isinstance(self[-1], AcabStatement):
            sen_copy = self
            last = None
        else:
            sen_copy = self[-1].path
            last = sef[-1]

        return (sen_copy, last)

    def verify(self):
        [x.verify() for x in self.words]

        return self


PrU.register_class(Sentence, PrU.print_sequence)
