"""
Defines a Sentence of Words, which can be a query, and
have fallback bindings
"""
from acab.config import AcabConfig

from .value import AcabValue, AcabStatement

util = AcabConfig.Get()

BIND_S       = util("Parsing.Structure", "BIND_S")
AT_BIND_S    = util("Parsing.Structure", "AT_BIND_S")
OPERATOR_S   = util("Parsing.Structure", "OPERATOR_S")
SEN_S        = util("Parsing.Structure", "SEN_S")
ANON_VALUE_S = util("Printing", "ANON_VALUE_S")

class Sentence(AcabStatement):
    """
    The Basic Sentence Class: Essentially a List of Words
    """
    @staticmethod
    def build(words, **kwargs):
        safe_words = [AcabValue.safe_make(x) for x in words]
        return Sentence(safe_words, **kwargs)

    def __init__(self, words=None, params=None, tags=None, data=None):
        if words is not None:
            assert(all([isinstance(x, AcabValue) for x in words])), words
            assert(not any([x.is_at_var for x in words[1:]]))
        else:
            words = []

        _type = AcabValue._type_system.SENTENCE
        super().__init__(words, data=data, params=params,
                         tags=tags, _type=_type)

    def __hash__(self):
        if self._hash_name is not None:
            return self._hash_name

        if self.name == ANON_VALUE_S:
            word_hashes = " ".join([str(hash(x)) for x in self.words])
            self._hash_name = hash(word_hashes)
        else:
            self._hash_name = hash(str(self) + str(self.type))

        return self._hash_name

    def __eq__(self, other):
        if isinstance(other, Sentence):
            return (len(self) == len(other)
                    and all([a == b for a,b in zip(self, other)]))

        return hash(self) == hash(other)

    def __iter__(self):
        return iter(self.words)

    def __getitem__(self, i):
        if isinstance(i, slice):
            return Sentence(self.words.__getitem__(i), data=self._data)
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


    def clear(self):
        """
        return modified copy
        """
        self_copy = self.copy()
        self_copy._value = []
        return self_copy

    def bind(self, bindings):
        """ Given a dictionary of bindings, reify the sentence,
        using those bindings.
        ie: .a.b.$x with {x: blah} => .a.b.blah
        return modified copy
        """
        assert(isinstance(bindings, dict))
        output = []

        for word in self:
            # early expand if a plain node
            if not word.is_var:
                output.append(word.copy())
                continue

            if not word._value in bindings:
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
            if isinstance(retrieved, AcabValue):
                copied_node._value = retrieved.value
                copied_node._data.update(retrieved._data)
            else:
                copied_node._value = retrieved
                copied_node._data[BIND_S] = False
            output.append(copied_node)

        return Sentence.build(output,
                              data=self._data,
                              params=self._params,
                              tags=self._tags)

    def add(self, *other):
        """ Return a copy of the sentence, with words added to the end.
        This can flatten entire sentences onto the end
        return modified copy
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
        return modified copy
        """
        assert(isinstance(value, AcabValue))
        last = self.words[-1]
        value_copy = value.copy()
        sen_copy = self.copy()

        value_copy._name = last.name
        if isinstance(value_copy, AcabStatement):
            value_copy.set_path(self)
        combined_data = last._data.copy()
        combined_data.update(value._data)
        value_copy._data.update(combined_data)

        sen_copy._value[-1] = value_copy
        return sen_copy

    def detach_statement(self, complete=False):
        """
        The inverse of attach_statement.
        Copy the sentence,
        Reduce the leaf of a sentence to a simple value
        Return modified copy, and the statement
        """
        statements = []

        if complete:
            sen_copy = self.clear()
            words = self.words[:]
        else:
            sen_copy = self.copy()
            words = [sen_copy.words.pop()]

        # collect leaf statements
        for word in words:
            if isinstance(word, (Sentence, AcabStatement)):
                sen_copy.words.append(word.to_simple_value())
                statements.append(word)
            else:
                sen_copy.words.append(word)

        return (sen_copy, statements)

    def verify(self):
        """ return modified copy """
        [x.verify() for x in self.words]

        return self
