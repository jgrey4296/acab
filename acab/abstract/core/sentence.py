"""
Defines a Sentence of Words, which can be a query, and
have fallback bindings
"""
from acab.abstract.config.config import AcabConfig

from .value import AcabValue, AcabStatement

util = AcabConfig.Get()

TYPE_INSTANCE      = util.value("Value.Structure", "TYPE_INSTANCE")
BIND               = util.value("Value.Structure", "BIND")
AT_BIND            = util.value("Value.Structure", "AT_BIND")
OPERATOR           = util.value("Value.Structure", "OPERATOR")
SENTENCE_TYPE      = util.value("Type.Primitive", "SENTENCE")
ANON_VALUE         = util.value("Symbols", "ANON_VALUE")

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
            assert(all([isinstance(x, AcabValue) for x in words])), breakpoint()
            assert(not any([x.is_at_var for x in words[1:]]))
        else:
            words = []

        super().__init__(words, data=data, params=params, tags=tags)

    def __str__(self):
        words_str = [str(x) for x in self.words]
        return " ".join(words_str)

    @property
    def type(self):
        """ Lazy Type Construction """
        if self._data[TYPE_INSTANCE] is None:
            self._data[TYPE_INSTANCE] = Sentence.build([SENTENCE_TYPE])

        return self._data[TYPE_INSTANCE]

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
                output.append(word)
                continue

            if not word._value in bindings:
                output.append(word)
                continue

            # Sentence invariant: only word[0] can have an at_bind
            if word.is_at_var:
                retrieved = bindings[AT_BIND + word._value]
            else:
                retrieved = bindings[word._value]


            if isinstance(retrieved, Sentence) and len(retrieved) == 1:
                copied = retrieved[0].copy()
                copied._data.update(word._data)
                copied._data[BIND] = False
                output.append(copied)
            elif isinstance(retrieved, AcabValue):
                copied = retrieved.copy()
                copied._data.update(word._data)
                copied._data[BIND] = False
                output.append(retrieved)
            else:
                # TODO how often should this actually happen?
                # won't most things be values already?
                # TODO get a type for basic values
                new_word = AcabValue(retrieved, data=word._data)
                new_word._data[BIND] = False
                output.append(new_word)

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

# This enables values to build their type sentences
AcabValue._set_sentence_constructor(Sentence.build)
