"""
The Core Value Class
"""
# pylint: disable=bad-whitespace
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from dataclasses import dataclass, field, InitVar

from weakref import ref
import logging as root_logger
from uuid import uuid1, UUID
from fractions import Fraction
from re import Pattern
from copy import deepcopy

from acab.abstract.config.config import AcabConfig

logging            = root_logger.getLogger(__name__)

util             = AcabConfig.Get("acab/abstract/config")
TYPE_INSTANCE    = util.value("Value.Structure", "TYPE_INSTANCE")
BIND             = util.value("Value.Structure", "BIND")
AT_BIND          = util.value("Value.Structure", "AT_BIND")
ANON_VALUE       = util.value("Symbols", "ANON_VALUE")
SENTENCE_TYPE      = util.value("Type.Primitive", "SENTENCE")
BIND_SYMBOL      = util.value("Symbols", "BIND")
AT_BIND_SYMBOL   = util.value("Symbols", "AT_BIND")
TYPE_BOTTOM_NAME = util.value("Data", "TYPE_BOTTOM_NAME")
UUID_CHOP        = bool(int(util.value("Print.Data", "UUID_CHOP")))

@dataclass(frozen=True)
class AcabValue:
    _value_types: ClassVar[Set[Any]]          = set([str, Pattern, list])

    name : str            = field(default=None)
    value : Any           = field(default=None)
    params : List[Any]    = field(default_factory=list)
    tags : Set[str]       = field(default_factory=set)
    data : Dict[str, Any] = field(default_factory=dict)
    uuid : UUID           = field(default_factory=uuid1)

    @staticmethod
    def safe_make(value: Any,
                  data: Optional[Dict[Any, Any]]=None,
                  _type: Optional['Sentence']=None,
                  **kwargs) -> 'AcabValue':
        """ Wrap the provided value in an AcabValue,
        but only if it isn't an AcabValue already """
        _data = {}
        if data is not None:
            _data.update(data)
        if _type is not None:
            _data.update({TYPE_INSTANCE: _type})

        if isinstance(value, AcabValue):
            assert(_type is None)
            new_val = value.set_data(_data)
            return new_val
        else:
            # TODO: detect base types
            return AcabValue(value, data=_data, **kwargs)

    def __post_init__(self):
        # Applicable values: Self + any registered
        value_type_tuple = tuple([AcabValue] + list(AcabValue._value_types))
        assert (self.value is None or isinstance(self.value, value_type_tuple)), breakpoint()

        # NOTE: use of setattr to override frozen temporarily to update name
        #
        # TODO: this could be a sieve?
        assert(self.name is not None or self.value is not None)
        name_update = None
        if self.name is not None:
            assert(isinstance(self.name, str))
        elif isinstance(self.value, Pattern):
            name_update = self.value.pattern
        elif isinstance(self.value, (list, AcabStatement)):
            name_update = ANON_VALUE
        else:
            name_update = str(self.value)

        if name_update is not None:
            object.__setattr__(self, "name", name_update)

        if self.value is None:
            object.__setattr__(self, "value", self.name)

        if TYPE_INSTANCE not in self.data:
            self.data[TYPE_INSTANCE] = TYPE_BOTTOM_NAME


    def __str__(self):
        """ the simplest representation of the value,
        for internal use.
        For reparseable output, use a PrintSemantics
        """
        if self.is_at_var:
            return AT_BIND_SYMBOL + ANON_VALUE
        elif self.is_var:
            return BIND_SYMBOL + ANON_VALUE
        else:
            return self.name


    def __repr__(self):
        uuid = str(self.uuid)
        if UUID_CHOP:
            uuid = "{}..{}".format(uuid[:4],uuid[-4:])
        val_str = ""
        if self.value is not self and self.value is not self.name:
            val_str = ":" + str(self.value)

        return "({}:{}:{}{})".format(self.__class__.__name__,
                                     uuid,
                                     str(self.name),
                                     val_str)

    @property
    def type(self) -> 'Sentence':
        """ Lazy Type Construction """
        type_matches_t = isinstance(self.data[TYPE_INSTANCE], Sentence)
        if not type_matches_t:
            self.data[TYPE_INSTANCE] = Sentence.build([self.data[TYPE_INSTANCE]])

        return self.data[TYPE_INSTANCE]

    # def __hash__(self):
    #     if self._hash_name is not None:
    #         return self._hash_name

    #     self._hash_name = hash(str(self))
    #     return self._hash_name

    # def __eq__(self, other):
    #     """ Base eq: compare hashes  """
    #     if id(self) == id(other):
    #         return True
    #     elif isinstance(other, str):
    #         return str(self) == other
    #     elif not isinstance(other, AcabValue):
    #         return False

    #     assert(isinstance(other, AcabValue))
    #     uuid_match = self._uuid == other._uuid
    #     hash_match = hash(self) == hash(other)
    #     return uuid_match or hash_match




    @property
    def is_var(self) -> bool:
        return self.data[BIND] is not False

    @property
    def is_at_var(self) -> bool:
        return self.data[BIND] == AT_BIND

    @property
    def var_set(self) -> Dict[str, Set[Any]]:
        """ Return a dict of sets of all bindings this value utilizes
        returns { 'in' : set(), 'out' : set() }
        """
        # ie: Query(a.b.$x? a.q.$w?).get_bindings() -> {'in': [], 'out': [x,w]}
        # Action(+(a.b.$x), -(a.b.$w)).get_bindings() -> {'in': [x,w], 'out': []}
        # a.b.$x -> {'in': [x], 'out' : [x]}

        # logging.debug("{} is using default var_set method".format(self.__class__))

        # TODO: get var_set of value if its an acab_value?
        out_set = set()
        in_set = set(self.params)
        if self.is_var:
            in_set.add(self)
            # TODO why in the out_set as well?
            out_set.add(self)

        return {'in': in_set, 'out': out_set}


    def alpha_rename(self):
        """
        TODO should variables be de bruijn indexed instead?
        return modified copy
        """
        raise NotImplementedError()

    def copy(self) -> 'AcabValue':
        """ Data needs to be able to be copied """
        try:
            new_copy = deepcopy(self)
        except TypeError as err:
            breakpoint()

            new_copy = deepcopy(self)
        # Override the UUID copy
        new_copy._uuid = uuid1()
        # Override cached hash name
        new_copy._hash_name = None
        return new_copy

    def bind(self, bindings) -> 'AcabValue':
        """ Data needs to be able to bind a dictionary
        of values to internal variables
        return modified copy
        """
        # TODO recurse this
        if self.is_var and self.value in bindings:
            return bindings[self.value]
        else:
            return self


    def set_data(self, data) -> 'AcabValue':
        """ Force a value's data to be updated,
        return modified copy
        """
        if data is not None:
            self.data.update(data)

        return self

    def apply_params(self, params, data=None) -> 'AcabValue':
        """
        return modified copy
        """
        safe_params = [AcabValue.safe_make(x, data=data) for x in params]
        self.params += safe_params
        return self

    def apply_tags(self, tags) -> 'AcabValue':
        """
        return modified copy
        """
        safe_tags = [x.value if isinstance(x, AcabValue) else x for x in tags]
        self.tags.update(safe_tags)
        return self

    def has_tag(self, *tags) -> bool:
        return all([t in self.tags for t in tags])

    def to_simple_value(self) -> 'AcabValue':
        simple_value = AcabValue.safe_make(self.name, data=self.data)
        simple_value.set_data({TYPE_INSTANCE: Sentence.build([TYPE_BOTTOM_NAME])})
        return simple_value







class AcabStatement(AcabValue):
    """ AcabStatement functions the same as AcabValue,
    but provides specific functionality for converting to a string
    """
    # @property
    # def value(self) -> 'AcabStatement':
    #     return self

    def __post_init__(self):
        super(AcabStatement, self).__post_init__()

    def to_abstract_sentences(self) -> List['Sentence']:
        """
        Represent a Complex Object in the verbose Core Language.
        (ie: just Words, Sentences, Variables, and Types)
        eg: Convert a Query "a.b(>20).c?" into a set of explicit sentences:
        query.[a.b.c]
        operator(λgreaterThan).[a.b].20.Bool
        """
        raise NotImplementedError()



class Sentence(AcabStatement):
    @staticmethod
    def build(words, **kwargs):
        safe_words = [AcabValue.safe_make(x) for x in words]
        return Sentence(value=safe_words, **kwargs)


    def __post_init__(self):
        super(Sentence, self).__post_init__()
        self.data[TYPE_INSTANCE] = SENTENCE_TYPE

    def __len__(self):
        return len(self.words)
    def __iter__(self):
        return iter(self.words)

    def __getitem__(self, i):
        if isinstance(i, slice):
            return Sentence(self.words.__getitem__(i), data=self._data)
        return self.words.__getitem__(i)

    @property
    def words(self):
        return self.value

    def clear(self):
        """
        return modified copy
        """
        self_copy = self.copy()
        self_copy.value = []
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


