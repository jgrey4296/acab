"""
The Core Value Class
"""
# pylint: disable=bad-whitespace
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar, replace

from weakref import ref
import logging as root_logger
from uuid import uuid1, UUID
from fractions import Fraction
from re import Pattern
from copy import deepcopy

from acab.abstract.config.config import AcabConfig
from acab.abstract.interfaces import value_interfaces as VI

logging            = root_logger.getLogger(__name__)

config             = AcabConfig.Get()
TYPE_INSTANCE    = config.value("Value.Structure", "TYPE_INSTANCE")
BIND             = config.value("Value.Structure", "BIND")
AT_BIND          = config.value("Value.Structure", "AT_BIND")
ANON_VALUE       = config.value("Symbols", "ANON_VALUE")
SENTENCE_TYPE      = config.value("Type.Primitive", "SENTENCE")
BIND_SYMBOL      = config.value("Symbols", "BIND")
AT_BIND_SYMBOL   = config.value("Symbols", "AT_BIND")
TYPE_BOTTOM_NAME = config.value("Data", "TYPE_BOTTOM_NAME")
UUID_CHOP        = bool(int(config.value("Print.Data", "UUID_CHOP")))
FALLBACK_MODAL   = config.value("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])


T = TypeVar('T', str, Pattern, list)

Value = 'AcabValue'
Sen   = 'Sentence'

@dataclass
class AcabValue(VI.ValueInterface, Generic[T]):
    _value_types: ClassVar[Set[Any]]          = set([str, Pattern, list])
    value : T = field(default=None)

    @staticmethod
    def safe_make(value: T,
                  name: str=None,
                  data: Optional[Dict[Any, Any]]=None,
                  _type: Optional[Sen]=None,
                  **kwargs) -> Value:
        """ Wrap the provided value in an AcabValue,
        but only if it isn't an AcabValue already """
        _data = {}
        if data is not None:
            _data.update(data)
        if _type is not None:
            _data.update({TYPE_INSTANCE: _type})

        if isinstance(value, AcabValue):
            assert(_type is None)
            new_val = value.copy()
            new_val.data.update(_data)
            return new_val
        else:
            return AcabValue(value=value, data=_data, **kwargs)

    def __post_init__(self):
        # Applicable values: Self + any registered
        value_type_tuple = tuple([AcabValue] + list(AcabValue._value_types))

        assert(self.value is None or isinstance(self.value, value_type_tuple))

        # NOTE: use of setattr to override frozen temporarily to update name
        #
        # TODO: this could be a sieve?
        name_update = None
        if self.name is None and self.value is None:
            name_update = self.__class__.__name__
        if self.name is not None:
            assert(isinstance(self.name, str)), breakpoint()
        elif isinstance(self.value, Pattern):
            name_update = self.value.pattern
        elif isinstance(self.value, (list, AcabStatement)):
            name_update = ANON_VALUE
        else:
            name_update = str(self.value)

        if name_update is not None:
            self.name = name_update

        if self.value is None:
            self.value = self.name

        if TYPE_INSTANCE not in self.data:
            self.data[TYPE_INSTANCE] = TYPE_BOTTOM_NAME

        if BIND not in self.data:
            self.data[BIND] = False

        if any([not isinstance(x, AcabValue) for x in self.params]):
            original_params = self.params[:]
            self.params.clear()
            self.params.extend([AcabValue.safe_make(x) for x in original_params])


    def __str__(self):
        """ the simplest representation of the value,
        for internal use.
        For reparseable output, use a PrintSemantics
        """
        if self.is_at_var:
            return AT_BIND_SYMBOL + self.name
        elif self.is_var:
            return BIND_SYMBOL + self.name
        else:
            return self.name


    def __repr__(self):
        val_str = ""
        if self.value is not self.name:
            val_str = "..."

        return "({}:{}:{})".format(self.__class__.__name__,
                                     str(self),
                                     val_str)

    def __hash__(self):
        return hash(str(self))

    def __eq__(self, other):
        """ Base eq: compare hashes  """
        if id(self) == id(other):
            return True
        elif isinstance(other, str):
            return str(self) == other
        elif not isinstance(other, AcabValue):
            return False
        elif self.uuid == other.uuid:
            return True
        else:
            return str(self) == str(other)



    @property
    def type(self) -> Sen:
        """ Lazy Type Construction """
        type_matches_t = isinstance(self.data[TYPE_INSTANCE], Sentence)
        if not type_matches_t:
            self.data[TYPE_INSTANCE] = Sentence.build([self.data[TYPE_INSTANCE]])

        return self.data[TYPE_INSTANCE]


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

    def copy(self, **kwargs) -> Value:
        """ copy the object, but give it a new uuid """
        if 'params' not in kwargs:
            kwargs['params'] = self.params[:]
        if 'tags' not in kwargs:
            kwargs['tags'] = self.tags.copy()
        if 'data' not in kwargs:
            kwargs['data'] = self.data.copy()

        return replace(self, uuid=uuid1(), **kwargs)


    def bind(self, bindings) -> Value:
        """ Data needs to be able to bind a dictionary
        of values to internal variables
        return modified copy
        """
        if self.is_var and self.value in bindings:
            assert(not self.params)
            return AcabValue.safe_make(bindings[self.value])

        if not any([x.is_var for x in self.params]):
            return self

        bound_params = [x.bind(bindings) for x in self.params]
        return self.copy(params=bound_params)


    def apply_params(self, params, data=None) -> Value:
        """
        return modified copy
        """
        if not bool(params):
            return self

        # TODO should these be just strings?
        safe_params = [x if isinstance(x, AcabValue) else AcabValue(x) for x in params]
        return self.copy(params=safe_params)

    def apply_tags(self, tags) -> Value:
        """
        return modified copy
        """
        if not bool(tags):
            return self

        safe_tags = [x.name if isinstance(x, AcabValue) else x for x in tags]
        return self.copy(tags=safe_tags)

    def has_tag(self, *tags) -> bool:
        return all([t in self.tags for t in tags])

    def to_word(self) -> Value:
        new_data = {}
        new_data.update(self.data)
        new_data.update({TYPE_INSTANCE: Sentence.build([TYPE_BOTTOM_NAME])})
        simple_value = Sentence.build([AcabValue.safe_make(self.name, data=new_data)])
        return simple_value

    def to_sentences(self):
        return [self]
class AcabStatement(AcabValue):
    """ AcabStatement functions the same as AcabValue,
    but provides specific functionality for converting to a string
    """
    def to_sentences(self) -> List[Sen]:
        """
        Represent a Complex Object in the verbose Core Language.
        (ie: just Words, Sentences, Variables, and Types)
        eg: Convert a Query "a.b(>20).c?" into a set of explicit sentences:
        query.[a.b.c]
        operator(λgreaterThan).[a.b].20.Bool
        """
        raise NotImplementedError()



@dataclass
class Sentence(AcabStatement, VI.SentenceInterface):


    @staticmethod
    def build(words, **kwargs):
        safe_words = [AcabValue.safe_make(x) for x in words]
        sen = Sentence(words=safe_words, **kwargs)
        return sen


    def __post_init__(self):
        self.value = self.words
        AcabValue.__post_init__(self)
        self.data[TYPE_INSTANCE] = SENTENCE_TYPE

    def __eq__(self, other):
        if isinstance(other, str):
            return str(self) == other
        elif not isinstance(other, Sentence):
            return False
        elif len(self) != len(other):
            return False
        else:
            return all([x == y for x,y in zip(self.words, other.words)])


    def __hash__(self):
        return hash(str(self))
    def __str__(self):
        words = FALLBACK_MODAL.join([str(x) for x in self.words])
        return "{}:{}".format(self.name, words)

    def __len__(self):
        return len(self.words)
    def __iter__(self):
        return iter(self.words)

    def __getitem__(self, i):
        if isinstance(i, slice):
            return Sentence.build(self.words.__getitem__(i), data=self.data)
        return self.words.__getitem__(i)

    @property
    def type(self) -> 'Sentence':
        """ Lazy Type Construction """
        type_matches_t = isinstance(self.data[TYPE_INSTANCE], Sentence)
        if not type_matches_t:
            self.data[TYPE_INSTANCE] = Sentence.build([self.data[TYPE_INSTANCE]])

        return self.data[TYPE_INSTANCE]

    def copy(self, **kwargs):
        if 'value' not in kwargs:
            kwargs['value'] = [x.copy() for x in self.value]

        return super(Sentence, self).copy(**kwargs)

    def clear(self):
        """
        return modified copy
        """
        return self.copy(value=[])

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

            if not word.value in bindings:
                output.append(word)
                continue

            # Sentence invariant: only word[0] can have an at_bind
            if word.is_at_var:
                retrieved = bindings[AT_BIND + word.value]
            else:
                retrieved = bindings[word.value]


            if isinstance(retrieved, Sentence) and len(retrieved) == 1:
                # Flatten len1 sentences:
                copied = retrieved[0].copy()
                copied.data.update(word.data)
                copied.data[BIND] = False
                output.append(copied)
            elif isinstance(retrieved, AcabValue):
                # Apply the variables data to the retrieval
                copied = retrieved.copy()
                copied.data.update(word.data)
                # Except retrieved isn't a binding
                copied.data[BIND] = False
                output.append(retrieved)
            else:
                # TODO how often should this actually happen?
                # won't most things be values already?
                # TODO get a type for basic values
                new_word = AcabValue(retrieved, data=word.data)
                new_word.data[BIND] = False
                output.append(new_word)

        return Sentence.build(output,
                              data=self.data,
                              params=self.params,
                              tags=self.tags)

    def add(self, *other):
        """ Return a copy of the sentence, with words added to the end.
        This can flatten entire sentences onto the end
        return modified copy
        """
        words = self.words
        for sen in other:
            assert(isinstance(sen, (list, Sentence)))
            words += [x for x in sen]

        new_sen = replace(self, value=words)
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
        combined_data = last.data.copy()
        combined_data.update(value.data)
        value_copy = value.copy(name=last.name, data=combined_data)

        new_words = self.words[:-1] + [value_copy]
        sen_copy = self.copy(words=new_words)

        return sen_copy

    def detach_statement(self):
        """
        The inverse of attach_statement.
        Copy the sentence,
        Reduce all words down to basic values
        Return modified copy, and the statement
        """
        statements = []
        out_words  = []

        # collect leaf statements
        for word in self.words:
            if isinstance(word, AcabStatement):
                out_words.append(word.to_word()[0])
                statements.append(word)
            else:
                out_words.append(word)

        sen_copy = self.copy(value=out_words)
        return (sen_copy, statements)



    def to_sentences(self):
        return [self]

