"""
The Core Value Classes: AcabValue, Instruction, Sentence
"""
import logging as root_logger
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from re import Pattern
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from uuid import UUID, uuid1
from weakref import ref

import acab.core.data.default_structure as DS
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.decorators.util import cache
from acab.error.acab_exception import AcabBasicException

logging          = root_logger.getLogger(__name__)

config           = AcabConfig.Get()
ANON_VALUE       = config.prepare("Symbols", "ANON_VALUE")()
BIND_SYMBOL      = config.prepare("Symbols", "BIND")()
AT_BIND_SYMBOL   = config.prepare("Symbols", "AT_BIND")()
FALLBACK_MODAL   = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()

UUID_CHOP        = bool(int(config.prepare("Print.Data", "UUID_CHOP")()))

T     = TypeVar('T', str, Pattern, list)

Value       = AT.Value
Sen         = AT.Sentence
Instruction = AT.Instruction

@dataclass(frozen=True)
class AcabValue(VI.Value_i, Generic[T]):
    _value_types : ClassVar[Set[Any]] = set([VI.Value_i, str, Pattern, list, type(None)])
    value        : T                  = field(default=None)

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
            _data.update({DS.TYPE_INSTANCE: _type})

        if isinstance(value, VI.Value_i):
            new_data = {}
            new_data.update(value.data)
            new_data.update(_data)
            return value.copy(data=new_data)


        return AcabValue(value=value, data=_data, **kwargs)

    def __post_init__(self):
        # Applicable values: Self + any registered
        value_type_tuple = tuple(list(AcabValue._value_types))

        if not isinstance(self.value, value_type_tuple):
            raise TypeError("AcabValue must wrap a valid type", self.value)

        # NOTE: use of setattr to override frozen temporarily to update name
        #
        # TODO: this could be a sieve?
        # TODO or move into safe_make
        # name update #########################################################
        name_update = None
        if self.name is None and self.value is None:
            name_update = self.__class__.__name__
        if self.name is not None:
            assert(isinstance(self.name, str)), self.name
        elif isinstance(self.value, Pattern):
            name_update = self.value.pattern
        elif isinstance(self.value, (list, VI.Instruction_i)):
            name_update = ANON_VALUE
        else:
            name_update = str(self.value)

        if name_update is not None:
            object.__setattr__(self, "name", name_update)
            # self.name = name_update
        # end of name update ##################################################

        if self.value is None:
            object.__setattr__(self, "value", self.name)
            # self.value = self.name

        if DS.TYPE_INSTANCE not in self.data:
            self.data[DS.TYPE_INSTANCE] = DS.TYPE_BOTTOM_NAME

        if DS.BIND not in self.data:
            self.data[DS.BIND] = False

        if any([not isinstance(x, VI.Value_i) for x in self.params]):
            original_params = self.params[:]
            self.params.clear()
            self.params.extend([AcabValue.safe_make(x, data={DS.BIND: True}) for x in original_params])


    @cache
    def __str__(self):
        """ the simplest representation of the value,
        for internal use.
        For reparseable output, use a PrintSemantics
        """
        return self.name


    @cache
    def __repr__(self):
        name_str = self.name

        if self.is_at_var:
            name_str = BIND_SYMBOL + name_str
        elif self.is_var:
            name_str = BIND_SYMBOL + name_str

        type_str = str(self.type)

        return "<{}::{}>".format(name_str, type_str)

    @cache
    def __hash__(self):
        return hash(repr(self))

    def __eq__(self, other):
        """ Base eq: compare hashes  """
        if id(self) == id(other):
            return True
        elif isinstance(other, str):
            return str(self) == other
        elif isinstance(other, UUID):
            return self.uuid == other
        elif not isinstance(other, VI.Value_i):
            return False
        elif self.uuid == other.uuid:
            return True
        else:
            return str(self) == str(other)



    def __lt__(self, other):
        if isinstance(other, AcabValue):
            return self.name < other.name
        elif isinstance(other, str):
            return self.name < str

        return TypeError("AcabValues can only be __lt__'d with AcabValues and Strings")


    @cache
    def key(self):
        """ Default Key Value->str for node mapping """
        return str(self.name)

    @property
    @cache
    def type(self) -> Sen:
        """ Lazily coerces type description to Sentence """
        type_matches_t = isinstance(self.data[DS.TYPE_INSTANCE], Sentence)
        if type_matches_t:
            return self.data[DS.TYPE_INSTANCE]

        if DS.SEMANTIC_HINT in self.data and isinstance(self.data[DS.SEMANTIC_HINT], Sentence):
            self.data[DS.TYPE_INSTANCE] = self.data[DS.SEMANTIC_HINT]
        else:
            self.data[DS.TYPE_INSTANCE] = Sentence.build([self.data[DS.TYPE_INSTANCE]])

        return self.data[DS.TYPE_INSTANCE]


    @property
    @cache
    def is_var(self) -> bool:
        return self.data[DS.BIND] is not False

    @property
    @cache
    def is_at_var(self) -> bool:
        return self.data[DS.BIND] == DS.AT_BIND

    def copy(self, **kwargs) -> Value:
        """ copy the object, but give it a new uuid """
        if 'params' not in kwargs:
            kwargs['params'] = self.params[:]
        if 'tags' not in kwargs:
            kwargs['tags'] = self.tags.copy()

        if 'data' not in kwargs:
            kwargs['data'] = self.data.copy()
        else:
            temp = self.data.copy()
            temp.update(kwargs['data'])
            kwargs['data'] = temp

        return replace(self, uuid=uuid1(), **kwargs)


    def bind(self, bindings) -> Value:
        raise Exception("Deprecated: use acab.modules.values.binding")

    def apply_params(self, params, data=None) -> Value:
        """
        return modified copy
        """
        if not bool(params):
            return self

        # TODO check params against self.params
        safe_params = [x if isinstance(x, VI.Value_i) else AcabValue(x) for x in params]
        return self.copy(params=safe_params)

    def apply_tags(self, tags:List[Value]) -> Value:
        """
        return modified copy
        """
        assert(all([isinstance(x, VI.Value_i) for x in tags]))
        if not bool(tags):
            return self

        tag_extension  = {x for x in self.tags}
        tag_extension.update(tags)
        return self.copy(tags=tag_extension)

    def has_tag(self, *tags:List[Value]) -> bool:
        return all([t in self.tags for t in tags])


    def attach_statement(self, value) -> Instruction:
        """
        Attach an unnamed statement to this value.
        Name the statement to the name of the former leaf
        return modified copy
        """
        assert(isinstance(value, VI.Value_i))
        combined_data = self.data.copy()
        combined_data.update(value.data)
        value_copy = value.copy(name=self.name, data=combined_data)
        return value_copy



    @property
    def has_var(self) -> bool:
        if self.is_var:
            return True
        if any([x.has_var for x in self.params]):
            return True
        if any([x.has_var for x in self.tags]):
            return True
        return False

    def to_word(self) -> Value:
        return self


class Instruction(AcabValue, VI.Instruction_i):
    """ Instruction functions the same as AcabValue,
    but provides specific functionality for converting to/from sentences
    """

    def __repr__(self):
        return "<{}::{}>".format(self.name, str(self.type))

    def to_word(self) -> Value:
        """ Convert a Statement to just an AcabValue, of it's name """
        new_data = {}
        new_data.update(self.data)
        new_data.update({DS.TYPE_INSTANCE: Sentence.build([DS.TYPE_BOTTOM_NAME])})
        simple_value = AcabValue.safe_make(self.name, data=new_data)
        return simple_value

    def to_sentences(self) -> List[VI.Sentence_i]:
        return []


@dataclass(frozen=True)
class Sentence(Instruction, VI.Sentence_i):

    @staticmethod
    def build(words, **kwargs):
        safe_words = [AcabValue.safe_make(x) for x in words]
        sen = Sentence(value=safe_words, **kwargs)
        return sen


    def __post_init__(self):
        AcabValue.__post_init__(self)
        self.data[DS.TYPE_INSTANCE] = DS.SENTENCE_PRIM

    def __eq__(self, other):
        if isinstance(other, str) and other[:2] == "_:":
            return str(self) == other
        elif isinstance(other, str):
            return str(self) == f"_:{other}"
        elif not isinstance(other, VI.Sentence_i):
            return False
        elif len(self) != len(other):
            return False
        else:
            return all([x == y for x,y in zip(self.words, other.words)])


    @cache
    def __repr__(self):
        name_str = self.key()
        val_str  = str(self)

        if self.is_at_var:
            name_str = BIND_SYMBOL + name_str
        elif self.is_var:
            name_str = BIND_SYMBOL + name_str

        type_str = str(self.type)

        return "<{}::{} [{}]>".format(name_str, type_str, val_str)

    @cache
    def __str__(self):
        return "{}".format(FALLBACK_MODAL.join([str(x) for x in self.words]))

    @cache
    def __len__(self):
        return len(self.words)
    def __iter__(self):
        return iter(self.words)

    def __getitem__(self, i):
        if isinstance(i, slice):
            return Sentence.build(self.words.__getitem__(i), data=self.data)
        return self.words.__getitem__(i)

    def __contains__(self, value:Union[str, Value]):
        return value in self.words

    @cache
    def key(self):
        return str(self.name)

    def copy(self, **kwargs) -> Sen:
        if 'value' not in kwargs:
            kwargs['value'] = [x.copy() for x in self.value]

        return super(Sentence, self).copy(**kwargs)

    def clear(self) -> Sen:
        """
        return modified copy
        """
        return self.copy(value=[])

    def bind(self, bindings:Dict[Any, Any]) -> Sen:
        raise Exception("Deprecated: use acab.modules.values.binding")

    def add(self, *other) -> Sen:
        """ Return a copy of the sentence, with words added to the end.
        This can flatten entire sentences onto the end
        return modified copy
        """
        words = self.words
        for sen in other:
            assert(isinstance(sen, (list, VI.Sentence_i)))
            words += [x for x in sen]

        new_sen = replace(self, value=words)
        return new_sen

    def prefix(self, prefix:Union[Value, Sen]) -> Sen:
        if isinstance(prefix, list):
            prefix = Sentence.build(prefix)
        elif not isinstance(self, Sentence):
            prefix = Sentence.build([prefix])

        return prefix.add(self)


    def attach_statement(self, value) -> Sen:
        """
        Copy the sentence,
        Replace the leaf with the provided statement,
        Name the statement to the name of the former leaf
        return modified copy
        """
        assert(isinstance(value, VI.Value_i))
        last = self.words[-1]
        combined_data = last.data.copy()
        combined_data.update(value.data)
        value_copy = value.copy(name=last.name, data=combined_data)

        new_words = self.words[:-1] + [value_copy]
        sen_copy = self.copy(value=new_words)

        return sen_copy

    def detach_statement(self) -> Tuple[Sen, List[Instruction]]:
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
            if isinstance(word, VI.Instruction_i):
                out_words.append(word.to_word())
                statements.append(word)
            else:
                out_words.append(word)

        sen_copy = self.copy(value=out_words)
        return (sen_copy, statements)


    @property
    @cache
    def is_var(self) -> bool:
        if len(self) > 1:
            return False

        return self[0].is_var

    @property
    @cache
    def is_at_var(self) -> bool:
        if len(self) > 1:
            return False

        return self[0].is_var

    @property
    def has_var(self) -> bool:
        if self.is_var:
            return True
        return any([x.is_var for x in self.words])


    def match(self, sen:Sen) -> List[Tuple[Value, Value]]:
        """ Match a target sentence's variables to self's target
        as tuples of (bind_name, value)

        eg: _:$x.b.$y match _:a.b.c -> [(x, a), (y, c)]
        """
        results = []
        if self.is_var:
            results.append((self[0], sen))
            return results

        for x,y in zip(self.words, sen.words):
            if x.is_var:
                results.append((x,y))

        return results


    def to_sentences(self) -> List[VI.Sentence_i]:
        simple_sen, statements = self.detach_statement()
        return [simple_sen]


    @property
    @cache
    def vars(self) -> List[Value]:
        return [x for x in self.words if x.is_var]
