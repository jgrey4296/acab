"""
The Core Value Class
"""
# pylint: disable=bad-whitespace
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from weakref import ref
import logging as root_logger
from uuid import uuid1
from fractions import Fraction
from re import Pattern
from copy import deepcopy

from acab.abstract.config.config import AcabConfig

logging            = root_logger.getLogger(__name__)

util             = AcabConfig.Get()
TYPE_INSTANCE    = util.value("Value.Structure", "TYPE_INSTANCE")
BIND             = util.value("Value.Structure", "BIND")
AT_BIND          = util.value("Value.Structure", "AT_BIND")
ANON_VALUE       = util.value("Symbols", "ANON_VALUE")
BIND_SYMBOL      = util.value("Symbols", "BIND")
AT_BIND_SYMBOL   = util.value("Symbols", "AT_BIND")
TYPE_BOTTOM_NAME = util.value("Data", "TYPE_BOTTOM_NAME")
UUID_CHOP        = bool(int(util.value("Print.Data", "UUID_CHOP")))

class AcabValue:
    value_types = set([int, float, Fraction, bool, str, Pattern, list, tuple])
    _sentence_constructor: type = None

    @staticmethod
    def _set_sentence_constructor(constructor: Callable):
        """
        Late binding to the type system, a clean import chain
        """
        AcabValue._sentence_constructor = constructor


    @staticmethod
    def safe_make(value: Any,
                  data: Optional[Dict[Any, Any]]=None,
                  _type: Optional['Sentence']=None) -> 'AcabValue':
        """ Wrap the provided value in an AcabValue,
        but only if it isn't an AcabValue already """
        if isinstance(value, AcabValue):
            assert(_type is None)
            new_val = value.copy().set_data(data)
            return new_val
        else:
            # TODO: detect base types
            return AcabValue(value, data=data, _type=_type)

    def __init__(self, value, data=None,
                 params=None, tags=None,
                 name=None, _type=None):

        # Applicable values: Self + any registered
        value_type_tuple = tuple([AcabValue] + list(AcabValue.value_types))
        assert (value is None or isinstance(value, value_type_tuple)), breakpoint()

        self._uuid = uuid1()
        self._name : Optional[str] = None
        self._value : Any = value
        self._hash_name : str = None

        self._params : List[Any] = []
        self._tags : Set[str] = set()
        self._data : Dict[str, Any] = {TYPE_INSTANCE: None, BIND : False}

        if data is not None:
            self._data.update(data)

        if _type is not None:
            if not isinstance(_type, AcabValue):
                # TODO remove need to wrap in list
                _type = AcabValue._sentence_constructor([_type])

            self._data[TYPE_INSTANCE] = _type

        if params is not None:
            self.apply_params(params)
        if tags is not None:
            self.apply_tags(tags)
        # TODO: this could be a sieve?
        if name is not None:
            assert(isinstance(name, str))
            self._name = name
        elif isinstance(self._value, Pattern):
            self._name = self._value.pattern
        else:
            self._name = str(self._value)

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
        uuid = str(self._uuid)
        if UUID_CHOP:
            uuid = "{}..{}".format(uuid[:4],uuid[-4:])
        val_str = ""
        if self.value is not self and self.value is not self.name:
            val_str = ":" + str(self.value)

        return "({}:{}:{}{})".format(self.__class__.__name__,
                                     uuid,
                                     str(self._name),
                                     val_str)

    def __hash__(self):
        if self._hash_name is not None:
            return self._hash_name

        self._hash_name = hash(str(self))
        return self._hash_name

    def __eq__(self, other):
        """ Base eq: compare hashes  """
        if id(self) == id(other):
            return True
        elif isinstance(other, str):
            return str(self) == other
        elif not isinstance(other, AcabValue):
            return False

        assert(isinstance(other, AcabValue))
        uuid_match = self._uuid == other._uuid
        hash_match = hash(self) == hash(other)
        return uuid_match or hash_match


    @property
    def name(self) -> str:
        return self._name

    @property
    def value(self):
        return self._value
    @property
    def type(self) -> 'Sentence':
        """ Lazy Type Construction """
        if self._data[TYPE_INSTANCE] is None:
            self._data[TYPE_INSTANCE] = AcabValue._sentence_constructor([TYPE_BOTTOM_NAME])

        return self._data[TYPE_INSTANCE]


    @property
    def is_var(self) -> bool:
        return self._data[BIND] is not False

    @property
    def is_at_var(self) -> bool:
        return self._data[BIND] == AT_BIND

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
        in_set = set(self._params)
        if self.is_var:
            in_set.add(self)
            # TODO why in the out_set as well?
            out_set.add(self)

        return {'in': in_set, 'out': out_set}

    @property
    def tags(self) -> Set[str]:
        return self._tags


    @property
    def params(self):
        return self._params
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

    def verify(self) -> 'AcabValue':
        """ Raise An Exception if this necessary
        return modified copy
        """
        assert(self._value is not None)
        value_type_tuple = tuple([AcabValue] + list(AcabValue.value_types))
        assert(isinstance(self._value, value_type_tuple))
        assert(self.type is not None)
        # TODO verify keys used in data


        return self

    def set_data(self, data) -> 'AcabValue':
        """ Force a value's data to be updated,
        return modified copy
        """
        if data is not None:
            self._data.update(data)

        return self

    def apply_params(self, params, data=None) -> 'AcabValue':
        """
        return modified copy
        """
        safe_params = [AcabValue.safe_make(x, data=data) for x in params]
        self._params += safe_params
        return self

    def apply_tags(self, tags) -> 'AcabValue':
        """
        return modified copy
        """
        safe_tags = [x.value if isinstance(x, AcabValue) else x for x in tags]
        self._tags.update(safe_tags)
        return self

    def has_tag(self, *tags) -> bool:
        return all([t in self._tags for t in tags])

    def to_simple_value(self) -> 'AcabValue':
        simple_value = AcabValue.safe_make(self._name, data=self._data)
        simple_value.set_data({TYPE_INSTANCE: AcabValue._sentence_constructor([TYPE_BOTTOM_NAME])})
        return simple_value


    def pprint(self, opts=None, **kwargs) -> str:
        raise DeprecationWarning("Use Print Semantics")



class AcabStatement(AcabValue):
    """ AcabStatement functions the same as AcabValue,
    but provides specific functionality for converting to a string
    """

    def __init__(self, value, **kwargs):
        super(AcabStatement, self).__init__(value, **kwargs)
        self._path = None


    @property
    def path(self) -> 'Sentence':
        return self._path
    @property
    def value(self) -> 'AcabStatement':
        return self

    @property
    def pprint_has_content(self) -> bool:
        raise DeprecationWarning("Use Print Semantics")

    def set_path(self, sen) -> 'AcabStatement':
        """
        return modified copy
        """
        assert(isinstance(sen, AcabValue))
        self._path = sen.copy()

        return self


    def to_abstract_sentences(self) -> List['Sentence']:
        """
        Represent a Complex Object in the verbose Core Language.
        (ie: just Words, Sentences, Variables, and Types)
        eg: Convert a Query "a.b(>20).c?" into a set of explicit sentences:
        query.[a.b.c]
        operator(λgreaterThan).[a.b].20.Bool
        """
        raise NotImplementedError()

    def pprint_body(self, val) -> str:
        raise DeprecationWarning("Use Print Semantics")
