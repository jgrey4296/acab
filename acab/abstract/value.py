"""
The Core Value Class
"""
from weakref import ref
import logging as root_logger
from uuid import uuid1
from fractions import Fraction
from re import Pattern
from copy import deepcopy

from acab import util
from acab.abstract.printing import util as PrU
from acab.abstract.type_base import TypeInstance, ATOM

logging = root_logger.getLogger(__name__)


class AcabValue:
    value_types = set([int, float, Fraction, bool, str, Pattern, list, tuple])

    @staticmethod
    def safe_make(value, data=None, _type=None):
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

        value_type_tuple = tuple(AcabValue.value_types)
        assert (value is None or isinstance(value, value_type_tuple)), type(value)

        self._uuid = uuid1()
        self._name = None
        self._value = value

        self._vars = []
        self._tags = set()
        self._data = {util.VALUE_TYPE_S: ATOM}

        self._data.update(util.DEFAULT_VALUE_DATA)

        if data is not None:
            self._data.update(data)

        if _type is not None:
            assert(isinstance(_type, TypeInstance))
            self._data[util.VALUE_TYPE_S] = _type

        if params is not None:
            self.apply_vars(params)
        if tags is not None:
            self.apply_tags(tags)
        if name is not None:
            self._name = name

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        return str(self.name)

    def __repr__(self):
        uuid = str(self._uuid)
        uuid_chop = "{}..{}".format(uuid[:4],uuid[-4:])
        return "({}:{}:{})".format(self.__class__.__name__,
                                   uuid_chop,
                                   str(self))

    def __hash__(self):
        return hash(str(self))

    def __eq__(self, other):
        return hash(self) == hash(other)


    @property
    def name(self):
        if self._name is not None:
            return self._name

        return self._value

    @property
    def value(self):
        return self._value
    @property
    def type(self):
        return self._data[util.VALUE_TYPE_S]

    @property
    def is_var(self):
        return self._data[util.BIND_S] is not False

    @property
    def is_at_var(self):
        return self._data[util.BIND_S] == util.AT_BIND_S

    @property
    def var_set(self):
        """ Return a dict of sets of all bindings this value utilizes
        returns { 'in' : set(), 'out' : set() }
        """
        # ie: Query(a.b.$x? a.q.$w?).get_bindings() -> {'in': [], 'out': [x,w]}
        # Action(+(a.b.$x), -(a.b.$w)).get_bindings() -> {'in': [x,w], 'out': []}
        # logging.debug("{} is using default var_set method".format(self.__class__))
        out_set = set()
        in_set = set(self._vars)
        if self.is_var:
            in_set.add(self)
            out_set.add(self)

        return {'in': in_set, 'out': out_set}


    def copy(self):
        """ Data needs to be able to be copied """
        return deepcopy(self)

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        if self.value in bindings:
            return bindings[self.value]
        else:
            return self

    def verify(self):
        """ Raise An Exception if this necessary """
        return self

    def set_data(self, data):
        if data is not None:
            self._data.update(data)

        return self

    def apply_vars(self, params, data=None):
        safe_params = [AcabValue.safe_make(x, data=data) for x in params]
        self._vars += safe_params
        return self

    def apply_tags(self, tags):
        safe_tags = [x.value if isinstance(x, AcabValue) else x for x in tags]
        self._tags.update(safe_tags)
        return self

    def has_tag(self, *tags):
        return all([t in self._tags for t in tags])


    def pprint(self, opts=None, **kwargs):
        return PrU.pprint(self, opts, **kwargs)



class AcabStatement(AcabValue):
    """ AcabStatement functions the same as AcabValue,
    but provides specific functionality for converting to a string
    """

    def __init__(self, value, **kwargs):
        super(AcabStatement, self).__init__(value, **kwargs)
        self._path = None


    @property
    def path(self):
        if self._path is None:
            return None
        return self._path
    @property
    def value(self):
        return self

    @property
    def pprint_has_content(self):
        return (True, True)


    def pprint(self, opts=None, **kwargs):
        return PrU.pprint(self, opts, **kwargs)

    def pprint_body(self, val):
        raise NotImplementedError()


    def set_path(self, sen):
        assert(isinstance(sen, AcabValue))
        self._path = sen.copy()

        return self


    def to_simple_value(self):
        simple_value = AcabValue(self._name)
        return simple_value


    def to_local_sentences(self):
        raise NotImplementedError()



PrU.register_class(AcabValue, PrU.print_value)
PrU.register_class(AcabStatement, PrU.print_statement)
