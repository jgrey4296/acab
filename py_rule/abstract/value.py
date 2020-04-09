"""
The Core Value Class
"""
import logging as root_logger
from uuid import uuid1
from fractions import Fraction
from re import Pattern

from py_rule import util
from py_rule.abstract.printing import util as PrU

logging = root_logger.getLogger(__name__)


class PyRuleValue:

    def __init__(self,
                 value,
                 type_str=None,
                 data=None,
                 params=None,
                 tags=None,
                 name=None):
        value_type_verify = (value is None or
                             isinstance(value, (int,
                                                float,
                                                bool,
                                                str,
                                                list,
                                                tuple,
                                                Fraction,
                                                Pattern,
                                                PyRuleValue)))
        assert value_type_verify, type(value)

        self._uuid = uuid1()
        self._name = None
        self._value = value
        self._path = None

        self._vars = []
        self._tags = set()
        self._data = {}

        self._data.update(util.DEFAULT_VALUE_DATA)

        if type_str is not None:
            self._data[util.VALUE_TYPE_S] = type_str
        if data is not None:
            self._data.update(data)
        if params is not None:
            self._vars += params
        if tags is not None:
            self._tags.update(tags)
        if name is not None:
            self._name = name

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        uuid = str(self._uuid)
        uuid_chop = "{}..{}".format(uuid[:4],uuid[-4:])
        return "{}:{}".format(uuid_chop, self.name)

    def __repr__(self):
        return "{}({})".format(self.__class__.__name__,
                               str(self))

    def __hash__(self):
        return hash(str(self))


    @property
    def name(self):
        if isinstance(self._value, PyRuleValue):
            return self._value.name
        elif self._name is not None:
            return self._name
        else:
            return self._value

    @property
    def value(self):
        return self._value
    @property
    def is_var(self):
        return self._data[util.BIND_S] is not False

    @property
    def is_at_var(self):
        return self._data[util.BIND_S] == util.AT_BIND_S

    @property
    def type(self):
        return self._data[util.VALUE_TYPE_S]

    @property
    def var_set(self):
        """ Return a dict of sets of all bindings this value utilizes
        returns { 'in' : set(), 'out' : set() }
        """
        # ie: Query(a.b.$x? a.q.$w?).get_bindings() -> {'in': [], 'out': [x,w]}
        # Action(+(a.b.$x), -(a.b.$w)).get_bindings() -> {'in': [x,w], 'out': []}
        logging.debug("{} is using default var_set method".format(self.__class__))
        return {'in': set(self._vars), 'out': set()}


    def copy(self):
        """ Data needs to be able to be copied """
        return PyRuleValue(self._value, type_str=self.type,
                           data=self._data, params=self._vars,
                           tags=self._tags, name=self._name)

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()

    def apply_onto(self, path, tvars=None, tags=None):
        """ Apply a value onto the leaf of a path.
        (eg: rules, typedefs etc), use abstract.parsing.util.STATEMENT_CONSTRUCTOR
        which will call this to apply the location name, and type variables """
        assert(isinstance(path, PyRuleValue) and path.type == util.SEN_S)
        node = path[-1]
        self._name = node.name
        self._path = path

        node._value                   = self
        node._data[util.VALUE_TYPE_S] = self.type
        node._data[util.BIND_S]       = False

        if tvars is not None:
            assert(all([isinstance(x, str) for x in tvars]))
            self._vars += tvars

        if tags is not None:
            assert(all([isinstance(x, str) for x in tags]))
            self._tags.update(tags)

        return self

    def has_tag(self, *tags):
        return all([t in self._tags for t in tags])

    def verify(self):
        """ Raise An Exception if this necessary """
        return

    def set_data(self, data):
        if data is not None:
            self._data.update(data)

    def pprint(self, **kwargs):
        return PrU.print_value(self, **kwargs)
