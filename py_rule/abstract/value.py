"""
The Core Value Class
"""
from weakref import ref
import logging as root_logger
from uuid import uuid1
from fractions import Fraction
from re import Pattern
from copy import deepcopy

from py_rule import util
from py_rule.abstract.printing import util as PrU

logging = root_logger.getLogger(__name__)


class PyRuleValue:
    value_types = set([int, float, Fraction, bool, str, Pattern, list, tuple])

    @staticmethod
    def safe_make(value, data=None):
        if isinstance(value, PyRuleValue):
            new_val = value.copy().set_data(data)
            return new_val
        else:
            return PyRuleValue(value, data=data)

    def __init__(self, value, type_str=None, data=None,
                 params=None, tags=None, name=None):

        value_type_tuple = tuple([PyRuleValue] + list(PyRuleValue.value_types))
        assert (value is None or isinstance(value, value_type_tuple)), type(value)

        self._uuid = uuid1()
        self._name = None
        self._value = value

        self._vars = []
        self._tags = set()
        self._data = {}

        self._data.update(util.DEFAULT_VALUE_DATA)

        if type_str is not None:
            self._data[util.VALUE_TYPE_S] = type_str
        if data is not None:
            self._data.update(data)
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
        raise NotImplementedError()

    def verify(self):
        """ Raise An Exception if this necessary """
        return self

    def set_data(self, data):
        if data is not None:
            self._data.update(data)

        return self

    def apply_vars(self, params, data=None):
        safe_params = [PyRuleValue.safe_make(x, data=data) for x in params]
        self._vars += safe_params
        return self

    def apply_tags(self, tags):
        safe_tags = [x.value if isinstance(x, PyRuleValue) else x for x in tags]
        self._tags.update(safe_tags)
        return self

    def has_tag(self, *tags):
        return all([t in self._tags for t in tags])


    def pprint(self, opts=None, **kwargs):
        return PrU.pprint(self, opts, **kwargs)


    def split_tests(self):
        """ Split tests into (alphas, betas, regexs) """
        if util.CONSTRAINT_S not in self._data:
            return ([], [], [])

        comps = self._data[util.CONSTRAINT_S]
        assert(isinstance(comps, list))
        alphas = []
        betas = []
        regexs = []
        for c in comps:
            if c.is_regex_test:
                regexs.append(c)
            elif c.is_alpha_test:
                alphas.append(c)
            else:
                betas.append(c)
        return (alphas, betas, regexs)


class PyRuleStatement(PyRuleValue):

    def __init__(self, value, **kwargs):
        super(PyRuleStatement, self).__init__(value, **kwargs)
        self._path = None


    @property
    def path(self):
        if self._path is None:
            return None
        return self._path()
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
        assert(isinstance(sen, PyRuleValue))
        self._path = ref(sen)

        return self


    def to_simple_value(self):
        simple_value = PyRuleValue(self._name)
        return simple_value





PrU.register_class(PyRuleValue, PrU.print_value)
PrU.register_class(PyRuleStatement, PrU.print_statement)
