"""
The Core Value Class
"""
from py_rule import util

class PyRuleValue:

    def __init__(self, type_str=None):
        self._type = util.NAME_S
        if type_str is not None:
            self._type = type_str
        # TODO: make this a GUID
        self._name = "AnonValue"
        self._path = None
        # Vars are *not* nodes, they are just strings
        # This is because they serve only one purpose,
        # and constructing nodes in abstract.parsing.utils
        # would be complicated
        self._vars = []

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise NotImplementedError()

    def copy(self):
        """ Data needs to be able to be copied """
        raise NotImplementedError()

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()

    def var_set(self):
        """ Data needs to be able to report internal variables """
        raise NotImplementedError()

    def set_name_and_vars(self, path, tvars=None):
        """ For creating more involved statements
        (like rules, typedefs etc), use abstract.parsing.util.STATEMENT_CONSTRUCTOR
        which will call this to apply the location name, and type variables """
        assert(isinstance(path, PyRuleValue) and path._type == util.SEN_S)
        node = path[-1]
        self._name = node._value
        self._path = path

        node._value  = self
        node._data[util.VALUE_TYPE_S] = self._type
        node._data[util.BIND_S] = False

        if tvars is not None:
            assert(all([isinstance(x, str) for x in tvars]))
            self._vars += tvars

        return self

    def value_string(self):
        return str(self)
