""" The Base Node Class the rest of PyRule extends """
from .value import PyRuleValue
from fractions import Fraction
from re import Pattern

class PyRuleNode(PyRuleValue):
    """ The Abstract Node Class """

    def __init__(self, value, data=None):

        value_type_verify = (value is None or
                             isinstance(value, (int,
                                                float,
                                                bool,
                                                str,
                                                Fraction,
                                                Pattern,
                                                PyRuleValue)))
        assert value_type_verify, type(value)

        self._value = value
        self._children = {}
        self._data = {}
        if data:
            self._data.update(data)


    def __str__(self):
        """ String should create a re-parseable output """
        raise Exception("Abstract str method")

    def __repr__(self):
        """ Repr should create an unambiguous debug string """
        raise Exception("Abstract repr method")

    def __hash__(self):
        return hash(str(self))

    def __len__(self):
        return len(self._children)

    def __bool__(self):
        return bool(self._children)

    def __contains__(self, v):
        return self.has_child(v)

    def __iter__(self):
        return iter(self._children.values())

    def value_string(self):
        if isinstance(self._value, PyRuleNode):
            return self._value.value_string()
        else:
            return str(self._value)

    def set_data(self, data):
        if data is not None:
            self._data.update(data)

    def add_child(self, node):
        assert(isinstance(node, PyRuleNode))
        self._children[node.value_string()] = node
        return node

    def get_child(self, node):
        if isinstance(node, str):
            return self._children[node]
        else:
            return self._children[node.value_string()]

    def has_child(self, node):
        if isinstance(node, str):
            return node in self._children
        else:
            return node.value_string() in self._children

    def remove_child(self, node):
        if node in self:
            if isinstance(node, str):
                del self._children[node]
            else:
                del self._children[node.value_string()]
            return True

        return False

    def clear_children(self):
        self._children = {}
