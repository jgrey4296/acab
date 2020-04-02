""" The Base Node Class the rest of PyRule extends """
from fractions import Fraction
from re import Pattern, search

from py_rule.util import BIND_S, AT_BIND_S
from py_rule import util

from .value import PyRuleValue

class PyRuleNode(PyRuleValue):
    """ The Abstract Node Class """

    def __init__(self, value, data=None, type_str=None):

        value_type_verify = (value is None or
                             isinstance(value, (int,
                                                float,
                                                bool,
                                                str,
                                                Fraction,
                                                Pattern,
                                                PyRuleValue)))
        assert value_type_verify, type(value)
        super(PyRuleNode, self).__init__(type_str=type_str)
        self._value = value
        self._children = {}
        self._data = {}
        if data:
            self._data.update(data)

    def __str__(self):
        """ String should create a re-parseable output """
        return self.opless_print()

    def __repr__(self):
        """ Repr should create an unambiguous debug string """
        raise NotImplementedError()

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
        if isinstance(self._value, PyRuleValue):
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


    def search_regex(self, regex):
        """ Test a regex on the Nodes value """
        result = search(regex._vars[0]._value, self.value_string())
        if result is not None:
            return result.groupdict()
        else:
            return None

    def test_regexs_for_matching(self, regexs, current_data, preupdate=None):
        """ Test a number of regexs on this Node
        Return a Tuple of Nones if failure, (dict, node) if success
        """
        new_data = current_data.copy()
        if preupdate is not None:
            for x, y in preupdate:
                new_data[x] = y
        invalidated = False
        for regex in regexs:
            result = self.search_regex(regex)
            if result is None:
                invalidated = True
                break

            for k, v in result.items():
                if k not in new_data:
                    new_data[k] = v
                elif new_data[k] != v:
                    invalidated = True
                    break

        if invalidated:
            return (None, None)
        else:
            return (new_data, self)


    def var_set(self):
        obj = super(PyRuleNode, self).var_set()
        if isinstance(self._value, PyRuleValue):
            val_set = self._value.var_set()
            obj['in'].update(val_set['in'])
            obj['out'].update(val_set['out'])

        #add to 'out' if node is a binding
        if (BIND_S in self._data and self._data[BIND_S]) \
           or (AT_BIND_S in self._data and self._data[AT_BIND_S]):
            obj['out'].add(self.value_string())

        #add annotations to 'in'
        for v in self._data.values():
            if isinstance(v, PyRuleValue):
                tempobj = v.var_set()
                obj['in'].update(tempobj['in'])
                obj['out'].update(tempobj['out'])

        return obj

    def opless_print(self):
        val = str(self._value)
        if util.VALUE_TYPE_S in self._data and self._data[util.VALUE_TYPE_S] == util.STRING_S:
            val = '"{}"'.format(val)
        elif util.VALUE_TYPE_S in self._data and self._data[util.VALUE_TYPE_S] == util.FLOAT_S:
            val = val.replace(".", "d")
        elif util.VALUE_TYPE_S in self._data and self._data[util.VALUE_TYPE_S] == util.REGEX_S:
            val = "/{}/".format(val)

        if util.AT_BIND_S in self._data:
            val = util.AT_VAR_SYMBOL_S + val
        elif util.BIND_S in self._data and self._data[util.BIND_S]:
            val = util.VAR_SYMBOL_S + val
        if util.CONSTRAINT_S in self._data:
            constraints = ", ".join(str(x) for x in self._data[util.CONSTRAINT_S])
            val += "({})".format(constraints)
        return val

