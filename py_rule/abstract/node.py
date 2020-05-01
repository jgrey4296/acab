""" The Base Node Class the rest of PyRule extends """
from re import search
import weakref

from py_rule import util
from py_rule.abstract.printing import util as PrU

from .value import PyRuleValue

class PyRuleNode(PyRuleValue):
    """ The Abstract Node Class """


    def __init__(self, value, data=None, type_str=None, tags=None, name=None):
        super(PyRuleNode, self).__init__(value,
                                         data=data,
                                         type_str=type_str,
                                         tags=tags,
                                         name=name)
        self._parent = None
        self._children = {}

    def __len__(self):
        return len(self._children)

    def __bool__(self):
        return bool(self._children)

    def __contains__(self, v):
        return self.has_child(v)

    def __iter__(self):
        return iter(self._children.values())


    @property
    def var_set(self):
        obj = super(PyRuleNode, self).var_set
        if isinstance(self._value, PyRuleValue):
            val_set = self._value.var_set
            obj['in'].update(val_set['in'])
            obj['out'].update(val_set['out'])

        #add to 'out' if node is a binding
        if self.is_var:
            obj['out'].add(self.name)

        #add annotations to 'in'
        for v in self._data.values():
            if isinstance(v, PyRuleValue):
                tempobj = v.var_set
                obj['in'].update(tempobj['in'])
                obj['out'].update(tempobj['out'])

        return obj


    def add_child(self, node):
        assert(isinstance(node, PyRuleNode))
        self._children[node.name] = node
        return node

    def get_child(self, node):
        if isinstance(node, str):
            return self._children[node]
        else:
            return self._children[node.name]

    def has_child(self, node):
        if isinstance(node, str):
            return node in self._children
        else:
            return node.name in self._children

    def remove_child(self, node):
        if node in self:
            if isinstance(node, str):
                del self._children[node]
            else:
                del self._children[node.name]
            return True

        return False

    def clear_children(self):
        self._children = {}


    def search_regex(self, regex):
        """ Test a regex on the Nodes value """
        result = search(regex._vars[0]._value, self.name)
        if result is not None:
            return result.groupdict()
        else:
            return None

    def test_regexs_for_matching(self, regexs, data):
        """ Test a number of regexs on this Node
        Return a Tuple of Nones if failure, (dict, node) if success
        """
        invalidated = False
        for regex in regexs:
            result = self.search_regex(regex)
            if result is None:
                invalidated = True
                break

            for k, v in result.items():
                if k not in data:
                    data[k] = v
                elif data[k] != v:
                    invalidated = True
                    break

        if invalidated:
            return (None, None)
        else:
            return (data, self)


    def set_parent(self, parent):
        assert(isinstance(parent, PyRuleNode))
        self._parent = weakref.ref(parent)

    def parentage(self):
        path = []
        current = self
        while current is not None:
            path.insert(0, current)
            current = current._parent
        return path


    def bind(self, bindings):
        """ Return a copy that has applied given bindings to this node """
        if not self._data[util.BIND_S]:
            return self.copy()
        else:
            copied = self.copy()
            copied._bind_to_value(bindings)
            return copied

    def _bind_to_value(self, data):
        """ Set the Nodes value to be one retrieved
        from passed in bindings """
        assert(self._value in data)
        assert(util.BIND_S in self._data and self._data[util.BIND_S])
        self._value = data[self._value]
        self._data[util.BIND_S] = False
