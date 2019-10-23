import py_rule.utils as utils
import IPython

class TrieNode:

    @staticmethod
    def Root():
        return TrieNode(utils.ROOT_S)

    def __init__(self, value, data=None):
        self._value = value
        self._children = {}
        self._data = {}
        if data:
            self._data.update(data)

    def __str__(self):
        """ Usable output """
        val = ""
        if utils.VALUE_TYPE_S not in self._data:
            val = str(self._value)
        elif utils.VALUE_TYPE_S in self._data and self._data[utils.VALUE_TYPE_S] == "string":
            val = '"{}"'.format(self._value)
        elif self._data[utils.VALUE_TYPE_S] == 'float':
            val = str(self._value)
            val.replace(".", "d")
        elif self._data[utils.VALUE_TYPE_S] == utils.REGEX_S:
            val = "/{}/".format(self._value)
        else:
            val = str(self._value)

        if utils.BIND_S in self._data and self._data[utils.BIND_S]:
            val = "$" + val

        if utils.CONSTRAINT_S in self._data:
            constraints = ", ".join(str(x) for x in self._data[utils.CONSTRAINT_S])
            val += "({})".format(constraints)

        if utils.OPERATOR_S in self._data:
            val += utils.EXOP_lookup[self._data[utils.OPERATOR_S]]

        return val

    def __repr__(self):
        """ Unambiguous printing """
        return "TrieNode({})".format(str(self))

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
        if isinstance(self._value, TrieNode):
            return self._value.value_string()
        else:
            return str(self._value)

    def set_data(self, data):
        if data is not None:
            self._data.update(data)

    def add_child(self, node):
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

    def opless_print(self):
        val = str(self._value)
        if utils.VALUE_TYPE_S in self._data and self._data[utils.VALUE_TYPE_S] == "string":
            val = '"{}"'.format(val)
        elif utils.VALUE_TYPE_S in self._data and self._data[utils.VALUE_TYPE_S] == 'float':
            val = val.replace(".", "d")
        elif utils.VALUE_TYPE_S in self._data and self._data[utils.VALUE_TYPE_S] == utils.REGEX_S:
            val = "/{}/".format(val)

        if utils.BIND_S in self._data and self._data[utils.BIND_S]:
            val = "$" + val
        if utils.CONSTRAINT_S in self._data:
            constraints = ", ".join(str(x) for x in self._data[utils.CONSTRAINT_S])
            val += "({})".format(constraints)
        return val

    def copy(self):
        newnode = TrieNode(self._value, self._data)
        newnode._children.update(self._children)
        return newnode


    def split_tests(self):
        """ Split tests into (alphas, betas, regexs) """
        if utils.CONSTRAINT_S not in self._data:
            return ([], [], [])

        comps = self._data[utils.CONSTRAINT_S]
        assert(isinstance(comps, list))
        alphas = []
        betas = []
        regexs = []
        for c in comps:
            if c.is_regex_test():
                regexs.append(c)
            elif c.is_alpha_test():
                alphas.append(c)
            else:
                betas.append(c)
        return (alphas, betas, regexs)

    def is_exclusive(self):
        """ Checks for the exclusion operator in this node """
        return self._data[utils.OPERATOR_S] is utils.EXOP.EX

    def looks_exclusive(self):
        """ Checks for implicit exclusivity by having 0 or 1 children """
        return len(self) <= 1

