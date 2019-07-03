import py_rule.utils as utils
import IPython

class TrieNode:

    @staticmethod
    def Root():
        return TrieNode(utils.ROOT)

    def __init__(self, value, data=None):
        self._value = value
        self._children = {}
        self._data = {}
        if data:
            self._data.update(data)

    def __str__(self):
        """ Usable output """
        val = ""
        if 'value_type' not in self._data:
            val = str(self._value)
        elif 'value_type' in self._data and self._data['value_type'] == "string":
            val = '"{}"'.format(self._value)
        elif self._data['value_type'] == 'float':
            val = str(self._value)
            val.replace(".", "d")
        else:
            val = str(self._value)

        if 'bind' in self._data and self._data['bind']:
            val = "$" + val

        if 'constraints' in self._data:
            constraints = ", ".join(str(x) for x in self._data['constraints'])
            val += "({})".format(constraints)

        if 'op' in self._data:
            val += utils.EXOP_lookup[self._data['op']]

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


    def add_child(self, node):
        self._children[str(node._value)] = node
        return node

    def get_child(self, node):
        return self._children[str(node._value)]

    def has_child(self, node):
        return str(node._value) in self._children

    def remove_child(self, node):
        if node in self:
            del self._children[str(node._value)]
            return True

        return False

    def clear_children(self):
        self._children = {}

    def opless_print(self):
        val = str(self._value)
        if self._data['value_type'] == "string":
            val = '"{}"'.format(val)
        elif self._data['value_type'] == 'float':
            val = val.replace(".", "d")

        if 'bind' in self._data and self._data['bind']:
            val = "$" + val
        if 'constraints' in self._data:
            constraints = ", ".join(str(x) for x in self._data['constraints'])
            val += "({})".format(constraints)
        return val

    def copy(self):
        newnode = TrieNode(self._value, self._data)
        newnode._children.update(self._children)
        return newnode


    def split_tests(self):
        """ Split tests into (alphas, betas, regexs) """
        if 'constraints' not in self._data:
            return ([], [], [])

        comps = self._data['constraints']
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
        return self._data['op'] is utils.EXOP.EX

    def looks_exclusive(self):
        """ Checks for implicit exclusivity by having 0 or 1 children """
        return len(self) <= 1

