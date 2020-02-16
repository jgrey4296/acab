from pyrule.abstract.node import PyRuleNode
import py_rule.utils as utils


class TrieNode(PyRuleNode):

    @staticmethod
    def Root():
        return TrieNode(utils.ROOT_S)

    def __init__(self, value, data=None):
        super().__init__(value, data)

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
