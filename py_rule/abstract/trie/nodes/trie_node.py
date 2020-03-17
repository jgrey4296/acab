from py_rule.abstract.node import PyRuleNode
import py_rule.util as util


class TrieNode(PyRuleNode):

    @staticmethod
    def Root():
        return TrieNode(util.ROOT_S)

    def __init__(self, value, data=None):
        super().__init__(value, data)

    def __str__(self):
        """ Usable output """
        val = ""
        if util.VALUE_TYPE_S not in self._data:
            val = str(self._value)
        elif util.VALUE_TYPE_S in self._data and self._data[util.VALUE_TYPE_S] == "string":
            val = '"{}"'.format(self._value)
        elif self._data[util.VALUE_TYPE_S] == 'float':
            val = str(self._value)
            val.replace(".", "d")
        elif self._data[util.VALUE_TYPE_S] == util.REGEX_S:
            val = "/{}/".format(self._value)
        else:
            val = str(self._value)

        if util.BIND_S in self._data and self._data[util.BIND_S]:
            val = "$" + val

        if util.CONSTRAINT_S in self._data:
            constraints = ", ".join(str(x) for x in self._data[util.CONSTRAINT_S])
            val += "({})".format(constraints)

        return val

    def __repr__(self):
        """ Unambiguous printing """
        return "TrieNode({})".format(str(self))

    def opless_print(self):
        val = str(self._value)
        if util.VALUE_TYPE_S in self._data and self._data[util.VALUE_TYPE_S] == "string":
            val = '"{}"'.format(val)
        elif util.VALUE_TYPE_S in self._data and self._data[util.VALUE_TYPE_S] == 'float':
            val = val.replace(".", "d")
        elif util.VALUE_TYPE_S in self._data and self._data[util.VALUE_TYPE_S] == util.REGEX_S:
            val = "/{}/".format(val)

        if util.BIND_S in self._data and self._data[util.BIND_S]:
            val = "$" + val
        if util.CONSTRAINT_S in self._data:
            constraints = ", ".join(str(x) for x in self._data[util.CONSTRAINT_S])
            val += "({})".format(constraints)
        return val

    def copy(self):
        newnode = TrieNode(self._value, self._data)
        newnode._children.update(self._children)
        return newnode

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
            if c.is_regex_test():
                regexs.append(c)
            elif c.is_alpha_test():
                alphas.append(c)
            else:
                betas.append(c)
        return (alphas, betas, regexs)

