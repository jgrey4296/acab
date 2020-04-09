from py_rule.abstract.node import PyRuleNode
import py_rule.util as util
from py_rule.abstract.printing import util as PrU


class TrieNode(PyRuleNode):

    @staticmethod
    def Root():
        return TrieNode(util.ROOT_S)


    def __init__(self, value, data=None, type_str=None, tags=None, name=None):
        super().__init__(value, data, type_str=type_str, tags=tags, name=name)


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
