""" Clause: Holds a single full set of tests for a query """
#from .Node import Node
from py_rule.utils import expandFact, MUTABLE
from py_rule.abstract.sentence import Sentence

#todo: split comps into alpha and beta
class Clause:
    """ A Single clause of a query.
    Holds a single string of information to test,
    from root to leaf """

    def __init__(self, sen, negated=False, fallback=None, type=None):
        #fallback: [(Bind, value)], for use if the clause fails
        assert(isinstance(sen, Sentence))
        #assert(all([isinstance(x, Node) for x in components[1:]]))
        self._negated = negated
        self._sentence= sen
        self._fallback = fallback
        if type is None:
            self._type = MUTABLE.CORE
        else:
            assert(isinstance(type, MUTABLE))
            self._type = type

    def __str__(self):
        if self._negated:
            neg = "~"
        else:
            neg = ""
        if self._fallback is not None:
            formatStr = " || " + ", ".join(["${}:{}".format(x[0], x[1]) for x in self._fallback])
        else:
            formatStr = ""
        return "{}{}?{}".format(neg,
                                str(self._sentence),
                                formatStr)

    def __repr__(self):
        if self._negated:
            neg = "~"
        else:
            neg = ""
        if self._fallback is not None:
            formatStr = " || " + ", ".join(["{}:{}".format(x[0], x[1]) for x in self._fallback])
        else:
            formatStr = ""
        return "Clause({}{}?{})".format(neg,
                                        str(self._sentence),
                                        formatStr)

    def expandBindings(self, bindings):
        """ Given a set of bindings, place them in place of variables in this clause """
        newSentence = expandFact(self._sentence, bindings)
        return self.__class__(newSentence, negated=self._negated)
