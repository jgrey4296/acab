""" Clause: Holds a single full set of tests for a query """
#from .Node import Node
from pyRule.utils import expandFact, MUTABLE

#todo: split comps into alpha and beta
class Clause:
    """ A Single clause of a query.
    Holds a single string of information to test,
    from root to leaf """

    def __init__(self, components, negated=False, fallback=None, type=None):
        #fallback: [(Bind, value)], for use if the clause fails
        assert(isinstance(components, list))
        #assert(all([isinstance(x, Node) for x in components[1:]]))
        self.negated = negated
        self.components = components
        self.fallback = fallback
        if type is None:
            self.type = MUTABLE.CORE
        else:
            assert(isinstance(type, MUTABLE))
            self.type = type

    def __repr__(self):
        if self.negated:
            neg = "~"
        else:
            neg = ""
        if self.fallback is not None:
            formatStr = " || " + ", ".join(["{}:{}".format(x[0], x[1]) for x in self.fallback])
        else:
            formatStr = ""
        return "{}{}?{}".format(neg, "".join([repr(x) for x in self.components]),
                                formatStr)

    def expandBindings(self, bindings):
        newComponents = expandFact(self.components, bindings)
        return self.__class__(newComponents, negated=self.negated)
