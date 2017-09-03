from .Node import Node
from pyRule.utils import expandFact

#todo: split comps into alpha and beta
class Clause:
    """ A Single clause of a query. 
    Holds a single string of information to test,
    from root to leaf """
    
    def __init__(self, components, negated=False, fallback=None):
        #fallback: [(Bind, value)], for use if the clause fails
        assert(all([isinstance(x, Node) for x in components[1:]]))
        self.negated = negated
        self.components = components
        self.fallback = fallback
        
    def __repr__(self):
        if self.negated:
            neg = "~"
        else:
            neg = ""
            
        return "{}{}?".format(neg, "".join([repr(x) for x in self.components]))

    def expandBindings(self, bindings):
        newComponents = expandFact(self.components, bindings)
        return Clause(newComponents, negated=self.negated)
