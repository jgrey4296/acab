from .Node import Node

#todo: split comps into alpha and beta
class Clause: 
    def __init__(self, components, negated=False):
        assert(all([isinstance(x, Node) for x in components]))
        self.negated = negated
        self.components = components

    def __repr__(self):
        if self.negated:
            neg = "~"
        else:
            neg = ""
            
        return "{}{}?".format(neg, "".join([repr(x) for x in self.components]))

