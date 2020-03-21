""" Contexts: A Container for all partial matches of a query being run """
from py_rule.util import AT_BIND_S

class Contexts:
    """ Container of available contexts for a match in the trie
    A list of tuples: ({}, LastAccessedNode)
    """
    @staticmethod
    def initial(start_node):
        init = Contexts()
        init._init_alt(start_node)
        return init

    def _init_alt(self, start_node):
        """ Setup the initial context of no bindings
        """
        self._matches = [({}, start_node)]


    #--------------------
    def __init__(self):
        # A list of (data,lastNode) tokens
        self._matches = []

    def __len__(self):
        return len(self._matches)

    def __getitem__(self, key):
        return self._matches[key][0]

    def __iter__(self):
        for x in self._matches:
            yield x[0]

    def __repr__(self):
        if bool(self):
            return "Context: {}".format(len(self))
        else:
            return "Context: False"

    def __bool__(self):
        return len(self._matches) > 0

    def append(self, *data):
        """ Add a number of matching possibilities into this set of contexts """
        assert(all([isinstance(x, tuple) for x in data]))
        for x,y in data:
            assert(x is not None and y is not None)
            self._matches.append((x,y))

    def fail(self):
        """ Remove all contexts, as none are suitable """
        self._matches = []

    def set_all_alts(self, target=None, binding=None):
        """ Duplicate the Contexts, with a specific node as the current leaf """
        assert (target is not None or binding)
        newContexts = Contexts()
        for (data, lastNode) in self._matches:
            if target is not None:
                newContexts.append((data, target))
            else:
                newContexts.append((data, data[AT_BIND_S + binding]))
        return newContexts

    def select(self, bounds=(None, None)):
        """ Select a certain number of binding groups from available contexts """
        raise DeprecationWarning()
