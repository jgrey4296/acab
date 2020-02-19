""" Contexts: A Container for all partial matches of a query being run """


class Contexts:
    """ Container of available contexts for a match in the trie
    A list of tuples: ({}, LastAccessedNode)
    """
    @staticmethod
    def initial(start_node):
        init = Contexts()
        init._init_alt(start_node)
        return init

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

    def _init_alt(self, start_node):
        """ Setup the initial context of no bindings
        """
        self._matches = [({}, start_node)]

    def append(self, data):
        """ Add a number of matching possibilities into this set of contexts """
        assert(len(data) == 2)
        if data[0] is not None and data[1] is not None:
            self._matches.append(data)

    def fail(self):
        """ Remove all contexts, as none are suitable """
        self._matches = []

    def verify_matches(self, targetWMEMatch):
        """ Ensure all alternatives have bound a certain number
            of wmes (ie: for each clause in query there needs to be a wme) """
        self._matches = [x for x in self._matches if len(x[0]) == targetWMEMatch]

    def set_all_alts(self, targetNode):
        """ Duplicate the Contexts, with a specific node as the current leaf """
        newContexts = Contexts()
        for (data, lastNode) in self._matches:
            newContexts._matches.append((data, targetNode))
        return newContexts

    def select(self, bounds=(None, None)):
        """ Select a certain number of binding groups from available contexts """
        raise DeprecationWarning()
