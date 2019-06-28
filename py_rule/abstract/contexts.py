""" Contexts: A Container for all partial matches of a query being run """
from random import shuffle, sample, randint
from py_rule.abstract.transforms import TROP
import IPython

class Contexts:
    """ Container of available contexts for a match in the trie
    Essentially a list of Dictionaries
    """
    @staticmethod
    def initial(startNode):
        init = Contexts()
        init._init_alt(startNode)
        return init

    def __init__(self):
        #A list of (data,lastNode) tokens
        self._matches = []

    def _init_alt(self, startNode):
        """ Setup the initial context of no bindings
            and no wmes """
        self._matches = [({}, startNode)]


    def select(self, bounds=(None, None)):
        """ Select a certain number of binding groups from available contexts """
        shuffle(self._matches)
        if bounds[0] is None and bounds[1] is None:
            return [self._matches[0][0]]
        if bounds[0] is TROP.SELECT_ALL and bounds[1] is TROP.SELECT_ALL:
            return [x[0] for x in self._matches]
        if bounds[1] is TROP.SELECT_ALL:
            upperBound = len(self._matches)
        else:
            upperBound = bounds[1]
            potentialAmnt = max(1, randint(bounds[0], upperBound))
        return [x[0] for x in sample(self._matches, potentialAmnt)]


    def append(self, data):
        """ Add a number of matching possibilities into this set of contexts """
        assert(len(data) == 2)
        if data[0] is not None and data[1] is not None:
            self._matches.append(data)

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

    def fail(self):
        """ Remove all contexts, as none are suitable """
        self._matches = []

    def verifyMatches(self, targetWMEMatch):
        """ Ensure all alternatives have bound a certain number
            of wmes (ie: for each clause in query there needs to be a wme) """
        self._matches = [x for x in self._matches if len(x[0]) == targetWMEMatch]

    def set_all_alts(self, targetNode):
        """ Duplicate the Contexts, with a specific node as the current leaf """
        newContexts = Contexts()
        for (data, lastNode) in self._matches:
            newContexts._matches.append((data, targetNode))
        return newContexts
