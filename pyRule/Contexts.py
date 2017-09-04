from random import shuffle, sample, randint
from pyRule.Transforms import TROP
import IPython

class Contexts:
      """ Container of available contexts for a match in the trie  """
      @staticmethod
      def initial(startNode):
            init = Contexts()
            init._init_alt(startNode)
            return init
      
      def __init__(self):
            #A list of (data,lastNode) tokens
            self._alternatives = []

      def _init_alt(self, startNode):
            """ Setup the initial context of no bindings 
            and no wmes """
            self._alternatives = [({}, startNode)]

            
      def select(self, bounds=(None, None)):
            shuffle(self._alternatives)
            if bounds[0] is None and bounds[1] is None:
                  return [self._alternatives[0][0]]
            if bounds[0] is TROP.SELECT_ALL and bounds[1] is TROP.SELECT_ALL:
                  return [x[0] for x in self._alternatives]
            if bounds[1] is TROP.SELECT_ALL:
                  upperBound = len(self._alternatives)
            else:
                  upperBound = bounds[1]
            potentialAmnt = max(1, randint(bounds[0], upperBound))
            return [x[0] for x in sample(self._alternatives, potentialAmnt)]
                 

      def append(self, data):
            assert(len(data) == 2)
            if data[0] is not None and data[1] is not None:
                  self._alternatives.append(data)
      
      def __len__(self):
            return len(self._alternatives)

      def __getitem__(self, key):
            return self._alternatives[key][0]

      def __iter__(self):
            for x in self._alternatives:
                  yield x[0]

      def __repr__(self):
            if bool(self):
                  return "Context: {}".format(len(self))
            else:
                  return "Context: False"

      def __bool__(self):
            return len(self._alternatives) > 0

      def fail(self):
            self._alternatives = []

      def verifyMatches(self, targetWMEMatch):
            """ Ensure all alternatives have bound a certain number
            of wmes (ie: for each clause in query there needs to be a wme) """
            self._alternatives = [x for x in self._alternatives if len(x[1]) == targetWMEMatch]

      def set_all_alts(self, targetNode):
            newContexts = Contexts()
            for (data, lastNode) in self._alternatives:
                  newContexts._alternatives.append((data, targetNode))
            return newContexts
