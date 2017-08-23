from random import shuffle

class Contexts:
      """ Container of available contexts for a match in the trie  """

      def __init__(self):
            #A list of (data,lastNode) tokens
            self._alternatives = []

      def select(self, min=0, max=1):
            #todo: select number based on input
            shuffle(self._alternatives)
            return self._alternatives[0][0]

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

      def __str__(self):
            if bool(self):
                  return "Context: {}".format(len(self))
            else:
                  return "Context: False"

      def __repr__(self):
            return str(self)
            
      @staticmethod
      def initial(startNode):
            init = Contexts()
            init._init_alt(startNode)
            return init
            
      def _init_alt(self, startNode):
            """ Setup the initial context of no bindings 
            and no wmes """
            self._alternatives = [({}, startNode)]
            
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
