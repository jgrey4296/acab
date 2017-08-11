class TrieContexts:
      """ Container of available contexts for a match in the trie  """

      def __init__(self):
            #A list of (data,lastNode) tokens
            self._alternatives = []

      def __len__(self):
            return len(self._alternatives)

      def __getitem__(self, key):
            return self._alternatives[key][0]

      def __iter__(self):
            for x in self._alternatives:
                  yield x[0]
      
      @staticmethod
      def initial(startNode):
            init = TrieContexts()
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
            newContexts = TrieContexts()
            for (data, lastNode) in self._alternatives:
                  newContexts._alternatives.append((data, targetNode))
            return newContexts
