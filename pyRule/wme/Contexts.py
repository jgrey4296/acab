class Contexts:
      """ Container of available contexts for a match
      ie: for Facts: (name, bob), (name, bill)
      and query: (name, X)
      gives a context with: [bob, bill]
      """

      def __init__(self):
            #A list of (data,[matchedWMEs]) tokens
            #len(matchedWMEs) == numberOfClauses
            self._alternatives = []

      @staticmethod
      def initial():
            init = Contexts()
            init._init_alt()
            return init
            
      def _init_alt(self):
            """ Setup the initial context of no bindings 
            and no wmes """
            self._alternatives = [({}, [])]
            
      def __len__(self):
            return len(self._alternatives)

      def __bool__(self):
            return len(self._alternatives) > 0

      def fail(self):
            self._alternatives = []

      def verifyMatches(self, targetWMEMatch):
            """ Ensure all alternatives have bound a certain number
            of wmes (ie: for each clause in query there needs to be a wme) """
            self._alternatives = [x for x in self._alternatives if len(x[1]) == targetWMEMatch]
      
