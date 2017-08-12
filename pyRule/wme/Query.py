class Query:
      """ A Query of the WME based knowledge base """

      def __init__(self, *args, negated=False):
            #[(alphaTests, bindings, betaTests)]
            self._clauses = []
            self._negatedClauses = []
            for clause in args:
                  if isinstance(clause, Query):
                        self._negatedClauses += clause._clauses
                        continue
                  if not isinstance(clause, list):
                        clause = [clause]
                  self._clauses.append(self._parseClause(clause))

      def __len__(self):
            return len(self._clauses)
            
      def _parseClause(self, clause):
            alphaTests = []
            bindings = []
            betaTests = []
            for subClause in clause:
                  if len(subClause) == 2:
                        #(field, val)
                        bindings.append(subClause)
                  elif len(subClause) == 3:
                        #(field, op, val)
                        if (not isinstance(subClause[2], str) \
                            or "#" not in subClause[2]) and \
                           "#" not in subClause[0]:
                              alphaTests.append(subClause)
                        else:
                              betaTests.append(subClause)
                        
            return (alphaTests, bindings, betaTests)
    
