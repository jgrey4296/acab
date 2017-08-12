from pyRule.utils import Clause

class Query:
    """ A Query for the Trie Knowledge base """

    def __init__(self, *args, negated=False):
        #ATTENTION: List of clauses, not List of tuples
        #[Clause]
        #Each clause is a list of tests and bindings
        self._clauses = []
        for clause in args:
            assert(isinstance(clause, Clause))
            self._clauses.append(clause)

    def __len__(self):
        return len(self._clauses)

    def splitClauses(self):
        pos = []
        neg = []
        for c in self._clauses:
            if c.negated is True:
                neg.append(c)
            else:
                pos.append(c)
        return (pos, neg)
