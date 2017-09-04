from .Clause import Clause
import IPython

class Query:
    """ A Query for the Trie Knowledge base """

    def __init__(self, clauses):
        #ATTENTION: List of clauses, not List of tuples
        #[Clause]
        #Each clause is a list of tests and bindings
        self._clauses = []
        for clause in clauses:
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

    def __repr__(self):
        clauseStrs = [repr(x) for x in self._clauses]
        return "\n\t".join(clauseStrs)

    def expandBindings(self, bindings):
        newClauses = []
        for x in self._clauses:
            newClauses.append(x.expandBindings(bindings))
        return Query(newClauses)
