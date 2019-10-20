""" Query: The Datastructure to hold a question to pose to the knowledgebase """
import IPython
from .sentence import Sentence

class Query:
    """ A Query for the Trie Knowledge base """

    def __init__(self, clauses):
        #ATTENTION: List of clauses, not List of tuples
        #[Clause]
        #Each clause is a list of tests and bindings
        self._clauses = []
        for clause in clauses:
            assert(isinstance(clause, Sentence))
            self._clauses.append(clause)

    def __len__(self):
        return len(self._clauses)

    def __repr__(self):
        clauseStrs = [repr(x) for x in self._clauses]
        return "\n\t".join(clauseStrs)

    def __str__(self):
        clauseStrs = [str(x) for x in self._clauses]
        return "\n\t".join(clauseStrs)

    def expand_bindings(self, bindings):
        """ Expand the individual clauses to have concrete values """
        assert(isinstance(bindings, dict))
        newClauses = []
        for x in self._clauses:
            newClauses.append(x.expand_bindings(bindings))
        return Query(newClauses)

    def splitClauses(self):
        """ Separate out the clauses of the query into positive and negative clauses """
        pos = []
        neg = []
        for c in self._clauses:
            if c._negated is True:
                neg.append(c)
            else:
                pos.append(c)
        return (pos, neg)
