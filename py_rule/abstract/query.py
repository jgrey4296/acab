""" Query: The Datastructure to hold a
question to pose to the working memory
"""
from py_rule.util import CONSTRAINT_S
from .sentence import Sentence
from . import production_operator as PO
from .comparison import Comparison

class Query(PO.ProductionContainer):
    """ A Query for the working memory """

    def __init__(self, clauses):
        # ATTENTION: List of clauses, not List of tuples
        # [Clause]
        # Each clause is a list of tests and bindings
        assert(all([isinstance(x, Sentence) for x in clauses]))
        super(Query, self).__init__(clauses)

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

    def split_clauses(self):
        """ Separate out the clauses of the query
        into positive and negative clauses
        """
        pos = []
        neg = []
        for c in self._clauses:
            if c._negated is True:
                neg.append(c)
            else:
                pos.append(c)
        return (pos, neg)

    def to_sentences(self):
        """ Return all comparisons in canonical form """
        # eg : a.test.$x(>$y)? = > -> $x -> $y -> bool
        # TODO should this actually be all *clauses*?
        constraint_words = [word for clause in self._clauses
                            for word in clause if CONSTRAINT_S in word._data]
        # for each constraint, create a sentence
        # only handles comparisons, not typings
        return [comp.to_sentence(word)
                for word in constraint_words
                for comp in word._data[CONSTRAINT_S]
                if isinstance(comp, Comparison)]

