""" Query: The Datastructure to hold a
question to pose to the working memory
"""
from py_rule import util

from . import production_operator as PO
from .comparison import Comparison
from .sentence import Sentence

class Query(PO.ProductionContainer):
    """ A Query for the working memory """

    def __init__(self, clauses, type_str=util.QUERY_S):
        # ATTENTION: List of clauses, not List of tuples
        # [Clause]
        # Each clause is a list of tests and bindings
        assert(all([isinstance(x, Sentence) for x in clauses]))
        super(Query, self).__init__(clauses, type_str=type_str)

    def __repr__(self):
        clause_strs = [repr(x) for x in self._clauses]
        return "\n\t".join(clause_strs)

    def __str__(self):
        clause_strs = [str(x) for x in self._clauses]
        return "\n\t".join(clause_strs)

    def expand_bindings(self, bindings):
        """ Expand the individual clauses to have concrete values """
        assert(isinstance(bindings, dict))
        new_clauses = []
        for x in self._clauses:
            new_clauses.append(x.expand_bindings(bindings))
        return Query(new_clauses)

    def split_clauses(self):
        """ Separate out the clauses of the query
        into positive and negative clauses
        """
        pos = []
        neg = []
        for c in self._clauses:
            if util.NEGATION_S in c._data and c._data[util.NEGATION_S]:
                neg.append(c)
            else:
                pos.append(c)
        return (pos, neg)

    def to_sentences(self, target=None):
        """ Return all comparisons in canonical form """
        # eg : a.test.$x(>$y)? = > -> $x -> $y -> bool
        # TODO should this actually be all *clauses*?
        constraint_words = [word for clause in self._clauses
                            for word in clause if util.CONSTRAINT_S in word._data]
        # for each constraint, create a sentence
        # only handles comparisons, not typings
        return [comp.to_sentence(word)
                for word in constraint_words
                for comp in word._data[util.CONSTRAINT_S]
                if isinstance(comp, Comparison)]

    def value_string(self):
        return self._name
