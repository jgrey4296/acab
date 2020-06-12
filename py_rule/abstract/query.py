""" Query: The Datastructure to hold a
question to pose to the working memory
"""
import logging as root_logger

from py_rule.util import BIND_S, OPERATOR_S
from py_rule import util
from py_rule.abstract.printing import util as PrU

from . import production_operator as PO
from .sentence import Sentence
from .node import PyRuleNode

logging = root_logger.getLogger(__name__)

class QueryOp(PO.ProductionOperator):
    """ Superclass for Comparisons.
    Instantiation of subclasses auto-registers
    the comparison into QueryOp.op_dict with an operator string
    """
    op_dict = {}

    def __init__(self, num_params=2, infix=False):
        # Registers self with class name,
        # DSL later binds to an operator
        super().__init__(num_params=num_params, infix=False)

        if self.op_str not in QueryOp.op_dict:
            QueryOp.op_dict[self.op_str] = self

    def __call__(self, a, b, data=None, engine=None, node=None):
        raise NotImplementedError()


class QueryComponent(PO.ProductionComponent):
    """ Describe a QueryComponent of values and maybe a binding """

    def __init__(self, op_str, param, data=None, type_str=None):
        if not isinstance(param, list):
            param = [param]
        super(QueryComponent, self).__init__(op_str,
                                             param,
                                             data=data,
                                             type_str=type_str)
        self.verify()

    def __call__(self, node, data=None, engine=None):
        """ Run a comparison on a node """
        self.verify()
        op = PO.ProductionOperator.op_dict[self.op]
        node_value = node.value.value
        params = self.get_params(data)

        return op(node_value, *params, data=data)


    @property
    def is_alpha_test(self):
        """ Return boolean of if test does not rely on other bindings """
        return bool(self._params) and not any([x.is_var for x in self._params])

    @property
    def is_regex_test(self):
        """ Return boolean if test is a regular expression test """
        return self.op == "operator.query.regmatch"


    def to_local_sentences(self, target=None):
        """ Create a comparison as a canonical sentence """
        # eg: 20(>30) :  > -> 20 -> 30 -> bool
        head = PyRuleValue(self.op, {OPERATOR_S : self})
        if target is None:
            return Sentence([head] + self._params)
        assert(isinstance(target, PyRuleValue))
        return [Sentence([head, target] + self._params)]


class Query(PO.ProductionContainer):
    """ A Query for the working memory """

    def __init__(self, clauses, type_str=util.QUERY_S):
        assert(all([isinstance(x, Sentence) for x in clauses]))
        super(Query, self).__init__(clauses, type_str=type_str)

    def __call__(self, ctxs=None, engine=None):
        return engine.query(self, ctxs=ctxs)

    def bind(self, bindings):
        """ Expand the individual clauses to have concrete values """
        assert(isinstance(bindings, dict))
        new_clauses = []
        for x in self.clauses:
            new_clauses.append(x.bind(bindings))
        return Query(new_clauses)

    def split_clauses(self):
        """ Separate out the clauses of the query
        into positive and negative clauses
        """
        pos = []
        neg = []
        for c in self.clauses:
            if util.NEGATION_S in c._data and c._data[util.NEGATION_S]:
                neg.append(c)
            else:
                pos.append(c)
        return (pos, neg)

    def to_local_sentences(self, target=None):
        """ Return all comparisons in canonical form """
        # eg : a.test.$x(>$y)? = > -> $x -> $y -> bool
        constraint_words = [word for clause in self.clauses
                            for word in clause if util.CONSTRAINT_S in word._data]
        # for each constraint, create a sentence
        # only handles comparisons, not typings
        constraint_sentences = [sen
                                for word in constraint_words
                                for comp in word._data[util.CONSTRAINT_S]
                                for sen in comp.to_local_sentences(word)
                                if isinstance(comp, QueryComponent)]

        total_sentences = self.clauses + constraint_sentences
        return total_sentences
