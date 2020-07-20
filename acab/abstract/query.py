"""
Query uses production_operator structuring.

A Query is a container of clauses.
Each Clause is a Sentence where individual words have Components describing tests.
Each Component combines a QueryOp with values to use.

"""
import logging as root_logger

from acab.util import BIND_S, OPERATOR_S
from acab import util
from acab.abstract.printing import util as PrU

from . import production_operator as PO
from . import type_base as TB
from .sentence import Sentence
from .node import AcabNode


logging = root_logger.getLogger(__name__)

class QueryOp(PO.ProductionOperator):
    """ Superclass for Comparisons.
    Instantiation of subclasses auto-registers
    the comparison into QueryOp.op_dict with an operator string
    """
    op_dict = {}

    def __init__(self, infix=False):
        # Registers self with class name,
        # DSL later binds to an operator
        super().__init__(infix=False)

        if self.op_str not in QueryOp.op_dict:
            QueryOp.op_dict[self.op_str] = self

    def __call__(self, a, b, data=None, engine=None, node=None):
        raise NotImplementedError()


class QueryComponent(PO.ProductionComponent):
    """ Describe a QueryComponent of values and maybe a binding """

    def __init__(self, op_str, param, data=None):
        if not isinstance(param, list):
            param = [param]
        super(QueryComponent, self).__init__(op_str,
                                             param,
                                             data=data)
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
        # TODO: refactor to not be hard coded
        return self.op == "operator.query.regmatch"


    def to_local_sentences(self, target=None):
        """ Create a comparison as a canonical sentence """
        # eg: 20(>30) :  > -> 20 -> 30 -> bool
        head = AcabValue(self.op, {OPERATOR_S : self})
        if target is None:
            return Sentence([head] + self._params)
        assert(isinstance(target, AcabValue))
        return [Sentence([head, target] + self._params)]


class Query(PO.ProductionContainer):
    """ A Query for the working memory """

    def __init__(self, clauses):
        assert(all([isinstance(x, Sentence) for x in clauses]))
        super(Query, self).__init__(clauses, _type=TB.QUERY)

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
