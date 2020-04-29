"""
Actions: Describes the *ENGINE AGNOSTIC* basic actions that a working memory can
perform, along with associated enums, and IR data structures
"""
import logging as root_logger

from py_rule import util
from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.node import PyRuleNode
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.printing import util as PrU

from . import production_operator as PO

logging = root_logger.getLogger(__name__)


# Action function template:
class ActionOp(PO.ProductionOperator):
    """ Superclass of all Actions.
    Instantiation of subclasses auto-registers
    the action into ActionOp.op_list with an operator string
    """
    op_list = {}

    def __init__(self, num_params=2, infix=False):
        # Registers self with class name,
        # DSL later binds to an operator
        super().__init__(num_params=num_params, infix=False)

        if self.op_str not in ActionOp.op_list:
            ActionOp.op_list[self.op_str] = self

    def __call__(self, *params, data=None, engine=None):
        raise NotImplementedError()



class ActionComponent(PO.ProductionComponent):
    """ The Core Action Class, holds an operator,
    and a list of values """


    def __init__(self, op_str, params=None, **kwargs):
        """ Create an action with an operator and values """
        assert all([isinstance(x, Sentence) for x in params]), params
        super(ActionComponent, self).__init__(op_str,
                                              params=params,
                                              op_class=ActionOp,
                                              **kwargs)

    @property
    def var_set(self):
        obj = super(ActionComponent, self).var_set
        return {'in' : obj['in'].union(obj['out']), 'out': set()}


    def copy(self):
        return ActionComponent(self.op, params=self._vars, type_str=self.type)

    def bind(self, bindings):
        """ Expand stored bindings at interpret time
        ie: +(.a.b.$x) + { x : .a.b.c } -> +(.a.b.a.b.c)
        """
        new_values = []
        for x in self._vars:
            if isinstance(x, Sentence):
                new_values.append(x.bind(bindings))
            else:
                new_values.append(x)
        return ActionComponent(self.op, new_values)

    def to_sentence(self):
        """ Return the action in canonical form """
        # eg : assert a.test  = assert -> a.test -> nil
        # TODO : params are sentences themselves
        head = PyRuleNode(self.op, {util.OPERATOR_S : self})
        sen = Sentence([head] + self._vars[:])
        return sen

    def pprint(self, **kwargs):
        return PrU.print_operator(self, wrap_vars=True, **kwargs)


class Action(PO.ProductionContainer):
    """ A Container for Action Specifications """

    def __init__(self, clauses, params=None, type_str=util.ACTION_S, **kwargs):
        super(Action, self).__init__(clauses,
                                     params=params,
                                     type_str=type_str,
                                     **kwargs)

    def bind(self, bindings):
        """ Expand stored bindings """
        exp_clauses = []
        for clause in self.clauses:
            exp_clauses.append(clause.bind(bindings))

        return Action(exp_clauses, params=self._vars)

    def copy(self):
        copied = Action(self.clauses, params=self._vars, type_str=self.type)
        return copied

    def to_sentences(self, target=None):
        # needs to return both the action sentences,
        # AND the action operators in canonical form
        raise NotImplementedError()
