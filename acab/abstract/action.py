"""
Actions: Describes the *ENGINE AGNOSTIC* basic actions that a working memory can
perform, along with associated enums, and IR data structures

Uses the structure of production operators.
"""
import logging as root_logger

from acab import util
from acab.abstract.value import AcabValue
from acab.abstract.sentence import Sentence
from acab.abstract.printing import util as PrU

from . import production_operator as PO

logging = root_logger.getLogger(__name__)


# Action function template:
class ActionOp(PO.ProductionOperator):
    """ Superclass of all Actions.
    Instantiation of subclasses auto-registers
    the action into ActionOp.op_dict with an operator string
    """
    op_dict = {}

    def __init__(self, infix=False):
        """ Registers self with class name,
         DSL later binds to an operator """
        super().__init__(infix=False)

        if self.op_str not in ActionOp.op_dict:
            ActionOp.op_dict[self.op_str] = self

    def __call__(self, *params, data=None, engine=None):
        """ The Abstract Call Method.
        Takes a variable number of params, the current context dict,
        and the engine itself for broader effects """
        raise NotImplementedError()



class ActionComponent(PO.ProductionComponent):
    """ The Core Action Class, holds an operator,
    and a list of values """


    def __init__(self, op_str, params=None, data=None):
        """ Create an action with an operator and values """
        assert all([isinstance(x, AcabValue) for x in params]), params
        super(ActionComponent, self).__init__(op_str,
                                              params,
                                              data=data)


    @property
    def var_set(self):
        """ Get the input and output variables of the component """
        obj = super(ActionComponent, self).var_set
        return {'in' : obj['in'].union(obj['out']), 'out': set()}


    def bind(self, bindings):
        """ Expand stored bindings at interpret time
        ie: +(.a.b.$x) + { x : .a.b.c } -> +(.a.b.a.b.c)
        """
        new_values = []
        for x in self._params:
            new_values.append(x.bind(bindings))
        return ActionComponent(self.op, new_values)

    def to_local_sentences(self):
        """ Return the action in canonical form """
        # eg : assert a.test  = assert -> a.test -> nil
        # TODO alpha renaming
        # ie: Head: +.$x22532(::sentence).$x26215(::sentence)
        # and Params: a.b.c.$x(::aval), a.b.$d(::aval)
        # and bind?
        head = AcabValue(self.op, {util.OPERATOR_S : self})
        sen = Sentence([head] + self._params[:])
        return [sen] + self._params[:]



class Action(PO.ProductionContainer):
    """ A Container for Action Specifications """

    def __init__(self, clauses, params=None, type_str=util.ACTION_S):
        super(Action, self).__init__(clauses,
                                     params=params,
                                     type_str=type_str)

    def bind(self, bindings):
        """ Expand stored bindings """
        exp_clauses = []
        for clause in self.clauses:
            exp_clauses.append(clause.bind(bindings))
        return Action(exp_clauses, params=self._vars)

    def to_local_sentences(self, target=None):
        # needs to return both the action sentences,
        # AND the action operators in canonical form
        raise NotImplementedError()



PrU.register_class(ActionComponent, PrU.print_operator_wrap)
