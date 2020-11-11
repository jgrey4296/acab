"""
Actions: Describes the *ENGINE AGNOSTIC* basic actions that a working memory can
perform, along with associated enums, and IR data structures

Uses the structure of production operators.
"""
import logging as root_logger

from acab.abstract.config.config import AcabConfig

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence

from acab.abstract.rule import production_operator as PO

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

OPERATOR_S            = util.value("Value.Structure", "OPERATOR")
CONTAINER_TYPE_PRIM_S = util.value("Type.Primitive", "CONTAINER")

# Action function template:
class ActionOp(PO.ProductionOperator):
    """ Superclass of all Actions.
    Instantiation of subclasses auto-registers
    the action into ActionOp.op_dict with an operator string
    """

    def __init__(self):
        """ Registers self with class name,
         DSL later binds to an operator """
        super().__init__()

    def __call__(self, *params, data=None, engine=None):
        """ The Abstract Call Method.
        Takes a variable number of params, the current context dict,
        and the engine itself for broader effects """
        raise NotImplementedError()



class ActionComponent(PO.ProductionComponent):
    """ The Core Action Class, holds an operator,
    and a list of values """

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

    def to_abstract_sentences(self):
        """ Return the action in canonical form """
        # eg : assert a.test  = assert -> a.test -> nil
        # TODO make head a reference for type checking
        # ie: Head: +.$x22532(::sentence).$x26215(::sentence)
        # and Params: a.b.c.$x(::aval), a.b.$d(::aval)
        # and bind?
        head = AcabValue(self.op, {OPERATOR_S : self})
        sen = Sentence([head] + self._params[:])
        return [sen] + self._params[:]



class Action(PO.ProductionContainer):
    """ A Container for Action Specifications """

    def __init__(self, clauses, params=None):
        _type = Sentence.build([CONTAINER_TYPE_PRIM_S])
        super(Action, self).__init__(clauses, params=params, _type=_type)


    def bind(self, bindings):
        """ Expand stored bindings """
        exp_clauses = []
        for clause in self.clauses:
            exp_clauses.append(clause.bind(bindings))
        return Action(exp_clauses, params=self._params)

    def to_abstract_sentences(self, target=None):
        # needs to return both the action sentences,
        # AND the action operators in canonical form
        raise NotImplementedError()
