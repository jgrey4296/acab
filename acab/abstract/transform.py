"""
Transform follows the structure of production_operator

Transform is a container of clauses.
Each clause is a Component, which combines values with an operator

"""
import logging as root_logger

from acab.config import AcabConfig
from acab.abstract.printing import util as PrU

from . import production_operator as PO
from . import type_base as TB

from .sentence import Sentence
from acab.abstract.value import AcabValue

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

class TransformOp(PO.ProductionOperator):

    def __init__(self):
        # Registers self with class name,
        # DSL later binds to an operator
        super().__init__()


    def __call__(self, a, b, data=None, engine=None):
        raise NotImplementedError("Abstract method needs to be implemented")


class TransformComponent(PO.ProductionComponent):
    """ Superclass of OperatorTransform. Holds an Operator """
    def to_local_sentences(self, target=None):
        head = AcabValue(self.op, data={'op_reference': self})
        return [Sentence([head] + self._params[:] + [self._rebind])]


class Transform(PO.ProductionContainer):
    """ Holds a number of separate transform
    operators together to apply to a binding set
    """

    # have min and max bounds
    def __init__(self, clauses):
        assert(all([isinstance(x, TransformComponent) for x in clauses]))
        super(Transform, self).__init__(clauses, _type=TB.TRANSFORM)

    @property
    def var_set(self):
        var_set = {'in': set(), 'out': set()}
        for clause in self.clauses:
            for word in clause._params:
                if word.is_var:
                    var_set['in'].add(word.value)
            var_set['out'].add(clause._rebind.value)

        return var_set



PrU.register_class(TransformComponent, PrU.print_operator_rebind)
