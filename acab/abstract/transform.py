"""
Transform follows the structure of production_operator

Transform is a container of clauses.
Each clause is a Component, which combines values with an operator

"""
import logging as root_logger

from acab import util
from acab.abstract.printing import util as PrU

from . import production_operator as PO
from . import type_base as TB

from .sentence import Sentence
from acab.abstract.value import AcabValue

logging = root_logger.getLogger(__name__)


class TransformOp(PO.ProductionOperator):
    op_dict = {}

    def __init__(self, infix=False):
        # Registers self with class name,
        # DSL later binds to an operator
        super().__init__(infix=False)

        if self.op_str not in TransformOp.op_dict:
            TransformOp.op_dict[self.op_str] = self


    def __call__(self, a, b, data=None, engine=None):
        raise NotImplementedError("Abstract method needs to be implemented")


class TransformComponent(PO.ProductionComponent):
    """ Superclass of OperatorTransform. Holds an Operator """
    def __init__(self, op_str, params, op_pos=0, rebind=None, data=None):
        super(TransformComponent, self).__init__(op_str, params,
                                                 data=data,
                                                 rebind=rebind,
                                                 op_pos=op_pos)


    def to_local_sentences(self, target=None):
        head = AcabValue(self.op, data={util.OPERATOR_S : self})
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
