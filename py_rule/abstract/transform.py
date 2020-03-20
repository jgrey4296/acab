""" Simple Transform functions to be used in rules """
import logging as root_logger
from .production_operator import ProductionOperator
from py_rule.error.pyrule_operator_exception import PyRuleOperatorException
from .node import PyRuleNode
from .value import PyRuleValue
from .sentence import Sentence
from py_rule import util
from py_rule.error.pyrule_operator_exception import PyRuleOperatorException

logging = root_logger.getLogger(__name__)


class TransformOp(ProductionOperator):
    op_list = { 1 : {},
                2 : {},
                3 : {} }

    def __init__(self, num_params=2, infix=False):
        # Registers self with class name,
        # DSL later binds to an operator
        super().__init__(num_params=num_params, infix=False)
        if self._op_str not in TransformOp.op_list[num_params]:
            TransformOp.op_list[num_params][self._op_str] = self

    def __call__(self, a, b):
        raise NotImplementedError("Abstract method needs to be implemented")

    def __str__(self):
        return self._op_str

    def __repr__(self):
        return "TransformOp({})".format(str(self))



class TransformComponent:
    """ Superclass of OperatorTransform. Holds an Operator """
    def __init__(self, op_str, params):
        assert(isinstance(params, tuple))
        self._op  = op_str
        assert(not any([util.AT_BIND_S in x._data for x in params]))
        self._params = params
        self._rebind = None

    def __refine_op_func(self, op_str):
        """ Replace the current op func set with a specific
        op func, used for type refinement """
        assert(op_str in TransformOp.op_list)
        self._op = op_str

    def to_sentence(self):
        head = PyRuleNode(self._op_str, { 'source' : self})
        return Sentence([head] + [x for x in self._params] + [self._rebind])


class OperatorTransform(TransformComponent):
    """ Describes a single transform.
    ie: the operator + any values it uses """
    def __init__(self, op_str, params):
        super().__init__(op_str, params)

    def __repr__(self):
        return "Transform({})".format(str(self))

    def __str__(self):
        op = self._op
        source = [x.opless_print() if isinstance(x, PyRuleNode)
                  else str(x) for x in self._params]
        if self._rebind is not None:
            rebind = " -> {}".format(self._rebind.opless_print())
        else:
            rebind = ""

        param_length = len(self._params)
        if param_length == 1:
            return "{} {}{}".format(op, source[0], rebind)
        elif param_length == 2:
            return "{} {} {}{}".format(source[0],
                                       op,
                                       source[1],
                                       rebind)
        elif param_length == 3:
            return "{} {} {}{}{}".format(source[0],
                                         op,
                                         source[1],
                                         source[2],
                                         rebind)

    def verify_op(self):
        """ Complains if the operator is not a defined Operator Enum """
        if self._op not in TransformOp.op_list:
            raise SyntaxError("Unknown Op: {}".format(self._op))

    def set_rebind(self, bind):
        """ Set this transform to rebind its result to a different variable """
        assert(util.AT_BIND_S not in bind._data)
        self._rebind = bind

    def __call__(self, ctx):
        op_func = TransformOp.op_list[self._op]
        params = [ctx[y._value] if y._data[util.BIND_S] else y._value for y in self._params]

        return op_func(*params, ctx)


class Transform:
    """ Holds a number of separate transform
    operators together to apply to a binding set
    """

    # have min and max bounds
    def __init__(self, components):
        assert(all([isinstance(x, TransformComponent) for x in components]))
        self._components = components[:]

    def __len__(self):
        return len(self._components)

    def __str__(self):
        return "\n".join([str(x) for x in self._components])

    def __repr__(self):
        return "\n".join([repr(x) for x in self._components])

    def verify_ops(self):
        for x in self._components:
            x.verify_op()

    def get_input_requirements(self):
        # return the set of input bound names
        return [y._value for x in self._components for y in x._params if y._data[util.BIND_S]]

    def get_output_spec(self):
        # return the set of output bound names
        return [x._rebind._value for x in self._components]

    def __call__(self, ctx):
        assert(isinstance(ctx, dict))
        for x in self._components:
            # rebind or reapply
            if x._rebind is None:
                raise PyRuleOperatorException("No rebind specified")
            else:
                ctx[x._rebind._value] = x(ctx)

        return ctx

    def to_sentences(self):
        return [x.to_sentence() for x in self._components]
