""" Simple Transform functions to be used in rules """
import logging as root_logger
from .production_operator import ProductionOperator
from .node import PyRuleNode
from py_rule import util
frpm py_rule.error.pyrule_operator_exceoption import PyRuleOperatorException

logging = root_logger.getLogger(__name__)


class TransformOp(ProductionOperator):
    op_list = {}

    def __init__(self, op_str, num_params=2):
        self._op_str = op_str
        self._num_params = num_params
        if op_str not in TransformOp.op_list:
            TransformOp.op_list[op_str] = {}
        TransformOp.op_list[op_str][num_params] = self

    def __call__(self, a, b):
        raise NotImplementedError("Abstract method needs to be implemented")

    def __str__(self):
        return self._op_str

    def __repr__(self):
        return "TransformOp({})".format(str(self))



class TypedTransformOp(TransformOp):
    """
    An extension of the TransformOp to enable
    operators to work on multiple different types
    """
    # TODO make this a trie
    op_list = {}

    def __init__(self, op_str, num_params=2, type_sig=None):
        self._op_str = op_str
        self._num_params = num_params
        # TODO if type_sig is not, generate a generic
        # TODO replace the following with a trie insert
        if op_str not in TypedTransformOp.op_list:
            TypedTransformOp.op_list[op_str] = {}
        if type_sig in TypedTransformOp.op_list:
            raise PyRuleOperatorException("Typed Operator Signature already exists: {}".format(type_sig))

    @staticmethod
    def resolve(op_str, num_params=2, type_sig=None):
        """ Resolve an operator to its most concrete form """
        # ie: query the op_list trie
        raise NotImplementedError()


class TransformComponent:
    """ Superclass of OperatorTransform. Holds an Operator """
    def __init__(self, op, num_params=2):
        # A wave function of potential operators
        # TODO make this a resolve call
        self._op = TransformOp.op_list[op][num_params]


class OperatorTransform(TransformComponent):
    """ Describes a single transform.
    ie: the operator + any values it uses """
    def __init__(self, op, params):
        assert(isinstance(params, tuple))
        super().__init__(op, len(params))
        self._params = params
        self._rebind = None

    def __repr__(self):
        return "Transform({})".format(str(self))

    def __str__(self):
        op = str(self._op)
        source = [x.opless_print() if isinstance(x, PyRuleNode)
                  else str(x) for x in self._params]
        if self._rebind is not None:
            rebind = " -> {}".format(self._rebind.opless_print())
        else:
            rebind = ""

        param_length = self._op._num_params
        if param_length == 1:
            return "{}{}{}".format(op, source[0], rebind)
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
        if self._op is None:
            raise SyntaxError("Unknown Op: {}".format(self._op))

    def set_rebind(self, bind):
        """ Set this transform to rebind its result to a different variable """
        self._rebind = bind


    # TODO add a refine method
    # TODO make call try operators from the wave function of ops

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
        # TODO
        # return the set of input bound names
        raise NotImplementedError()

    def get_output_spec(self):
        # TODO
        # return the set of output bound names
        raise NotImplementedError()

    def __call__(self, ctx):
        assert(isinstance(ctx, dict))
        # TODO: transfer logic into operator transforms
        for x in self._components:
            # lookup op
            opFunc = x._op
            param_length = opFunc._num_params
            # get params:
            params = [ctx[y._value] if y._data[util.BIND_S]
                      else y._value for y in x._params]

            result = opFunc(*params, ctx)

            # rebind or reapply
            if x._rebind is None:
                ctx[x._params[0]._value] = result
            else:
                ctx[x._rebind._value] = result

        return ctx
