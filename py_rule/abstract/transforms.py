""" Simple Transform functions to be used in rules """
import logging as root_logger
from enum import Enum
from random import uniform, sample, randint
from math import floor
from re import sub
from py_rule.trie.nodes.trie_node import TrieNode
import IPython

logging = root_logger.getLogger(__name__)

TROP = Enum("Transform_ops", "ADD SUB MUL DIV RAND REMAIN ROUND NEG REGEX FORMAT SELECT SELECT_ALL")

class TransformOp:
    op_list = {}

    def __init__(self, op_str, num_params=2):
        self._op_str = op_str
        self._num_params = num_params
        if op_str not in TransformOp.op_list:
            TransformOp.op_list[op_str] = {}
        TransformOp.op_list[op_str][num_params] = self

    def __call__(self, a, b):
        raise Exception("Abstract method needs to be implemented")


    def __str__(self):
        return self._op_str

    def __repr__(self):
        return "TransformOp({})".format(str(self))


class AddOp(TransformOp):
    def __init__(self):
        super().__init__("+")

    def __call__(self, a, b, data):
        return a + b


class SubOp(TransformOp):
    def __init__(self):
        super().__init__("-")

    def __call__(self, a, b, data):
        return a - b


class MulOp(TransformOp):
    def __init__(self):
        super().__init__("*")

    def __call__(self, a, b, data):
        return a * b


class DivOp(TransformOp):
    def __init__(self):
        super().__init__("/")

    def __call__(self, a, b, data):
        return a / b


class RandOp(TransformOp):
    def __init__(self):
        super().__init__("<->")

    def __call__(self, a=0, b=1, data=None):
        """ Uniform Rand between a=0 and b=1 """
        return uniform(a, b)


class RemainOp(TransformOp):
    def __init__(self):
        super().__init__("%")

    def __call__(self, a, b, data):
        #divde and get remainder?
        raise Exception("Not implemented yet")


class RoundOp(TransformOp):
    def __init__(self):
        super().__init__('_', 1)

    def __call__(self, a, data):
        #round to integer
        return floor(a)


class NegOp(TransformOp):
    def __init__(self):
        super().__init__("-", 1)

    def __call__(self, a, data):
        #invert the number
        return -a


class RegexOp(TransformOp):
    def __init__(self):
        super().__init__("~=", 3)

    def __call__(self, a, b, replacement, data):
        """ Substitute a pattern with a value from passed in data
        a : the replacement
        b: the pattern

        """
        return sub(b, replacement, a)


class FormatOp(TransformOp):
    def __init__(self):
        super().__init__("~{}", 1)

    def __call__(self, a, data):
        """ Use str.format variant with a data dictionary
        Replaces variables in the string with bound values
        """
        return a.format(**data)


#--------------------------------------------------

class TransformComponent:
    """ Superclass of Transforms. Holds an Operator """
    def __init__(self, op, num_params=2):
        self._op = TransformOp.op_list[op][num_params]


class OperatorTransform(TransformComponent):
    """ The main transform type. applies the operator to values """
    def __init__(self, op, params):
        super().__init__(op, len(params))
        self._params = params
        self._rebind = None

    def __repr__(self):
        return "Transform({})".format(str(self))

    def __str__(self):
        op = str(self._op)
        source = [x.opless_print() if isinstance(x, TrieNode) else str(x) for x in self._params]
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
            raise Exception("Unknown Op: {}".format(self._op))

    def set_rebind(self, bind):
        """ Set this transform to rebind its result to a different variable """
        self._rebind = bind


AddOp()
SubOp()
MulOp()
DivOp()
RandOp()
RemainOp()
RoundOp()
NegOp()
RegexOp()
FormatOp()

class Transform:
    """ Holds a number of separate transform operators together to apply to a binding set """

    #have min and max bounds
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
        #TODO
        #return the set of input bound names
        return set([])

    def get_output_spec(self):
        #TODO
        #return the set of output bound names
        return set([])
