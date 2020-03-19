"""
Actions: Describes the *ENGINE AGNOSTIC* basic actions that a working memory can
perform, along with associated enums, and IR data structures
"""
from .production_operator import ProductionOperator
from py_rule.error.pyrule_operator_exception import PyRuleOperatorException
from py_rule import util as util
from py_rule.abstract.sentence import Sentence
import logging as root_logger

logging = root_logger.getLogger(__name__)


# Action function template:
class ActionOp(ProductionOperator):
    """ Superclass of all Actions.
    Instantiation of subclasses auto-registers
    the action into ActionOp.op_list with an operator string
    """
    op_list = {}

    def __init__(self, num_params=2, infix=False):
        # Registers self with class name,
        # DSL later binds to an operator
        super().__init__(num_params=num_params, infix=False)
        if self._op_str not in ActionOp.op_list:
            ActionOp.op_list[self._op_str] = self

    def __call__(self, engine, params):
        raise NotImplementedError()

    def __str__(self):
        return self._op_str

    def __repr__(self):
        return "ActionOp: {}".format(str(self))


class Action:
    """ The Core Action Class, holds an operator,
    and a list of values """


    def __init__(self, op, values, type_=None):
        """ Create an action with an operator and values """
        assert all([isinstance(x, Sentence) for x in values]), values
        self._op = op
        self._op_func = None
        self._values = values

        # the actions that group together logically
        # ie: expanded action macros
        self._linkedActions = []

    def __str__(self):
        op = str(self._op)
        args = []
        for val in self._values:
            if isinstance(val, list):  # and isinstance(val[0], Node):
                args.append("".join([str(x) for x in val]))
            else:
                args.append(str(val))
        return "{}({})".format(op, ",".join(args))

    def __repr__(self):
        return "Action({})".format(str(self))

    def is_custom(self):
        """ Indicate whether the operator of the action denotes a custom
        registered action """
        return isinstance(self._op, str)

    def verify_op(self):
        """ Check the Action is using a valid operator (ACT enum) """
        if self._op._op_str not in ActionOp.op_list.keys() and not self.is_custom():
            raise AttributeError("Unrecognised Action: {}".format(self._op))

    def get_values(self, data):
        """ Output a list of bindings from this action """
        output = []
        # TODO update this for AT_BIND
        for x in self._values:
            if isinstance(x, Sentence):
                output.append(x.expand_bindings(data))
            elif isinstance(x, list):  # and all([isinstance(y, Node) for y in x]):
                output.append([y.bind(data) for y in x])
            elif hasattr(x, '_data') and x._data[util.BIND_S]:
                output.append(data[x.value])
            else:
                output.append(x)
        return output

    def expand_bindings(self, bindings):
        """ Expand stored bindings at interpret time
        ie: +(.a.b.$x) + { x : .a.b.c } -> +(.a.b.a.b.c)
        """
        new_values = []
        for x in self._values:
            if isinstance(x, Sentence):
                new_values.append(x.expand_bindings(bindings))
            else:
                new_values.append(x)
        return Action(self._op, new_values)

    def __call__(self, engine, data):
        # lookup op
        opFunc = self._op
        # get values from data
        values = self.get_values(data)
        # perform action op with data
        opFunc(self, engine, values)

    def to_sentence(self):
        """ Return the action in canonical form """
        # eg : assert a.test  = assert -> a.test -> nil
        sen = Sentence([self._op] + self._values[:])
        return sen


class ActionMacro:
    """ Storage for a sequence of actions to take, separate from a rule,
    so it can be expanded into multiple rules.
    """
    def __init__(self, name, params, actions):
        assert(isinstance(name, str))
        assert(isinstance(params, list))
        assert(isinstance(actions, list))
        self._name = name
        self._params = params
        self._actions = actions

    def __str__(self):
        return "#{}({}):\n{}\nend".format(self._name,
                                          ",".join([str(x) for x in self._params]),
                                          "\n".join([str(x) for x in self._actions]))

    def __repr__(self):
        return "ActMacroDef: #{}({} | {})".format(self._name,
                                                  len(self._params),
                                                  len(self._actions))

    def expand_bindings(self, bindings):
        """ Expand the macro  out based on the bindings passed in """
        newActions = [x.expand_bindings(bindings) for x in self._actions]
        return ActionMacro(self._name,
                           self._params.copy(),
                           newActions)


class ActionMacroUse:
    """ The counterpart to an ActionMacro, denotes where to expand a macro into """
    def __init__(self, name, params):
        assert(isinstance(name, str))
        assert(isinstance(params, list))
        self._name = name
        self._params = params

    def expand_bindings(self, bindings):
        raise NotImplementedError()

    def __str__(self):
        return "#{}({})".format(self._name,
                                ",".join([str(x) for x in self._params]),)

    def __repr__(self):
        return "ActMacroUse: #{}({})".format(self._name,
                                             len(self._params))
