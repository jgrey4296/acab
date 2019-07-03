"""
Actions: Describes the *ENGINE AGNOSTIC* basic actions that a knowledgebase can perform,
along with associated enums, and IR data structures
"""

from collections import namedtuple
from enum import Enum
from py_rule import utils as util
from py_rule.abstract.sentence import Sentence
import IPython
import logging as root_logger
logging = root_logger.getLogger(__name__)

#Action operators:
ACTS = Enum('Action_ops', 'ADD RETRACT PRINT CUSTOM ACT_MACRO')

ACTMACRONAME = namedtuple('ActMacroId', 'name')

#TODO: add rule modification actions:
#add/remove penumbral condition
#add/remove penumbral action
#add/remove tag?
#insert/remove penumbral action from sequence
#modify hierarcy

#Action function template:
class ActionOp:
    op_list = {}

    def __init__(self, op_str):
        self._op_str = op_str
        ActionOp.op_list[op_str] = self

    def __call__(self, engine, params):
        raise Exception("Abstract Method needs to be implemented")

    def __str__(self):
        return self._op_str

    def __repr__(self):
        return "ActionOp: {}".format(str(self))


class ActionAdd(ActionOp):
    def __init__(self):
        super().__init__("+")

    def __call__(self, engine, params):
        """ Assert the params into the engine """
        #assert(all([isinstance(x, Node) for x in params[0]]))
        engine.add(params[0])


class ActionRetract(ActionOp):
    def __init__(self):
        super().__init__("-")

    def __call__(self, engine, params):
        """ Remove the params from the engine """
        #assert(all([isinstance(x, Node) for x in params[0]]))
        engine.retract(params[0])


class ActionPrint(ActionOp):
    def __init__(self):
        super().__init__("@")

    def __call__(self, engine, params):
        """ Trigger a logging statement """
        for x in params:
            print(x)
            logging.info("Engine Output: {}".format(x))


class ActionCustom(ActionOp):
    def __init__(self):
        super().__init__("#")

    def __call__(self, engine, params):
        engine.call_registered_function(params)


ActionAdd()
ActionRetract()
ActionPrint()
ActionCustom()

class Action:
    """ The Core Action Class, holds an operator,  and a list of values """
    def __init__(self, op, values, type_=None):
        """ Create an action with an operator and values """
        assert(isinstance(values, list))
        #todo: assert that values are a fact string, value, or binding
        self._op = op
        self._values = values
        if type_ is None:
            self._type = util.MUTABLE.CORE
        else:
            assert(isinstance(type_, util.MUTABLE))
            self._type = type_
        #the actions that group together logically
        #ie: expanded action macros
        self._linkedActions = []

    def __str__(self):
        if isinstance(self._op, str):
            op = self._op
        else:
            op = ACTS_REVERSE_LOOKUP[self._op]
        args = []
        for val in self._values:
            if isinstance(val, list): #and isinstance(val[0], Node):
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
        if self._op not in ACTS_LOOKUP and not self.is_custom():
            raise Exception("Unrecognised Action: {}".format(self._op))

    def get_values(self, data):
        """ Output a list of bindings from this action """
        output = []
        for x in self._values:
            if x._data['bind']:
                output.append(data[x.value])
            elif isinstance(x, list): #and all([isinstance(y, Node) for y in x]):
                output.append([y.bind(data) for y in x])
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


class ActionMacro:
    """ Storage for a sequence of actions to take, separate from a rule,
    so it can be expanded into multiple rules.
    """
    def __init__(self, name, params, actions):
        assert(isinstance(name, ACTMACRONAME))
        assert(isinstance(params, list))
        assert(isinstance(actions, list))
        self._name = name
        self._params = params
        self._actions = actions

    def expand_bindings(self, bindings):
        """ Expand the macro  out based on the bindings passed in """
        newActions = [x.expand_bindings(bindings) for x in self._actions]
        return ActionMacro(self._name,
                           self._params.copy(),
                           newActions)


class ActionMacroUse:
    """ The counterpart to an ActionMacro, denotes where to expand a macro into """
    def __init__(self, name, params):
        assert(isinstance(name, ACTMACRONAME))
        assert(isinstance(params, list))
        self._name = name
        self._params = params

    def expand_bindings(self, bindings):
        #todo: does this need to be implemented?
        return self
