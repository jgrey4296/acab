"""
Actions: Describes the *ENGINE AGNOSTIC* basic actions that a knowledgebase can perform,
along with associated enums, and IR data structures
"""

import logging as root_logger
from enum import Enum
from collections import namedtuple
from pyRule import utils as util
import IPython
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
# def [name](engine, *params)
def E_ADD(engine, params):
    """ Assert the params into the engine """
    #assert(all([isinstance(x, Node) for x in params[0]]))
    engine.add(params[0])

def E_RETRACT(engine, params):
    """ Remove the params from the engine """
    #assert(all([isinstance(x, Node) for x in params[0]]))
    engine.retract(params[0])

def E_PRINT(engine, params):
    """ Trigger a logging statement """
    for x in params:
        print(x)
        logging.info("Engine Output: {}".format(x))


ACTS_LOOKUP = {
    ACTS.ADD : E_ADD,
    ACTS.RETRACT : E_RETRACT,
    ACTS.PRINT : E_PRINT
}

ACTS_REVERSE_LOOKUP = {
    ACTS.ADD : "+",
    ACTS.RETRACT : "-",
    ACTS.PRINT : "@",
}

class Action:
    """ The Core Action Class, holds an operator,  and a list of values """
    def __init__(self, op, values, type=None):
        """ Create an action with an operator and values """
        assert(isinstance(op, (ACTS, str)))
        assert(isinstance(values, list))
        #todo: assert that values are a fact string, value, or binding
        self._op = op
        self._values = values
        if type is None:
            self._type = util.MUTABLE.CORE
        else:
            assert(isinstance(type, util.MUTABLE))
            self._type = type
        #the actions that group together logically
        #ie: expanded action macros
        self._linkedActions = []

    def expandBindings(self, bindings):
        """ Expand stored bindings at interpret time
        ie: +(.a.b.$x) + { x : .a.b.c } -> +(.a.b.a.b.c)
        """
        newValues = []
        for x in self._values:
            if isinstance(x, util.Bind) and x.value in bindings:
                newValues.append(bindings[x.value])
            elif isinstance(x, list):
                newValues.append(util.expandFact(x, bindings))
            else:
                newValues.append(x)
        return Action(self._op, newValues)

    def __repr__(self):
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
            if isinstance(x, util.Bind):
                output.append(data[x.value])
            elif isinstance(x, list): #and all([isinstance(y, Node) for y in x]):
                output.append([y.bind(data) for y in x])
            else:
                output.append(x)
        return output


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

    def expandBindings(self, bindings):
        """ Expand the macro  out based on the bindings passed in """
        newActions = [x.expandBindings(bindings) for x in self._actions]
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

    def expandBindings(self, bindings):
        #todo: does this need to be implemented?
        return self
