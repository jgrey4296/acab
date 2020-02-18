"""
Definitions of the Core Performance Operators
"""
import logging as root_logger
from py_rule.abstract.action import ActionOp
from enum import Enum

logging = root_logger.getLogger(__name__)
ACTS = Enum('Action_ops', 'ADD RETRACT PRINT CUSTOM ACT_MACRO')

# Action operators:

# TODO: add rule modification actions:
# add/remove penumbral condition
# add/remove penumbral action
# add/remove tag?
# insert/remove penumbral action from sequence
# modify hierarchy


class ActionAdd(ActionOp):
    def __init__(self):
        super().__init__("+")

    def __call__(self, engine, params):
        """ Assert the params into the engine """
        # assert(all([isinstance(x, Node) for x in params[0]]))
        engine.add(params[0])


class ActionRetract(ActionOp):
    def __init__(self):
        super().__init__("-")

    def __call__(self, engine, params):
        """ Remove the params from the engine """
        # assert(all([isinstance(x, Node) for x in params[0]]))
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


