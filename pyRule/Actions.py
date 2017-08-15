import logging as root_logger
from enum import Enum
from pyRule import utils as util
logging = root_logger.getLogger(__name__)

#Action operators:
ACTS = Enum('Action_ops', 'ADD RETRACT PRINT CUSTOM')

#Action function template:
# def [name](engine, *params)
def E_ADD(engine, params):
    for x in params:
        engine.add(x)

def E_RETRACT(engine, params):
    for x in params:
        engine.retract(x)

def E_PRINT(engine, params):
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
    def __init__(self, op, values):
        assert(isinstance(op, ACTS) or isinstance(op, str))
        assert(isinstance(values, list))
        #todo: assert that values are a fact string, value, or binding
        self._op = op
        self._values = values

    def __repr__(self):
        if isinstance(self._op, str):
            return "{}({})".format(self._op,
                                   ", ".join([repr(x) for x in self._values]))


    def verify_op(self):
        if self._op not in ACTS_LOOKUP \
           and not isinstance(self._op, str):
            raise Exception("Unrecognised Action: {}".format(self._op))


    def get_values(self, data):
        output = []
        for x in self._values:
            if isinstance(x, util.Bind):
                output.append(data[x.value])
            else:
                output.append(x)
        return output
