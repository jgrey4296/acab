import logging as root_logger
from enum import Enum
from pyRule import utils as util
from pyRule.trie import Node
logging = root_logger.getLogger(__name__)

#Action operators:
ACTS = Enum('Action_ops', 'ADD RETRACT PRINT CUSTOM')

#Action function template:
# def [name](engine, *params)
def E_ADD(engine, params):
    assert(len(params), 1)
    assert(all([isinstance(x, Node) for x in params[0]]))
    engine.add(params[0])

def E_RETRACT(engine, params):
    assert(len(params), 1)
    assert(all([isinstance(x, Node) for x in params[0]]))
    engine.retract(params[0])


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
        else:
            return "{}({})".format(ACTS_REVERSE_LOOKUP[self._op],
                                   ", ".join([repr(x) for x in self._values]))

    def is_custom(self):
        return isinstance(self._op, str) or not isinstance(self._op, ACTS)

    def verify_op(self):
        if self._op not in ACTS_LOOKUP \
           and not isinstance(self._op, str):
            raise Exception("Unrecognised Action: {}".format(self._op))


    def get_values(self, data):
        output = []
        for x in self._values:
            if isinstance(x, util.Bind):
                output.append(data[x.value])
            if isinstance(x, list) and all([isinstance(y, Node) for y in x]):
                output.append([y.bind(data) for y in x])
            else:
                output.append(x)
        return output
