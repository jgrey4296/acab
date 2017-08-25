import logging as root_logger
from enum import Enum
from pyRule import utils as util
from pyRule.trie import Node
import IPython
logging = root_logger.getLogger(__name__)

#Action operators:
ACTS = Enum('Action_ops', 'ADD RETRACT PRINT CUSTOM')

#Action function template:
# def [name](engine, *params)
def E_ADD(engine, params):
    assert(all([isinstance(x, Node) for x in params[0]]))
    engine.add(params[0])

def E_RETRACT(engine, params):
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
            op = self._op
        else:
            op = ACTS_REVERSE_LOOKUP[self._op]
        args = []
        for val in self._values:
            if isinstance(val, list) and isinstance(val[0], Node):
                args.append("".join([str(x) for x in val[1:]]))
            else:
                args.append(str(val))
        return "{}({})".format(op, ",".join(args))
        
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
            elif isinstance(x, list) and all([isinstance(y, Node) for y in x]):
                output.append([y.bind(data) for y in x])
            else:
                output.append(x)
        return output
