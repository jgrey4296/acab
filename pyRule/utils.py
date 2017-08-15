from enum import Enum
from collections import namedtuple

#Trie exclusion operator:
EXOP = Enum('EXOP', 'DOT EX ROOT')
EXOP_lookup = {
    EXOP.DOT : ".",
    EXOP.EX : "!",
    EXOP.ROOT : "||"
    }

META_OP = Enum("Meta Ops", "ORDER BIND COMP NEGATED")

#Action operators:
ACTS = Enum('Action_ops', 'ASSERT RETRACT PRINT CUSTOM')
ACTS_LOOKUP = {
    ACTS.ASSERT : "+",
    ACTS.RETRACT : "-",
    ACTS.PRINT : "@",
}

#Basic Data Structures
class Bind: #pylint: disable=too-few-public-methods 
    """ Simple holder to designate a binding action """
    def __init__(self, v):
        self.value = v
        
    def __repr__(self):
        return "$" + self.value
    
class Action:
    def __init__(self, op, values):
        assert(isinstance(op, ACTS) or isinstance(op, str))
        #todo: assert that values are a fact string, value, or binding
        self._op = op
        self._values = values

    def __repr__(self):
        if isinstance(self._op, str):
            return "{}({})".format(self._op,
                                   ", ".join([repr(x) for x in self._values]))

