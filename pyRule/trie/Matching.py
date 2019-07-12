"""
Provides support for querying. 
"""
import logging as root_logger
from pyRule import utils as util
from pyRule.Comparisons import COMP_LOOKUP
from .Node import Node
logging = root_logger.getLogger(__name__)


def test_alphas(value, comps):
    """ Run alpha tests against a retrieved value """
    return all([COMP_LOOKUP[x.op](value, x.value) for x in comps])

def test_betas(value, comps, data):
    """ Run a beta tests against a retrieved value, with supplied bindings """
    return all([COMP_LOOKUP[x.op](value, data[x.bind.value]) for x in comps])


def exclusion_matches(a, b):
    """ Compare the EXOP of a node, with whether that exop
    is in the children of the other node/parent """
    logging.info("Running exclusion match test")
    assert(isinstance(a, Node))
    assert(isinstance(b, Node))
    result = a.is_exclusive() and not b.has_exclusive()
    if result:
        logging.debug("C: {}, Node: {}, {}".format(a.is_exclusive(),
                                                   b.is_exclusive(), str(b)))
        logging.debug("Mismatch EX num")
    return result


def non_bind_value_match(a, b, betas, regexs, data):
    """ Compare two values without caring about binding """
    logging.info("Running non bind value match: {}{}".format(repr(b), repr(a)))
    assert(isinstance(a, Node))
    assert(isinstance(b, Node))
    tested = False
    newNode, newData = (None, None)
    if not a.get_meta_eval(util.META_OP.BIND):
        logging.debug("Not Bind: {}|{}".format(a._value, b._children.keys()))
        tested = True
        if a in b:# and test_betas(a._value, betas,data):
            logging.debug("Suitable value")
            newData, newNode = b._children[a._value].test_regexs_for_matching(regexs, data)

    return (tested, newNode, newData)

def existing_bind_match(a, b, betas, regexs, data):
    """ Compare an existing bound value to the children of b """
    logging.info("Running existing bind match: {}{}".format(repr(b), repr(a)))
    assert(isinstance(a, Node))
    assert(isinstance(b, Node))
    tested = False
    newNode, newData = (None, None)
    if a._value in data:
        tested = True
        if data[a._value] in b._children and test_betas(data[a._value], betas, data):
            newData, newNode = b._children[data[a._value]].test_regexs_for_matching(regexs, data)
    return (tested, newNode, newData)

def create_new_bindings(a, b, alphas, betas, regexs, data):
    """ Create new bindings for a previously unbound variable """
    logging.info("Creating new bindings: {}{}".format(repr(b), repr(a)))
    assert(isinstance(a, Node))
    assert(isinstance(b, Node))
    output = []
    potentials = b._children.keys()
    passing = [x for x in potentials if test_alphas(x, alphas) and test_betas(x, betas, data)]
    for x in passing:
        output.append(b._children[x].test_regexs_for_matching(regexs,
                                                              data,
                                                              preupdate=(a._value, x)))
    return output
