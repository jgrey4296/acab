"""
Functions to facilitate matching Trie-Nodes to queries
"""
import logging as root_logger

from py_rule.abstract.trie.nodes.trie_node import TrieNode

from . import util

logging = root_logger.getLogger(__name__)


def test_alphas(node, comps):
    """ Run alpha tests against a retrieved value """
    return all([x(node) for x in comps])

def test_betas(node, comps, data):
    """ Run a beta tests against a retrieved value, with supplied bindings """
    return all([x(node, data=data) for x in comps])

def exclusion_matches(a, b):
    """ Compare the EXOP of a node, with whether that exop
    is in the children of the other node/parent """
    logging.info("Running exclusion match test: {} {}".format(str(a), str(b)))
    assert(isinstance(a, TrieNode))
    assert(isinstance(b, TrieNode))
    result = util.node_is_exclusive(a) and not util.node_looks_exclusive(b)
    if result:
        logging.info("C: {}, Node: {}, {}".format(util.node_is_exclusive(a),
                                                  util.node_is_exclusive(b), str(b)))
        logging.info("Mismatch EX num")
    return result

def non_bind_value_match(a, b, betas, regexs, data):
    """ Compare two values without caring about bindings
    a is the test node, b is the existing node
    """
    logging.info("Running non bind value match: {}{}".format(repr(b), repr(a)))
    assert(isinstance(a, TrieNode))
    assert(isinstance(b, TrieNode))
    tested = False
    new_node, new_data = (None, None)
    if not a.is_var:
        logging.info("Not Bind: {}|{}".format(str(a), b._children.keys()))
        tested = True
        if a in b:  # and test_betas(a._value, betas,data):
            logging.info("Suitable value")
            new_data, new_node = b.get_child(a).test_regexs_for_matching(regexs,
                                                                         data)

    return (tested, new_node, new_data)

def existing_bind_match(a, b, betas, regexs, data):
    """ Compare an existing bound value to the children of b
    a is the test node, b is the existing node
    """
    logging.info("Running existing bind match: {}{}".format(repr(b), repr(a)))
    assert(isinstance(a, TrieNode))
    assert(isinstance(b, TrieNode))
    tested = False
    new_node, new_data = (None, None)
    if a.name in data:
        tested = True
        if data[a.name] in b._children \
           and test_betas(data[a.name], betas, data):
            new_data, new_node = b._children[data[a.name]].test_regexs_for_matching(regexs,
                                                                                    data)
    return (tested, new_node, new_data)

def create_new_bindings(a, b, alphas, betas, regexs, data):
    """ Create new bindings for a previously unbound variable.
    a: the clause component,
    b: the node
    """
    logging.info("Creating new bindings: {} {} ({})".format(repr(b),
                                                            repr(a),
                                                            len(alphas)))
    assert(isinstance(a, TrieNode))
    assert(isinstance(b, TrieNode))
    output = []
    potentials = b._children.values()
    passing = [x for x in potentials if test_alphas(x, alphas)
               and test_betas(x, betas, data)]
    logging.info("Passing: {}".format(len(passing)))
    for x in passing:
        output.append(x.test_regexs_for_matching(regexs,
                                                 data,
                                                 preupdate=[(a._value, x._value),
                                                            (util.AT_BIND_S + a._value, x)]))
    return output
