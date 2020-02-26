""" TrieRule: The Subclass of a rule specifically for trie-implemented
rule engines
"""
from py_rule.abstract.rule import Rule
from py_rule.abstract.sentence import Sentence
from .parsing import FactParser as FP


class TrieRule(Rule):
    """ An implementation of an Abstract Rule class, specifically for
    Trie based engines and working memory
    """

    def __init__(self, query, actions, transform=None, name=None, tags=None):
        if name is None:
            name = FP.parseString(".rule.anon.{}".format(Rule.__count))[0]
            Rule.__count += 1
        else:
            assert(isinstance(name, Sentence))
        super().__init__(query, actions, transform, name, tags)

    def to_node_lists(self):
        """ Convert a rule to a list of node lists  """
        # TODO
        baseName = None
        conditions = []
        transforms = []
        actions = []

        raise NotImplementedError()

    @staticmethod
    def from_trie(node):
        """ given a root node of a trie, create a rule from it """
        # TODO
        raise NotImplementedError()
