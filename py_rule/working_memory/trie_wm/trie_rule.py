""" TrieRule: The Subclass of a rule specifically for trie-implemented
rule engines
"""
from py_rule.abstract.rule import Rule


class TrieRule(Rule):
    """ An implementation of an Abstract Rule class, specifically for
    Trie based engines and working memory
    """

    def __init__(self, query, action=None, transform=None):
        super().__init__(query,
                         action=action,
                         transform=transform)

    def to_node_lists(self):
        """ Convert a rule to a list of node lists  """
        # TODO
        base_name = None
        conditions = []
        transforms = []
        actions = []

        raise NotImplementedError()

    @staticmethod
    def from_trie(node):
        """ given a root node of a trie, create a rule from it """
        # TODO
        raise NotImplementedError()
