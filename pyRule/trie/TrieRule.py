""" TrieRule: The Subclass of a rule specifically for trie-implemented
rule engines
"""
from pyRule.Rule import Rule
from . import FactParser as FP

class TrieRule(Rule):

    def __init__(self, query, actions, transform=None, name=None, tags=None):
        if name is None:
            name = FP.parseString(".rule.anon.{}".format(Rule.__count))
            Rule.__count += 1
        else:
            assert(isinstance(name, list))
        super().__init__(query, actions, transform, name, tags)


        
    
    def to_node_lists(self):
        """ Convert a rule to a list of node lists  """
        baseName = None
        conditions = []
        transforms = []
        actions = []

        
        return []

    @staticmethod
    def from_trie(node):
        """ given a root node of a trie, create a rule from it """
        #todo
        return Rule()

