import logging as root_logger
from .Query import Query
from .Node import Node
import pyRule.utils as util
import pyRule.Transforms as T
import pyRule.Actions as A
import IPython
logging = root_logger.getLogger(__name__)

class Rule:
    __count = 0

    
    def __init__(self, query, actions, transform=None, name=None, tags=None):
        assert(query is None or isinstance(query, Query))
        assert(isinstance(actions, list))
        assert(all([isinstance(x, A.Action) for x in actions]))
        assert(transform is None or isinstance(transform, T.Transform))
        assert(tags is None or all([isinstance(x, str) for x in tags]))
        if name is None:
            #todo: convert this to a fact string
            self._name = ".rule.anon_{}".format(Rule.__count)
            Rule.__count += 1
        else:
            assert(isinstance(name, list))
            self._name = name
        self._query = query
        self._transform = transform
        self._actions = actions
        if tags is not None:
            self._tags = set(tags)
        else:
            self._tags = set()
            
    def __repr__(self):
        nameStr = "".join([repr(x) for x in self._name[1:]])
        tagsStr = ", ".join(["#{}".format(x) for x in self._tags])
        if self._query is not None:
            queryStr = "\t" + repr(self._query) + "\n\n"
        else:
            queryStr = ""
        if self._transform is not None:
            transformStr = "\t" + repr(self._transform) + "\n\n"
        else:
            transformStr = ""
        if len(self._actions):
            actionsStr = "\n\t".join([repr(x) for x in self._actions]) + "\n"
        else:
            actionsStr = ""
        return "{}:\n{}{}{}{}end".format(nameStr,
                                         tagsStr,
                                         queryStr,
                                         transformStr,
                                         actionsStr)
    
    def is_coherent(self): #raises an Exception othewise
        """ Verify that the outputs of the query match the 
        inputs of the transform, match the inputs of the actions """
        if self._transform is not None:
            self._transform.verify_ops()

        if len(self._actions) > 0:
            [x.verify_op() for x in self._actions]
        #if nothing raises an exception:
        return True


    def to_node_lists(self):
        """ Convert a rule to a list of node lists  """
        return []

    @staticmethod
    def from_trie(self, node):
        """ given a root node of a trie, create a rule from it """
        return Rule()

    def expandBindings(self, bindings):
        #expand the name
        newName = util.expandFact(self._name, bindings)
        #expand the query
        newQuery = self._query.expandBindings(bindings)
        #expand the actions
        newActions = [x.expandBindings(bindings) for x in self._actions]
        return Rule(newQuery,
                    newActions,
                    transform=self._transform,
                    name=newName,
                    tags=self._tags)
