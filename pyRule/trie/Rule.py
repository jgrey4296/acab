import logging as root_logger
from .Query import Query
import pyRule.utils as util

logging = root_logger.getLogger(__name__)

class Rule:
    __count = 0

    
    def __init__(self, query, actions, transform=None, name=None, tags=None):
        assert(isinstance(query, Query))
        assert(isinstance(actions, list))
        assert(all([isinstance(x, util.Action) for x in actions]))
        assert(isinstance(transform, util.Transform))
        if name is None:
            self._name = "anon_{}".format(Rule.__count)
            Rule.__count += 1
        self._query = query
        self._transform = transform
        self._actions = actions
        #todo: fill set
        self._tags = Set()
        
    def __repr__(self):
        queryStr = repr(self._query)
        transformStr = repr(self._transform)
        actionsStr = repr(self._actions)
        return "rule {}\n{}\n{}\n{}".format(self._name,
                                            queryStr,
                                            transformStr,
                                            actionsStr)
    
    def is_coherent(self): #raises an Exception othewise
        """ Verify that the outputs of the query match the 
        inputs of the transform, match the inputs of the actions """
        self._transform.verify_ops()
        [x.verify_op() for x in self._actions]


