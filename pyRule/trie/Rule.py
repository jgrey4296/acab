import logging as root_logger
from .Query import Query
import pyRule.utils as util
import pyRule.Transforms as T
import pyRule.Actions as A

logging = root_logger.getLogger(__name__)

class Rule:
    __count = 0

    
    def __init__(self, query, actions, transform=None, name=None, tags=None):
        assert(isinstance(query, Query))
        assert(isinstance(actions, list))
        assert(all([isinstance(x, A.Action) for x in actions]))
        assert(transform is None or isinstance(transform, T.Transform))
        assert(tags is None or all([isinstance(x, str) for x in tags]))
        if name is None:
            self._name = "anon_{}".format(Rule.__count)
            Rule.__count += 1
        else:
            self._name = name
        self._query = query
        self._transform = transform
        self._actions = actions
        #todo: fill set
        if tags is not None:
            self._tags = set(tags)
        else:
            self._tags = set()
            
    def __repr__(self):
        #TODO: make this reparsable
        #todo: turn tags back into #tag
        nameStr = "".join([repr(x) for x in self._name])
        tagsStr = ", ".join(["#{}".format(x) for x in self._tags])
        queryStr = repr(self._query)
        transformStr = repr(self._transform)
        actionsStr = ",\n\t".join([repr(x) for x in self._actions])
        return "{}:\n\t{}\n\n\t{}\n\n\t{}\n\n\t{}\nEND".format(nameStr,
                                                                    tagsStr,
                                                                    queryStr,
                                                                    transformStr,
                                                                    actionsStr)
    
    def is_coherent(self): #raises an Exception othewise
        """ Verify that the outputs of the query match the 
        inputs of the transform, match the inputs of the actions """
        self._transform.verify_ops()
        [x.verify_op() for x in self._actions]


