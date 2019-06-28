""" Rule:  Stores the representation of an entire rule for an engine.
    Holds a query of clauses, bindings are passed to a transform,
    the results are passed to the action list """
import logging as root_logger
import IPython
import py_rule.utils as util
import py_rule.abstract.transforms as T
import py_rule.abstract.actions as A
from .query import Query

logging = root_logger.getLogger(__name__)

class Rule:
    """ A Rule holds a query (of N Clauses), a set of transforms,
    and a set of actions. It can be tagged with attributes.
    """

    __count = 0

    def __init__(self, query, actions, transform=None, name=None, tags=None):
        assert(query is None or isinstance(query, Query))
        assert(isinstance(actions, list))
        assert(all([isinstance(x, (A.Action, A.ActionMacroUse)) for x in actions]))
        assert(transform is None or isinstance(transform, T.Transform))
        assert(tags is None or all([isinstance(x, str) for x in tags]))
        if name is not None:
            self._name = name
        self._query = query
        self._transform = transform
        self._actions = actions
        if tags is not None:
            self._tags = set(tags)
        else:
            self._tags = set()

    def __str__(self):
        nameStr = "".join([str(x) for x in self._name])
        if bool(self._tags):
            tagsStr = "\t" + ", ".join(sorted(["#{}".format(x) for x in self._tags])) + "\n\n"
        else:
            tagsStr = ""
        if self._query is not None:
            queryStr = "\t" + str(self._query) + "\n\n"
        else:
            queryStr = ""
        if self._transform is not None:
            transformStr = "\t" + str(self._transform) + "\n\n"
        else:
            transformStr = ""
        if bool(self._actions):
            actionsStr = "\t" + "\n\t".join([str(x) for x in self._actions]) + "\n"
        else:
            actionsStr = ""
        return "{}:\n{}{}{}{}end".format(nameStr,
                                         tagsStr,
                                         queryStr,
                                         transformStr,
                                         actionsStr)


    def __repr__(self):
        """ Create a representation of the rule. Not implementation specific """
        nameStr = "".join([repr(x) for x in self._name])
        if bool(self._tags):
            tagsStr = "\t" + ", ".join(sorted(["#{}".format(x) for x in self._tags])) + "\n\n"
        else:
            tagsStr = ""
        if self._query is not None:
            queryStr = "\t" + repr(self._query) + "\n\n"
        else:
            queryStr = ""
        if self._transform is not None:
            transformStr = "\t" + repr(self._transform) + "\n\n"
        else:
            transformStr = ""
        if bool(self._actions):
            actionsStr = "\t" + "\n\t".join([repr(x) for x in self._actions]) + "\n"
        else:
            actionsStr = ""
        return "{}:\n{}{}{}{}end".format(nameStr,
                                         tagsStr,
                                         queryStr,
                                         transformStr,
                                         actionsStr)

    def has_tag(self, t):
        return t in self._tags

    def is_coherent(self): #raises an Exception othewise
        """ Verify that the outputs of the query match the
        inputs of the transform, match the inputs of the actions """
        if self._transform is not None:
            self._transform.verify_ops()

        if bool(self._actions):
            verified = [x.verify_op() for x in self._actions]
        #if nothing raises an exception:
        return True

    def expandBindings(self, bindings):
        """ Return a new Rule, modified to have bindings replaced with their values """
        assert(isinstance(bindings, dict))
        #expand the name
        newName = util.expandFact(self._name, bindings)
        #expand the query
        newQuery = self._query.expandBindings(bindings)
        #expand the actions
        newActions = [x.expandBindings(bindings) for x in self._actions]
        return self.__class__(newQuery,
                    newActions,
                    transform=self._transform,
                    name=newName,
                    tags=self._tags)

    def expandActionMacros(self, macros):
        """ Return a new Rule, where action macros have been expanded into
        the sequence of actions they represent """
        expandedActions = []
        for action in self._actions:
            if isinstance(action, A.Action):
                expandedActions.append(action)
            else:
                assert(isinstance(action, A.ActionMacroUse))
                assert(action._name in macros)
                #the macro:
                aMacro = macros[action._name]
                #get the call params
                cPars = action._params
                #get the formal params
                fPars = aMacro._params
                #create the rebind dictionary
                bindDict = util.build_rebind_dict(fPars, cPars)
                #expand the individual actions
                exActs = [x.expandBindings(bindDict) for x in aMacro._actions]
                #splice
                expandedActions += exActs

        #return a copy of with the expanded action list
        return self.__class__(self._query,
                    expandedActions,
                    transform=self._transform,
                    name=self._name,
                    tags=self._tags)

    def invert(self):
        """ Take a simple, non-transformational rule,
        and invert it, making the asserted actions the conditions,
        and the conditions the actions """
        #TODO
        raise Exception("Unimplemented")
