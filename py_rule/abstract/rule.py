""" Rule:  Stores the representation of an entire rule for an engine.
    Holds a query of clauses, bindings are passed to a transform,
    the results are passed to the action list
"""
import logging as root_logger
import py_rule.util as util
from .value import PyRuleValue
from .transform import Transform
from .action import Action, ActionMacroUse
from .query import Query

logging = root_logger.getLogger(__name__)


class Rule(PyRuleValue):
    """ A Rule holds a query (of N Clauses), a set of transforms,
    and a set of actions. It can be tagged with attributes.
    """

    __count = 0
    # TODO handle None's better
    def __init__(self, query, action=None, transform=None, name=None, tags=None):
        assert(query is None or isinstance(query, Query))
        assert(action is None or isinstance(action, Action))
        assert(transform is None or isinstance(transform, Transform))
        assert(tags is None or all([isinstance(x, str) for x in tags]))
        if name is not None:
            self._name  = name
        self._query     = query
        self._transform = transform
        self._action    = action
        if tags is not None:
            self._tags = set(tags)
        else:
            self._tags = set()
        Rule.__count += 1

    def __str__(self):
        nameStr = str(self._name)
        # Construct Tags str
        if bool(self._tags):
            tagsStr = "\t" + ", ".join(sorted(["#{}".format(x)
                                               for x in self._tags])) + "\n\n"
        else:
            tagsStr = ""

        # Construct Query Str
        if self._query is not None:
            queryStr = "\t" + str(self._query) + "\n\n"
        else:
            queryStr = ""

        # Construct Transform Str
        if self._transform is not None:
            transformStr = "\t" + str(self._transform) + "\n\n"
        else:
            transformStr = ""

        # Construct Action Str
        if bool(self._action):
            actionsStr = "\t" + "\n\t".join([str(x)
                                             for x in self._action]) + "\n"
        else:
            actionsStr = ""
        return "{}:\n{}{}{}{}end".format(nameStr,
                                         tagsStr,
                                         queryStr,
                                         transformStr,
                                         actionsStr)

    def __repr__(self):
        """ Create a representation of the rule.
        Not implementation specific
        """
        nameStr = "".join([repr(x) for x in self._name])

        # Construct Tag Str
        if bool(self._tags):
            tagsStr = "\t" + ", ".join(sorted(["#{}".format(x)
                                               for x in self._tags])) + "\n\n"
        else:
            tagsStr = ""

        # Construct Query Str
        if self._query is not None:
            queryStr = "\t" + repr(self._query) + "\n\n"
        else:
            queryStr = ""

        # Construct Transform Str
        if self._transform is not None:
            transformStr = "\t" + repr(self._transform) + "\n\n"
        else:
            transformStr = ""

        # Construct Action Str
        if bool(self._action):
            actionsStr = "\t" + "\n\t".join([repr(x)
                                             for x in self._action]) + "\n"
        else:
            actionsStr = ""
        return "{}:\n{}{}{}{}end".format(nameStr,
                                         tagsStr,
                                         queryStr,
                                         transformStr,
                                         actionsStr)

    def has_tag(self, *tags):
        return all([t in self._tags for t in tags])

    def is_coherent(self):  # can raise an Exception from verify_op
        """ Verify that the outputs of the query match the
        inputs of the transform, match the inputs of the actions """
        if self._transform is not None:
            self._transform.verify_ops()

        if bool(self._action):
            verified = [x.verify_op() for x in self._action]
        # if nothing raises an exception:
        return True

    def expand_bindings(self, bindings):
        """ Return a new Rule, modified to have
        bindings replaced with their values
        """
        assert(isinstance(bindings, dict))
        new_query = None
        new_action = None
        # Transforms don't need to expand bindings
        transform_copy = None

        if self._transform is not None:
            transform_copy = self._transform.copy()

        # expand the name
        new_name = self._name.expand_bindings(bindings)
        # expand the query
        if self._query is not None:
            new_query = self._query.expand_bindings(bindings)

        # expand the actions
        if self._action is not None:
            new_action = self._action.expand_bindings(bindings)


        return self.__class__(new_query,
                              new_action,
                              transform=transform_copy,
                              name=new_name,
                              tags=self._tags)

    def expand_action_macros(self, macros):
        """ Return a new Rule, where action macros have been expanded into
        the sequence of actions they represent """
        expandedActions = []
        for action in self._action:
            if isinstance(action, Action):
                expandedActions.append(action)
            else:
                assert(isinstance(action, ActionMacroUse))
                assert(action._name in macros)
                # the macro:
                aMacro = macros[action._name]
                # get the call params
                cPars = action._params
                # get the formal params
                fPars = aMacro._params
                # create the rebind dictionary
                bindDict = util.build_rebind_dict(fPars, cPars)
                # expand the individual actions
                exActs = [x.expand_bindings(bindDict) for x in aMacro._action]
                # splice
                expandedActions += exActs

        # return a copy of with the expanded action list
        return self.__class__(self._query,
                              expandedActions,
                              transform=self._transform,
                              name=self._name,
                              tags=self._tags)
