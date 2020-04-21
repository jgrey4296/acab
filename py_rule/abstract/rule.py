""" Rule:  Stores the representation of an entire rule for an engine.
    Holds a query of clauses, bindings are passed to a transform,
    the results are passed to the action list
"""
import logging as root_logger

import py_rule.util as util
from py_rule.abstract.printing import util as PrU

from .value import PyRuleValue
from .transform import Transform
from .action import Action
from .query import Query

logging = root_logger.getLogger(__name__)


class Rule(PyRuleValue):
    """ A Rule holds a query (of N Clauses), a set of transforms,
    and a set of actions. It can be tagged with attributes.
    """

    __count = 0
    # TODO handle None's better
    def __init__(self, query=None, action=None, transform=None, name="AnonRule"):
        assert(query is None or isinstance(query, Query))
        assert(action is None or isinstance(action, Action))
        assert(transform is None or isinstance(transform, Transform))
        super().__init__(None, type_str=util.RULE_S, name=name)
        self._query     = query
        self._transform = transform
        self._action    = action
        Rule.__count += 1


    @property
    def var_set(self):
        obj = super(Rule, self).var_set
        query_set = self._query.var_set
        transform_set = self._transform.var_set
        action_set = self._action.var_set

        obj['in'].update(*[x['in'] for x in [query_set, transform_set, action_set]])
        obj['out'].update(*[x['out'] for x in [query_set, transform_set, action_set]])

        return obj


    def copy(self):
        query, action, transform = (None, None, None)
        if self._query is not None:
            query = self._query.copy()
        if self._action is not None:
            action = self._action.copy()
        if self._transform is not None:
            transform = self._transform.copy()

        return Rule(query=query,
                    action=action,
                    transform=transform,
                    name=self._name)

    def is_coherent(self):  # can raise an Exception from verify_op
        """ Verify that the outputs of the query match the
        inputs of the transform, match the inputs of the actions """
        if self._transform is not None:
            self._transform.verify()

        if bool(self._action):
            dummy = [x.verify() for x in self._action]
        # if nothing raises an exception:
        return True

    def bind(self, bindings):
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

        # expand the query
        if self._query is not None:
            new_query = self._query.bind(bindings)

        # expand the actions
        if self._action is not None:
            new_action = self._action.bind(bindings)


        new_rule = self.__class__(new_query,
                                  new_action,
                                  transform=transform_copy)
        new_rule._tags.update(self._tags)

        return new_rule

    def pprint(self, **kwargs):
        return PrU.print_statement(self, **kwargs)
