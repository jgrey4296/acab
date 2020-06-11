""" Rule:  Stores the representation of an entire rule for an engine.
    Holds a query of clauses, bindings are passed to a transform,
    the results are passed to the action list
"""
import logging as root_logger

import py_rule.util as util
from py_rule.abstract.printing import util as PrU

from .production_operator import ProductionContainer
from .transform import Transform
from .action import Action
from .query import Query

logging = root_logger.getLogger(__name__)

# TODO: create subclass that flattens/aggregates contexts
class Rule(ProductionContainer):
    """ A Rule holds a query (of N Clauses), a set of transforms,
    and a set of actions. It can be tagged with attributes.
    """

    __count = 0
    def __init__(self, query=None, action=None, transform=None, name="AnonRule", type_str=util.RULE_S):
        assert(query is None or isinstance(query, Query))
        assert(action is None or isinstance(action, Action))
        assert(transform is None or isinstance(transform, Transform))
        super().__init__(None, type_str=type_str, name=name)
        self._query     = query
        self._transform = transform
        self._action    = action
        Rule.__count += 1

    def __call__(self, ctxs=None, engine=None):
        """ Rule Logic, returns action proposals """
        if ctxs is None:
            ctxs = []
        assert(self.verify())
        assert(isinstance(ctxs, list))
        assert(all([isinstance(x, dict) for x in ctxs]))
        assert(all([x.value in y for x in self._vars for y in ctxs]))
        logging.info("Running Rule: {}".format(self._name))

        query_result = []
        if ctxs is not None:
            query_result = [x.copy() for x in ctxs]

        # Run the query
        if self._query is not None:
            query_result = self._query(ctxs=query_result, engine=engine)
            if not bool(query_result):
                logging.info("Rule {} Failed".format(self._name))
                return []

        # TODO: Layer/agenda/pipelines need to collapse the context here?

        # Run any transforms
        # This is *not* an unnecessary comprehension
        # because of how parse results work
        transformed = query_result[:]
        if self._transform:
            transformed = self._transform(ctxs=transformed, engine=engine)

        # return final passing dictionaries
        results = []
        for data in transformed:
            results.append((data, self))

        return results


    @property
    def var_set(self):
        obj = super(Rule, self).var_set
        query_set = self._query.var_set
        transform_set = self._transform.var_set
        action_set = self._action.var_set

        obj['in'].update(*[x['in'] for x in [query_set, transform_set, action_set]])
        obj['out'].update(*[x['out'] for x in [query_set, transform_set, action_set]])

        return obj


    def verify(self):  # can raise an Exception from verify_op
        """ Verify that the outputs of the query match the
        inputs of the transform, match the inputs of the actions """
        # TODO get query, transform, action var sets and unify

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


    def pprint_body(self, val):
        head, body = self.pprint_has_content

        if self._query is not None:
            val += "\t"
            val += self._query.pprint()

        if self._transform is not None:
            val += "\n\t"
            val += self._transform.pprint()

        if self._action is not None:
            val += "\n\t"
            val += self._action.pprint()

        return val

    @property
    def pprint_has_content(self):
        head = any([bool(x) for x in [self._vars,
                                      self._tags]])
        body = any([x is not None for x in [self._query,
                                            self._transform,
                                            self._action]])

        return (head, body)



PrU.register_statement({util.RULE_S : util.RULE_HEAD_S})
PrU.register_class(Rule, PrU.print_statement)
