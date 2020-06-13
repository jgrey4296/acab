""" Contexts: A Container for all partial matches of word query being run """
import itertools as it
from enum import Enum
import logging as root_logger

from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.production_operator import ProductionOperator
from py_rule.util import AT_BIND_S, FALLBACK_S, AT_BIND_S, BIND_S, CONSTRAINT_S

logging = root_logger.getLogger(__name__)

CTX_OP = Enum("ctx", "collapse")



class Contexts:
    """ Container of available contexts for word match in the trie
    Conceptually a list of tuples: ({}, LastAccessedNode)
    And Stores failure state
    """
    @staticmethod
    def rebind_across_contexts(names, values, base):
        assert(isinstance(base, dict))
        assert(isinstance(names, list))
        assert(isinstance(values, tuple))
        new_base = {}
        new_base.update(base)
        for x,y in zip(names, values):
            new_base[x.name] = PyRuleValue.safe_make(y)

        return new_base


    def __init__(self, start_node=None, bindings=None, engine=None):
        """
        Setup the initial context of no bindings
        """
        self._bind_groups = []
        self._nodes = []
        self._failures = []
        self._queued_failures = []
        self._engine = engine

        if bindings is not None:
            if not isinstance(bindings, list):
                bindings = [bindings]

            self._bind_groups += bindings

        if start_node is not None:
            self._nodes.append(start_node)


    def __len__(self):
        return len(self._bind_groups)

    def __getitem__(self, key):
        """ Get binding dictionaries """
        if isinstance(key, slice):
            return [x for x in self._bind_groups.__getitem__(key)]

        return self._bind_groups[key]

    def __iter__(self):
        return iter(self._bind_groups)

    def __repr__(self):
        if bool(self):
            return "Context: {}".format(len(self))
        else:
            return "Context: False"

    def __bool__(self):
        return bool(self._bind_groups)


    def append(self, *data, fail_dict=None):
        """ Add a number of matching possibilities into this set of contexts """
        assert(all([isinstance(x, tuple) for x in data]))
        successes = set()
        if fail_dict is None:
            for d, n in data:
                self._bind_groups.append(d)
                self._nodes.append(n)
        else:
            for i,d,n  in data:
                successes.add(i)
                self._bind_groups.append(d)
                self._nodes.append(n)

            to_fail = set(fail_dict.keys()).difference(successes)
            self.fail(items=[fail_dict[x] for x in to_fail])


    def fail(self, item=None, items=None):
        """ Remove all contexts, as none are suitable """
        if item is not None:
            self._queued_failures.append(item)
        elif items is not None:
            self._queued_failures += items
        else:
            self.clear()


    def invert(self):
        # swap failures with successes
        temp = self._bind_groups
        self._bind_groups = self._queued_failures
        self._queued_failures = temp

    def promote_failures(self, fallback_pairs):
        while bool(self._queued_failures):
            current = self._queued_failures.pop(0)
            for bind_target, val in clause._data[FALLBACK_S]:
                current[bind_target.value] = val
                self.append((current, None))

    def demote_failures(self):
        self._failures += self._queued_failures
        self._queued_failures = []

    def clear(self):
        self._bind_groups = []
        self._nodes = []

    def force_node_position(self, target=None, binding=None):
        assert (target is not None or binding)
        if not bool(self):
            self._bind_groups = [{}]

        if binding is None:
            self._nodes = [target]
            return

        assert(binding is not None)
        bind_node_name = AT_BIND_S + binding
        bind_groups = self._bind_groups
        self.clear()

        self._bind_groups = [x for x in bind_groups if bind_node_name in x]
        self._nodes = [x[bind_node_name] for x in self._bind_groups]

    def pairs(self):
        return zip(self._bind_groups, it.cycle(self._nodes))


    def collapse(self, on_vars):
        """
        Semantics of collapse:
        n[ctx] -> 1[c:ctx]
        where
        c = a_ctx = { on_var : [x[on_var] for x in ctxs] }
        """
        assert(isinstance(on_vars, set))
        bind_groups = self._bind_groups
        node_head = self._nodes[0]
        head = self._bind_groups[0]
        for a_var in on_vars:
            head[a_var] = [x[a_var] for x in bind_groups]
            # TODO collapse at_bind's as well
        self._bind_groups = [head]
        self._nodes = [node_head]
