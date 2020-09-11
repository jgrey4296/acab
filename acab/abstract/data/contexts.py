"""
Contexts: A Container for mutually inclusive binding fragments
Essentially a possibility space of rule results

"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, AliasType
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

import itertools as it
from enum import Enum
import logging as root_logger

from acab.abstract.core.value import AcabValue
from acab.config import AcabConfig

util = AcabConfig.Get()

AT_BIND_S = util("Parsing.Structure", "AT_BIND_S")
FALLBACK_S = util("Parsing.Structure", "FALLBACK_S")
BIND_S = util("Parsing.Structure", "BIND_S")
CONSTRAINT_S = util("Parsing.Structure", "CONSTRAINT_S")


logging = root_logger.getLogger(__name__)

CTX_OP = Enum("ctx", "collapse")


class Contexts:
    """ Container of available contexts for word match in the trie
    Conceptually a list of tuples: ({}, LastAccessedNode)
    And Stores failure state
    """
    @staticmethod
    def rebind_across_contexts(names, values, base):
        """ Utility method to perform alpha conversion """
        assert(isinstance(base, dict))
        assert(isinstance(names, list))
        assert(isinstance(values, tuple))
        new_base = {}
        new_base.update(base)
        for x,y in zip(names, values):
            new_base[x.name] = AcabValue.safe_make(y)

        return new_base


    def __init__(self, start_node=None, bindings=None, engine=None):
        """
        Setup the initial context of no bindings
        """
        # Link the context with what asked it:
        self._query_history = []
        self._query_fail_clause = None
        self._query_remainder = []
        # Bind Groups
        self._bind_groups : List[Dict] = []
        # Nodes
        self._nodes : List[AcabNode] = []
        # Failures: Records true failures
        self._failures : List[Dict] = []
        # Queued Failures: Records failures that may potentially have fallback values
        self._queued_failures : List[Dict] = []

        # The Engine used for operator retrieval
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


    @property
    def nodes(self):
        return self._nodes

    def append(self, *data, fail_dict=None):
        """
        Add pairs of (bind_dict, node) to the context
        """
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
        """
        Either add specific failures to the queue,
        or remove all bind dicts, as none are suitable
        """
        if item is not None:
            self._queued_failures.append(item)
        elif items is not None:
            self._queued_failures += items
        else:
            self.clear()


    def invert(self):
        """
        Swap failures with successes.
        This is used when evaluating negated clauses
        """
        temp = self._bind_groups
        self._bind_groups = self._queued_failures
        self._queued_failures = temp

    def promote_failures(self, fallback_pairs):
        """
        Raise potential failures to successes if they have
        appropriate fallback values
        """
        while bool(self._queued_failures):
            current = self._queued_failures.pop(0)
            for bind_target, val in clause._data[FALLBACK_S]:
                current[bind_target.value] = val

            self.append((current, None))

    def demote_failures(self):
        """ Demote all potential failures to true failures """
        self._failures += self._queued_failures
        self._queued_failures = []

    def clear(self):
        """ Clear the entire context ready for new additions """
        self._bind_groups = []
        self._nodes = []

    def force_node_position(self, target=None, binding=None):
        """
        Override current node positions for bind groups,
        either with a single target node (ie: root)
        or with a binding in the bind groups (ie: @x)
        """
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
        """ Return an iterator of bind groups with their location node """
        return zip(self._bind_groups, it.cycle(self._nodes))


    def collapse(self, on_vars):
        """
        Context collapse on specific vars.
        Flattens many contexts into one, with specified variables
        now as lists accumulated from across the contexts.

        Semantics of collapse:
        1[ctx]n -> 1[c:ctx]1
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
