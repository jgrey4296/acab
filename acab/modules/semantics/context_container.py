from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import logging as root_logger
logging = root_logger.getLogger(__name__)

from dataclasses import InitVar, dataclass, field, replace
from uuid import uuid1, UUID

from acab.abstract.config import GET
import acab.abstract.interfaces.context_interface as CtxInt
from acab.abstract.core.production_abstractions import ProductionComponent
from acab.modules.semantics.constraints import ConstraintCollection
import acab.error.acab_semantic_exception as ASErr

config = GET()
CONSTRAINT_S = config.value("Parse.Structure", "CONSTRAINT")

CtxIns      = 'ContextInstance'
Constraints = 'ConstraintCollection'
ProdComp    = 'ProductionComponent'
Operator    = 'ProductionOperator'
Value       = 'AcabValue'
Statement   = 'AcabStatement'
Sentence    = 'Sentence'
Node        = 'AcabNode'


@dataclass
class ContextContainer(CtxInt.ContextContainer):

    # Operators could be a pair: (semantics, struct) to query
    # TODO operators is just the results of a prior query
    _operators     : CtxIns            = field(default=None)

    _active        : List[UUID]         = field(init=False, default_factory=list)
    _purgatory     : List[UUID]         = field(init=False, default_factory=list)
    _failed        : List[UUID]         = field(init=False, default_factory=list)
    _total         : Dict[UUID, CtxIns] = field(init=False, default_factory=dict)

    _negated       : bool              = field(init=False, default=False)
    _query_clause  : Sentence          = field(init=False, default=None)
    _root_node     : Node              = field(init=False, default=None)
    _collapse_vars : Set[str]          = field(init=False, default_factory=set)

    @staticmethod
    def build(ops:CtxIns=None):
        """ Create the empty context instance """
        return ContextContainer(_operators=ops)

    def __post_init__(self):
        initial = ContextInstance()
        self._total[initial.uuid] = initial
        self._active.append(initial.uuid)


    def __call__(self, root_node, query_sen, data, collapse_vars, is_negated=False):
        """ Prefaces __enter__, storing the relevant values """
        self._negated      = is_negated
        self._query_clause = query_sen
        self._root_node    = root_node
        self._collapse_vars.update(collapse_vars)

        return self

    def __enter__(self):
        # set all instances to start at node, unless start_word is an at_binding,
        # in which case get the bound node
        # handle negated behaviour
        root_word   = self._query_clause[0]
        active_list = self.active_list()
        if root_word.is_at_var:
            self._ctxs = [x.set_current_binding(root_word) for x in active_list]
        else:
            self._ctxs = [x.set_current_node(self._root_node) for x in active_list]

        return self


    def __exit__(self, exc_type, exc_value, traceback):
        # TODO Handle instances in _purgatory
        # collapse bindings as necessary
        self._collapse()
        self._negated       = False
        self._collapse_vars = set()
        self._root_node     = None

        # TODO handle exception

    def __len__(self):
        return len(self._active)

    def __getitem__(self, index):
        if isinstance(index, int):
            ctx_uuid = self._active[index]
        else:
            ctx_uuid = index
        return self._total[ctx_uuid]

    def __bool__(self):
        return bool(self._active)

    def fail(self, instance: CtxIns, word: Value, node: Node):
        """ Record a failure, the query sentence that failed,
        and the word that it failed on """
        # add failure details to the instance, of word and query clause

        # add to _purgatory
        instance._failure_word = word
        self._purgatory.append(instance.uuid)

    def test(self, ctx: CtxIns, possible: List[Node], word: Value):
        """
        run a word's tests on available nodes, with an instance
        """
        constraints : Constraints = ConstraintCollection.build(word, self._operators)
        assert(len(possible) == 1 or constraints._bind)
        successes = []

        for node in possible:
            try:
                constraints.test_all(node, ctx)
                successes.append(node)
            except ASErr.AcabSemanticTestFailure as err:
                self.fail(ctx, word, node)

        # Handle successes
        # success, so copy and extend ctx instance
        bound_ctxs = ctx.bind(word, successes)
        self.push(bound_ctxs)
        return bound_ctxs

    def push(self, ctxs):
        if not isinstance(ctxs, list):
            ctxs = [ctxs]

        # Add to contextcontainer
        assert(not any([x.uuid in self._total for x in ctxs]))
        self._total.update({x.uuid: x for x in ctxs})
        self._active += [x.uuid for x in ctxs]



    def pop(self, top=False) -> CtxIns:
        """ Get a copy of the active contexts,
        so ctxs can be modified as semantics go
        """
        if top:
            return self._total[self._active.pop()]
        else:
            return self._total[self._active.pop(0)]


    def active_list(self, clear=False):
        the_list = [self._total[x] for x in self._active]
        if clear:
            self._active = []

        return the_list

    def _collapse(self):
        """
        Context collapse on specific vars.
        Flattens many contexts into one, with specified variables
        now as lists accumulated from across the contexts.

        Semantics of collapse:
        1[ctx]n -> 1[c:ctx]1
        where
        c = a_ctx = { on_var: [x[on_var] for x in _ctxs] }
        """
        if not bool(self._collapse_vars):
            return

        # select instances with bindings
        # Merge into single new instance
        # replace
        raise NotImplementedError()


    def merge(self, ctxCon):
        raise NotImplementedError()
@dataclass
class ContextInstance(CtxInt.ContextInstance):

    data         : Dict[Any, Any]  = field(default_factory=dict)
    nodes        : Dict[Any, Node] = field(default_factory=dict)
    uuid         : UUID            = field(default_factory=uuid1)

    _remaining_query  : List[Value]       = field(init=False, default=None)
    _parent_ctx       : CtxIns            = field(init=False, default=None)
    _continuation     : Statement         = field(init=False, default=None)
    _current          : Node              = field(init=False, default=None)
    _failure_sentence : Sentence          = field(init=False, default=None)
    _failure_word     : Value             = field(init=False, default=None)

    def __hash__(self):
        return hash(self.uuid)

    def __contains__(self, value: Value):
        return str(value) in self.data
    def __getitem__(self, value: Value):
        if value in self:
            return self.data[str(value)]
        else:
            return value

    def copy(self):
        copied = replace(self,
                         uuid=uuid1(),
                         data=self.data.copy(),
                         nodes=self.nodes.copy(),
                         )
        copied._parent_ctx = self.uuid

        assert(self.uuid != copied.uuid)
        assert(id(self.data) != id(copied.data))
        return copied

    def bind(self, word, nodes) -> [CtxIns]:
        extensions = [(self.copy(), x) for x in nodes]
        word_str = str(word)
        for ctxInst, node in extensions:
            ctxInst.set_current_node(node)
            if word.is_var:
                ctxInst.data[word_str]  = node.value
                ctxInst.nodes[word_str] = node

        return [x[0] for x in extensions]

    def bind_dict(self, the_dict):
        copied = self.copy()
        copied.data.update(the_dict)
        return copied


    def set_current_node(self, node):
        self._current = node
        return self

    def set_current_binding(self, word):
        if word.name not in self.nodes:
            raise ASErr.AcabSemanticException("No Recognised binding", (word, self.nodes))

        self.set_current_node(self.nodes[word.name])
        return self

    def merge_with(self, var, instance_list):
        # TODO
        merged_instance = ContextInstance()


        return merged_instance

    def set_continuation(self, instruction):
        self._continuation = instruction

    def continuation(self):
        # Run the continuation (eg: run the transform and action of a rule)
        pass



@dataclass
class MutableContextInstance():
    """ Wrap A Context Instance with an smart dictionary.
    Changes are inserted into the dictionary, until finish is called.
    Finish creates a new CtxIns, integrating changes """

    base         : CtxIns          = field()
    data         : Dict[Any, Any]  = field(default_factory=dict)
    uuid         : UUID            = field(default_factory=uuid1)

    @staticmethod
    def build(ctx: ContextInstance):
        return MutableContextInstance(base=ctx)

    def finish(self):
        return self.base.bind_dict(self.data)

    def __contains__(self, value: Value):
        key = str(value)
        return key in self.data or key in self.base

    def __getitem__(self, value: Value):
        key = str(value)
        if key in self.data:
            return self.data[key]
        elif key in self.base:
            return self.base[key]
        else:
            return value

    def __setitem__(self, key: Value, value: Value):
        self.data[str(key)] = value
