import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

from dataclasses import InitVar, dataclass, field, replace, FrozenInstanceError
from enum import Enum
from uuid import UUID, uuid1

import acab.abstract.interfaces.context as CtxInt
import acab.error.acab_semantic_exception as ASErr
from acab.abstract.config import GET
from acab.abstract.core.production_abstractions import ProductionComponent, ProductionContainer
from acab.modules.semantics.constraints import ConstraintCollection
from acab.abstract.interfaces.value import Sentence_i

config = GET()
CONSTRAINT_S = config.prepare("Parse.Structure", "CONSTRAINT")()

CtxIns           = CtxInt.ContextInstance_i
CtxSet           = CtxInt.ContextSet_i
Constraints      = 'ConstraintCollection'
ProdComp         = ProductionComponent
ProdCon          = ProductionContainer
Operator         = 'ProductionOperator'
Value            = 'AcabValue'
Statement        = 'AcabStatement'
Sentence         = Sentence_i
Node             = 'AcabNode'
ModuleComponents = "ModuleComponents"
NamedCtxSet      = "NamedCtxSet"

DELAYED_E = Enum("Delayed Instruction Set", "ACTIVE FAIL DEACTIVATE CLEAR MERGE")

@dataclass(frozen=True)
class ContextInstance(CtxInt.ContextInstance_i):

    data              : Dict[str, Any]  = field(default_factory=dict)
    nodes             : Dict[str, Node] = field(default_factory=dict)
    uuid              : UUID            = field(default_factory=uuid1)
    _parent_ctx       : CtxIns          = field(default=None)

    # TODO These need custom setters
    _remaining_query  : List[Value]     = field(init=False, default=None)
    _current          : Node            = field(init=False, default=None)
    _failure_sentence : Sentence        = field(init=False, default=None)
    _failure_word     : Value           = field(init=False, default=None)
    _depth            : int             = field(init=False, default=0)
    _lineage          : set             = field(init=False, default_factory=set)

    def __post_init__(self):
        if self._parent_ctx is not None:
            object.__setattr__(self, "_depth", self._parent_ctx._depth + 1)
            lineage = set([self._parent_ctx.uuid, self.uuid]).union(self._parent_ctx._lineage)
            object.__setattr__(self, "_lineage", lineage)

    def __hash__(self):
        return hash(self.uuid)

    def __contains__(self, value: Value):
        return str(value) in self.data
    def __getitem__(self, value: Value):
        if str(value) in self:
            return self.data[str(value)]
        else:
            return value

    # def __setitem(self, key: Any, value: Any):
    #     raise ASErr.AcabSemanticException("Context Instances can't directly set a value, use MutableContextInstance")

    def __bool__(self):
        return bool(self.data)
    def __len__(self):
        return len(self.data)
    def __iter__(self):
        return iter(self.data.values())

    def __repr__(self):
        binds  = ", ".join([x for x in self.data.keys()])
        remain = len(self._remaining_query) if self._remaining_query is not None else 0
        return f"(CtxInst: Bindings: {binds}. QRemain: {remain})"

    def copy(self, **kwargs):
        logging.debug("Copied Ctx Instance")
        if 'data' not in kwargs:
            kwargs['data'] = self.data.copy()

        copied = replace(self,
                         uuid=uuid1(),
                         data=kwargs['data'],
                         nodes=self.nodes.copy(),
                         _parent_ctx=self,
                         )

        assert(self.uuid != copied.uuid)
        assert(id(self.data) != id(copied.data))
        return copied

    def bind(self, word, nodes) -> [CtxIns]:
        # Make len(nodes) new ctxins for the new bindings
        extensions = [(self.copy(), x) for x in nodes]
        # Get the binding name. ie: $x
        word_str = str(word)
        # Now bind
        for ctxInst, node in extensions:
            ctxInst.set_current_node(node)
            if word.is_var:
                ctxInst.data[word_str]  = node.value
                ctxInst.nodes[word_str] = node

        return [x[0] for x in extensions]

    def bind_dict(self, the_dict):
        data_copy = self.data.copy()
        data_copy.update(the_dict)
        return self.copy(data=data_copy)

    def set_current_node(self, node):
        object.__setattr__(self, "_current", node)
        return self

    def set_current_binding(self, word):
        if word.name not in self.nodes:
            raise ASErr.AcabSemanticException("No Recognised binding", (word, self.nodes))

        self.set_current_node(self.nodes[word.name])
        return self

    def merge_with(self, var, instance_list):
        # TODO
        raise NotImplementedError()
        merged_instance = ContextInstance()


        return merged_instance

    def to_sentences(self):
        raise NotImplementedError()


@dataclass
class ContextSet(CtxInt.ContextSet_i, CtxInt.DelayedCommands_i):

    # TODO make as a stack?
    # operators are just the results of a prior query
    _operators           : CtxIns                 = field(default=None)

    _parent              : Optional[CtxSet]       = field(default=None)

    _total               : Dict[UUID, CtxIns]     = field(default_factory=dict)
    _active              : List[UUID]             = field(default_factory=list)

    _failed              : List[UUID]             = field(init=False, default_factory=list)
    _named_sets          : Dict[Any, NamedCtxSet] = field(init=False, default_factory=dict)
    _uuid                : UUID                   = field(init=False, default_factory=uuid1)

    delayed_e            : Enum                   = field(init=False, default=DELAYED_E)
    instance_constructor : CtxIns                 = field(init=False, default=ContextInstance)

    @staticmethod
    def build(ops:Union[None, CtxIns, List[ModuleComponents]]=None):
        """ Create the empty context instance,
        constructing the operator bindings if necessary
        """
        if ops is None:
            return ContextSet()

        if isinstance(ops, CtxInt.ContextInstance_i):
            return ContextSet(_operators=ops)

        assert(isinstance(ops, list)), ops
        # Get Flat List of Operator Sentences:
        operators = [y for x in ops for y in x.operators]
        # Build the CtxInst data dict:
        op_dict = {str(x) : x[-1] for x in operators}
        # TODO abstract building ctxinst's to the set
        instance = ContextInstance(op_dict)
        # TODO add sugar names
        return ContextSet(_operators=instance)

    def subctx(self, selection:List[Union[CtxIns, UUID]]=None):
        """
        Build a subset of this ctxset.,
        either with reified instances, or their UUIDs
        """
        if selection is None:
            selection     = self._active
        elif all([isinstance(x, ContextInstance) for x in selection]):
            selection     = [x.uuid for x in selection]

        # active_lineage = {y for x in self.active_list() for y in x._lineage}
        selection = [x.uuid for x in self.active_list() if x._lineage.intersection(selection)]
        # selection     = [x for x in selection if x in active_lineage]
        obj_selection = {x : self._total[x] for x in selection}

        assert(all([isinstance(x, ContextInstance) for x in obj_selection.values()]))
        assert(all([isinstance(x, UUID) for x in selection]))
        subctx = ContextSet(_operators=self._operators,
                            _parent=self,
                            _total=obj_selection,
                            _active=selection)


        return subctx

    def __post_init__(self):
        logging.debug("ContextSet Created")
        if not bool(self._total):
            initial = ContextInstance()
            self._total[initial.uuid] = initial
            self._active.append(initial.uuid)


    def __hash__(self):
        return hash(self._uuid)
    def __len__(self):
        return len(self._active)

    def __getitem__(self, index, *indices) -> Union[CtxIns, CtxSet]:
        """ Access the active CtxSet, for:
        a specifc instance (by int index),
        a subselection (using a slice),
        a named/continuation set (using the instruction that named it)
        """
        result = None
        if isinstance(index, int):
            ctx_uuid = self._active[index]
            result   = self._total[ctx_uuid]
        elif isinstance(index, UUID):
            result = self._total[index]
        elif isinstance(index, slice):
            result = self._active[index]
        elif isinstance(index, list):
            result = [self._active[x] for x in index]
        elif isinstance(index, (Sentence, ProductionContainer)) and index in self._named_sets:
            result = self._named_sets[index].uuids
        else:
            raise Exception(f"Unrecognised arg to getitem: {index}")

        if isinstance(result, list):
            result = self.subctx(result)

        return result

    def __bool__(self):
        return bool(self._active)

    def __repr__(self):
        return f"(CtxSet: Active:{len(self._active)} Failed:{len(self._failed)} Total:{len(self._total)})"

    def fail(self, instance: CtxIns, word: Value, node: Node):
        """ Record a failure, the query sentence that failed,
        and the word that it failed on """
        # add failure details to the instance, of word and query clause
        logging.debug(f"{repr(self)}: Failing: {node}")
        object.__setattr__(instance, "_failure_word", word)
        # instance._failure_word = word
        self._failed.append(instance.uuid)

    def test(self, ctx: CtxIns, possible: List[Node], word: Value):
        """
        run a word's tests on available nodes, with an instance
        """
        logging.debug(f"{repr(self)}: Testing/Extending on {len(possible)} : {possible}")
        constraints : Constraints = ConstraintCollection.build(word, self._operators)
        assert(len(possible) == 1 or constraints._bind)
        successes = []

        for node in possible:
            try:
                constraints.test_all(node, ctx)
                successes.append(node)
            except ASErr.AcabSemanticTestFailure as err:
                logging.debug(f"Tests failed on {node.value}:\n\t{err}")
                self.fail(ctx, word, node)

        # Handle successes
        # success, so copy and extend ctx instance
        bound_ctxs = ctx.bind(word, successes)
        self.push(bound_ctxs)
        return bound_ctxs

    def push(self, ctxs:Union[CtxIns, List[CtxIns]]):
        if not isinstance(ctxs, list):
            ctxs = [ctxs]

        # Add to set
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

    def failed_list(self):
        return [self._total[x] for x in self._failed]



    def set_parent(self, parent:CtxSet):
        self._parent = parent



    def build_named_set(self, inst, uuids:List[UUID]):
        assert(inst not in self._named_sets)
        self._named_sets[inst] = NamedCtxSet(inst, uuids)
        return self._named_sets[inst]


@dataclass
class MutableContextInstance:
    """ Wrap A Context Instance with an smart dictionary.
    Changes are inserted into the dictionary, until finish is called.
    Finish creates a new CtxIns, integrating changes """

    parent       : CtxSet          = field()
    base         : CtxIns          = field()
    data         : Dict[Any, Any]  = field(default_factory=dict)
    uuid         : UUID            = field(default_factory=uuid1)

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

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback):
        self.parent.push(self.base.bind_dict(self.data))


@dataclass(frozen=True)
class NamedCtxSet:
    """ A CtxInst for packaging continuations """

    instruction : ProductionContainer = field()
    uuids       : List[UUID]          = field()
    # TODO instruction state


@dataclass(frozen=True)
class ContextQueryState:
    """ State of the current query """

    negated       : bool               = field()
    query_clause  : Sentence           = field()
    root_node     : Node               = field()
    collect_vars  : Set[str]           = field()
    ctxs          : CtxSet             = field()

    def __enter__(self):
        # set all instances to start at node, unless start_word is an at_binding,
        # in which case get the bound node
        # handle negated behaviour
        root_word : Value          = self.query_clause[0]
        active_list : List[CtxIns] = self.ctxs.active_list()
        if root_word.is_at_var:
            [x.set_current_binding(root_word) for x in active_list]
        else:
            [x.set_current_node(self.root_node) for x in active_list]

        return self


    def __exit__(self, exc_type, exc_value, traceback):
        # collect bindings as necessary
        self.collect()
        # self.negated      = False
        # self.collect_vars = set()
        # self.root_node    = None

        # TODO handle exception


    def collect(self):
        """
        Context collecton specific vars.
        Flattens many contexts into one, with specified variables
        now as lists accumulated from across the contexts.

        Semantics of collect:
        0[ctxs]0 -> fail
        1[ctxs]n -> 1[α]1
        where
        α : ctx = ctxs[0] ∪ { β : ctx[β] for ctx in ctxs[1:] }
        β : var to collect


        """
        if not bool(self.collect_vars):
            return

        # select instances with bindings
        # Merge into single new instance
        # replace
        raise NotImplementedError()
