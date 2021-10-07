import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

from dataclasses import FrozenInstanceError, InitVar, dataclass, field, replace
from enum import Enum
from uuid import UUID, uuid1

import acab.interfaces.context as CtxInt
import acab.error.semantic_exception as ASErr
from acab.core.config import GET
from acab.core.data.production_abstractions import (ProductionComponent,
                                                        ProductionContainer)
from acab.interfaces.value import Sentence_i
from acab.error.semantic_exception import AcabSemanticException
from acab.modules.context.constraints import ConstraintCollection

config = GET()

CONSTRAINT_S     = config.prepare("Value.Structure", "CONSTRAINT")()
NEGATION_S       = config.prepare("Value.Structure", "NEGATION")()

CtxIns           = CtxInt.ContextInstance_i
CtxSet           = CtxInt.ContextSet_i
Constraints      = 'ConstraintCollection'
ProdComp         = ProductionComponent
ProdCon          = ProductionContainer
Operator         = 'ProductionOperator'
Value            = 'AcabValue'
Statement        = 'AcabStatement'
Sen              = Sentence_i
Node             = 'AcabNode'
ModuleComponents = "ModuleComponents"
NamedCtxSet      = "NamedCtxSet"

DELAYED_E = Enum("Delayed Instruction Set", "ACTIVE FAIL DEACTIVATE CLEAR MERGE")

@dataclass(frozen=True)
class ContextFailState:
    """
    Utility dataclass for holding a ctx with information about where it failed
    """
    ctx       : CtxIns         = field()
    query     : Sen            = field()
    failed_on : Value          = field()
    node      : Optional[Node] = field()


@dataclass(frozen=True)
class ContextInstance(CtxInt.ContextInstance_i):

    data              : Dict[str, Any]  = field(default_factory=dict)
    nodes             : Dict[str, Node] = field(default_factory=dict)
    uuid              : UUID            = field(default_factory=uuid1)
    _parent_ctx       : CtxIns          = field(default=None)
    exact             : bool            = field(default=False)

    _current          : Node            = field(init=False, default=None)
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
        if isinstance(value, Sentence_i) and value.is_var and value[0] in self:
            return self.data[str(value[0])]

        return str(value) in self.data

    def __getitem__(self, value: Value):
        if self.exact and value not in self:
            raise AcabSemanticException("Not Found in Context", value)

        if isinstance(value, Sentence_i) and value.is_var and value[0] in self:
            return self.data[str(value[0])]
        elif str(value) in self:
            return self.data[str(value)]
        else:
            return value

    # def __setitem(self, key: Any, value: Any):
    #     raise ASErr.AcabSemanticException("Context Instances can't directly set a value, use MutableContextInstance")

    def __getattribute__(self, value):
        try:
            return object.__getattribute__(self, value)
        except AttributeError as err:
            if not value in self:
                raise err
            return self.__getitem__(value)

    def __bool__(self):
        return bool(self.data)
    def __len__(self):
        return len(self.data)
    def __iter__(self):
        return iter(self.data.values())

    def __repr__(self):
        binds  = ", ".join([x for x in self.data.keys()])
        return f"(CtxInst: Bindings: {binds}.)"

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

    def bind(self, word, nodes, sub_binds=None) -> [CtxIns]:
        if sub_binds is None:
            sub_binds = []
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

            matches = []
            for key, bind in sub_binds:
                if key in node.value.data:
                    val = node.value.data
                elif hasattr(node.value, key):
                    val = getattr(node.value, key)
                else:
                    raise ASErr.AcabSemanticException("Bad SubBind", key, bind)

                matches += bind.match(val)

            for x,y in matches:
                if x not in ctxInst.data:
                    ctxInst.data[str(x)] = y

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

    # For nesting ctxsets
    _parent              : Optional[CtxSet]       = field(default=None)

    _total               : Dict[UUID, CtxIns]     = field(default_factory=dict)
    _active              : List[UUID]             = field(default_factory=list)

    _failed              : List[ContextFailState] = field(init=False, default_factory=list)
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
        # Add any sugar forms:
        op_dict.update({x[-1]._acab_operator_sugar : x[-1] for x in operators if hasattr(x[-1], "_acab_operator_sugar")})

        # TODO abstract building ctxinst's to the set
        instance = ContextInstance(op_dict, exact=True)
        # TODO add sugar names from config
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
        elif isinstance(index, (Sentence_i, ProductionContainer)) and index in self._named_sets:
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

    def fail(self, instance: CtxIns, word: Value, node: Node, query:Sen):
        """ Record a failure, the query sentence that failed,
        and the word that it failed on """
        # add failure details to the instance, of word and query clause
        logging.debug(f"{repr(self)}: Failing: {node}")

        fail_state = ContextFailState(instance,
                                      query,
                                      word,
                                      node)
        self._failed.append(fail_state)

    def push(self, ctxs:Union[CtxIns, List[CtxIns], UUID, List[UUID]]):
        if not isinstance(ctxs, list):
            ctxs = [ctxs]

        if all([isinstance(x, UUID) for x in ctxs]):
            assert(all([x in self._total for x in ctxs]))
            self._active += ctxs

        else:
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
        return self._failed


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



