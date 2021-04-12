#!/usr/bin/env python3
# Main System

@dataclass
class SemanticSystem(Generic[T], metaclass=abc.ABCMeta):
    """ A Complete semantic system """

    # Downward guarantees of what semantics may contextually rely upon
    guarantees        : Set[Handler]               = field(default_factory=list)
    # Downward expectations of what semantics must publicly provide
    expectations      : Set[SemanticUnion]         = field(init=False, default_factory=list)
    # Map a value to a semantics
    mapping           : Dict[T, SemanticUnion]     = field(default_factory=dict)
    search_order      : SemSearch                  = field(default_factory=list)
    # Handler registration
    handlers          : Dict[str, Handler] = field(default_factory=dict)

    # The collected interface of all public facing semantics
    # used for getattr?
    _interface_union  : Set[str]                   = field(init=False, default_factory=set)
    # The collected interface of all contextual semantics
    # used for a ctx_getattr
    _contextual_union : Set[str]                   = field(init=False, default_factory=set)

    _context_stack : List[Dict[Any,Any]] = field(init=False, default_factory=list)

    def __post_init__(self):
        # assert a DEFAULT is in mapping

        # set a default search order if empty

        # interface union and contextual union

        # verify all mappings handle required interface
        for semantics in self.mapping.values():
            # TODO and check guarantees satisfy semantics' expectations
            # assert(not difference(semantics.expectations, self.guarantees))
            assert(all([isinstance(semantics, x) for x in self.expectations]))

    def __call__(self, instruction) -> Any:
        # push to context stack
        # Run entry hooks, add result to context stack
        # try:
        ## Get semantic instance from mapping
        ## perform instruction
        # run exit hooks
        # pop context stack
        pass

    def retrieve(self, T) -> SemanticUnion:
        """ Get a mapped semantics, using the search order """
        chosen: SemanticUnion = self.bottom_semantic
        descendents_to_update = []
        # search_order: List[Callable[[AcabPrintSemantics, Printable], Optional[SemanticSpec]]] = []
        search_order = self._search_order[:]

        for x in search_order:
            search_f = x
            if x in self._search_lookup:
                search_f = self._search_lookup[x]

            assert(callable(search_f))
            result = search_f(self, current_val)
            if result is not None:
                chosen = result
                break

        # TODO update _type_semantics chain with found bindings from hierarchy
        #
        if len(descendents_to_update) > 1:
            self.mapping.update({x : retrieved for x in descendents_to_update})

        return chosen

    def initialize(self):
        """ Setup any guarantees of the system """
        # TODO specify when to setup these
        pass

    def run_handler(self, h_type: Sentence, *args, **kwargs):
        """ Call a registered handler """
        pass
