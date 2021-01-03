#!/usr/bin/env python3
import acab.abstract.interfaces.semantic_interfaces as SI

class WaveformMixin(Generic[T], SI.SemanticMixin):
    """ TODO Formerly Contexts
    Records variable Waveforms as they collapse to a result
    """

    bind_groups       : List[Dict[str, Any]] = field(init=False, default_factory=list)
    nodes             : List[Node]           = field(init=False, default_factory=list)
    failures          : List[Dict[str, Any]] = field(init=False, default_factory=list)
    queued_failures   : List[Dict[str, Any]] = field(init=False, default_factory=list)
    query_fail_clause : Optional[Sentence]   = field(init=False, default=None)

    @abc.abstractmethod
    def append(self) -> Any:
        pass

    @abc.abstractmethod
    def fail(self) -> Any:
        pass

    @abc.abstractmethod
    def clear(self) -> Any:
        pass

    @abc.abstractmethod
    def collapse(self) -> Any:
        pass

    @abc.abstractmethod
    def group_by_type(self) -> Any:
        pass

    @abc.abstractmethod
    def promote_failures(self) -> Any:
        pass

    @abc.abstractmethod
    def demote_failures(self) -> Any:
        pass

    @abc.abstractmethod
    def force_node_position(self) -> Any:
        pass

    @abc.abstractmethod
    def rebind_across_contexts(self) -> Any:
        pass
