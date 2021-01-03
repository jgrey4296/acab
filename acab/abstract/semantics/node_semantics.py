#!/usr/bin/env python3
import acab.abstract.interfaces.semantic_interfaces as SI

class NodeSemantics(Generic[T], SI.SemanticMixin):
    """ The Core Node Semantics """

    @abc.abstractmethod
    def accessible(self, node: T,
                   data: Dict[Any, Any],
                   term: Value) -> List[T]:
        """
        Retrieve a list of all nodes accessible from this node,
        according to a constraint term
        """
        pass

    @abc.abstractmethod
    def equal(self, word: T, word2: T) -> bool:
        pass

    @abc.abstractmethod
    def add(self, node: T, word: List[Value]) -> Tuple[bool, T]:
        pass

    @abc.abstractmethod
    def get(self, node: T, query_term: Value) -> Optional[T]:
        """ Getting a node from the data structure """
        pass

    @abc.abstractmethod
    def contain(self, node: T, query_term: Value) -> bool:
        """ Getting Node inclusion in a set """
        pass

    @abc.abstractmethod
    def delete(self, node: T, to_delete: Value) -> Optional[T]:
        """ Removing a node from the data structure """
        pass


    @abc.abstractmethod
    def make(self, val: T):
        pass
