#!/usr/bin/env python3

import abc

class ContextInterface(metaclass=abc.ABCMetaClass):
    """  """

    @abc.abstractmethod
    def append(self) -> :
        pass

    @abc.abstractmethod
    def fail(self) -> :
        pass

    @abc.abstractmethod
    def clear(self) -> :
        pass

    @abc.abstractmethod
    def collapse(self) -> :
        pass

    @abc.abstractmethod
    def group_by_type(self) -> :
        pass

    @abc.abstractmethod
    def promote_failures(self) -> :
        pass

    @abc.abstractmethod
    def demote_failures(self) -> :
        pass

    @abc.abstractmethod
    def force_node_position(self) -> :
        pass

    @abc.abstractmethod
    def rebind_across_contexts(self) -> :
        pass
