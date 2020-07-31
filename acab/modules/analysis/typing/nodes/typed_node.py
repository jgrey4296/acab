import logging as root_logger

from acab.abstract.value import AcabValue
from acab.abstract.node import AcabNode
from acab.abstract.type_base import TypeInstance, ATOM
from acab.modules.analysis.typing import type_exceptions as te

logging = root_logger.getLogger(__name__)


class MonoTypedNode(AcabNode):
    """ Base Node for a Type Trie """

    def __init__(self, value, _type=None):
        assert(_type is None or isisntance(_type, TypeInstance))
        super().__init__(value)
        self._type_instance = _type or ATOM
        self._var_node = None

    @property
    def is_var(self):
        return self.value.is_var
    @property
    def type_instance(self):
        return self._type_instance


    def apply_type_instance(self, _type):
        self._type_instance = _type

    def unify_types(self, _type, lookup=None):
        assert(_type is None or isinstance(_type, TypeInstance))

        if self.type_instance == _type:
            return None

        # TODO: unify type instance and type vars separately?

        if self._type_instance < _type:
            self.apply_type_instance(_type)
        elif not _type < self._type_instance:
            raise te.TypeConflictException(_type.pprint(),
                                           self.type_instance.pprint(),
                                           self.name)

        if self._var_node is not None:
            self._var_node.apply_type_instance(self.type_instance)

        return self
