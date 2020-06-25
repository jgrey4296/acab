import logging as root_logger

from acab.abstract.value import AcabValue
from acab.abstract.node import AcabNode
from acab.modules.analysis.typing.util import TYPE_DEC_S
import acab.error.type_exceptions as te

from acab.modules.analysis.typing.values.type_instance import TypeInstance
logging = root_logger.getLogger(__name__)


class MonoTypedNode(AcabNode):
    """ Base Node for a Type Trie """

    def __init__(self, value, _type=None):
        assert(_type is None or isisntance(_type, TypeInstance))
        super().__init__(value)
        self._type_instance = _type
        self._var_node = None

    @property
    def is_var(self):
        return self.value.is_var
    @property
    def type_instance(self):
        return self._type_instance


    def apply_type_instance(self, _type):
        if self._type_instance is None:
            assert(_type is None or isinstance(_type, TypeInstance)), breakpoint()
            self._type_instance = _type

    def unify_types(self, _type):
        assert(_type is None or isinstance(_type, TypeInstance))

        if self.type_instance == _type:
            return None

        # TODO: unify type instance and type vars separately?

        # TODO: use <
        if (self.type_instance is not None and _type is not None) and self._type_instance != _type:
            raise te.TypeConflictException(self, _type, self.name)

        self.apply_type_instance(_type)

        if self._var_node is not None:
            self._var_node.apply_type_instance(self.type_instance)

        return self
