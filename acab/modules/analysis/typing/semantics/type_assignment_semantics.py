#!/usr/bin/env python3
import logging as root_logger
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar


from acab.abstract.data.node import AcabNode
from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node_semantics import AcabNodeSemantics
from acab.modules.analysis.typing import type_exceptions as te
from acab.modules.analysis.typing import util
from acab.modules.semantics.basic_semantics import BasicNodeSemantics

logging = root_logger.getLogger(__name__)



class TypingAssignmentSemantics(BasicNodeSemantics):

    def lift(self, word : AcabValue, constructor : Callable) -> AcabNode:
        """ The Most Basic Lift """
        assert(isinstance(word, AcabValue))
        # constructor will default to type bottom if word.type is none
        _type = word.type
        return constructor(word, _type=_type)




class TypeAssignmentNode(AcabNode):
    """ A Node in the Type Assignment Struct.
    Used in type inference.
    Enables linking with variable type struct"""

    def __init__(self, value, _type=None, var_node=None):
        assert(_type is None or isinstance(_type, Sentence.build))
        assert(var_node is None or isinstance(var_node, AcabNode))
        super().__init__(value)
        self._type_instance = _type or ATOM
        self._var_node = var_node


    def _default_setup(self, path : [AcabNode], data : Dict[Any,Any], context : Dict[Any,Any]):
        """ Link the assignment with a context variable if necessary"""
        if self.is_var:
            # add the var to the var struct
            var_struct = context["var_struct"]
            result = var_struct.add(Sentence.build([self.value]))

        if bool(result) and bool(result[0]):
            reference_var_node = result[0][0]
            reference_var_node.add_node(self)
            self._var_node = reference_var_node


    @property
    def is_var(self):
        return self.value.is_var

    @property
    def type_instance(self):
        return self._type_instance

    def apply_type_instance(self, _type):
        self._type_instance = _type

    def unify_types(self, _type, lookup: Dict[Any, Any]=None):
        assert(_type is None or isinstance(_type, Sentence.build))

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

    def _default_setup(self, path : [AcabNode], data : Dict[Any,Any], context : Dict[Any,Any]):
        # TODO
        """ Post-addition update method.
        links self to a lookup-struct word if self is a variable """
        word = self.value

        var_struct = context['var_struct']

        # apply type if necessary
        self.unify_types(word.type)


        if self.is_var and self._var_node is None and var_struct is not None:
            # if var, connect to var type struct
            result = var_struct.add([self._value])
            self._var_node = result[0][0]
            self._var_node.add_node(self)

    def clear_var_node(self):
        self._var_node = None
