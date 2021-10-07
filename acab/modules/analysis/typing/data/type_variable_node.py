#!/usr/bin/env python3
"""
Node for tracking uses of:
$var :: type

"""
import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Iterable, Iterator, List,
                    Mapping, Match, MutableMapping, Optional, Sequence, Set,
                    Tuple, TypeVar, Union, cast)

from acab.core.data.node import AcabNode
from acab.core.data.values import AcabValue, Sentence
from acab.interfaces import semantic as SI
from acab.modules.analysis.typing import type_exceptions as te
from acab.modules.semantics.basic_node_semantics import BasicNodeSemantics

logging = root_logger.getLogger(__name__)


class VarTypeNode(AcabNode):
    """ Node describing a variable's type """

    def __init__(self, value, _type=None):
        assert(_type is None or isinstance(_type, Sentence))
        super().__init__(value)
        # TODO or type_bottom
        self._type_instance = _type
        self._nodes = set([])
        self._var_names = set([])

    @property
    def is_var(self):
        return self.value.is_var
    @property
    def type_instance(self):
        return self._type_instance


    def apply_type_instance(self, _type):
        self._type_instance = _type

    def unify_types(self, _type, lookup=None):
        assert(_type is None or isinstance(_type, Sentence.build))

        if self.type_instance == _type:
            return None

        # TODO: unify type instance and type vars separately?

        if self._type_instance < _type:
            self.apply_type_instance(_type)
        elif not _type < self._type_instance:
            raise te.TypeConflictException(str(_type),
                                           str(self.type_instance),
                                           self.name)


        return self



    # TODO convert to node semantics

    def add_node(self, node):
        assert(isinstance(node, AcabNode))
        self._nodes.add(node)
        if node.is_var:
            self._var_names.add(node.name)

    def propagate(self):
        if self.type_instance is not None:
            for n in self._nodes:
                n.unify_types(self.type_instance)

    def merge(self, nodes):
        assert(all([isinstance(x, VarTypeNode) for x in nodes]))
        logging.debug("Merging Variables: {} into {}".format(", ".join([str(x.name) for x in nodes]),
                                                             self.value))
        # update self to point to all assignment nodes
        var_nodes = [y for x in nodes for y in x._nodes]
        for n in var_nodes:
            self.add_node(n)

    def clear_assignments(self):
        for node in self._nodes:
            node.clear_var_node()
        self._nodes = set([])
        self._type_instance = None
