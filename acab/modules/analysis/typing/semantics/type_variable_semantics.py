#!/usr/bin/env python3
import logging as root_logger

# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode
from acab.abstract.data.node_semantics import AcabNodeSemantics

from acab.modules.analysis.typing import type_exceptions as te

import logging as root_logger

logging = root_logger.getLogger(__name__)

class TypingVarSemantics(AcabNodeSemantics):

    def __init__(self):
        pass
    def accessible(self, word : AcabNode, term : AcabValue) -> [AcabNode]:
        """
        Retrieve a list of all nodes accessible from this node,
        according to a constraint term
        """
        raise NotImplementedError()

    def equal(self, word : AcabNode, word2 : AcabNode) -> bool:
        raise NotImplementedError()

    def lift(self, word : AcabValue) -> AcabNode:
        """ Lifting a value to a data node """
        # could include vocabulary tracking a la spacy
        raise NotImplementedError()


    def add(self, node : AcabNode, to_add : AcabValue, node_constructor : Callable) -> AcabNode:
        raise NotImplementedError()

    def get(self, node : AcabNode, query_term : AcabValue) -> Optional[AcabNode]:
        """ Getting a node from the data structure """
        raise NotImplementedError()

    def contain(self, node : AcabNode, query_term : AcabValue) -> bool:
        """ Getting Node inclusion in a set """
        raise NotImplementedError()

    def delete(self, node : AcabNode, to_delete : AcabValue) -> Optional[AcabNode]:
        """ Removing a node from the data structure """
        raise NotImplementedError()

    
class VarTypeNode(AcabNode):
    """ Node describing a variable's type """

    def __init__(self, value, _type=None):
        assert(_type is None or isinstance(_type, Sentence.build))
        super().__init__(value)
        # TODO or type_bottom
        self._type_instance = _type
        self._var_node = None
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
            raise te.TypeConflictException(_type.pprint(),
                                           self.type_instance.pprint(),
                                           self.name)

        if self._var_node is not None:
            self._var_node.apply_type_instance(self.type_instance)

        return self



# TODO convert to node semantics



    def add_node(self, node):
        assert(isinstance(node, AcabNode))
        self._nodes.add(node)
        if node.is_var:
            self._var_names.add(node.name)
        # # TODO: make this a weak ref?
        # node._var_node = self

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




