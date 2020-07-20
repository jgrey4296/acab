import logging as root_logger

from acab.abstract.node import AcabNode
from acab.abstract.trie.trie import Trie
from acab.abstract.type_base import TypeInstance

from acab.modules.analysis.typing import type_exceptions as te
from acab.modules.analysis.typing.values.type_definition import TypeDefinition, SumTypeDefinition

from .typedef_node import TypeDefTrieNode
from .var_type_node import VarTypeTrieNode


import acab.modules.analysis.typing.util as util

from .type_assignment_node import TypeAssignmentTrieNode

logging = root_logger.getLogger(__name__)

class SumTypeDefTrieNode(TypeDefTrieNode):
    """ A Specialised TypeDefTrieNode to validate Sum Types  """

    def __init__(self, value):
        super(SumTypeDefTrieNode, self).__init__(value)

    def validate(self, usage_trie, create_var):
        assert(callable(create_var))
        assert(isinstance(usage_trie, TypeAssignmentTrieNode)), breakpoint()

        if self.trie is None:
            raise te.TypeUndefinedException(self.value.name, usage_trie)

        type_var_lookup = self._generate_polytype_bindings(usage_trie, create_var)
        newly_typed = []

        # usage_trie must be a child of the curr_def
        if usage_trie.name not in self.trie.root:
            raise te.TypeStructureMismatch(self.definition.path.pprint(),
                                           [usage_trie.name])

        # Otherwise match against that specific type
        the_type = self.trie.root.get_child(usage_trie.name)
        queue = [(the_type, [usage_trie])]

        while bool(queue):
            curr_def, curr_usage_set = queue.pop(0)
            self._log_status(curr_def, curr_usage_set)
            newly_typed += self._check_local_type_structure(curr_def, curr_usage_set, type_var_lookup)

            # Handle Children:
            if len(curr_def._children) == 1:
                queue += self._handle_single_child(curr_def,
                                                   curr_usage_set)
            elif len(curr_def._children) > 1:
                queue += self._handle_multiple_children(curr_def,
                                                        curr_usage_set)

            logging.debug("----------")

        self._update_polytype_bindings(usage_trie, type_var_lookup)

        return newly_typed
