import logging as root_logger

from acab.abstract.node import AcabNode
from acab.abstract.trie.trie import Trie
from acab.abstract.type_base import TypeInstance
from acab.modules.analysis.typing import type_exceptions as te
from acab.modules.analysis.typing.values.type_definition import TypeDefinition

from .var_type_node import VarTypeTrieNode


import acab.modules.analysis.typing.util as util

from .type_assignment_node import TypeAssignmentTrieNode

logging = root_logger.getLogger(__name__)

# Log messages to use, because they are long:
LOG_MESSAGES = {}
LOG_MESSAGES['validate_top'] = "Validating: {} on {}"
LOG_MESSAGES['curr_def'] = "Current Definition to Validate: {} : {}"
LOG_MESSAGES['curr_use_set'] = "Current Usage Set: {}"
LOG_MESSAGES['no_children'] = "Val: No Children, assigning type: {} to {}"
LOG_MESSAGES['match_type_usage'] = "Matching Type {} onto usage set"
LOG_MESSAGES['mult_child'] = "Current Def has multiple children, checking for conflicts in structure"

class TypeDefTrieNode(AcabNode):
    """ A Node describing a type definition """

    def __init__(self, value):
        logging.debug("TypeDefTrieNode: init: {}".format(value))
        super().__init__(value)
        self._typedef_trie = None
        self._definition = None

    @property
    def definition(self):
        return self._definition

    @property
    def trie(self):
        return self._typedef_trie


    def set_data(self, data):
        """ Overrides AcabNode.set_data.
        Builds the subtrie of a type definition at the end of being added
        to the definition trie.
        """
        assert(isinstance(data, TypeDefinition))
        if self.definition is not None and self.definition != data:
            raise te.TypeRedefinitionException(self.definition)

        logging.debug("TypeDef.set_data: {}".format(data))
        if self.definition is None:
            self._definition = data
            # construct the internal trie
            self._typedef_trie = Trie(node_type=TypeAssignmentTrieNode)
            self.trie.root._type_instance = self.definition.build_type_instance()

            for x in data.structure:
                self.trie.add(x, None,
                              update=lambda c, n, p, d: c.update(n))

    def validate(self, usage_trie, create_var):
        """ Given an instance trie node, type check / infer it
        against self's description of a type.
        returning a list of nodes that have been inferred
        """
        assert(callable(create_var))
        assert(isinstance(usage_trie, TypeAssignmentTrieNode)), breakpoint()
        logging.debug(LOG_MESSAGES['validate_top'].format(repr(self),
                                                          repr(usage_trie)))
        if self.trie is None:
            raise te.TypeUndefinedException(self.value.name, usage_trie)

        type_var_lookup = self._generate_polytype_bindings(usage_trie, create_var)
        # Loop over all elements of the defined type
        newly_typed = []
        # The queue holds tuples of the definition node, and its corresponding declaration nodes
        queue = [(self.trie.root, [usage_trie])]
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

        # apply type_var_lookup back onto the usage_trie if necessary
        self._update_polytype_bindings(usage_trie, type_var_lookup)

        return newly_typed

    def _update_polytype_bindings(self, usage_trie, lookup_dict):
        assert(usage_trie.type_instance is not None)
        assert(isinstance(lookup_dict, dict))
        definition_bindings = [lookup_dict[x.name] for x in self.definition.vars]
        definition_instances = [x if isinstance(x, TypeInstance) else x.type_instance for x in definition_bindings]

        usage_trie.type_instance._params = definition_instances

    def _generate_polytype_bindings(self, usage_trie, create_var):
        """ Generate a temporary binding environment for the definition's
        type parameters """
        type_var_lookup = {}
        if self.definition.vars \
           and usage_trie.type_instance \
           and usage_trie.type_instance.vars:
            zipped = zip(self.definition.vars, usage_trie.type_instance.vars)
            type_var_lookup = {x.name: y for x, y in zipped}

        # TODO if type param is an acabvalue var.. coerce to a type var
        # infer type vars in polytype if missing
        for x in self.definition.vars:
            if x in type_var_lookup:
                continue
            else:
                # create a new type var
                type_var_lookup[x.name] = create_var(x.name)

        return type_var_lookup

    def _check_local_type_structure(self, curr_def, curr_usage_set, lookup):
        """ Compare Defined Structure to actual structure """
        if curr_def.value == util.ROOT_S:
            return []

        if curr_def.is_var and curr_def.name in lookup:
            curr_def = lookup[curr_def.name]

        # TODO: unify curr_def type instance vars with those in lookup
        _type = None
        if isinstance(curr_def, TypeInstance):
            _type = curr_def
        else:
            _type = curr_def.type_instance

        if _type is not None:
            _type = _type.build_type_instance(lookup)

        if isinstance(curr_def, VarTypeTrieNode):
            dummy = [curr_def.add_node(x) for x in curr_usage_set]


        type_attempts = [x.unify_types(_type) for x in curr_usage_set]
        return [x for x in type_attempts if x is not None]

    def _handle_single_child(self, curr_def, curr_usage_set):
        # Special case of handle_multiple_children
        logging.debug("Curr Def has a single child")
        queue_vals = []
        conflicts = []
        child = list(curr_def.children)[0]

        new_usage_set = [y for x in curr_usage_set for y in x.children]

        if not child.is_var:
            conflicts = [x for x in new_usage_set if not x.is_var and child.name != x.name]

        if bool(conflicts):
            raise te.TypeStructureMismatch(self.definition.path.pprint(),
                                           conflicts)

        if bool(new_usage_set):
            queue_vals.append((child, new_usage_set))

        return queue_vals

    def _handle_multiple_children(self, curr_def, curr_usage_set):
        logging.debug(LOG_MESSAGES['mult_child'])
        assert(len(curr_def.children) > 1)
        # With multiple children, match keys
        queue_vals = []
        defset = {x for x in curr_def._children.keys()}
        usageset = {y for x in curr_usage_set for y, n
                    in x._children.items() if not n.is_var}
        conflicts = usageset.difference(defset)
        if bool(conflicts):
            raise te.TypeStructureMismatch(self.definition.path.pprint(),
                                           conflicts)

        logging.debug("No Conflicts found, checking children")
        for key in usageset:
            new_usage_set = [x._children[key] for x in curr_usage_set if key in x._children]
            new_child = curr_def._children[key]
            if bool(new_usage_set):
                queue_vals.append((new_child, new_usage_set))

        return queue_vals


    def _log_status(self, curr_def, curr_usage_set):
        curr_def_type = curr_def._type_instance
        curr_use_str = ", ".join([str(x) for x in curr_usage_set])
        logging.debug(LOG_MESSAGES['curr_def'].format(curr_def.value,
                                                      repr(curr_def_type)))
        logging.debug(LOG_MESSAGES['curr_use_set'].format(curr_use_str))

    def _log_no_children(self, curr_def, curr_def_type, curr_usage_set):
        c_u_s_str = ", ".join([str(x) for x in curr_usage_set])
        logging.debug(LOG_MESSAGES['no_children'].format(curr_def_type,
                                                         c_u_s_str))

    def _log_match_type_usage(self, curr_def_type):
        log_msg = LOG_MESSAGES['match_type_usage'].format(curr_def_type)
        logging.debug(log_msg)
