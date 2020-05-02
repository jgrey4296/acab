import logging as root_logger

from py_rule.abstract.node import PyRuleNode
from py_rule.abstract.trie.trie import Trie
from py_rule.error import type_exceptions as te
from py_rule.modules.analysis.typing.type_definition import TypeDefinition
import py_rule.modules.analysis.typing.util as util

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


class TypeDefTrieNode(PyRuleNode):
    """ A Node describing a type definition """

    def __init__(self, value):
        logging.debug("TypeDefTrieNode: init: {}".format(value))
        assert(isinstance(value,PyRuleNode))
        super().__init__(value)
        self._typedef_trie = None


    def set_data(self, data):
        """ OverridesPyRuleNode.set_data.
        Builds the subtrie of a type definition at the end of being added
        to the definition trie.
        """
        assert(isinstance(data, TypeDefinition))
        logging.debug("TypeDef.set_data: {}".format(data))
        if util.TYPE_DEF_S not in self._data:
            self._data[util.TYPE_DEF_S] = data
            # construct the internal trie
            self._typedef_trie = Trie(node_type=TypeAssignmentTrieNode)
            self._typedef_trie._root._type_instance = self._data[util.TYPE_DEF_S].build_type_declaration()
            for x in self._data[util.TYPE_DEF_S].structure:
                self._typedef_trie.add(x, None,
                                       update=lambda c, n, p, d: c.type_match_wrapper(n))

        if self._data[util.TYPE_DEF_S] != data:
            raise te.TypeRedefinitionException(self._data[util.TYPE_DEF_S])

    def validate(self, usage_trie):
        """ Given a declaration trie node, type check / infer it
        against self's description of a type.
        returning a list of nodes that have been inferred
        """
        assert(isinstance(usage_trie, TypeAssignmentTrieNode))
        logging.debug(LOG_MESSAGES['validate_top'].format(repr(self),
                                                          repr(usage_trie)))
        if self._typedef_trie is None:
            raise te.TypeUndefinedException(self._value.name, usage_trie)

        type_var_lookup = self._generate_polytype_bindings(usage_trie)
        # Loop over all elements of the defined type
        newly_typed = []
        # The queue holds tuples of the definition node, and its corresponding declaration nodes
        queue = [(self._typedef_trie._root, [usage_trie])]
        while queue:
            curr_def, curr_usage_set = queue.pop(0)
            self._log_status(curr_def, curr_usage_set)
            curr_def_type = self._retrieve_type_declaration(curr_def, type_var_lookup)
            logging.debug("Current Def Type: {}".format(str(curr_def_type)))
            self._check_local_type_structure(curr_def, curr_usage_set)

            # Always assign current type to usage set
            newly_typed += self._apply_type_to_set(curr_usage_set, curr_def_type)

            # Handle Children:
            if len(curr_def._children) == 1:
                queue += self._handle_single_child(curr_def,
                                                   curr_usage_set)
            elif len(curr_def._children) > 1:
                queue += self._handle_multiple_children(curr_def,
                                                        curr_usage_set)

            logging.debug("----------")
        return newly_typed


    def _generate_polytype_bindings(self, usage_trie):
        """ Generate a temporary binding environment for the definition's
        type parameters """
        type_var_lookup = {}
        if self._data[util.TYPE_DEF_S]._vars \
           and usage_trie._type_instance \
           and usage_trie._type_instance._vars:
            zipped = zip(self._data[util.TYPE_DEF_S]._vars, usage_trie._type_instance._vars)
            type_var_lookup = {x: y for x, y in zipped}
        return type_var_lookup

    def _retrieve_type_declaration(self, curr_def, type_var_lookup):
        """ Use the temporary binding environment to lookup the relevant
        type declaration """
        if curr_def.name != util.ROOT_S \
           and curr_def._is_var \
           and curr_def._value.name in type_var_lookup:
            return type_var_lookup[curr_def._value.name]
        elif curr_def._type_instance is not None:
            return curr_def._type_instance.build_type_declaration(type_var_lookup)
        else:
            return None

    def _check_local_type_structure(self, curr_def, curr_usage_set):
        """ Compare Defined Structure to actual structure """
        if curr_def._value != util.ROOT_S:
            for x in curr_usage_set:
                if not x._is_var and curr_def.name != x.name:
                    raise te.TypeStructureMismatch(curr_def.name,
                                                   [x.name])

    def _apply_type_to_set(self, usage_set, def_type):
        """ Apply type declarations to nodes """
        if def_type is None:
            return []
        type_attempts = [x.type_match(def_type) for x in usage_set]
        return [x for x in type_attempts if x is not None]

    def _handle_single_child(self, curr_def, curr_usage_set):
        # Special case of handle_multiple_children
        logging.debug("Curr Def has a single child")
        queue_vals = []
        child = list(curr_def._children.values())[0]


        new_usage_set = [y for x in curr_usage_set for y in x._children.values()]
        if bool(new_usage_set):
            queue_vals.append((child, new_usage_set))

        return queue_vals

    def _handle_multiple_children(self, curr_def, curr_usage_set):
        logging.debug(LOG_MESSAGES['mult_child'])
        assert(len(curr_def._children) > 1)
        # With multiple children, match keys
        queue_vals = []
        defset = {x for x in curr_def._children.keys()}
        usageset = {y for x in curr_usage_set for y, n
                    in x._children.items() if not n._is_var}
        conflicts = usageset.difference(defset)
        if bool(conflicts):
            raise te.TypeStructureMismatch(curr_def.path,
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
        logging.debug(LOG_MESSAGES['curr_def'].format(curr_def._value,
                                                      repr(curr_def_type)))
        logging.debug(LOG_MESSAGES['curr_use_set'].format(curr_use_str))

    def _log_no_children(self, curr_def, curr_def_type, curr_usage_set):
        c_u_s_str = ", ".join([str(x) for x in curr_usage_set])
        logging.debug(LOG_MESSAGES['no_children'].format(curr_def_type,
                                                         c_u_s_str))

    def _log_match_type_usage(self, curr_def_type):
        log_msg = LOG_MESSAGES['match_type_usage'].format(curr_def_type)
        logging.debug(log_msg)
