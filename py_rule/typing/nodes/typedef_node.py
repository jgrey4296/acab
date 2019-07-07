import py_rule.utils as utils
from py_rule.trie.trie import Trie
from py_rule.typing.ex_types import TypeDefinition
from py_rule.trie.nodes.trie_node import TrieNode
from .type_assignment_node import TypeAssignmentTrieNode
from py_rule.typing import type_exceptions as te

log_messages = {}
log_messages['validate_top'] = "Validating: {} on {} ({})"
log_messages['curr_def'] = "Current Definition to Validate: {} : {} : {}"
log_messages['curr_use_set'] = "Current Usage Set: {}"
log_messages['no_children'] = "Val: No Children, assigning type: {} to {}"
log_messages['match_type_usage'] = "Matching Type {} onto usage set"
log_messages['mult_child'] = "Current Def has multiple children, checking for conflicts in structure"



class TypeDefTrieNode(TrieNode):

    def __init__(self, value):
        logging.debug("TypeDefTrieNode: init: {}".format(value))
        super().__init__(value)
        self._typedef_trie = None

    def __repr__(self):
        return "TypeDefTrieNode: {}".format("".join([repr(x) for x in self.path]))

    def set_data(self, data):
        """ Overrides TrieNode.set_data.
        Builds the subtrie of a type definition at the end of being added
        to the definition trie.
        """
        assert(isinstance(data, TypeDefinition))
        logging.debug("TypeDef.set_data: {}".format(data))
        if self.data is None:
            self.data = data
            #construct the internal trie
            self._typedef_trie = Trie(node_type=TypeAssignmentTrieNode)
            self._typedef_trie._root._type = self.data.build_type_declaration()
            for x in self.data.structure:
                self._typedef_trie.add(x, None,
                                       update=lambda c, n, p, d: c.type_match_wrapper(n))
            return

        if self.data != data:
            raise te.TypeRedefinitionException(self.data)

    def validate(self, usage_trie):
        usage_path = "".join([repr(x) for x in usage_trie.path])
        logging.debug(log_messages['validate_top'].format(repr(self),
                                                          repr(usage_trie),
                                                          usage_path))
        assert(isinstance(usage_trie, TrieNode))
        if self._typedef_trie is None:
            raise TypeUndefinedException(self.name, usage_trie)

        type_var_lookup = self.make_type_var_lookup(usage_trie)

        # Loop over all elements of the defined type
        newly_typed = []
        queue = [(self._typedef_trie._root, [usage_trie])]
        while queue:
            curr_def, curr_usage_set = queue.pop(0)

            self._log_status(curr_def, curr_usage_set)

            #use stored type variables if on a typedef variable node
            curr_def_type = self._typedef_var_lookup(self, curr_def,
                                                     type_var_lookup)

            # if you are at the root, check you aren't
            #a variable reference to elsewhere
            self._handle_vars(curr_def, curr_usage_set)

            #if there are no children, assign a type and finish
            if not bool(curr_def) and curr_def_type is not None:
                self._log_no_children(curr_def, curr_def_type, curr_usage_set)
                newly_typed += self._match_on_set(curr_usage_set, curr_def_type)

            #if there is one child, match all usage set against it
            elif len(curr_def) == 1:
                logging.debug("Curr Def has a single child")
                child = list(curr_def._children.values())[0]
                if curr_def_type is not None:
                    self._log_match_type_usage(curr_def_type)
                    newly_typed += self._match_on_set(curr_usage_set, curr_def_type)

                new_usage_set = [y for x in curr_usage_set for y in x._children.values()]
                if bool(new_usage_set):
                    queue.append((child, new_usage_set))

            else: #curr_def._children > 1
                logging.debug(log_messages['mult_child'])
                # With multiple children, match keys
                defset = { x for x in curr_def._children.keys() }
                usageset = { y for x in curr_usage_set for y,n in x._children.items() if not n.is_var }
                conflicts = usageset.difference(defset)
                if bool(conflicts):
                    raise te.TypeStructureMismatch(curr_def.path,
                                                   conflicts)
                logging.debug("No Conflicts found, checking children")
                for key in usageset:
                    new_usage_set = [x._children[key] for x in curr_usage_set if key in x._children]
                    new_child = curr_def._children[key]
                    if bool(new_usage_set):
                        queue.append((new_child, new_usage_set))
            logging.debug("----------")
        return newly_typed


    def _make_type_var_lookup(self, usage_trie):
        type_var_lookup = {}
        if self.data._vars and usage_trie._type and usage_trie._type._args:
            zipped = zip(self.data._vars, usage_trie._type._args)
            type_var_lookup = { x : y for x,y in zipped }
        return type_var_lookup

    def _typedef_var_lookup(self, curr_def, type_var_lookup):
        if curr_def.is_var and curr_def.name in type_var_lookup:
            return type_var_lookup[curr_def.name]
        elif curr_def._type is not None:
            return curr_def._type.build_type_declaration(type_var_lookup)

    def _handle_vars(self, curr_def, curr_usage_set):
        if curr_def.name != "__root":
            for x in curr_usage_set:
                if not x.is_var and curr_def.name != x.name:
                    raise te.TypeStructureMismatch(curr_def.name, [x.name])

    def _match_on_set(self, usage_set, def_type):
        type_attempts = [x.type_match(def_type) for x in usage_set]
        return [x for x in type_attempts if x is not None]


    def _log_status(self, curr_def, curr_usage_set):
        curr_def_type = curr_def._type
        curr_use_str = ", ".join([str(x) for x in curr_usage_set])
        curr_def_path = "".join([repr(x) for x in curr_def.path])
        logging.debug(log_messages['curr_def'].format(curr_def_path,
                                                      curr_def.name,
                                                      repr(curr_def_type)))
        logging.debug(log_messages['curr_use_set'].format(curr_use_str))

    def _log_no_children(self, curr_def, curr_def_type, curr_usage_set):
                c_u_s_str = ", ".join([str(x) for x in curr_usage_set])
                logging.debug(log_messages['no_children'].format(curr_def_type,
                                                                 c_u_s_str))

    def _log_match_type_usage(self, curr_def_type):
        log_msg = log_messages['match_type_usage'].format(curr_def_type)
        logging.debug(log_msg)
