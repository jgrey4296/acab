#!/usr/bin/env python3

"""
Node description of a basic type definition node
"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from dataclasses import dataclass, field

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.core.node import AcabNode
from acab.modules.semantics.query_semantic_mixin import QuerySemanticMixin

from acab.modules.analysis.typing import type_exceptions as te
from acab.modules.analysis.typing.values.operator_definition import OperatorDefinition
from acab.modules.analysis.typing.values.type_definition import TypeDefinition, SumTypeDefinition
from acab.modules.structures.trie.trie import Trie
from acab.modules.structures.trie.trie_semantics import BasicTrieSemantics
from acab.modules.semantics.basic_node_semantics import BasicNodeSemantics

import acab.modules.analysis.typing.util as util

from acab.abstract.interfaces import semantic as SI

from .type_assignment_semantics import TypeAssignmentNode
from .type_variable_semantics import VarTypeNode

import logging as root_logger
logging = root_logger.getLogger(__name__)

# Log messages to use, because they are long:
LOG_MESSAGES = {}
LOG_MESSAGES['curr_def']         = "Current Definition to Validate: {} : {}"
LOG_MESSAGES['curr_use_set']     = "Current Usage Set: {}"
LOG_MESSAGES['match_type_usage'] = "Matching Type {} onto usage set"
LOG_MESSAGES['mult_child']       = "Current Def has multiple children, checking for conflicts in structure"
LOG_MESSAGES['no_children']      = "Val: No Children, assigning type: {} to {}"
LOG_MESSAGES['validate_top']     = "Validating: {} on {}"


class TypingDefinitionSemantics(BasicNodeSemantics, SI.IndependentSemantics_i, SI.SemanticSystem_i):

    def up(self, word : AcabValue, constructor : Callable) -> AcabNode:
        """ The Most Basic Lift """
        assert(isinstance(word, AcabValue))
        return constructor(word)


    def add(self, node : AcabNode, word: AcabValue, node_constructor : Callable) -> Tuple[bool, AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(word, AcabValue))

        is_new_node = False
        result = self.get(node, word)

        if result is None:
            result = self.up(word, node_constructor)
            node.add_child(result)
            is_new_node = True
        elif (isinstance(word, TypeDefinition)
              and result.definition is not None
              and result.definition.structure != word.structure):
            raise te.TypeRedefinitionException(result.definition)


        return True, result


@dataclass
class TypeDefNode(AcabNode):
    """ A Node describing a type definition """
    _typedef_trie : 'AcabStruct' = field(init=False)
    _var_binds : 'AcabStruct' = field(init=False)

    @property
    def definition(self):
        return self.value

    @property
    def trie(self):
        return self._typedef_trie


    def _default_setup(self, path : [AcabNode], data : Dict[Any, Any], context : Dict[Any, Any]):
        """ Overrides AcabNode.set_data.
        Builds the static assignment expression of a definition, to use to type check against
        """
        assignment_semantics = data["assignment"]
        var_semantics = data["variable"]

        logging.debug("TypeDef.set_data: {}".format(data))
        # construct the internal trie
        self._typedef_trie = Trie(semantics=assignment_semantics)
        self._var_binds = Trie(semantics=var_semantics)
        # TODO May be unnecessary:
        self.trie.root._type_instance = self.definition.build_type_instance()

        for x in self.definition.structure:
            self.trie.add(x, context_data={"var_struct" : self._var_binds})

    def validate(self, usage_trie, create_var):
        """ Given an instance trie node, type check / infer it
        against self's description of a type.
        returning a list of nodes that have been inferred

        ie: check IS against SHOULD (u_t v self)
        """
        assert(callable(create_var))
        assert(isinstance(usage_trie, TypeAssignmentNode)), breakpoint()
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
        definition_instances = [x if isinstance(x, Sentence.build) else x.type_instance for x in definition_bindings]

        usage_trie.type_instance.params = definition_instances

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
        if isinstance(curr_def, Sentence.build):
            _type = curr_def
        else:
            _type = curr_def.type_instance

        if _type is not None:
            _type = _type.build_type_instance(lookup)

        if isinstance(curr_def, VarTypeNode):
            dummy = [curr_def.add_node(x) for x in curr_usage_set]


        type_attempts = [x.unify_types(_type) for x in curr_usage_set]
        return [x for x in type_attempts if x is not None]

    def _handle_single_child(self, curr_def, curr_usage_set):
        # Special case of handle_multiple_children
        logging.debug("Curr Def has a single child")
        queue_vals = []
        conflicts = []
        child = list(curr_def.children.values())[0]

        new_usage_set = [y for x in curr_usage_set for y in x.children.values()]

        if not child.is_var:
            conflicts = [x for x in new_usage_set if not x.is_var and child.name != x.name]

        if bool(conflicts):
            raise te.TypeStructureMismatch(str(self.definition.path),
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
            raise te.TypeStructureMismatch(str(self.definition.path),
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



class SumTypeDefNode(TypeDefNode):
    """ A Specialised TypeDefNode to validate Sum Types  """

    def validate(self, usage_trie, create_var):
        assert(callable(create_var))
        assert(isinstance(usage_trie, TypeAssignmentNode)), breakpoint()

        if self.trie is None:
            raise te.TypeUndefinedException(self.value.name, usage_trie)

        type_var_lookup = self._generate_polytype_bindings(usage_trie, create_var)
        newly_typed = []

        # usage_trie must be a child of the curr_def
        if usage_trie.name not in self.trie.root:
            raise te.TypeStructureMismatch(str(self.definition.path),
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


@dataclass
class OperatorDefNode(TypeDefNode):
    """ Operator definition nodes are the head to
    a trie of all that operator's type signatures.
    It pattern matches on provided usage, and types nodes if
    there is only one matching pattern """

    # This is a set to hold multiple possible operators
    # eg: AddOp: $x(::Num).$x.$x
    # and AddOp: $x(::String).$x.$x
    _op_possibilities : Set[Any] = field(init=False)
    # was: self._data[util.TYPE_DEF_S] = set()


    def set_data(self, data):
        # Does not error about redefinition, intentionally
        assert(isinstance(data, OperatorDefinition))
        logging.debug("OperatorDef.set_data: {}".format(data))
        self._data[util.TYPE_DEF_S].add(data)

        # add to internal search trie
        if self._typedef_trie is None:
            # TODO setup the semantics for this trie
            self._typedef_trie = Trie() #TypeAssignmentNode
            self._typedef_trie.root._type_instance = data.build_type_instance()

        for x in data.structure:
            # TODO This might need to generate new vars
            self._typedef_trie.add(x, {util.OP_DEF_S : data},
                                   update=lambda c, n, p, d: c.update(n))

    def validate(self, usage_trie, create_var):
        assert(callable(create_var))
        assert(isinstance(usage_trie, TypeAssignmentNode))
        logging.debug(LOG_MESSAGES['validate_top'].format(repr(self),
                                                          repr(usage_trie)))
        if self.trie is None:
            raise te.TypeUndefinedException(self.name, usage_trie)

        newly_typed = []

        # Get op_def patterns which are applicable to usage trie
        matches = self.trie.filter_candidates(usage_trie,
                                              pattern_match_type_signature)

        # Construct result dictionary
        # Combine all matches together by their usage
        result_dict = {}
        for match in matches:
            path = "".join([str(y) for x, y in match])
            if path not in result_dict:
                result_dict[path] = []
            result_dict[path].append(match)

        # Only apply types that have been reduced to one possibility
        for match_group in result_dict.values():
            if len(match_group) == 1:
                # apply types
                for the_def, the_use in match_group[0]:
                    _type = the_def.type_instance
                    newly_typed.append(the_use.unify_types(_type))
                # refine the type of the operator at head
                func_name = match_group[0][0]._data[util.OP_DEF_S]._func_name
                # TODO update this
                match_group[0][1]._data[util.OPERATOR_S].__refine_op_func(func_name)
                continue

        return [x for x in newly_typed if x is not None]


def pattern_match_type_signature(head, available):
    if head.type is None:
        return available

    return [x for x in available if x.type_instance is None
            or head.type_instance == x.type_instance]
