#!/usr/bin/env python3

"""
Node description of a basic type definition node
"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar


from acab.abstract.core.type_base import TypeInstance
from acab.abstract.data.node import AcabNode
from acab.abstract.core.value import AcabValue

from acab.modules.analysis.typing import type_exceptions as te
from acab.modules.analysis.typing.values.operator_definition import OperatorDefinition
from acab.modules.analysis.typing.values.type_definition import TypeDefinition, SumTypeDefinition
from acab.modules.structures.trie.trie import Trie
from acab.modules.structures.trie.trie_semantics import BasicTrieSemantics

import acab.modules.analysis.typing.util as util

from .typing_node_assignment_semantics import TypeAssignmentTrieNode
from .typing_node_var_semantics import VarTypeTrieNode

import logging as root_logger
logging = root_logger.getLogger(__name__)

# Log messages to use, because they are long:
LOG_MESSAGES = {}
LOG_MESSAGES['curr_def'] = "Current Definition to Validate: {} : {}"
LOG_MESSAGES['curr_use_set'] = "Current Usage Set: {}"
LOG_MESSAGES['match_type_usage'] = "Matching Type {} onto usage set"
LOG_MESSAGES['mult_child'] = "Current Def has multiple children, checking for conflicts in structure"
LOG_MESSAGES['no_children'] = "Val: No Children, assigning type: {} to {}"
LOG_MESSAGES['validate_top'] = "Validating: {} on {}"
from acab.abstract.data.node_semantics import  AcabNodeSemantics


class TypingDefinitionSemantics(AcabNodeSemantics):

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

    def test_word(self, query_term, query_context):
        """ query word, with a sequence of tests,
        in all available bind groups, BFS manner
        """
        assert(not query_term.is_at_var)
        engine = query_context._engine
        # TODO: validate on activate context too
        alphas, betas, subbinds, annotations = self._validate_and_split_constraints(query_term, engine=engine)
        callable_annotations = [word for word in annotations if hasattr(word, "__call__")]
        data_set = {word : d for word,d in enumerate(query_context.pairs())}
        pairs  = list(enumerate(query_context.pairs()))
        query_context.clear()

        potentials = [(i, d[0], passing) for i,d in pairs for passing in self.accessible(d, query_term)]
        # Apply Tests
        # TODO: test type instance if not ATOM
        passing = [self._run_subbinds(n, subbinds, d, i, engine=engine) for i,d,n in potentials
                   if self._test_alphas(n, alphas, d, engine=engine)
                   and self._test_betas(n, betas, d, engine=engine)
                   and self._test_annotations(n, callable_annotations, d, engine)]

        to_add = [query_context.prepare_tuple_dict(i, d, n, query_term) for i,d,n in passing if n is not None]

        query_context.append(*to_add, fail_dict=data_set)
        return annotations



    # internal
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

        ie: check IS against SHOULD (u_t v self)
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



class OperatorDefTrieNode(TypeDefTrieNode):
    """ Operator definition nodes are the head to
    a trie of all that operator's type signatures.
    It pattern matches on provided usage, and types nodes if
    there is only one matching pattern """

    def __init__(self, value):
        super().__init__(value)
        # This is a set to hold multiple possible operators
        # eg: AddOp: $x(::Num).$x.$x
        # and AddOp: $x(::String).$x.$x
        self._data[util.TYPE_DEF_S] = set()


    def set_data(self, data):
        # Does not error about redefinition, intentionally
        assert(isinstance(data, OperatorDefinition))
        logging.debug("OperatorDef.set_data: {}".format(data))
        self._data[util.TYPE_DEF_S].add(data)

        # add to internal search trie
        if self._typedef_trie is None:
            self._typedef_trie = Trie(node_type=TypeAssignmentTrieNode)
            self._typedef_trie.root._type_instance = data.build_type_instance()

        for x in data.structure:
            # TODO This might need to generate new vars
            self._typedef_trie.add(x, {util.OP_DEF_S : data},
                                   update=lambda c, n, p, d: c.update(n))

    def validate(self, usage_trie, create_var):
        assert(callable(create_var))
        assert(isinstance(usage_trie, TypeAssignmentTrieNode))
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
