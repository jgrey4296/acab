import logging as root_logger

import py_rule.modules.analysis.typing.util as util
from py_rule.abstract.trie.trie import Trie
from py_rule.error import type_exceptions as te
from py_rule.util import OPERATOR_S
from py_rule.modules.analysis.typing.operator_definition import OperatorDefinition

from .typedef_node import TypeDefTrieNode
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


class OperatorDefTrieNode(TypeDefTrieNode):
    """ Operator definition nodes are the head to
    a trie of all that operator's type signatures.
    It pattern matches on provided usage, and types nodes if
    there is only one matching pattern """

    def __init__(self, value):
        super().__init__(value)
        self._data[util.TYPE_DEF_S] = set()

    def __repr__(self):
        return "OperatorDefTrieNode({})".format(repr(self._value))

    def set_data(self, data):
        # Does not error about redefinition, intentionally"
        assert(isinstance(data, OperatorDefinition))
        logging.debug("OperatorDef.set_data: {}".format(data))

        self._data[util.TYPE_DEF_S].add(data)

        # add to internal search trie
        if self._typedef_trie is None:
            self._typedef_trie = Trie(node_type=TypeAssignmentTrieNode)
            self._typedef_trie._root._type = data.build_type_declaration()

        for x in data._structure:
            self._typedef_trie.add(x, {util.OP_DEF_S : data},
                                   update=lambda c, n, p, d: c.type_match_wrapper(n))

    def validate(self, usage_trie):

        assert(isinstance(usage_trie, TypeAssignmentTrieNode))
        logging.debug(LOG_MESSAGES['validate_top'].format(repr(self),
                                                          repr(usage_trie)))
        if self._typedef_trie is None:
            raise te.TypeUndefinedException(self._name, usage_trie)

        newly_typed = []

        # Match actual statements against the operator definition
        # getting back the matches, and how much they match
        matches = self._typedef_trie.match_as_pattern(usage_trie,
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
        for match_group in result_dict:
            if len(match_group) == 1:
                # apply types
                for the_def, the_use in match_group:
                    the_use.type_match_wrapper(the_def)
                    # refine the type of the operator at head
                func_name = match_group[-1][0]._data[util.OP_DEF_S]._func_name
                match_group[0][1]._data[OPERATOR_S].__refine_op_func(func_name)
                continue

        return newly_typed


def pattern_match_type_signature(head, available):
    if head._type is None:
        return available

    return [x for x in available if x._type is None or head._type == x._type]
