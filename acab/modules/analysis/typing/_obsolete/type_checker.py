"""

"""
import logging as root_logger
from functools import partial
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from uuid import uuid1

from acab.core.data.node import AcabNode
from acab.core.data.instruction import ProductionOperator
from acab.core.data.value import Instruction, AcabValue, Sentence
#
# MUST BE FULL PATH otherwise type instances are built twice for some reason
# NOT : from . import util as TU
# from . import util as TU
from acab.modules.analysis.typing import util as TU
from acab.modules.semantics.independent import BasicNodeSemantics
from acab.modules.structures.trie.trie import Trie
from acab.modules.structures.trie.trie_semantics import BasicTrieSemantics

from . import type_exceptions as te
from .semantics.type_assignment_semantics import (TypeAssignmentNode,
                                                  TypingAssignmentSemantics)
from .semantics.type_definition_semantics import (OperatorDefNode,
                                                  SumTypeDefNode, TypeDefNode,
                                                  TypingDefinitionSemantics)
from .semantics.type_variable_semantics import TypingVarSemantics, VarTypeNode
from .values import type_definition as TD
from .values.operator_definition import OperatorDefinition
from .values.type_definition import SumTypeDefinition, TypeDefinition

logging = root_logger.getLogger(__name__)


class TypeChecker:
    """ Abstract Class for Type Checking """
    # parse their locations, and add them as definitions

    def __init__(self):
        super(TypeChecker, self).__init__()
        ass_semantics = BasicTrieSemantics({AcabNode : BasicNodeSemantics()},
                                           {AcabValue : (TypeAssignmentNode, {})})

        var_semantics = BasicTrieSemantics({AcabNode : TypingVarSemantics()},
                                           {AcabValue : (VarTypeNode, {})})

        sub_context_data = {'assignment': ass_semantics,
                            'variable'  : var_semantics}

        def_semantics = BasicTrieSemantics({AcabNode : TypingDefinitionSemantics()},
                                           {AcabValue : (AcabNode, {}, None),
                                            TypeDefinition : (TypeDefNode, sub_context_data),
                                            SumTypeDefinition : (SumTypeDefNode, sub_context_data),
                                            OperatorDefinition : (OperatorDefNode, sub_context_data)})

        self._definitions = Trie(semantics=def_semantics)
        self._assignments = Trie(semantics=ass_semantics)
        self._variables   = Trie(semantics=var_semantics)

        # TODO add basic types
        # self.add_definition(*TD.build_primitive_definitions())


    def __call__(self, sentences: List[Sentence]):
        """ Pass in data to type check """
        # Gets all leaf sentences and statements
        logging.info("Running Type Checker")
        logging.info("Checking {} sentences".format(len(sentences)))

        local_contexts_to_check = []
        for sen in sentences:
            if isinstance(sen[-1], TD.TypeDefinition):
                self.add_definition(sen)
            elif isinstance(sen[-1], Instruction):
                local_contexts_to_check.append(sen[-1])
            else:
                self.add_assertion(sen)

        logging.info("Definitions and Assertions added")
        logging.info("Checking {} local contexts".format(len(local_contexts_to_check)))
        for statement in local_contexts_to_check:
            self.add_statement(statement)

        logging.info("Checking Totality")
        self.validate()


    def _get_known_typed_nodes(self):
        # propagate known variable types
        # TODO shift to type bottom
        # TODO use config values and build a sentence
        dummy = [x.propagate() for x in self._variables.get_nodes(lambda x: x.type_instance != "ATOM")]
        # get all known declared types
        # TODO: get references as well
        val_queue = {y for y in self._assignments.get_nodes(lambda x: x.type_instance != "ATOM")}
        return val_queue

    def _merge_equivalent_nodes(self):
        """ merge equivalent variables.
        ie: a.b.$c and a.b.$d share the same ._variables node """
        parents_of_equiv_vars = self._assignments.get_nodes(TU.has_equivalent_vars_pred)
        for p in parents_of_equiv_vars:
            var_nodes = {x._var_node for x in p._children.values() if x.is_var}
            head = var_nodes.pop()
            head.merge(var_nodes)
            dummy = [self._variables.remove([x]) for x in var_nodes]


    def clear_context(self):
        """ Clear variables """
        # Clear self._variables and unregister its nodes from
        # vars in declarations
        var_nodes = self._variables.get_nodes()
        dummy = [x.clear_assignments() for x in var_nodes]

        # remove all sentences in declarations that start with a variable
        # TODO or an operator
        dummy = [self._assignments.remove([x]) for x in var_nodes]

        # remove the variables
        dummy = [self._variables.remove([x]) for x in var_nodes]

    def query(self, queries):
        """ Get the type of a sentence leaf """
        if isinstance(queries, Sentence):
            queries = [queries]

        results = []
        for line in queries:
            queried = self._assignments.query(line)
            if queried is None:
                continue
            # Compare a sentence type(instance) to the node's type_instance
            # (as a node can have its own type)

            # 1: var, no type:
            # 2: var, type:
            # 3: No var, no type:
            # 4: no var, type:


            # TODO type semantics __lt__
            types_match = (line[-1].type < queried[0][line[-1].name].type)
            if not types_match:
                raise te.TypeConflictException(line[-1].type,
                                               queried.type_instance,
                                               "".join([str(x) for x in line]))
            results.append(queried)
        return results

    def validate(self):
        """ Infer and check types """
        self._merge_equivalent_nodes()
        typed_queue = self._get_known_typed_nodes()

        # Use known types to infer unknown types
        create_var = partial(TU.create_type_var, self)
        dealt_with = set()
        while bool(typed_queue):
            head = typed_queue.pop()
            assert(isinstance(head, TypeAssignmentNode)), breakpoint()
            if head in dealt_with:
                continue
            dealt_with.add(head)

            # check the head
            # TODO : for operators (eg: transforms),
            # make operators an entire node, with their path to the definition accessible
            head_type = self._definitions.query(head.type_instance.path)
            if head_type is None:
                raise te.TypeUndefinedException(head.type_instance, head)

            # Propagate the type to all connected variables
            # TODO: if var nodes also link *into* polytype def nodes
            # that would solve generalisation?
            if head.is_var:
                head._var_node.unify_types(head.type_instance)
                head._var_node.propagate()
                assert(all([isinstance(x, TypeAssignmentNode) for x in head._var_node._nodes])), breakpoint()
                typed_queue.update([x for x in head._var_node._nodes if isinstance(x, TypeAssignmentNode)])

            # Apply a known type to a node, get back newly inferred types
            results = head_type.validate(head, create_var)
            assert(all([isinstance(x, TypeAssignmentNode) for x in results])), breakpoint()
            typed_queue.update(results)

            # TODO if head validation returns only operators, and
            # the queue is only operators, then error

        return True

    def add_definition(self, *definitions):
        for a_def in definitions:
            assert(isinstance(a_def, Sentence))
            assert(isinstance(a_def[-1], TD.TypeDefinition))
            self._definitions.add(a_def)

    def add_assertion(self, *sens):
        for x in sens:
            assert(isinstance(x, Sentence))
            self._assignments.add(x, context_data={"var_struct" : self._variables})

    def add_statement(self, statement):
        """
        Statements are treated as having their own local context.
        So add it, type check it, and then clear any variable associations
        """
        sentences = statement.to_sentences()

        for sen in sentences:
            self.add_assertion(sen)

        self.validate()
        self.clear_context()



