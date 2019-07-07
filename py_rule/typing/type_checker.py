from py_rule.trie.trie import Trie
from py_rule.abstract.sentence import Sentence
from .nodes.typedef_node import TypeDefTrieNode
from .nodes.var_type_node import VarTypeTrieNode
from .nodes.type_assignment_node import TypeAssignmentTrieNode
from .ex_types import TypeDefinition
import py_rule.typing.type_exceptions as te
import py_rule.typing.util as U

import logging as root_logger
logging = root_logger.getLogger(__name__)


class TypeChecker:
    """ Abstract Class for Type Checking """

    def __init__(self):
        self._definitions = Trie(TypeDefTrieNode)
        self._declarations = Trie(TypeAssignmentTrieNode)
        self._variables = Trie(VarTypeTrieNode)
        self._context_prefix_stack = []

    def __str__(self):
        return "Defs: {}, Decs: {}, Vars: {}".format(str(self._definitions),
                                                     str(self._declarations),
                                                     str(self._variables))

    def __repr__(self):
        return "TypeChecker({})".format(str(self))

    def __call__(self, data):
        definitions, rules, assertions = data
        #add definitions
        for x in definitions:
            self.add_definition(x)
        #add the assertions
        for x in assertions:
            self.add_assertion(x)

        #for each rule:
        for x in rules:
            self.push_typing_context()
            self.add_rule(x)
            self.validate()
            self.pop_typing_context()


    def pop_typing_context(self):
        """ Pop a typechecking context """
        self._context_prefix_stack.pop()

    def push_typing_context(self, prefix):
        """ Push a typechecking context """
        self._context_prefix_stack.append(prefix)

    def query(self, sen):
        """ Get the type of a sentence leaf """
        queries = []
        for line in query:
            queried = self._declarations.query(line)
            if line[-1]._type is not None and queried._type != line[-1]._type:
                raise te.TypeConflictException(line[-1]._type,
                                               queried._type,
                                               "".join([str(x) for x in line]))
            queries.append(queried)
        return queries


    def validate(self):
        """ Infer and check types """

        #merge equivalent variables
        parents_of_equiv_vars = self._declarations.get_nodes(U.has_equivalent_vars_pred)
        if bool(parents_of_equiv_vars):
            logging.debug("Has Equivalent Vars")
            for p in parents_of_equiv_vars:
                var_nodes = {x._var_node for x in p._children.values() if x.is_var}
                head = var_nodes.pop()
                head.merge(var_nodes)
                [self._variables.remove(x._path) for x in var_nodes]

        #Get known variable types
        val_queue = set()
        for x in self._variables.get_nodes(lambda x: x._type is not None):
            x.propagate()
            val_queue.update(x.nodes)
        #And known types generally
        val_queue.update({y for y in self._declarations.get_nodes(lambda x: x._type is not None)})

        #Use known types to infer unknown types
        dealt_with = set()
        while bool(val_queue):
            head = val_queue.pop()
            if head in dealt_with:
                continue
            dealt_with.add(head)
            #check the head
            head_type = self._definitions.query(head._type._path)
            if head_type is None:
                raise te.TypeUndefinedException(head._type, head)

            #Apply a known type to a node, get back newly inferred types
            newly_typed = head_type.validate(head)
            for x in newly_typed:
                val_queue.add(x)
                if x._is_var:
                    #If inferred the type of a variable,
                    #apply it to the var_type node
                    x._var_node.type_match(x._type)
                    val_queue.update(x.var_node._nodes)

        return True


    def add_definition(self, definition):
        assert(isinstance(definition, TypeDefinition))
        self._definitions.add(definition._path, definition)

    def add_assertion(self, sen):
        assert(isinstance(sen, Sentence))
        self._declarations.add(sen, None,
                               update=lambda c, v, p, d: c.update(v, d),
                               u_data=self._variables)

    def add_rule(self, value):
        #prefix variables
        current_prefix = self._context_prefix_stack[-1]
        #add the conditions

        #add the transforms

        #add the actions
