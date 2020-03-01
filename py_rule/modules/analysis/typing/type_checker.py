"""
An Entry point for type checking

Type checking is a triple of tries: (type_definitions,
 type_assignments, type_variables)

Definitions are structured polytypes.
Assignments start untyped, and are gradually refined.
Variables connect variables in the assignments to definitions.

The Tries use nodes of TypeDefTrieNode,
TypeAssignmentTrieNode, and VarTypeTrieNode.

The values of the trie nodes are pyrule.abstract.values, subclassed as Types.
They are TypeDefinitions, and TypeInstantiations.


When a TypeChecker is called with (Definitions, Rules, Assertions),
it adds the definitions, add the assertions,
then adds each rule, treating each rule as a separate context.

Definitions are located by their path (eg: type.test )
which holds the TypeDefintion (which contains its structure and vars)
(eg: { type.test.name!$x :: string, type.test.age!$y :: int })

type_assignments is the main 'work' trie, holding all sentences.
When an assertion/declaration is added, it adds the node then updates
the node to connect any variables in the sentence with variables
in the type_variable trie.
(eg: a.thing.$x,  a.different.thing :: type.test)

Variables connect different locations of the assignment trie together
(eg: a.thing.$x, another.thing.$x will share the same var node: $x)


When validate is called:
equivalent pathed nodes are merged,
then known types are applied everywhere they can be
the linked type definition then DFS's
ensure it matches the definition.
and any newly typed nodes are then ready to be used in the next iteration



If validate succeeds, it returns True. If it Fails, it raises an Exception

"""
from .type_definition import TypeDefinition
from .nodes.type_assignment_node import TypeAssignmentTrieNode
from .nodes.typedef_node import TypeDefTrieNode
from .nodes.var_type_node import VarTypeTrieNode
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.trie.trie import Trie
import py_rule.error.type_exceptions as te
from . import util as TU
import logging as root_logger

logging = root_logger.getLogger(__name__)


class TypeChecker:
    """ Abstract Class for Type Checking """

    def __init__(self):
        self._definitions = Trie(TypeDefTrieNode)
        self._declarations = Trie(TypeAssignmentTrieNode)
        self._variables = Trie(VarTypeTrieNode)

    def __str__(self):
        return "Defs: {}, Decs: {}, Vars: {}".format(str(self._definitions).replace('\n', ' '),
                                                     str(self._declarations).replace('\n', ' '),
                                                     str(self._variables).replace('\n', ' '))

    def __repr__(self):
        return "TypeChecker({})".format(str(self))

    def __call__(self, data):
        """ Pass in data to type check """
        # TODO: switch to just assertions
        definitions, rules, assertions = data
        # add definitions
        for x in definitions:
            self.add_definition(x)
        # TODO add operators
        # add the assertions
        for x in assertions:
            self.add_assertion(x)

        # for each rule: (in place of functions)
        for x in rules:
            self.push_typing_context()
            self.add_rule(x)
            self.validate()
            self.pop_typing_context()

    def clear_context(self):
        """ Clear variables """
        # Clear self._variables and unregister its nodes from
        # vars in declarations
        var_nodes = self._variables.get_nodes()
        [x.clear_assignments() for x in var_nodes]

        # remove all sentences in declarations that start with a variable
        [self._declarations.remove([x]) for x in var_nodes]

        # remove the variables
        [self._variables.remove([x]) for x in var_nodes]

    def query(self, queries):
        """ Get the type of a sentence leaf """
        if isinstance(queries, Sentence):
            queries = [queries]

        results = []
        for line in queries:
            queried = self._declarations.query(line)
            if queried is None:
                continue
            line_is_typed = TU.TYPE_DEC_S in line[-1]._data
            result_is_typed = TU.TYPE_DEC_S in queried._data
            types_match = (line_is_typed
                           and result_is_typed
                           and line[-1]._data[TU.TYPE_DEC_S] == queried._data[TU.TYPE_DEC_S])
            if line_is_typed and result_is_typed and not types_match:
                raise te.TypeConflictException(line[-1]._data[TU.TYPE_DEC_S],
                                               queried._data[TU.TYPE_DEC_S],
                                               "".join([str(x) for x in line]))
            results.append(queried)
        return results

    def validate(self):
        """ Infer and check types """

        self._merge_equivalent_nodes()
        typed_queue = self._get_known_typed_nodes()

        # Use known types to infer unknown types
        dealt_with = set()
        while bool(typed_queue):
            head = typed_queue.pop()
            if head in dealt_with:
                continue
            dealt_with.add(head)

            # check the head
            head_type = self._definitions.query(head._type._path)
            if head_type is None:
                raise te.TypeUndefinedException(head._type, head)

            # Propagate the type to all connected variables
            if head._is_var:
                head._var_node.type_match(head._type)
                head._var_node.propagate()
                typed_queue.update(head._var_node._nodes)

            # Apply a known type to a node, get back newly inferred types
            typed_queue.update(head_type.validate(head))

        return True

    def _get_known_typed_nodes(self):
        # propagate known variable types
        [x.propagate() for x in self._variables.get_nodes(lambda x: x._type is not None)]
        # get all known declared types
        val_queue = {y for y in self._declarations.get_nodes(lambda x: x._type is not None)}
        return val_queue

    def _merge_equivalent_nodes(self):
        """ merge equivalent variables. ie:
        a.b.$c and a.b.$d share the same ._variables node """
        parents_of_equiv_vars = self._declarations.get_nodes(TU.has_equivalent_vars_pred)
        for p in parents_of_equiv_vars:
            var_nodes = {x._var_node for x in p._children.values() if x._is_var}
            head = var_nodes.pop()
            head.merge(var_nodes)
            [self._variables.remove([x]) for x in var_nodes]

    def add_definition(self, definition):
        assert(isinstance(definition, TypeDefinition))
        self._definitions.add(definition._path, definition)

    def add_assertion(self, sen):
        assert(isinstance(sen, Sentence))
        self._declarations.add(sen, None,
                               update=lambda c, v, p, d: c.update(v, d),
                               u_data=self._variables)

    def add_rule(self, value):
        # TODO: needs to be finished
        for c in value._query._clauses:
            # add the conditions
            continue

        for t in value._transform._components:
            # add the transforms
            continue

        for a in value._actions:
            # add the actions
            continue

        self.validate()
        self.clear_context()
