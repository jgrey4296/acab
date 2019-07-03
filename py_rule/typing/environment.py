from .ex_types import TypeDefinition
import py_rule.trie.trie as T
import util as U
import type_exceptions as te
import logging as root_logger
import IPython
logging = root_logger.getLogger(__name__)


class Environment:

    def __init__(self, init_types):
        # defined types
        self.type_equations = T.Trie(node_type=T.TypeDefTrieNode)
        # currently known types
        self.type_assignments = T.Trie(node_type=T.TypeAssignmentTrieNode)
        # variable types
        self.var_types = T.Trie(node_type=T.VarTypeTrieNode)

        for x in init_types:
            self.add(x)

    def __repr__(self):
        return "Environment: Vars: ({}), Equ: {}, Assn: {}".format(len(self.var_types),
                                                                   len(self.type_equations),
                                                                   len(self.type_assignments))

    def query(self, query):
        """ Check a query type against assignments """
        queries = []
        for line in query:
            queried = self.type_assignments.query(line)
            if line[-1]._type is not None and queried._type != line[-1]._type:
                raise te.TypeConflictException(line[-1]._type,
                                               queried._type,
                                               "".join([str(x) for x in line]))
            queries.append(queried)
        return queries

    def validate(self):
        """ Infer and check types """

        #merge equivalent variables
        parents_of_equiv_vars = self.type_assignments.get_nodes(U.has_equivalent_vars_pred)
        if bool(parents_of_equiv_vars):
            logging.debug("Has Equivalent Vars")
            for p in parents_of_equiv_vars:
                var_nodes = {x.var_node for x in p._children.values() if x.is_var}
                head = var_nodes.pop()
                head.merge(var_nodes)
                [self.var_types.remove(x.path) for x in var_nodes]

        #Get known variable types
        val_queue = set()
        for x in self.var_types.get_nodes(lambda x: x._type is not None):
            x.propagate()
            val_queue.update(x.nodes)
        #And known types generally
        val_queue.update({y for y in self.type_assignments.get_nodes(lambda x: x._type is not None)})

        #Use known types to infer unknown types
        dealt_with = set()
        while bool(val_queue):
            head = val_queue.pop()
            if head in dealt_with:
                continue
            dealt_with.add(head)
            #check the head
            head_type = self.type_equations.query(head._type.path)
            if head_type is None:
                raise te.TypeUndefinedException(head._type, head)

            #Apply a known type to a node, get back newly inferred types
            newly_typed = head_type.validate(head)
            for x in newly_typed:
                val_queue.add(x)
                if x.is_var:
                    #If inferred the type of a variable,
                    #apply it to the var_type node, then
                    x.var_node.type_match(x._type)
                    val_queue.update(x.var_node.nodes)

        return True

    def add(self, other):
        logging.debug("Environment: Adding {}".format(other))
        if isinstance(other, TypeDefinition):
            self.add_typedef(other)
            return
        #if op: add_op

        #----------
        # Type Assignment:
        self.add_sentence(other)

    def add_typedef(self, typedef):
        self.type_equations.add(typedef.path, typedef)

    def add_sentence(self, lst):
        #add the sentence to the type assignment trie
        self.type_assignments.add(lst, None,
                                  update=lambda c, n, p, d: c.update(n, d),
                                  u_data=self.var_types)

    def add_operations(self, op):
        return True
