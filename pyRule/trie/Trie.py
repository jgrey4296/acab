from .Node import Node
from . import FactParser as FP
from . import QueryParser as QP
from .Contexts import Contexts
from .Query import Query
from .Clause import Clause
from . import Matching
from pyRule.utils import EXOP, META_OP
from pyRule.Comparisons import COMP_LOOKUP
import logging as root_logger
import re
import IPython
logging = root_logger.getLogger(__name__)


class Trie:
    """ A Trie based knowledge base """
    
    def __init__(self, input=None):
        self._root = Node.Root()
        self._last_node = self._root
        if input != None:
            self.assertS(input)

    def __eq__(self, other):
        assert(isinstance(other, Trie))
        return self._root == other._root
            
    def __str__(self):
        return self._root.root_str()
    
    def assertS(self,s):
        """ Assert multiple facts from a single string """
        parsed = FP.parseString(s)
        for x in parsed:
            self.assertFact(x)

    def retractS(self, s):
        """ Retract multiple facts from a single string """
        parsed = FP.parseString(s)
        for x in parsed:
            self.retractFact(x)
        
    def assertFact(self, factList):
        """ Assert a [FactNode] list """
        assert(all([isinstance(x, Node) for x in factList]))
        self._clear_last_node()
        for newNode in factList:
            self._last_node = self._last_node.insert(newNode)


    def retractFact(self, factList):
        """ Retract a [FactNode] list """
        assert(all([isinstance(x, Node) for x in factList]))
        #go down to the child, and remove it
        self._clear_last_node()
        lastInList = factList.pop()
        
        for node in factList:
            self._last_node = self._last_node.get(node)
            if self._last_node is None:
                return

        self._last_node.delete_node(lastInList)


    def _clear_last_node(self):
        """ Reset internal memory to point to the root.
        currently only used for retraction
        """
        self._last_node = self._root

    def queryS(self, s):
        """ Query a string """
        query = QP.parseString(s)
        return self.queryFact(query)

    def _reconstruct_query_from_trie(self):
        #TODO
        return False
    
    def queryFact(self, query):
        """ Query a TrieQuery instance """
        assert(isinstance(query, Query))
        self._clear_last_node()
        initial_context = Contexts.initial(self._root)
        return self._internal_query(query, initial_context)

    
    def _internal_query(self, query, ctxs):
        #Go down the trie, running each test as necessary
        #annotating contexts as necessary
        contexts = ctxs
        pos, neg = query.splitClauses()

        logging.debug("Testing clauses: {} {}".format(len(pos), len(neg)))
        for clause in pos:
            updated_contexts = self._match_clause(clause, contexts)
            updated_contexts = updated_contexts.set_all_alts(self._root)
            if bool(updated_contexts) is False:
                logging.debug("A positive clause is false")
                if clause.fallback is not None:
                    ##add it to all alts, set contexts = updated_contexts, continue
                    raise Exception("Unimplemented clause fallback")
                else:
                    contexts = updated_contexts
                break
            contexts = updated_contexts

        for negClause in neg:
            result = self._match_clause(negClause, contexts)
            logging.debug("neg result: {}".format(str(result)))
            if bool(result) is True:
                logging.debug("A Negative clause is true")
                contexts.fail()
                break
        
        return contexts
    
    def _match_clause(self, clause, contexts):
        assert(isinstance(clause, Clause))
        logging.debug("Testing Clause: {}".format(repr(clause)))
        #early exit:
        if not contexts:
            return contexts
        currentContexts = contexts
        #Go down from the root by query element:
        #Failure at any point means don't add the updated context

        #For each part of the clause, ie: .a in .a.b.c
        for c in clause.components:
            logging.debug("Testing node: {}".format(repr(c)))
            logging.debug("Current Contexts: {}".format(len(currentContexts)))
            alphas, betas, regexs = c.split_tests()
            newContexts = Contexts()
            
            #test each  active alternative
            for (data,lastNode) in currentContexts._alternatives:
                newData = None
                newNode = None
                newBindings = []
                tested = False
                #check exclusion status, should continue the loop if false
                b_exclusion_matches = Matching.exclusion_matches(c, lastNode)
                if b_exclusion_matches:
                    continue
                
                #compare non-bound value, returns (newNode, newData)?
                tested, newNode, newData = Matching.non_bind_value_match(c, lastNode,
                                                                 betas,
                                                                 regexs, data)

                if not tested:
                    #compare already bound value, returns (newNode, newData)?
                    tested, newNode, newData = Matching.existing_bind_match(c, lastNode,
                                                                    betas, regexs,
                                                                    data)

                if not tested:
                    #create new bindings as necessary, returns [(newNode, newData)]
                    newBindings = Matching.create_new_bindings(c, lastNode,
                                                               alphas, betas,
                                                               regexs, data)
                
                if newData is not None:
                    newContexts.append((newData, newNode))
                else:
                    newContexts._alternatives += [x for x in newBindings if x[0] is not None]

                #end of internal loop for an active alternative
                    
            #all alternatives tested for this clause component, update and progress
            currentContexts = newContexts

        #every alternative tested for each clause component, return the final set of contexts 
        return currentContexts

