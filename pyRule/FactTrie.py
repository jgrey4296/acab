from .FactNode import FactNode, EXOP
from . import FactParse as FP
from .QueryParser import parseString as queryParse
from .TrieContexts import TrieContexts
from .TrieQuery import TrieQuery
from .utils import Clause, EXOP
from .Comparisons import COMP_LOOKUP

class FactTrie:

    def __init__(self):
        self._root = FactNode.Root()
        self._last_node = self._root

    def assertSMulti(self,s):
        parsed = FP.parseStrings(s)
        for x in parsed:
            self.assertFact(x)
        
    def assertS(self,s):
        self.assertFact(FP.parseString(s))

    def retractS(self,s):
        self.retractFact(FP.parseString(s))
        
    def assertFact(self, factList):
        assert(all([isinstance(x, FactNode) for x in factList]))
        assert(factList[0].is_root())
        self._clear_last_node()
        for newNode in factList[1:]:
            self._last_node = self._last_node.insert(newNode)


    def retractFact(self, factList):
        assert(all([isinstance(x, FactNode) for x in factList]))
        assert(factList[0].is_root())
        #go down to the child, and remove it
        self._clear_last_node()
        lastInList = factList.pop()
        
        for node in factList[1:-1]:
            self._last_node = self._last_node.get(node)
            if self._last_node is None:
                return

        self._last_node.delete_node(lastInList)


    def _clear_last_node(self):
        self._last_node = self._root

    def queryS(self, s):
        query = queryParse(s)
        return self.queryFact(query)
        
    def queryFact(self, query):
        assert(isinstance(query, TrieQuery))
        self._clear_last_node()
        initial_context = TrieContexts.initial(self._root)
        return self._internal_query(query, initial_context)

    
    def _internal_query(self, query, ctxs):
        #Go down the trie, running each test as necessary
        #annotating contexts as necessary
        contexts = ctxs
        for clause in query._clauses:
            contexts = self._match_clause(clause, contexts)
            contexts = contexts.set_all_alts(self._root)
            
            
        #todo: negative clauses
        
        return contexts
    
    def _match_clause(self, clause, contexts):
        assert(isinstance(clause, Clause))
        #early exit:
        if not contexts:
            return contexts
        currentContexts = contexts
        #Go down from the root by query element:
        #failure means remove the context
        for c in clause.components:
            alphas, betas = self.split_alpha_and_beta_tests(c.comps)
            
            newContexts = TrieContexts()
            for (data,lastNode) in currentContexts._alternatives:
                #check exclusion status
                if c.op is EXOP.EX and len(lastNode) != 1:
                    continue

                #check value, if its not a bind
                if c.value != None:
                    if c.value not in lastNode._children:
                        continue
                    else:
                        #can only run beta tests
                        if self.test_betas(c.value, betas, data):
                            newNode = lastNode._children[c.value]
                            newContexts._alternatives.append((data.copy(),newNode))
                        continue
                    
                else: #is bind
                    assert(c.bind != None)
                    if c.bind.value in data: #already bound
                        if data[c.bind.value] not in lastNode._children:
                            #already bound but doesnt match
                            continue
                        else:
                            #already bound, does match
                            newNode = lastNode._children[data[c.bind.value]]
                            newContexts._alternatives.append((data.copy(), newNode))
                            continue
                    else: #not already bound
                        #get potentials
                        potentials = lastNode._children.keys()
                        #filter by running alphas and betas
                        passing = [x for x in potentials if self.test_alphas(x, alphas) and self.test_betas(x, betas, data)]
                        #bind and store successes
                        for x in passing:
                            newNode = lastNode._children[x]
                            newData = data.copy()
                            newData[c.bind.value] = x
                            newContexts._alternatives.append((newData,newNode))

            currentContexts = newContexts

        return currentContexts

    def split_alpha_and_beta_tests(self, comps):
        alphas = [x for x in comps if x.value != None]
        betas = [x for x in comps if x.bind != None]
        return (alphas, betas)
                        
    def test_alphas(self, value, comps):
         return all([COMP_LOOKUP[x.op](value,x.value) for x in comps])

    def test_betas(self, value, comps, data):
        return all([COMP_LOOKUP[x.op](value,data[x.bind.value]) for x in comps])
                    
                        
               
