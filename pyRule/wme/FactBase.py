from .WME import WME
from pyRule.Contexts import Contexts
from pyRule.Query import Query
from pyRule.Clause import Clause

#todo: eq, str, 

class FactBase:
    """ Main class for WME based knowledge base """
    
    def __init__(self ):
        self._wmes = []
        self._wmeDict = {}
        self._hashes = set()
        self._currentTime = 0

    def clear(self):
        self._wmes = []
        self._wmeDict = {}
        self._hashes = set()
        self._currentTime = 0

    def incTime(self):
        self._currentTime += 1
        
    def assertWME(self, *args):
        """ Put a fact into the knowledge base """
        outputData = []
        for possibleData in args:
            if not isinstance(possibleData,WME):
                wme = WME(possibleData, self._currentTime)
            else:
                wme = possibleData
            wmeHash = hash(wme)
            if wmeHash in self._hashes:
                raise Exception("Assert WME Failed: WME already asserted")
            self._wmes.append(wme)
            self._wmeDict[wmeHash] = wme
            self._hashes.add(wmeHash)
            assert(len(self._wmes) == len(self._hashes))
            outputData.append(wme)
        return outputData
        
    def retractWME(self, wme):
        """ Remove a fact from the knowledge base """
        if not isinstance(wme,WME):
            raise Exception("Retracting WME Failed: Not passed a WME")
        wmeHash = hash(wme)
        if wmeHash not in self._hashes:
            return 0
        self._wmes = [x for x in self._wmes if hash(x) != wmeHash]
        del self._wmeDict[wmeHash]
        self._hashes.remove(wmeHash)
        assert(len(self._wmes) == len(self._hashes))
        assert(len(self._wmeDict) == len(self._wmes))
        return 1
        
    def query(self, query):
        """ Given a query of clauses comprising:
        alpha, binding, and beta tests, run it and return
        any matching wmes and bindings """
        contexts = Contexts.initial()
        for clause in query._clauses:
            #pass the clause and intermediate results through
            contexts = self._matchWMEs(clause,contexts)

        #then check negative clauses
        negContext = contexts
        for clause in query._negatedClauses:
            #test each negated clause,
            #fail the query if any pass
            negResponse = self._matchWMEs(clause,negContext)
            if negResponse:
                contexts.fail()
                break
        
        contexts.verifyMatches(len(query))
        return contexts
            

    def _matchWMEs(self, clause, contexts):
        """ Internal match procedure. 
        Searches all wmes, running alpha tests,
        then bindings, then beta comparisons,
        before adding passing wmes to the context """

        #Early fail out
        if not contexts:
            return contexts

        (alphaTests, bindOps, betaTests) = clause
        passingContexts = Contexts()
        for wme in self._wmes:
            #Alpha Tests
            if not self._test_alpha(wme, alphaTests):
                continue
            
            #bind
            newContexts = self._bind_values(wme, bindOps, contexts) 
            if len(newContexts) == 0:
                continue
            
            #Beta Tests
            passingContexts._alternatives += self._test_beta(wme, newContexts, betaTests)._alternatives
                             
        return passingContexts

    def _bind_values(self, wme, bindOps, contexts):
        """ Add in a new binding to each context, unless it conflicts """
        newContexts = Contexts()
        for (data,matchedWMEs) in contexts._alternatives:
            failContext = False
            newData = data.copy()
            newMatchedWMEs = matchedWMEs.copy()
            for (field, bindName) in bindOps:
                if field not in wme._data:
                    failContext = True
                    break
                if bindName in newData and wme._data[field] != newData[bindName]:
                    failContext = True
                    break
                newData[bindName] = wme._data[field]

            if not failContext:
                newMatchedWMEs.append(wme)
                newContexts._alternatives.append((newData,newMatchedWMEs))
            else:
                continue

        return newContexts
        
    
    def _test_alpha(self, wme, alphaTests):
        """ Run alpha tests (intra-wme) """
        #todo: also test intra-wme tests
        for (field, op, val) in alphaTests:
            if not field in wme._data:
                return False
            if not op(wme._data[field], val):
                return False
        return True

    def _test_beta(self, wme, contexts, betaTests):
        """ Run beta (inter-wme) tests on a wme and context """
        newContexts = Contexts()
        for (data, matchedWMEs) in contexts._alternatives:
            failMatch = False
            newData = data.copy()
            newMatchedWMEs = matchedWMEs.copy()
            for (field, op, bindName) in betaTests:
                #compare wme to token
                if "#" not in field and field not in wme._data:
                    failMatch = True
                    break
                elif "#" not in field and not op(wme._data[field],newData[bindName]):
                    failMatch = True
                    break
                #compare token to token
                elif "#" in field and not op(newData[field], newData[bindName]):
                    failMatch = True
                    break
                
            if not failMatch:
                newContexts._alternatives.append((newData,newMatchedWMEs))
                                
        return newContexts
        
    
    def __len__(self):
        """ The number of wmes in the factbase """
        return len(self._hashes)
    
