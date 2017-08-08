from .utils import Clause
import IPython
#https://ipython.readthedocs.io/en/stable/config/options/terminal.html

#in shell: ipython --simple-prompty --matplotlib

class TrieQuery:
    """ A Query for the Trie Knowledge base """

    def __init__(self, *args, negated=False):
        #ATTENTION: List of clauses, not List of tuples
        #[Clause]
        #Each clause is a list of tests and bindings
        self._clauses = []
        for clause in args:
            assert(isinstance(clause, Clause))
            self._clauses.append(clause)

    def __len__(self):
        return len(self._clauses)
