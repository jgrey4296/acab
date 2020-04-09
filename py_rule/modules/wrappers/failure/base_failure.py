"""
https://github.com/App-vNext/Polly
See taxonomies file

a Means to specify fault handling policies

is failure a lattice?
-------
Retry
Circuit-break
Timeout
Bulkhead
Cache
Fallback

degrade
fix
cancel
alternate

-------

a.retry.times.3

wrap an.action with a.retry then cancel
wrap an.action with a.timeout then degrade



"""
from py_rule.abstract.value import PyRuleValue

class Failure(PyRuleValue):
    """ A Description of how a task can fail """

    def __init__(self):
        return

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise NotImplementedError()

    @property
    def var_set(self):
        """ Data needs to be able to report internal variables """
        raise NotImplementedError()


    def copy(self):
        """ Data needs to be able to be copied """
        raise NotImplementedError()

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()
