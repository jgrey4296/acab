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
from acab.core.value.value import AcabValue

class Failure(AcabValue):
    """ A Description of how a task can fail """

    def __init__(self):
        return

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise NotImplementedError()

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()
