""""
a formalisation of the Vygotsky/Engestrom
Activity Theory analytic pyramid

possible:
the hierarchy of Activities - Actions - Operations
"""
from py_rule.abstract.value import PyRuleValue

class Activity(PyRuleValue):
    """ Base Activity Description """

    def __init__(self):
        # constraints?
        self._actor              = None
        self._object             = None
        self._tool               = None
        # Description of actions resulting from activity?
        self._outcome            = None
        # Query?
        self._rules              = None
        # Template of roles?
        self._community          = None
        # Mapping of roles to tasks?
        self._division_of_labour = None

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise NotImplementedError()

    def copy(self):
        """ Data needs to be able to be copied """
        raise NotImplementedError()

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()

    def var_set(self):
        """ Data needs to be able to report internal variables """
        raise NotImplementedError()


