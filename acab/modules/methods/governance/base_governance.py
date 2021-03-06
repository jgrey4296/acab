"""
An Organisation and Governance description DSL
specifies the structure of an organisation,
its roles, activities,
procedures for change,

and how its IGU functions with regards to
admin, regulation, certification, information,
enforcement and gatekeeping

Structure
Requirements


"""
from acab.abstract.value import AcabValue

class GovernanceDescription(AcabValue):
    """ A Description of how a governing organisation operates """

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


    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()
