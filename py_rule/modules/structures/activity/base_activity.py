""""
a formalisation of the Vygotsky/Engestrom
Activity Theory analytic pyramid

possible:
the hierarchy of Activities - Actions - Operations

"law and order".community.[[ bystanders, legislators, lawmen, victims, conspirators, criminals ]]


"law and order".activity.crime.violence.[[
  community: [ bystanders, legislators, lawmen, victims, conspirators, criminals ]

  conflicts: [ bystanders | lawmen | victims | criminals ]

  dol.bystanders.[[ witness ]]
  dol.lawmen.[[ interfere($criminal), arrest($criminal) ]]
  dol.criminals.[[ do(harm, $victim) ]]
  dol.victims.[[ interfere(criminals) ]]

  artifacts.[[ $object(::physical) ]]

  rules.[[ society.crime.violence -< $x? ]]

  metrics.[[ bystander: ~victim?,
   lawmen: catch.$criminal?,
   criminal: ~$lawmen.catch.$criminal?,
   victim: survive?
]]

]]



"""
from py_rule.abstract.value import PyRuleValue

class ActivityField(PyRuleValue):
    """ Describe an Activity Field """

    def __init__(self):
        # Query
        self._rules              = None
        # Template of roles
        self._community          = None
        # Mapping of roles to action fields
        self._division_of_labour = None
        # Mapping of roles to metrics
        self._metrics            = None

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
        # instantiate the field with actual actors 
        raise NotImplementedError()



class ActionField(PyRuleValue):
    """ Collection Core actions together for use """

    def __init__(self):
        self._actor = None
        self._actions = []

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
        # instantiate the field with actual actors 
        raise NotImplementedError()


