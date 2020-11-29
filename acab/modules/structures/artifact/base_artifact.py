"""
A Means of representing how physical artifacts are used
both instrumentally and symbolically in the world
and how they are specified, constrained and constructed

   raw_materials
+  construction_tools   :
+  construction_actions :
*  construction_time    :
*  variance             :
------------------------
=  tool


"""
from acab.abstract.core.values import AcabValue

class Artifact(AcabValue):
    """ Description of a physical artifact """

    def __init__(self):
        # innate properties
        self._properties           = None
        # limitations
        self._constraints          = None
        # actions and their consequenes
        self._core_actions         = None
        # how the artifact is constructed
        self._construction         = None
        self._specifications       = None
        self._conventions          = None
        self._degradation          = None

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise NotImplementedError()

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()

    def var_set(self):
        """ Data needs to be able to report internal variables """
        raise NotImplementedError()
