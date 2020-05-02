"""
Defines the required functions a type needs to implement
"""
from .pyrule_type import Type

class TypeClass(Type):
    """ Definition of a coherent collection of functions """

    def __init__(self):
        super(TypeClass, self).__init__()



class TypeClassInstance(Type):
    """ A Pairing of a type, with a type class it implements """

    def __init__(self):
        super(TypeClassInstance, self).__init__()
