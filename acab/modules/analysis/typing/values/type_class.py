"""
Defines the required functions a type needs to implement
"""
from .acab_type import TypeStatement

class TypeClass(TypeStatement):
    """ Definition of a coherent collection of functions """

    def __init__(self):
        super(TypeClass, self).__init__()



class TypeClassInstance():
    """ A Pairing of a type, with a type class it implements """

    def __init__(self):
        super(TypeClassInstance, self).__init__()
