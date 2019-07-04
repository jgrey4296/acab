from .environment import Environment




class TypeChecker:
    """ Abstract Class for Type Checking """

    def __init__(self):
        self._definitions = None
        self._declarations = None
        self._variables = None
        self._stack = []


    def __call__(self, data):
        definitiosn, rules, assertions = data
        #add definitions

        #add the assertions

        #add the rules one by one

    def pop(self):
        """ Pop a typechecking context """
        raise Exception("Not Implemented")

    def push(self):
        """ Push a typechecking context """
        raise Exception("Not Implemented")

    def query(self, sen):
        """ Get the type of a sentence leaf """
        raise Exception("Not Implemented")


    def validate(self):
        raise Exception("Not Implemented")
