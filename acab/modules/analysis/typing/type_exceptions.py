"""
Exception types raised by type checking
"""
from acab.error.acab_base_exception import AcabBaseException

class AcabTypingException(AcabBaseException):

    def __init__(self):
        return


class TypeRedefinitionException(AcabTypingException):

    def __init__(self, typename):
        self.typename = typename

    def __str__(self):
        return "Type Redefinition Attempt: {}".format(self.typename)

    __repr__ = __str__

class TypeConflictException(AcabTypingException):

    def __init__(self, env_type, new_type, stmt):
        self._env_type = env_type
        self._new_type = new_type
        self._stmt = stmt

    def __str__(self):
        return "Exception: Expected ({}) but got ({}) in {} ".format(self._env_type,
                                                                     self._new_type,
                                                                     self._stmt)

    __repr__ = __str__

class TypeUndefinedException(AcabTypingException):

    def __init__(self, attempted_type, stmt):
        self.attempted_type = attempted_type
        self.stmt = stmt

    def __str__(self):
        return "Exception: Attempted to declare as missing type {} in {}".format(self.attempted_type,
                                                                                 self.stmt)

class TypeVariableConflictException(AcabTypingException):

    def __init__(self, node_path):
        self.node_path = node_path

    def __str__(self):
        return "Node specified as both a var and not a var: {}".format("".join(self.node_path))

class TypeStructureMismatch(AcabTypingException):

    def __init__(self, typename, conflicts):
        self.typename = typename
        self.conflicts = conflicts

    def __str__(self):
        return "{} Structure Mismatch: {}".format(str(self.typename),
                                                  ", ".join([str(x) for x in self.conflicts]))
