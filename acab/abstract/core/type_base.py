"""
The Base Types of Acab.
Only Provides automatic *primitives*.
Manual typing and Product/Sum types are implemented
in the separate typing module.
"""
from uuid import uuid1
from copy import copy

from acab.config import AcabConfig

util = AcabConfig.Get()

TYPE_FMT_S = util("Printing", "TYPE_FMT_S")
QUERY_HEAD_S = util("Parsing.Statements", "QUERY_HEAD_S")
TRANSFORM_HEAD_S = util("Parsing.Statements", "TRANSFORM_HEAD_S")
ACTION_HEAD_S = util("Parsing.Statements", "ACTION_HEAD_S")
RULE_HEAD_S = util("Parsing.Statements", "RULE_HEAD_S")
PRIMITIVE_SIGNIFIER = util("Printing", "PRIMITIVE_SIGNIFIER")

class TypeInstance:
    """ A Type Instance can be polytyped or monotyped """
    TypeCounter = 0
    _type_system = None

    @staticmethod
    def _set_type_system(type_system):
        TypeInstance._type_system = type_system

    @staticmethod
    def get_alias_chars():
        sigils = [x[-1]._primitive._type_alias for x in TypeInstance._type_system.primitives]
        sigils_str = "".join([x for x in sigils if x is not None])
        return sigils_str

    def __init__(self, path, params=None, type_alias_str=None):
        """ Construct a Type Instance with a _path in the type trie """
        assert(params is None or all([isinstance(x, (str, TypeInstance)) or hasattr(x, "type") for x in params])), breakpoint()
        self._uuid = uuid1()
        self._path = path
        self._type_alias = type_alias_str
        self._params = []

        if params is not None:
            self._params += params

    def __hash__(self):
        return hash(self.path)

    def __eq__(self, other):
        """
        Two Type Instances are equal if have the same path,
        and if they have matching parameters
        """
        if not (other or isinstance(other, TypeInstance)):
            return False

        # TODO may need to handle type aliases
        path_match = self._path == other._path
        args_match = all([a == b for a, b in zip(self.vars, other.vars)])
        return path_match and args_match

    def __lt__(self, other):
        """
        Operator to form a partial order over all types
        ATOM is TOP
        TODO BOTTOM
        """
        assert(isinstance(other, TypeInstance))
        if self == ATOM:
            return True

        return False


    def __str__(self):
        path_strs = [str(x) for x in self._path]

        if self._type_alias is not None:
            path_strs = [self._type_alias]

        params = ""
        if self._params:
            params = "({})".format(", ".join([str(x) for x in self._params]))

        prim = ""
        if self._primitive:
            prim = PRIMITIVE_SIGNIFIER


        final = ".".join([prim] + path_strs)
        final += params
        return final

    def __repr__(self):
        return "(TypeInstance {})".format(str(self))


    def __call__(self, node, data=None, engine=None):
        """
        An example light weight annotation for querying
        This *could* be wrapped in a ProductionOperator, or a QueryOperator,
        but this way requires less setup
        """
        return self == node.value.type


    @property
    def path(self):
        return self._path
    @property
    def head(self):
        return self.path[-1]
    @property
    def vars(self):
        return self._params
    @property
    def var_set(self):
        raise NotImplementedError()


    def build_type_instance(self, the_dict=None):
        """ Given a type instance and a dictionary
        of values for variables, build a monotyped instance
        ie: treat instance as a function
        """
        index = self.path[-1]

        if self._primitive:
            return self

        if the_dict is not None and index.is_var and index.name in the_dict:
            new_type = the_dict[index.name]
            return new_type.copy()

        if the_dict is None:
            return copy(self)

        new_args = []
        for x in self.vars:
            if isinstance(x, str) and x in the_dict:
                new_args.append(the_dict[x])
            else:
                new_args.append(x)

        return TypeInstance(self.path, params=new_args)


    def pprint(self, opts=None):
        raise DeprecationWarning("Use Print Semantics")
