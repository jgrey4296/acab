"""
The Acab type descriptor.
Essentially a wrapper around a sentence

Only provides type *instances* of primitives in abstract
Actual Type *checking* and *inference* is a module
"""
from uuid import uuid1
from copy import copy

from acab.abstract.core.value import AcabValue

from acab.config import AcabConfig

util = AcabConfig.Get()

TYPE_INSTANCE_S = util("Parsing.Structure", "TYPE_INSTANCE_S")
TYPE_FMT_S = util("Printing", "TYPE_FMT_S")
QUERY_HEAD_S = util("Parsing.Statements", "QUERY_HEAD_S")
TRANSFORM_HEAD_S = util("Parsing.Statements", "TRANSFORM_HEAD_S")
ACTION_HEAD_S = util("Parsing.Statements", "ACTION_HEAD_S")
RULE_HEAD_S = util("Parsing.Statements", "RULE_HEAD_S")

class TypeInstance(AcabValue):
    """ A Type Instance can be polytyped or monotyped """
    TypeCounter = 0

    @staticmethod
    def get_alias_chars():
        raise DeprecationWarning()

    def __init__(self, path, params=None):
        """ Construct a Type Instance with a _path in the type trie """
        raise DeprecationWarning()
        assert(params is None or all([isinstance(x, (str, AcabValue._type_system._instance_constructor))
                                      or hasattr(x, "type") for x in params])), breakpoint()
        super(TypeInstance, self).__init__(path,
                                           data=None,
                                           params=None, tags=None,
                                           name=None, _type=TYPE_INSTANCE_S)

        # TODO : Contracts
        self._contracts: List['TypeClass']

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
        path_match = self.value == other.value
        args_match = all([a == b for a, b in zip(self.vars, other.vars)])
        return path_match and args_match

    def __lt__(self, other):
        """
        Operator to form a partial order over all types
        ATOM is TOP
        TODO BOTTOM
        """
        assert(isinstance(other, TypeInstance))
        if self == AcabValue._type_system.BOTTOM:
            return True

        return False


    def __str__(self):
        path_str = str(self.value)

        params = ""
        if self._params:
            params = "({})".format(", ".join([str(x) for x in self._params]))

        # TODO: put this into util.config
        final = "τ:" + path_str
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
        return self.value
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

        if not bool(self._params):
            return self

        if the_dict is not None and index.is_var and index.name in the_dict:
            # TODO whats going on here
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
