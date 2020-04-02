from py_rule.abstract.value import PyRuleValue

from . import util
from .pyrule_type import Type

class TypeInstance(Type):
    """ A Type Instance can be polytyped or monotyped """
    TypeCounter = 0

    def __init__(self, name=None, args=None, type_str=util.TYPE_DEC_S):
        """ Construct a Type Instance with a name in the type trie """
        super(TypeInstance, self).__init__(type_str=type_str)
        if name is not None:
            self._name = name
        if args is not None:
            self._vars += args

    def __hash__(self):
        return hash(str(self._name))

    def __repr__(self):
        args = ""
        if self._vars:
            args = "({})".format(", ".join(repr(x) for x in self._vars))
        return "(::{}{})".format(self._name, args)

    def __str__(self):
        return str(self._name)

    def __eq__(self, other):
        # TODO: match inheritance
        if not other:
            return False
        type_match = type(self) == type(other)
        name_match = self._name == other._name
        args_match = all([a == b for a, b in zip(self._vars, other._vars)])
        return type_match and name_match and args_match

    def __lt__(self, other):
        """ Operator to form a partial order over all types """
        raise NotImplementedError()

    def build_type_declaration(self, the_dict):
        """ Given a type instance and a dictionary
        of values for variables, build a monotyped instance
        """
        if str(self) in the_dict:
            new_type = the_dict[str(self)]
            return TypeInstance(new_type._name,
                                new_type._vars)

        new_args = [the_dict[x] if x in the_dict
                    else x for x in self._vars]
        return TypeInstance(self._name, new_args)

    def var_set(self):
        obj = super(TypeInstance, self).var_set()
        if isinstance(self._name, PyRuleValue):
            name_set = self._name.var_set()
            obj['in'].update(name_set['in'])
            obj['in'].update(name_set['out'])
        return obj
