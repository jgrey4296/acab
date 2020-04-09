from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.printing import util as PrU

from . import util
from .pyrule_type import Type


class TypeInstance(Type):
    """ A Type Instance can be polytyped or monotyped """
    TypeCounter = 0

    def __init__(self, name, args=None, type_str=util.TYPE_DEC_S):
        """ Construct a Type Instance with a name in the type trie """
        assert(isinstance(name, Sentence))
        assert(args is None or all([isinstance(x, (str, TypeInstance)) for x in args]))
        super(TypeInstance, self).__init__(name, type_str=type_str, params=args)

    def __hash__(self):
        return hash(str(self._name))

    # def __repr__(self):
    #     args = ""
    #     if self._vars:
    #         args = "({})".format(", ".join(repr(x) for x in self._vars))
    #     return "(::{}{})".format(self._name, args)

    # def __str__(self):
    #     return str(self._name)

    def copy(self):
        return TypeInstance(self._value, args=self.vars)

    def __eq__(self, other):
        # TODO: match inheritance
        if not other:
            return False
        assert(isinstance(other, TypeInstance))
        type_match = self._value == other._value
        args_match = all([a == b for a, b in zip(self.vars, other.vars)])
        return type_match and args_match

    def __lt__(self, other):
        """ Operator to form a partial order over all types """
        raise NotImplementedError()

    def build_type_declaration(self, the_dict):
        """ Given a type instance and a dictionary
        of values for variables, build a monotyped instance
        """
        index = self._value[-1].name
        instance_is_var = util.is_var(self._value[-1])
        if instance_is_var and index in the_dict:
            new_type = the_dict[index]
            return new_type.copy()

        new_args = []
        for x in self.vars:
            if isinstance(x, str) and x in the_dict:
                new_args.append(the_dict[x])
            else:
                assert(isinstance(x, TypeInstance))
                new_args.append(x)
        return TypeInstance(self._value, new_args)

    def var_set(self):
        obj = super(TypeInstance, self).var_set()
        if isinstance(self._name, PyRuleValue):
            name_set = self._name.var_set()
            obj['in'].update(name_set['in'])
            obj['in'].update(name_set['out'])
        return obj

    def copy(self):
        return TypeInstance(self._value, args=self.vars)

    def pprint(self, **kwargs):
        return PrU.print_value(self, **kwargs)
