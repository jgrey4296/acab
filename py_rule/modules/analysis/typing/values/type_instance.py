from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.printing import util as PrU

from py_rule.modules.analysis.typing import util
from .pyrule_type import Type


class TypeInstance(Type):
    """ A Type Instance can be polytyped or monotyped """
    TypeCounter = 0

    def __init__(self, path, params=None, type_str=util.TYPE_DEC_S):
        """ Construct a Type Instance with a _path in the type trie """
        assert(isinstance(path, Sentence))
        assert(params is None or all([isinstance(x, (str, TypeInstance, PyRuleValue)) for x in params]))
        super(TypeInstance, self).__init__(path, type_str=type_str, params=params)

    def __hash__(self):
        return hash(str(self._name))

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


    @property
    def var_set(self):
        obj = super(TypeInstance, self).var_set
        if isinstance(self._name, PyRuleValue):
            name_set = self._name.var_set
            obj['in'].update(name_set['in'])
            obj['in'].update(name_set['out'])
        return obj


    def build_type_instance(self, the_dict=None):
        """ Given a type instance and a dictionary
        of values for variables, build a monotyped instance
        """
        index = self.path[-1]

        if the_dict is not None and index.is_var and index.name in the_dict:
            new_type = the_dict[index.name]
            return new_type.copy()

        if the_dict is None:
            return self.copy()

        new_args = []
        for x in self.vars:
            if isinstance(x, PyRuleValue) and x.name in the_dict:
                new_args.append(the_dict[x.name])
            else:
                assert(isinstance(x, TypeInstance))
                new_args.append(x)

        return TypeInstance(self.path, params=new_args)


    def pprint(self, opts=None):
        return "::{}".format(self._value.pprint(opts))

# TODO Make a type instance query op
