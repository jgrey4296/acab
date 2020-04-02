from .pyrule_type import Type
from .type_instance import TypeInstance
from .util import TYPE_DEF_S
from py_rule.util import BIND_S, STRUCTURE_S, VALUE_TYPE_S
from py_rule.abstract.sentence import Sentence
from py_rule.error.pyrule_parse_exception import PyRuleParseException

class TypeDefinition(Type):
    """ Defines the Structure of a type """

    def __init__(self, structure, type_str=TYPE_DEF_S):
        """ Structure creates the dict of locations.
        Only leaves get type anotations. Thus:
        { .a.$x :: String, .b.$c :: Num, .d!$e::Location }
        """
        # The name is the location. eg: .types.person
        assert isinstance(structure, list)
        assert all([isinstance(x, Sentence) for x in structure])
        super().__init__(type_str=type_str)
        # TODO unify shared variables across structure sentences to have
        # the same type
        self._structure = structure

    def __str__(self):
        result = self._name
        result += "(::{}):\n".format(STRUCTURE_S)
        if bool(self._vars):
            result += "\t | {} | \n".format(", ".join([str(x) for x in self._vars]))
        result += "\t"
        result += "\n\t".join([str(x) for x in self._structure])
        result += "\nEND"
        return result

    def __repr__(self):
        return "Type Def({})".format(str(self))

    def build_type_declaration(self):
        return TypeInstance(self._name)

    def value_string(self):
        return self._name

    def var_set(self):
        obj = super(TypeDefinition, self).var_set()
        for s in self._structure:
            temp = s.var_set()
            obj['in'].update(temp['in'])
            obj['out'].update(temp['out'])

        return obj

    def verify(self):
        vars = set(self._vars)
        for s in self._structure:
            temp = s.var_set()
            vars.difference_update(temp['in'])
            vars.difference_update(temp['out'])

        if bool(vars):
            raise PyRuleParseException()
