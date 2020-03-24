from .pyrule_type import Type
from .type_instance import TypeInstance
from .util import TYPE_DEF_S
from py_rule.util import BIND_S, STRUCTURE_S, VALUE_TYPE_S
from py_rule.abstract.sentence import Sentence

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
        self._vars = []

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
        return TypeInstance(self._name, args=self._vars[:])

    def value_string(self):
        return self._name
