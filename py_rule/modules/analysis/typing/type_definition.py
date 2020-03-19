from .pyrule_type import Type
from .type_instance import TypeInstance
from py_rule.util import BIND_S, STRUCTURE_S
from py_rule.abstract.sentence import Sentence

class TypeDefinition(Type):
    """ Defines the Structure of a type """

    def __init__(self, name, path, structure, tvars):
        """ Structure creates the dict of locations.
        Only leaves get type anotations. Thus:
        { .a.$x :: String, .b.$c :: Num, .d!$e::Location }
        """
        # The name is the location. eg: .types.person
        assert isinstance(name, str)
        assert isinstance(path, Sentence)
        assert isinstance(structure, list)
        assert all([isinstance(x, Sentence) for x in structure])
        assert isinstance(tvars, list)

        self._name = name
        self._path = path
        # TODO unify shared variables across structure sentences to have
        # the same type
        self._structure = structure
        self._vars = []
        if tvars is not None:
            assert(all([x._data[BIND_S] for x in tvars]))
            # TODO check all tvars are in structure sentences
            self._vars += tvars

    def __str__(self):
        result = STRUCTURE_S + "::"
        result += str(self._name)
        if bool(self._vars):
            result += "({})".format(", ".join([str(x) for x in self._vars]))
        result += ":\n\n"
        result += "\n".join([str(x) for x in self._structure])
        result += "\n\nEND"
        return result

    def __repr__(self):
        return "Type Def({})".format(str(self))

    def build_type_declaration(self):
        return TypeInstance(self._name, self._path, self._vars[:])
