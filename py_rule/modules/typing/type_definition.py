from .pyrule_type import Type
from .type_instance import TypeInstance
from py_rule.util import BIND_S


class TypeDefinition(Type):
    """ Defines the Structure of a type """

    def __init__(self, name, path, structure, tvars):
        """ Structure creates the dict of locations.
        Only leaves get type anotations. Thus:
        { .a.$x :: String, .b.$c :: Num, .d!$e::Location }
        """
        # The name is the location. eg: .types.person
        self._name = name
        self._path = path
        self._structure = structure
        # TODO: add function signatures?
        self._vars = []
        if tvars is not None:
            assert(all([x._data[BIND_S] for x in tvars]))
            self._vars += tvars

    def __str__(self):
        result = "::"
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
