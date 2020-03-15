from .type_definition import TypeDefinition
from .type_instance import TypeInstance
from py_rule.util import BIND_S


class OperatorDefinition(TypeDefinition):
    """ Defines the type signature of an operator"""

    def __init__(self, name, path, structure, tvars, func_name, op_str):
        """ The name of an operator and its type signature,
        with the binding to a ProductionOperator that is
        syntax sugared, and its inline place"""
        # eg: operator.+.$x(::num).$y(::num).$z(::num).num_plus
        # TODO check up on tvars and implement them here
        super().__init__(name, path, structure, tvars)
        self._func_name = func_name
        self._op_str = op_str


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