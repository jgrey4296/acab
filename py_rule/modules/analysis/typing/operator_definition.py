from .type_definition import TypeDefinition
from .type_instance import TypeInstance
from py_rule.abstract.sentence import Sentence
from py_rule.util import BIND_S, FUNC_S, VALUE_TYPE_S
from .util import OP_DEF_S


class OperatorDefinition(TypeDefinition):
    """ Defines the type signature of an operator"""

    def __init__(self, structure, sugar_syntax=None):
        """ The name of an operator and its type signature,
        with the binding to a ProductionOperator that is
        syntax sugared, and its inline place"""
        # eg: operator.+.$x(::num).$y(::num).$z(::num).num_plus
        super().__init__([structure], type_str=OP_DEF_S)
        self._func_name = sugar_syntax

    def __str__(self):
        result = FUNC_S + "::"
        result += str(self._name)
        if bool(self._vars):
            result += "({})".format(", ".join([str(x) for x in self._vars]))
        result += ":"
        result += str(self._structure[0])
        if self._func_name is not None:
            result += " => {}".format(self._func_name)

        return result

    def __repr__(self):
        return "Type Def({})".format(str(self))

    def build_type_declaration(self):
        return TypeInstance(self._name, self._path, self._vars[:])

