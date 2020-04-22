from py_rule.abstract.sentence import Sentence
from py_rule.error.pyrule_parse_exception import PyRuleParseException
from py_rule.util import STRUCTURE_S, VALUE_TYPE_S, NAME_S
from py_rule.abstract.printing import util as PrU

from .pyrule_type import TypeStatement
from .type_instance import TypeInstance
from .util import TYPE_DEF_S, TYPE_DEC_S


class TypeDefinition(TypeStatement):
    """ Defines the Structure of a type """

    def __init__(self, structure, params=None, type_str=TYPE_DEF_S):
        """ Structure creates the dict of locations.
        Only leaves get type anotations. Thus:
        { .a.$x :: String, .b.$c :: Num, .d!$e::Location }
        """
        # The name is the location. eg: .types.person
        assert isinstance(structure, list)
        assert all([isinstance(x, Sentence) for x in structure])
        super().__init__(structure, params=params, type_str=type_str)

        # unify shared variables across structure sentences to have the same type
        # go through all sentences
        variables = {}
        for sentence in self.structure:
            # track variables
            for word in sentence:
                if word.is_var:
                    if word._value not in variables:
                        variables[word._value] = {'types': set(), 'instances': []}
                    variables[word._value]['instances'].append(word)
                    # find variables with type annotations
                    if TYPE_DEC_S in word._data:
                        variables[word._value]['types'].add(word._data[TYPE_DEC_S])

        # Then unify all the variables to have the same type
        for the_dict in variables.values():
            types, instances = the_dict.values()
            assert(len(types) < 2)
            if bool(types):
                type_instance = types.pop()
                for word in instances:
                    word._data[TYPE_DEC_S] = type_instance

    @property
    def structure(self):
        return self._value

    @property
    def var_set(self):
        obj = super(TypeDefinition, self).var_set
        for s in self.structure:
            temp = s.var_set
            obj['in'].update(temp['in'])
            obj['out'].update(temp['out'])

        return obj


    def build_type_declaration(self):
        just_path = self.path.copy()
        if just_path[-1]._data[VALUE_TYPE_S] == TYPE_DEF_S:
            just_path[-1]._data[VALUE_TYPE_S] = NAME_S
            just_path[-1]._value = self.name
        return TypeInstance(just_path, args=self.vars)

    def verify(self):
        input_vars = set(self.vars)
        for s in self.structure:
            temp = s.var_set
            input_vars.difference_update(temp['in'])
            input_vars.difference_update(temp['out'])

        if bool(input_vars):
            raise PyRuleParseException()


    def pprint(self, is_structured=True, **kwargs):
        return super(TypeDefinition, self).pprint(is_structured=is_structured,
                                                  **kwargs)
