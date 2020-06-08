from py_rule.abstract.sentence import Sentence
from py_rule.error.pyrule_parse_exception import PyRuleParseException
from py_rule.util import STRUCTURE_S, VALUE_TYPE_S, NAME_S
from py_rule.abstract.printing import util as PrU

from .pyrule_type import TypeStatement
from .type_instance import TypeInstance
from py_rule.modules.analysis.typing.util import TYPE_DEF_S, TYPE_DEC_S

PrU.register_statement({TYPE_DEF_S : STRUCTURE_S})
# TODO register class

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
        super().__init__(None, params=params, type_str=type_str)
        self._structure = []

        if bool(structure):
            self._structure += structure

        self.unify_structure_variables()

    def __eq__(self, other):
        path_eq = self.path == other.path
        structure_len = len(self.structure) == len(other.structure)
        structure_eq = [x == y for x,y in zip(self.structure, other.structure)]

        return path_eq and structure_len and structure_eq


    @property
    def structure(self):
        return self._structure

    @property
    def var_set(self):
        obj = super(TypeDefinition, self).var_set
        for s in self.structure:
            temp = s.var_set
            obj['in'].update(temp['in'])
            obj['out'].update(temp['out'])

        return obj


    def build_type_instance(self, the_dict=None):
        just_path, statement = self.path.detach_statement()
        assert(statement is not None)

        if the_dict is None:
            return TypeInstance(just_path, params=self.vars)

        new_args = []
        for x in self.vars:
            if isinstance(x, PyRuleValue) and x.name in the_dict:
                new_args.append(the_dict[x.name])
            else:
                assert(isinstance(x, TypeInstance))
                new_args.append(x)

        return TypeInstance(just_path, params=new_args)


    def verify(self):
        input_vars = set(self.vars)
        for s in self.structure:
            temp = s.var_set
            input_vars.difference_update(temp['in'])
            input_vars.difference_update(temp['out'])

        if bool(input_vars):
            raise PyRuleParseException()

    def unify_structure_variables(self):

        # unify shared variables across structure sentences to have the same type
        # go through all sentences
        variables = {}
        for sentence in self.structure:
            # track variables
            var_words = [x for x in sentence if x.is_var]
            missing_vars = [x.value for x in var_words if x.value not in variables]
            variables.update({x: {'types': set(), 'instances': []} for x in missing_vars})

            for word in var_words:
                variables[word.value]['instances'].append(word)
                    # find variables with type annotations
                if TYPE_DEC_S in word._data:
                    variables[word.value]['types'].add(word._data[TYPE_DEC_S])

        # Then unify all the variables to have the same type
        for the_dict in variables.values():
            types, instances = the_dict.values()
            assert(len(types) < 2)
            if bool(types):
                type_instance = types.pop()
                for word in instances:
                    word._data[TYPE_DEC_S] = type_instance
