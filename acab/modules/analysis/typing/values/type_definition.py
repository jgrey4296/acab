from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence

from acab.error.acab_parse_exception import AcabParseException

from acab.modules.analysis.typing.util import TYPE_DEFINITION, SUM_DEFINITION, TYPE_DEF_S
from acab.modules.analysis.typing import type_exceptions as TE

from acab.abstract.config.config import AcabConfig

from .acab_type import TypeStatement

util = AcabConfig.Get()

PRIMITIVE_S     = util.value("Typing.Primitives", "PRIMITIVE")
TYPE_INSTANCE_S = util.value("Parse.Structure", "TYPE_INSTANCE")
NAME_S          = util.value("Parse.Structure", "NAME")


# TODO register class

class TypeDefinition(TypeStatement):
    """ Defines the Structure of a Product type """

    def __init__(self, structure, params=None, data=None):
        """ Structure creates the dict of locations.
        Only leaves get type anotations. Thus:
        { .a.$x :: String, .b.$c :: Num, .d!$e::Location }
        """
        # The name is the location. eg: .types.person
        assert isinstance(structure, list)
        assert all([isinstance(x, Sentence) for x in structure])
        if data is None:
            data = {}
        if TYPE_INSTANCE_S not in data:
            data[TYPE_INSTANCE_S] = TYPE_DEFINITION

        super().__init__(None, params=params, data=data)

        if bool(structure):
            self._structure += structure

        self.unify_structure_variables()

    def __eq__(self, other):
        path_eq = self.path == other.path
        structure_len = len(self.structure) == len(other.structure)
        structure_eq = all([x == y for x,y in zip(self.structure, other.structure)])

        return path_eq and structure_len and structure_eq


    def set_primitive(self):
        self.set_data({PRIMITIVE_S: True})
        return self

    @property
    def var_set(self):
        obj = super(TypeDefinition, self).var_set
        for s in self.structure:
            temp = s.var_set
            obj['in'].update(temp['in'])
            obj['out'].update(temp['out'])

        return obj


    def build_type_instance(self, the_dict=None):
        just_path = self.path
        statement = self

        if the_dict is None:
            return AcabValue._sentence_constructor(just_path, params=self.vars)

        new_args = []
        for x in self.vars:
            if isinstance(x, AcabValue) and x.name in the_dict:
                new_args.append(the_dict[x.name])
            else:
                assert(isinstance(x, Sentence))
                new_args.append(x)

        return AcabValue._sentence_constructor(just_path, params=new_args)



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
                variables[word.value]['types'].add(word.type)

        # Then unify all the variables to have the same type
        for the_dict in variables.values():
            types, instances = the_dict.values()
            # TODO convert this to correct form
            # if AcabValue._type_system.BOTTOM in types:
                # types.remove(AcabValue._type_system.BOTTOM)

            if len(types) > 1:
                raise TE.TypeConflictException(types.pop(), types, self)

            if bool(types):
                type_instance = types.pop()
                for word in instances:
                    word._data[TYPE_DEF_S] = type_instance



class SumTypeDefinition(TypeDefinition):
    """ Defines a Sum Type  """

    def __init__(self, structure, params=None):
        # Flatten Product Types out of Structure:
        # TODO: improve this
        flat_structure = []
        for sen in structure:
            prefix = Sentence.build(sen.words[:-1] + [sen.words[-1].to_simple_value()])
            flat_structure.append(prefix)
            flat_structure += [Sentence.build(prefix.words + x.words) for x in sen[-1].structure]

        if _type is None:
            _type = SUM_DEFINITION
        super(SumTypeDefinition, self).__init__(flat_structure,
                                                params=params,
                                                data={TYPE_INSTANCE_S: _type})
        assert(bool(self.structure))
