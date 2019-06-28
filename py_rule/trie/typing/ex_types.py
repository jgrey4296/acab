import logging as root_logger
import IPython
logging = root_logger.getLogger(__name__)


class Type:
    """ An unrestricted type """

    def __repr__(self):
        return "|∀|"


class TypeDefinition(Type):
    """ Can define Structure of a type """

    def __init__(self, name, path, structure, tvars):

        """ Structure creates the dict of locations.
        Only leaves get type anotations. Thus:
        { .a.$x :: String, .b.$c :: Num, .d!$e::Location }
        """
        #The name is the location. eg: .types.person
        self.name = name
        self.path = path
        self.structure = structure
        self._vars = []
        if tvars is not None:
            self._vars += tvars

    def __repr__(self):
        return "Type Def: {}".format(self.name)

    def build_type_declaration(self):
        return MonoTypeVar(self.name, self.path, self._vars[:])


class MonoTypeVar(Type):
    """ A MonoType Instance """
    TypeCounter = 0

    def __init__(self, name=None, path=None, args=None):
        if path is None:
            path = name
        if args is None:
            args = []
        self.name = name
        self.path = path
        self._args = args

    def __hash__(self):
        return hash("".join([str(x) for x in self.path]))

    def __repr__(self):
        args = ""
        if self._args:
            args = "({})".format(", ".join(repr(x) for x in self._args))
        return "(::{}{})".format(self.name, args)

    def __str__(self):
        return self.name.name

    def __eq__(self, other):
        #todo: match inheritance
        if not other:
            return False
        type_match = type(self) == type(other)
        name_match = self.name == other.name
        args_match = all([a == b for a,b in zip(self._args, other._args)])
        return type_match and name_match and args_match

    def __lt__(self, other):
        raise Exception("To be implemented")


    def build_type_declaration(self, the_dict):
        if str(self) in the_dict:
            new_type = the_dict[str(self)]
            return MonoTypeVar(new_type.name,
                               new_type.path,
                               new_type._args)

        new_args = [the_dict[str(x)] if str(x) in the_dict else x for x in self._args]
        return MonoTypeVar(self.name,
                           self.path,
                           new_args)


