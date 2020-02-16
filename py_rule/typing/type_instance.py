from .pyrule_type import Type

class TypeInstance(Type):
    """ A Type Instance can be polytyped or monotyped """
    TypeCounter = 0

    def __init__(self, name=None, path=None, args=None):
        """ Construct a Type Instance with a name,
        and its path in the type trie,
        """
        if path is None:
            path = name
        if args is None:
            args = []
        self._name = name
        self._path = path
        self._args = args

    def __hash__(self):
        return hash("".join([str(x) for x in self._path]))

    def __repr__(self):
        args = ""
        if self._args:
            args = "({})".format(", ".join(repr(x) for x in self._args))
        return "(::{}{})".format(self._name, args)

    def __str__(self):
        return self._name

    def __eq__(self, other):
        # todo: match inheritance
        if not other:
            return False
        type_match = type(self) == type(other)
        name_match = self._name == other._name
        args_match = all([a == b for a, b in zip(self._args, other._args)])
        return type_match and name_match and args_match

    def __lt__(self, other):
        """ Operator to form a partial order over all types """
        raise Exception("To be implemented")

    def build_type_declaration(self, the_dict):
        """ Given a type instance and a dictionary
        of values for variables, build a monotyped instance
        """
        if str(self) in the_dict:
            new_type = the_dict[str(self)]
            return MonoTypeVar(new_type._name,
                               new_type._path,
                               new_type._args)

        new_args = [the_dict[x.value_string()] if x.value_string() in the_dict
                    else x for x in self._args]
        return MonoTypeVar(self._name, self._path, new_args)
