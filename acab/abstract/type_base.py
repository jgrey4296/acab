"""
The Base Types of Acab.
Only Provides automatic *primitives*.
Manual typing and Product/Sum types are implemented
in the separate typing module.
"""
from acab.util import TYPE_FMT_S
from acab.abstract.printing import util as PrU

class _Bootstrap_Value:
    """
    A Bootstrap Value for primitive type paths,
    without having to load sentence or AcabValue

    Main Requirements: is_var and name
    """

    def __init__(self, value):
        self._value = value

    def __str__(self):
        return self.name

    def __eq__(self, other):
        assert(hasattr(other, "name"))
        return self.name == other.name

    def __hash__(self):
        return hash(str(self))
    @property
    def is_var(self):
        return False

    @property
    def name(self):
        return self._value


class _Bootstrap_Sentence:
    """
    A Bootstrap List/Sentence to not have to load Sentence

    Main Requirements: attribute access and eq
    """

    def __init__(self, values):
        assert(isinstance(values, list))
        self._value = [_Bootstrap_Value(x) for x in values]

    def __str__(self):
        return self.pprint()
    def __getitem__(self, i):
        return self.words.__getitem__(i)

    def __eq__(self, other):
        hasattr(other, "words")
        word_eq = all([x == y for x,y in zip(other.words, self.words)])
        return word_eq

    @property
    def words(self):
        return self._value

    def pprint(self, opts=None):
        return ".".join([str(x) for x in self.words])


class TypeInstance:
    """ A Type Instance can be polytyped or monotyped """
    TypeCounter = 0
    Primitives = []

    def __init__(self, path, params=None, primitive=False, type_alias_str=None):
        """ Construct a Type Instance with a _path in the type trie """
        assert(hasattr(path, "type")), breakpoint()
        if primitive:
            TypeInstance.Primitives.append(path)

        assert(params is None or all([isinstance(x, (str, TypeInstance)) or hasattr(x, "type") for x in params])), breakpoint()
        self._path = path
        self._type_alias = type_alias_str
        self._vars = []
        self._primitive = primitive


        if params is not None:
            self._vars += params

    def __hash__(self):
        return hash(str(self.path))

    def __eq__(self, other):
        # TODO: match inheritance
        if not other:
            return False
        if not (isinstance(other, TypeInstance)):
            return False
        path_match = self._path == other._path
        args_match = all([a == b for a, b in zip(self.vars, other.vars)])
        return path_match and args_match

    def __lt__(self, other):
        """ Operator to form a partial order over all types """
        assert(isinstance(other, TypeInstance))
        if self == ATOM:
            return True

        return False


    def __str__(self):
        return self.pprint()

    def __repr__(self):
        return "(TypeInstance {})".format(str(self))

    @property
    def path(self):
        return self._path
    @property
    def head(self):
        return self.path[-1]
    @property
    def vars(self):
        return self._vars
    @property
    def var_set(self):
        raise NotImplementedError()


    def build_type_instance(self, the_dict=None):
        """ Given a type instance and a dictionary
        of values for variables, build a monotyped instance
        """
        index = self.path[-1]

        if self._primitive:
            return self

        if the_dict is not None and index.is_var and index.name in the_dict:
            new_type = the_dict[index.name]
            return new_type.copy()

        if the_dict is None:
            return self.copy()

        new_args = []
        for x in self.vars:
            if isinstance(x, str) and x in the_dict:
                new_args.append(the_dict[x])
            else:
                new_args.append(x)

        # TODO: copy the path
        return TypeInstance(self.path, params=new_args)


    def pprint(self, opts=None):
        if self._type_alias is not None:
            return TYPE_FMT_S.format(self._type_alias)
        else:
# Construct the primitive types
ATOM = TypeInstance(primitive=["atom"])
STRING = TypeInstance(primitive=["string"])
NUMBER = TypeInstance(primitive=["number"])
REGEX = TypeInstance(primitive=["regex"])

SENTENCE = TypeInstance(primitive=["sentence"])

OPERATOR = TypeInstance(primitive=["operator"])
COMPONENT = TypeInstance(primitive=["component"])
CONTAINER = TypeInstance(primitive=["container"])

QUERY = TypeInstance(primitive=["query"])
TRANSFORM = TypeInstance(primitive=["transform"])
ACTION = TypeInstance(primitive=["action"])
RULE = TypeInstance(primitive=["rule"], type_alias_str="ρ")
