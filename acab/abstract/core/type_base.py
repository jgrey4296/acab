"""
The Base Types of Acab.
Only Provides automatic *primitives*.
Manual typing and Product/Sum types are implemented
in the separate typing module.
"""
from uuid import uuid1
from copy import copy
from acab.config import AcabConfig

util = AcabConfig.Get()

TYPE_FMT_S = util("Printing", "TYPE_FMT_S")
QUERY_HEAD_S = util("Parsing.Statements", "QUERY_HEAD_S")
TRANSFORM_HEAD_S = util("Parsing.Statements", "TRANSFORM_HEAD_S")
ACTION_HEAD_S = util("Parsing.Statements", "ACTION_HEAD_S")
RULE_HEAD_S = util("Parsing.Statements", "RULE_HEAD_S")

class _Bootstrap_Value:
    """
    A Bootstrap Value for primitive type paths,
    without having to load sentence or AcabValue

    Main Requirements: is_var and name
    """

    def __init__(self, value):
        self._value = value
        self._primitive = None

    def __str__(self):
        return str(self.name)

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

    def __init__(self, values, primitive):
        assert(isinstance(values, list))
        self._value = [_Bootstrap_Value(x) for x in values]
        self._value[-1]._primitive = primitive

    def __str__(self):
        return ".".join([str(x) for x in self._value])

    def __repr__(self):
        return "BootstrapSentence({})".format(str(self))

    def __getitem__(self, i):
        return self.words.__getitem__(i)

    def __eq__(self, other):
        hasattr(other, "words")
        if len(self.words) != len(other.words):
            return False

        word_eq = all([x == y for x,y in zip(other.words, self.words)])
        return word_eq

    def __hash__(self):
        return hash(str(self))
    @property
    def words(self):
        return self._value

    def pprint(self, opts=None):
        return ".".join([str(x) for x in self.words])


class TypeInstance:
    """ A Type Instance can be polytyped or monotyped """
    TypeCounter = 0
    Primitives = []

    @staticmethod
    def get_alias_chars():
        sigils = [x[-1]._primitive._type_alias for x in TypeInstance.Primitives]
        sigils_str = "".join([x for x in sigils if x is not None])
        return sigils_str

    def __init__(self, path, params=None, primitive=False, type_alias_str=None):
        """ Construct a Type Instance with a _path in the type trie """
        if primitive:
            path = _Bootstrap_Sentence(path, self)
            assert(path not in TypeInstance.Primitives), breakpoint()
            TypeInstance.Primitives.append(path)

        assert(params is None or all([isinstance(x, (str, TypeInstance)) or hasattr(x, "type") for x in params])), breakpoint()
        self._uuid = uuid1()
        self._path = path
        self._type_alias = type_alias_str
        self._params = []
        self._primitive = primitive

        if params is not None:
            self._params += params

    def __hash__(self):
        return hash(self.path)

    def __eq__(self, other):
        """
        Two Type Instances are equal if have the same path,
        and if they have matching parameters
        """
        if not (other or isinstance(other, TypeInstance)):
            return False

        if other._primitive and other._type_alias is not None and self._path.pprint() == other._type_alias:
            return True

        path_match = self._path == other._path
        args_match = all([a == b for a, b in zip(self.vars, other.vars)])
        return path_match and args_match

    def __lt__(self, other):
        """
        Operator to form a partial order over all types
        ATOM is TOP
        TODO BOTTOM
        """
        assert(isinstance(other, TypeInstance))
        if self == ATOM:
            return True

        return False


    def __str__(self):

        path = str(self._path)
        if self._type_alias is not None:
            path += self._type_alias

        path += ".".join([str(x) for x in self._params])

        if self._primitive:
            return "primitive." + path

        return path

    def __repr__(self):
        return "(TypeInstance {})".format(str(self))


    def __call__(self, node, data=None, engine=None):
        """
        An example light weight annotation for querying
        This *could* be wrapped in a ProductionOperator, or a QueryOperator,
        but this way requires less setup
        """
        return self == node.value.type


    @property
    def path(self):
        return self._path
    @property
    def head(self):
        return self.path[-1]
    @property
    def vars(self):
        return self._params
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
            return copy(self)

        new_args = []
        for x in self.vars:
            if isinstance(x, str) and x in the_dict:
                new_args.append(the_dict[x])
            else:
                new_args.append(x)

        return TypeInstance(self.path, params=new_args)


    def pprint(self, opts=None):
        raise DeprecationWarning("Use Print Semantics")


# Construct the primitive types
ATOM      = TypeInstance(path=["atom"], primitive=True)
STRING    = TypeInstance(path=["string"], primitive=True)
NUMBER    = TypeInstance(path=["number"], primitive=True)
REGEX     = TypeInstance(path=["regex"], primitive=True)

SENTENCE  = TypeInstance(path=["sentence"], primitive=True)

OPERATOR  = TypeInstance(path=["operator"], primitive=True)
COMPONENT = TypeInstance(path=["component"], primitive=True)
CONTAINER = TypeInstance(path=["container"], primitive=True)

QUERY     = TypeInstance(path=["query"], type_alias_str=QUERY_HEAD_S, primitive=True)
TRANSFORM = TypeInstance(path=["transform"], type_alias_str=TRANSFORM_HEAD_S, primitive=True)
ACTION    = TypeInstance(path=["action"], type_alias_str=ACTION_HEAD_S, primitive=True)
RULE      = TypeInstance(path=["rule"], type_alias_str=RULE_HEAD_S, primitive=True)
