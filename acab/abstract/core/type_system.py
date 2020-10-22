#!/usr/bin/env python
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.config import AcabConfig

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.core.type_base import TypeInstance

from acab.abstract.data.struct_semantics import AcabStructureSemantics
from acab.abstract.printing.print_semantics import AcabPrintSemantics

util = AcabConfig.Get()

TYPE_FMT_S = util("Printing", "TYPE_FMT_S")
QUERY_HEAD_S = util("Parsing.Statements", "QUERY_HEAD_S")
TRANSFORM_HEAD_S = util("Parsing.Statements", "TRANSFORM_HEAD_S")
ACTION_HEAD_S = util("Parsing.Statements", "ACTION_HEAD_S")
RULE_HEAD_S = util("Parsing.Statements", "RULE_HEAD_S")
VALUE_TYPE_S = util("Parsing.Structure", "VALUE_TYPE_S")

# Construct the primitive types
def build_simple_type_system():
    type_system = AcabTypeSystem("Basic", instance_constructor=TypeInstance)
    STRING    = type_system.add_primitive(path=["string"])
    NUMBER    = type_system.add_primitive(path=["number"])
    REGEX     = type_system.add_primitive(path=["regex"])

    OPERATOR  = type_system.add_primitive(path=["operator"])
    COMPONENT = type_system.add_primitive(path=["component"])
    CONTAINER = type_system.add_primitive(path=["container"])
    
    QUERY     = type_system.add_primitive(path=["query"])
    TRANSFORM = type_system.add_primitive(path=["transform"])
    ACTION    = type_system.add_primitive(path=["action"])
    RULE      = type_system.add_primitive(path=["rule"])

    return type_system


class AcabTypeSystem:
    """ A Packaged Type System.
    Primitives, semantics, printing rules

    """
    _all_systems = {}
    _active_type_system = None

    @staticmethod
    def MOVE(name):
        """ Move to the named type system """
        raise NotImplementedError()

    @staticmethod
    def GET(**kwargs):
        if AcabTypeSystem._active_type_system is None or "OVERRIDE" in kwargs:
            if 'name' in kwargs:
                name = kwargs['name']
                del kwargs['name']
            else:
                # TODO randomise this
                name = "default"
            new_system = AcabTypeSystem(name, **kwargs)
            AcabTypeSystem._all_systems[new_system.name]
            AcabTypeSystem._active_type_system = new_system

        return AcabTypeSystem._active_type_system

    def __init__(self, name, primitives=None, instance_constructor=None, checker=None):
        AcabValue._set_type_system(self)
        self._name = name
        self._primitives: List[TypeInstance] = {}
        self._all_types = []
        self._semantics: List[AcabStructureSemantics]= []
        self._printing: List[AcabPrintSemantics] = []
        self._instance_constructor: Callable = instance_constructor
        self._check_action = checker

        # Bootstrap type system:
        bottom_atom = self._instance_constructor("ATOM")
        bootstrap_sentence = self._instance_constructor("SENTENCE")
        self._type_bottom = bottom_atom
        self._primitives["SENTENCE"] = bootstrap_sentence

        self._type_bottom._data.update({VALUE_TYPE_S: self.BOTTOM})
        self._type_bottom._value = Sentence.build(["ATOM"])
        self._primitives["SENTENCE"]._value = Sentence.build(["SENTENCE"])

        # Bootstrapped, rest base references and continue:
        AcabValue._set_type_system(self)

        if primitives is not None:
            primitives: List[List[str]]
            [self.add_primitive(x) for x in primitives]

    @property
    def name(self):
        return self._name

    @property
    def INSTANCE(self):
        return self._instance_constructor

    @property
    def ATOM(self):
        return self._type_bottom
    @property
    def BOTTOM(self):
        return self._type_bottom

    @property
    def CHECK(self):
        return self._check_action

    @property
    def SENTENCE(self):
        if "SENTENCE" in self._primitives:
            return self._primitives["SENTENCE"]

        return self.BOTTOM

    # TODO def STRING REGEX

    @property
    def CONTAINER(self):
        if "CONTAINER" in self._primitives:
            return self._primitives["CONTAINER"]

        return self.BOTTOM

    def add_primitive(self, path=None):
        assert(isinstance(path, list))
        path_sentence = Sentence.build(path)
        new_instance = self._instance_constructor(path)
        self._primitives[path_sentence] = new_instance
        return new_instance


    def add_type_definition(self, **kwargs):
        raise NotImplementedError()

    def get_alias_chars(self):
        sigils = [x[-1]._primitive._type_alias for x in TypeInstance._type_system.primitives]
        sigils_str = "".join([x for x in sigils if x is not None])
        return sigils_str
