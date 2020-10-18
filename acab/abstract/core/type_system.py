#!/usr/bin/env python

from acab.config import AcabConfig
from acab.abstract.core.type_base import TypeInstance
from acab.abstract.data.struct_semantics import AcabStructureSemantics
from acab.abstract.printing.print_semantics import AcabPrintSemantics

from acab.abstract.core.sentence import Sentence

util = AcabConfig.Get()

# Construct the primitive types
def build_simple_type_system():
    type_system = AcabTypeSystem(instance_constructor=TypeInstance,
                                 bottom=["ATOM"])
    ATOM      = type_system.add_primitive(path=["atom"])
    STRING    = type_system.add_primitive(path=["string"])
    NUMBER    = type_system.add_primitive(path=["number"])
    REGEX     = type_system.add_primitive(path=["regex"])

    SENTENCE  = type_system.add_primitive(path=["sentence"])

    OPERATOR  = type_system.add_primitive(path=["operator"])
    COMPONENT = type_system.add_primitive(path=["component"])
    CONTAINER = type_system.add_primitive(path=["container"])
    
    QUERY     = type_system.add_primitive(path=["query"], alias=QUERY_HEAD_S)
    TRANSFORM = type_system.add_primitive(path=["transform"], alias=TRANSFORM_HEAD_S)
    ACTION    = type_system.add_primitive(path=["action"], alias=ACTION_HEAD_S)
    RULE      = type_system.add_primitive(path=["rule"], alias=RULE_HEAD_S)

    return type_system


class AcabTypeSystem:
    """ A Packaged Type System.
    Primitives, semantics, printing rules

    """
    _active_type_system = None

    @staticmethod
    def GET(**kwargs):
        if AcabTypeSystem._active_type_system is None:
            AcabTypeSystem._active_type_system = AcabTypeSystem(**kwargs)

        return AcabTypeSystem._active_type_system

    def __init__(self, primitives=None, semantics=None, printing=None,
                 instance_constructor=None, checker=None):

        self._primitives: List[TypeInstance] = {}
        self._all_types = []
        self._semantics: List[AcabStructureSemantics]= []
        self._printing: List[AcabPrintSemantics] = []
        self._instance_constructor: Callable = instance_constructor
        self._check_action = checker

        bottom = self.add_primitive(path=["ATOM"])
        self._type_bottom = bottom

        if primitives is not None:
            primitives: List[List[str]]
            [self.add_primitive(x) for x in primitives]

        if semantics is not None:
            self._semantics += semantics

        if printing is not None:
            self._printing += printing

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
        return self._primitives["SENTENCE"]
    # TODO def STRING REGEX

    def add_primitive(self, path=None, alias=None):
        assert(isinstance(path, list))
        path_sentence = Sentence.build(path)
        new_instance = self._instance_constructor(path, type_alias_str=alias)
        self._primitives[path_sentence] = new_instance
        return new_instance


    def add_type_definition(self, **kwargs):
        raise NotImplementedError()
