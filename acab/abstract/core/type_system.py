#!/usr/bin/env python
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.config import AcabConfig

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence

from acab.abstract.data.struct_semantics import AcabStructureSemantics
from acab.abstract.printing.print_semantics import AcabPrintSemantics

util = AcabConfig.Get()

TYPE_PRIMITIVE_LIST = util("Typing.Primitives", as_list=True)
TYPE_BOTTOM_NAME_S = util("Typing.Primitives", "TYPE_BOTTOM_NAME_S")
VALUE_TYPE_S = util("Parsing.Structure", "VALUE_TYPE_S")

# Construct the primitive types
def build_simple_type_system():
    type_system = AcabTypeSystem("Basic", primitives=TYPE_PRIMITIVE_LIST)
    return type_system


class AcabTypeSystem:
    """ A Packaged Type System.
    Maps Type Sentences to semantics
    """
    _all_systems = {}
    _active_type_system = None

    # TODO add print call to print semantics

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

    def __init__(self, name, primitives=None, checker=None):
        AcabValue._sentence_constructor = Sentence.build
        self._name = name
        self._all_types = set()
        self._semantics: List[AcabStructureSemantics]= []
        self._printing: List[AcabPrintSemantics] = []
        self._type_mapping = {}
        self._check_action = checker
        self._type_bottom = None

        if primitives is not None:
            [self.register_type(x) for x in primitives]

    @property
    def name(self):
        return self._name


    @property
    def CHECK(self):
        return self._check_action

    def register_type(self, sen):
        if not isinstance(sen, Sentence):
            sen = Sentence.build([sen])
        self._all_types.add(sen)

    def add_type_definition(self, **kwargs):
        raise NotImplementedError()

    def get_alias_chars(self):
        sigils = [x[-1]._primitive._type_alias for x in AcabValue._type_system.primitives]
        sigils_str = "".join([x for x in sigils if x is not None])
        return sigils_str
