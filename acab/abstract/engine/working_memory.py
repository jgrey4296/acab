"""
The abstract form of a working memory

Working Memory is responsible for the core system capabilities.
It provides an ontology structure, a base parser for that structure,
and integrates dsl fragments into that base.

The canonical working memory is the Trie_WM.
It uses an Exclusion Logic Semantics on a Trie data structure,
Parsers to define sentences and rules in that data structure,
and can load modules to extend its capabilities.

The working memory, at core, can Add, Retract, and Query facts.

From a module it loads Value, Statement, and Annotation parsers.

"""
import pyparsing as pp
import logging as root_logger
from fractions import Fraction
# https://docs.python.org/3/library/abc.html
import abc

from copy import deepcopy
from dataclasses import dataclass, field, InitVar, replace
from fractions import Fraction
from re import Pattern
from uuid import uuid1, UUID
from weakref import ref

from acab.abstract.core.node import AcabNode
from acab.abstract.engine.bootstrap_parser import BootstrapParser
from acab.abstract.interfaces.dsl_interface import DSL_Interface
from acab.abstract.rule.production_abstractions import ProductionOperator

from acab.abstract.interfaces import working_memory_interface as WMI

from acab.modules.semantics.basic_semantics import BasicNodeSemantics
from acab.modules.structures.trie.trie_semantics import BasicTrieSemantics

logging = root_logger.getLogger(__name__)

@dataclass
class WorkingMemory(WMI.WorkingMemoryInterface, WMI.InterruptableWMInterface, WMI.DSLBuilderInterface):
    """ The Abstract Working Memory """
    _structure : 'AcabStructure' = None

    # def __init__(self, init, struct=None, semantics=None):
        # Use a Bootstrap DSL for specification
        # self._bootstrap_parser = BootstrapParser()
        # Listeners are treated as a query *bag*
        # self._listeners = set()
        # self._listener_threshold = Fraction(1,2)
