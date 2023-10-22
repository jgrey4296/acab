"""
A Canonical Place to reference various ACAB types

"""
##-- imports
from __future__ import annotations

from dataclasses import InitVar, dataclass, field
from enum import Enum
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Generic, Iterable,
                    Iterator, Mapping, Match, MutableMapping, NewType,
                    Sequence, Tuple, Type, TypeAlias, TypeVar, cast)

if TYPE_CHECKING:
    import pyparsing as pp
    from acab.core import util
    import acab.core.defaults.value_keys as DS
    from acab.core.value import instruction
    from acab.core.parsing import annotation
    from acab.core.util import delayed_commands
    from acab.interfaces import (config, sieve, context, data, debugger, dsl, engine,
                                 handler_system, module_loader, printing,
                                 semantic, value)

from . import function_aliases

##-- end imports

T = TypeVar('T')

# Generic Type Aliases default to Unknown, unless given a TypeVar in the Alias.
# Hence the TValore and TNode

# The Types which can be wrapped in an acab Value
ValueCore   = str | Pattern | list | None
ValueCore_t = str | Pattern[Any] | list[Any] | None
TValCore  = TypeVar('TValCore', bound=ValueCore)

fns = function_aliases
# The type of a string which can be parsed into a simple sentence
DataDict            : TypeAlias = dict[str,Any]
Sieve               : TypeAlias = "sieve.AcabSieve[T]"
# The interfaces of how the core value types behave
Value               : TypeAlias = "value.Value_i[TValCore]"
Instruction         : TypeAlias = "value.Instruction_i"
Sentence            : TypeAlias = "value.Sentence_i"

# Types describing enums of value accessible data
ValueData             : TypeAlias = str
# StructComponent     : TypeAlias = "DS.STRUCT_COMP_E"
# TypePrimitive       : TypeAlias = "DS.TYPE_PRIM_E"

# The types describing core structures for storing values
Node                : TypeAlias = "data.Node_i"
TNode = TypeVar('TNode', bound=Node)
DataStructure       : TypeAlias = "data.Structure_i[TNode]"
StructView          : TypeAlias = "data.StructView"

# Instructions
Operator            : TypeAlias = "value.Operator_i[TValCore]"
Action              : TypeAlias = "value.Action_i"
Container           : TypeAlias = "instruction.ProductionContainer"
ProductionStructure : TypeAlias = "instruction.ProductionStructure"

# Fragments of Systems
ModuleFragment      : TypeAlias = "fragments.ModuleFragment"
HandlerFragment     : TypeAlias = "fragments.HandlerFragment_i"
DSL_Fragment        : TypeAlias = "fragments.DSL_Fragment_i"

# Types for assembling handler systems
Handler             : TypeAlias = "handler_system.Handler_i"
HandlerComponent    : TypeAlias = "handler_system.HandlerComponent_i"
HandlerSystem       : TypeAlias = "handler_system.HandlerSystem_i"
HandlerOverride     : TypeAlias = "handler_system.HandlerOverride"
HandlerSpec         : TypeAlias = "handler_system.HandlerSpec_i"

# Types for Assembling Semantic Systems
StructureSemantics  : TypeAlias = "semantic.StructureSemantics_i"
StatementSemantics  : TypeAlias = "semantic.StatementSemantics_i"
ValueSemantics      : TypeAlias = "semantic.ValueSemantics_i"
SemanticSystem      : TypeAlias = "semantic.SemanticSystem_i"

# Types for Assembling Print Systems
PrintSemantics      : TypeAlias = "printing.PrintSemantics_i"
PrintSystem         : TypeAlias = "printing.PrintSystem_i"


# Types for describing binding contexts of values
CtxSet              : TypeAlias = "context.ContextSet_i"
CtxIns              : TypeAlias = "context.ContextInstance_i"
Constraint          : TypeAlias = "context.Constraint_i"
DelayedCommands     : TypeAlias = "delayed_commands.DelayedCommands_i"

# Types for describing DSLs
DSL_Builder         : TypeAlias = "dsl.DSL_Builder_i"
DSL_Spec            : TypeAlias = "dsl.DSL_Spec_i"

Annotation          : TypeAlias = "annotation.ValueAnnotation"
RepeatAnnotation    : TypeAlias = "annotation.ValueRepeatAnnotation"
Parser              : TypeAlias = fns.Parser

# Types which provide for running systems
Debugger            : TypeAlias = "debugger.AcabDebugger_i"
ModuleLoader        : TypeAlias = "module_loader.ModuleLoader_i"
Engine              : TypeAlias = "engine.AcabEngine_i"

# Types for using the config system
ConfigSpec          : TypeAlias = "config.ConfigSpec_d"
Config              : TypeAlias = "config.Config_i"
