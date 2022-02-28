#!/usr/bin/env python3
"""
A Canonical Place to reference various ACAB types

"""
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Generic, Iterable,
                    Iterator, Mapping, Match, MutableMapping, NewType, Type,
                    Sequence, Tuple, TypeAlias, TypeVar, cast)

if TYPE_CHECKING:
    import pyparsing as pp
    from acab.core import util
    from acab.core.config import config
    from acab.core.data import default_structure, instruction
    from acab.core.parsing import annotation
    from acab.core.util import delayed_commands
    from acab.interfaces import (context, data, debugger, dsl, engine,
                                 handler_system, module_loader, printing,
                                 semantic, value)

# The type of a string which can be parsed into a simple sentence
pseudo              = NewType('pseudo', str)
# The interfaces of how the core value types behave
Value               : TypeAlias = "value.Value_i"
Instruction         : TypeAlias = "value.Instruction_i"
Sentence            : TypeAlias = "value.Sentence_i"

# Types describing enums of value accessible data
# ValueData           : TypeAlias = "default_structure.DATA_STRUCT_E"
# StructComponent     : TypeAlias = "default_structure.STRUCT_COMP_E"
# TypePrimitive       : TypeAlias = "default_structure.TYPE_PRIM_E"

# The types describing core structures for storing values
Node                : TypeAlias = "data.Node_i"
DataStructure       : TypeAlias = "data.Structure_i"

# Instructions
Operator            : TypeAlias = "instruction.ProductionOperator"
Component           : TypeAlias = "instruction.ProductionComponent"
Container           : TypeAlias = "instruction.ProductionContainer"
ProductionStructure : TypeAlias = "instruction.ProductionStructure"

# Types for assembling handler systems
Handler             : TypeAlias = "handler_system.Handler"
HandlerComponent    : TypeAlias = "handler_system.HandlerComponent_i"
HandlerSystem       : TypeAlias = "handler_system.HandlerSystem_i"
HandlerOverride     : TypeAlias = "handler_system.HandlerSystem_i.HandlerOverride"
Handler_Fragment    : TypeAlias = "semantic.Handler_Fragment"

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
DSL_Fragment        : TypeAlias = "dsl.DSL_Fragment"
DSL_Builder         : TypeAlias = "dsl.DSL_Builder_i"

Annotation          : TypeAlias = "annotation.ValueAnnotation"
RepeatAnnotation    : TypeAlias = "annotation.ValueRepeatAnnotation"
Parser              : TypeAlias = Callable

# Types which provide for running systems
Debugger            : TypeAlias = "debugger.AcabDebugger_i"
ModuleComponents    : TypeAlias = "module_loader.ModuleComponents"
Engine              : TypeAlias = "engine.AcabEngine_i"

# Types for using the config system
ConfigSpec          : TypeAlias = "config.ConfigSpec_i"
Config              : TypeAlias = "config.AcabConfig"
