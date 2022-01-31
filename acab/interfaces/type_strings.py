#!/usr/bin/env python3
"""
A Canonical Place to reference various ACAB types

"""
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                Mapping, Match, MutableMapping, Sequence, Tuple, NewType,
                TypeAlias, TypeVar, cast)
from dataclasses import dataclass, field, InitVar
from enum import Enum

# The type of a string which can be parsed into a simple sentence
pseudo              : TypeAlias = str
# The interfaces of how the core value types behave
Value               : TypeAlias = NewType("acab.interfaces.value.Value_i", Any)
Instruction         : TypeAlias = NewType("acab.interfaces.value.Instruction_i", Any)
Sentence            : TypeAlias = NewType("acab.interfaces.value.Sentence_i", Any)

# Types describing enums of value accessible data
ValueData           : TypeAlias = NewType("acab.core.data.default_structure.DATA_STRUCT_E", Enum)
StructComponent     : TypeAlias = NewType("acab.core.data.default_structure.STRUCT_COMP_E", Enum)
TypePrimitive       : TypeAlias = NewType("acab.core.data.default_structure.TYPE_PRIM_E", Enum)

# The types describing core structures for storing values
Node                : TypeAlias = NewType("acab.interfaces.data.Node_i", Any)
DataStructure       : TypeAlias = NewType("acab.interfaces.Structure_i", Any)

# Instructions
Operator            : TypeAlias = NewType("acab.core.data.instruction.ProductionOperator", Any)
Component           : TypeAlias = NewType("acab.core.data.instruction.ProductionComponent", Any)
Container           : TypeAlias = NewType("acab.core.data.instruction.ProductionContainer", Any)
ProductionStructure : TypeAlias = NewType("acab.core.data.instruction.ProductionStructure", Any)

# Types for assembling handler systems
Handler             : TypeAlias = NewType("acab.interfaces.handler_system.Handler", Any)
HandlerComponent    : TypeAlias = NewType("acab.interfaces.handler_system.HandlerComponent_i", Any)
HandlerSystem       : TypeAlias = NewType("acab.interfaces.handler_system.HandlerSystem_i", Any)
HandlerOverride     : TypeAlias = NewType("acab.interfaces.handler_system.HandlerSystem_i.HandlerOverrider", Any)
Handler_Fragment    : TypeAlias = NewType("acab.interfaces.semantic.Handler_Fragment", Any)

# Types for Assembling Semantic Systems
StructureSemantics  : TypeAlias = NewType("acab.interfaces.semantic.StructureSemantics_i", Any)
StatementSemantics  : TypeAlias = NewType("acab.interfaces.semantic.StatementSemantics_i", Any)
ValueSemantics      : TypeAlias = NewType("acab.interfaces.semantic.ValueSemantics_i", Any)
PrintSemantics      : TypeAlias = NewType("acab.interfaces.printing.PrintSemantics_i", Any)
SemanticSystem      : TypeAlias = NewType("acab.interfaces.semantic.SemanticSystem_i", Any)

# Types for Assembling Print Systems
PrintSystem         : TypeAlias = NewType("acab.interfaces.printing.PrintSystem_i", Any)


# Types for describing binding contexts of values
CtxSet              : TypeAlias = NewType("acab.interfaces.context.ContextSet_i", Any)
CtxIns              : TypeAlias = NewType("acab.interfaces.context.ContextInstance_i", Any)
Constraint          : TypeAlias = NewType("acab.interfaces.context.Constraint_i", Any)
DelayedCommands     : TypeAlias = NewType("acab.core.util.DelayedCommands_i", Any)

# Types for describing DSLs
DSL_Fragment        : TypeAlias = NewType("acab.interfaces.dsl.DSL_Fragment", Any)
DSL_Builder         : TypeAlias = NewType("acab.interfaces.dsl.DSL_Builder_i", Any)

Annotation          : TypeAlias = NewType("acab.core.parsing.annotation.ValueAnnotation", Any)
RepeatAnnotation    : TypeAlias = NewType("acab.core.parsing.annotation.RepeatAnnotation", Any)
Parser              : TypeAlias = NewType("pp.ParserElement", Any)

# Types which provide for running systems
Debugger            : TypeAlias = NewType("acab.interfaces.debugger.AcabDebugger_i", Any)
ModuleComponents    : TypeAlias = NewType("acab.core.data.interfaces.module_loader.ModuleComponents", Any)
Engine              : TypeAlias = NewType("acab.interfaces.engine.AcabEngine_i", Any)

# Types for using the config system
ConfigSpec          : TypeAlias = NewType("acab.core.config.config.ConfigSpec", Any)
Config              : TypeAlias = NewType("acab.core.config.config.AcabConfig", Any)
