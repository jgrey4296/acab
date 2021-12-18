#!/usr/bin/env python3
"""
A Canonical Place to reference various ACAB types

"""
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

# Pseudo Sentence, a string starting with _:
pseudo               = str

Value                = "acab.interfaces.value.Value_i"
Instruction          = "acab.interfaces.value.Instruction_i"
Sentence             = "acab.interfaces.value.Sentence_i"

Node                 = "acab.interfaces.data.Node_i"
DataStructure        = "acab.interfaces.Structure_i"

Operator             = "acab.core.data.instruction.ProductionOperator"
Component            = "acab.core.data.instruction.ProductionComponent"
Container            = "acab.core.data.instruction.ProductionContainer"
ProductionStructure  = "acab.core.data.instruction.ProductionStructure"

ModuleComponents     = "acab.core.data.interfaces.module_loader.ModuleComponents"

Handler              = "acab.interfaces.handler_system.Handler"
HandlerComponent     = "acab.interfaces.handler_system.HandlerComponent_i"
HandlerSystem        = "acab.interfaces.handler_system.HandlerSystem_i"
HandlerOverride      = "acab.interfaces.handler_system.HandlerSystem_i.HandlerOverrider"
Handler_Fragment     = "acab.interfaces.semantic.Handler_Fragment"

# Semantics
StructureSemantics   = "acab.interfaces.semantic.StructureSemantics_i"
StatementSemantics   = "acab.interfaces.semantic.StatementSemantics_i"
ValueSemantics       = "acab.interfaces.semantic.ValueSemantics_i"
PrintSemantics       = "acab.interfaces.printing.PrintSemantics_i"

SemanticSystem       = "acab.interfaces.semantic.SemanticSystem_i"
PrintSystem          = "acab.interfaces.printing.PrintSystem_i"


# Context
CtxSet               = "acab.interfaces.context.ContextSet_i"
CtxIns               = "acab.interfaces.context.ContextInstance_i"
Constraint           = "acab.interfaces.context.Constraint_i"
DelayedCommands      = "acab.core.util.DelayedCommands_i"

Debugger             = "acab.interfaces.debugger.AcabDebugger_i"

DSL_Fragment         = "acab.interfaces.dsl.DSL_Fragment"
DSL_Builder          = "acab.interfaces.dsl.DSL_Builder_i"

Engine               = "acab.interfaces.engine.AcabEngine_i"



ConfigSpec           = "acab.core.config.config.ConfigSpec"
Config               = "acab.core.config.config.AcabConfig"

Annotation           = "acab.core.parsing.annotation.ValueAnnotation"
RepeatAnnotation     = "acab.core.parsing.annotation.RepeatAnnotation"

Parser               = "pp.ParserElement"
