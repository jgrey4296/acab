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
Statement            = "acab.interfaces.value.Statement_i"
Sentence             = "acab.interfaces.value.Sentence_i"

Node                 = "acab.interfaces.data.Node_i"
DataStructure        = "acab.interfaces.Structure_i"

Operator             = "acab.core.data.production_abstractions.ProductionOperator"
Component            = "acab.core.data.production_abstractions.ProductionComponent"
Container            = "acab.core.data.production_abstractions.ProductionContainer"
ProductionStructure  = "acab.core.data.production_abstractions.ProductionStructure"

ModuleComponents     = "acab.core.data.interfaces.module_loader.ModuleComponents"

# Semantics
# TODO abstraction sem -> statement sem
# TODO dependent sem   -> structure sem
# TODO independent sem -> value sem
Semantic_Fragment    = "acab.interfaces.semantic.Semantic_Fragment"
DependentSemantics   = "acab.interfaces.semantic.DependentSemantics_i"
AbstractionSemantics = "acab.interfaces.semantic.AbstractionSemantics_i"
IndependentSemantics = "acab.interfaces.semantic.IndependentSemantics_i"
PrintSemantics       = "acab.interfaces.printing.PrintSemantics_i"

SemanticSystem       = "acab.interfaces.semantic.SemanticSystem_i"
PrintSystem          = "acab.interfaces.printing.PrintSystem_i"


# Context
CtxSet               = "acab.interfaces.context.ContextSet_i"
CtxIns               = "acab.interfaces.context.ContextInstance_i"
Constraint           = "acab.interfaces.context.Constraint_i"
DelayedCommands      = "acab.core.util.DelayedCommands_i"

Debugger             = "acab.interfaces.debugger.AcabDebugger_i"

Bootstrapper         = "acab.interfaces.dsl.Bootstrapper_i"
DSL_Fragment         = "acab.interfaces.dsl.DSL_Fragment_i"
DSL_Builder          = "acab.interfaces.dsl.DSL_Builder_i"

Engine               = "acab.interfaces.engine.AcabEngine_i"

Handler              = "acab.interfaces.handler_system.Handler"
HandlerComponent     = "acab.interfaces.handler_system.HandlerComponent_i"
HandlerSystem        = "acab.interfaces.handler_system.HandlerSystem_i"
HandlerOverride      = "acab.interfaces.handler_system.HandlerSystem_i.HandlerOverrider"


ConfigSpec           = "acab.core.config.config.ConfigSpec"
Config               = "acab.core.config.config.AcabConfig"

Annotation           = "acab.core.parsing.annotation.ValueAnnotation"
RepeatAnnotation     = "acab.core.parsing.annotation.RepeatAnnotation"

Parser               = "pp.ParserElement"
