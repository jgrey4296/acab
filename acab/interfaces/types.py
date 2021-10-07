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

Operator             = "acab.abstract.core.production_abstractions.ProductionOperator"
Component            = "acab.abstract.core.production_abstractions.ProductionComponent"
Container            = "acab.abstract.core.production_abstractions.ProductionContainer"
ProductionStructure  = "acab.abstract.core.production_abstractions.ProductionStructure"

ModuleComponents     = "acab.abstract.core.interfaces.module_loader.ModuleComponents"

# Semantics
Semantic_Fragment    = "acab.interfaces.semantic.Semantic_Fragment"
DependentSemantics   = "acab.interfaces.semantic.DependentSemantics_i"
AbstractionSemantics = "acab.interfaces.semantic.AbstractionSemantics_i"
IndependentSemantics = "acab.interfaces.semantic.IndependentSemantics_i"
PrintSemantics       = "acab.interfaces.printing.PrintSemantics_i"

PrintSystem          = "acab.interfaces.printing.PrintSystem_i"


# Context
CtxSet               = "acab.interfaces.context.ContextSet_i"
CtxIns               = "acab.interfaces.context.ContextInstance_i"
Constraint           = "acab.interfaces.context.Constraint_i"
DelayedCommands      = "acab.interfaces.context.DelayedCommands_i"

Debugger             = "acab.interfaces.debugger.AcabDebugger_i"

Bootstrapper         = "acab.interfaces.dsl.Bootstrapper_i"
DSL_Fragment         = "acab.interfaces.dsl.DSL_Fragment_i"
DSL_Builder          = "acab.interfaces.dsl.DSL_Builder_i"

Engine               = "acab.interfaces.engine.AcabEngine_i"

Handler              = "acab.interfaces.handler_system.Handler"
HandlerComponent     = "acab.interfaces.handler_system.HandlerComponent_i"
HandlerSystem        = "acab.interfaces.handler_system.HandlerSystem_i"
HandlerOverride      = "acab.interfaces.handler_system.HandlerSystem_i.HandlerOverrider"


ConfigSpec           = "acab.abstract.config.config.ConfigSpec"
Config               = "acab.abstract.config.config.AcabConfig"

Annotation           = "acab.abstract.parsing.annotation.ValueAnnotation"
RepeatAnnotation     = "acab.abstract.parsing.annotation.RepeatAnnotation"

Parser               = "pp.ParserElement"
