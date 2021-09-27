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

Value                = "acab.abstract.interfaces.value.Value_i"
Statement            = "acab.abstract.interfaces.value.Statement_i"
Sentence             = "acab.abstract.interfaces.value.Sentence_i"

Node                 = "acab.abstract.interfaces.data.Node_i"
DataStructure        = "acab.abstract.interfaces.Structure_i"

Operator             = "acab.abstract.core.production_abstractions.ProductionOperator"
Component            = "acab.abstract.core.production_abstractions.ProductionComponent"
Container            = "acab.abstract.core.production_abstractions.ProductionContainer"
ProductionStructure  = "acab.abstract.core.production_abstractions.ProductionStructure"

ModuleComponents     = "acab.abstract.core.interfaces.module_loader.ModuleComponents"

# Semantics
Semantic_Fragment    = "acab.abstract.interfaces.semantic.Semantic_Fragment"
DependentSemantics   = "acab.abstract.interfaces.semantic.DependentSemantics_i"
AbstractionSemantics = "acab.abstract.interfaces.semantic.AbstractionSemantics_i"
IndependentSemantics = "acab.abstract.interfaces.semantic.IndependentSemantics_i"
PrintSemantics       = "acab.abstract.interfaces.printing.PrintSemantics_i"

PrintSystem          = "acab.abstract.interfaces.printing.PrintSystem_i"


# Context
CtxSet               = "acab.abstract.interfaces.context.ContextSet_i"
CtxIns               = "acab.abstract.interfaces.context.ContextInstance_i"
Constraint           = "acab.abstract.interfaces.context.Constraint_i"
DelayedCommands      = "acab.abstract.interfaces.context.DelayedCommands_i"

Debugger             = "acab.abstract.interfaces.debugger.AcabDebugger_i"

Bootstrapper         = "acab.abstract.interfaces.dsl.Bootstrapper_i"
DSL_Fragment         = "acab.abstract.interfaces.dsl.DSL_Fragment_i"
DSL_Builder          = "acab.abstract.interfaces.dsl.DSL_Builder_i"

Engine               = "acab.abstract.interfaces.engine.AcabEngine_i"

Handler              = "acab.abstract.interfaces.handler_system.Handler"
HandlerComponent     = "acab.abstract.interfaces.handler_system.HandlerComponent_i"
HandlerSystem        = "acab.abstract.interfaces.handler_system.HandlerSystem_i"
HandlerOverride      = "acab.abstract.interfaces.handler_system.HandlerSystem_i.HandlerOverrider"


ConfigSpec           = "acab.abstract.config.config.ConfigSpec"
Config               = "acab.abstract.config.config.AcabConfig"

Annotation           = "acab.abstract.parsing.annotation.ValueAnnotation"
RepeatAnnotation     = "acab.abstract.parsing.annotation.RepeatAnnotation"
