#!/usr/bin/env python3
import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from collections import defaultdict

logging = root_logger.getLogger(__name__)

from acab import types as AT
from acab.abstract.core import default_structure as DS
from acab.abstract.decorators.semantic import RunInSubCtxSet
from acab.interfaces import semantic as SI
from acab.modules.context.context_set import (ContextSet,
                                                MutableContextInstance)
from acab.modules.semantics.util import SemanticBreakpointDecorator
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.abstract.core.production_abstractions import ProductionOperator

CtxIns = AT.CtxIns

class DFSQueryAbstraction(SI.AbstractionSemantics_i):
    """
    For running DFS queries like:
    a.b.$x?
    @x ᛦ $y(#tag, ::rule)?
    """

    def __call__(self, instruction, semsys, ctxs=None, data=None):
        default = semsys.lookup()
        struct  = default.struct
        # Where the sentence is Sen([@x, $y(..)], ᛦ)
        word    = instruction[-1]
        # TODO use ctxins
        # Queue: List[Tuple[List[Value], Node]]
        with ContextQueryState(negated, instruction, struct.root, collapse_vars, ctxs):
            for ctxInst in ctxs.active_list(clear=True):
                potentials = []
                queue      = [ctxInst._current]
                while bool(queue):
                    current = queue.pop(0)
                    semantics, _  = self.lookup(current)
                    accessible    = semantics.access(current, None, data)
                    if bool(accessible):
                        queue      += accessible
                        potentials += accessible
                # After all nodes have been found:
                if not bool(potentials):
                    ctxs.fail(ctxInst, word, None)
                else:
                    # Test them
                    ctxs.test(ctxInst, accessible, word)
