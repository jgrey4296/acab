#!/usr/bin/env python3
import logging as root_logger
from collections import defaultdict
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

from acab import types as AT
from acab.core.data import default_structure as DS
from acab.core.data.production_abstractions import ProductionOperator
from acab.core.decorators.semantic import RunInSubCtxSet
from acab.interfaces import semantic as SI
from acab.error.semantic_exception import AcabSemanticException
from acab.modules.context.context_set import ContextSet, MutableContextInstance
from acab.modules.semantics.util import SemanticBreakpointDecorator

CtxIns = AT.CtxIns

# Secondary Statements:
class LayerAbstraction(SI.StatementSemantics_i):
    """ A Layer of rules.
    ie: Query for rules.
    Select rules to run.
    run selection of rules.
    select passing rules to complete.
    run passing selection.
    """
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Run a layer, returning actions to perform """
        layer = instruction

        if DS.QUERY_COMPONENT in layer:
            semsys(layer[DS.QUERY_COMPONENT], ctxs=ctxs)

        if not bool(ctxs):
            return

        # TODO needs to be applied to all actives
        if DS.TRANSFORM_COMPONENT in layer:
            semsys.run(layer[DS.TRANSFORM_COMPONENT], ctxs=ctxs)

        if DS.ACTION_COMPONENT in layer:
            semsys.run(layer[DS.ACTION_COMPONENT], ctxs=ctxs)

class AgendaAbstraction(SI.StatementSemantics_i):
    """ A Layer-specific transform, to run operators on ctxs """
    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Runs an agenda rule on activated rules """
        # setup

        # run limited query

        # run transform on ctxs
        #
        raise NotImplementedError()



class AtomicPipelineAbstraction(SI.StatementSemantics_i):
    """ A Means of sequencing layers, run all layers per tick """

    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Run this pipeline on the given engine for a tick """
        # Setup
        pipeline = instruction

        for layer in pipeline:
            # Run the layer
            continue

        raise NotImplementedError()

class TemporalPipelineAbstraction(SI.StatementSemantics_i):
    """ A Means of sequencing layers, one layer per tick """

    def __call__(self, instruction, semsys, ctxs=None, data=None):
        """ Run this pipeline on the given engine for a tick """
        # Setup
        pipeline = instruction
        # Determine layer to run
        layer = None
        # run it

        raise NotImplementedError()

