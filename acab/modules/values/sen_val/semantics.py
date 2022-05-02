#!/usr/bin/env python3
import logging as logmod
from collections import defaultdict
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from acab import AcabConfig
from acab import types as AT
from acab.core.semantics.basic import StatementSemantics
from acab.core.util.decorators.semantic import RunInSubCtxSet
from acab.core.value import default_structure as DS
from acab.core.value.instruction import ProductionOperator
from acab.error.semantic import AcabSemanticException
from acab.interfaces import semantic as SI
from acab.modules.context.context_instance import MutableContextInstance
from acab.modules.values.sen_val.context_sen_bind_query_manager import \
    ContextSenBindQueryManager

config     = AcabConfig()

CtxIns = AT.CtxIns

class SenQuerySemantics(StatementSemantics, SI.StatementSemantics_i):
    """
    For When querying:
    a.b.$x(::$y)?
    $y?

    meaning to query the entire sentence $y refers to
    """
    def query(self, sen, semSys, ctxs=None, data=None):
        assert(ctxs is not None)

        temp_ctxs = semSys.build_ctxset(ctxs._operators)
        # remove empty ctx:
        temp_ctxs.run_delayed()
        cqm = ContextSenBindQueryManager(sen, struct.root, ctxs)
        with cqm:
            for query_clause, ctxInst, current_node in cqm.active:
                # query(::sen)
                temp_ctxs.push(ctxInst)
                semSys(query_clause, ctxs=temp_ctxs)
                # TODO cqm.queue_ctxs(temp_ctxs.active_list)

        return cqm.finished
