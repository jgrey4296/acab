#!/usr/bin/env python3
import logging as logmod
from collections import defaultdict
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from acab import types as AT
from acab.core.data import default_structure as DS
from acab.core.data.instruction import ProductionOperator
from acab.core.decorators.semantic import RunInSubCtxSet
from acab.error.semantic import AcabSemanticException
from acab.interfaces import semantic as SI
from acab.modules.context.context_set import ContextSet, MutableContextInstance
from acab.modules.values.sen_val.context_sen_bind_query_manager import \
    ContextSenBindQueryManager

CtxIns = AT.CtxIns



class SenQuerySemantics(SI.StatementSemantics_i):
    """
    For When querying:
    a.b.$x(::$y)?
    $y?

    meaning to query the entire sentence $y refers to
    """


    def query(self, sen, semSys, ctxs=None, data=None):
        if ctxs is None:
            raise ASErr.AcabSemanticException("Ctxs is none to SenQuerySemantics.query", sen)

        temp_ctxs = semSys.build_ctxset(ctxs._operators)
        # remove empty ctx:
        temp_ctxs.run_delayed()
        with ContextSenBindQueryManager(sen, struct.root, ctxs) as cqm:
            for query_clause, ctxInst, current_node in cqm.active:
                # query(::sen)
                temp_ctxs.push(ctxInst)
                semSys(query_clause, ctxs=temp_ctxs)
                # TODO cqm.update_contexts(temp_ctxs)

        return ctxs
