#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.abstract.interfaces import semantic_interfaces as SI
from acab.modules.semantics.context_container import MutableContextInstance

CtxIns = 'ContextInstance'

class RuleAbstraction(SI.AbstractionSemantics):
    def __call__(self, struct, ProdSem, rule) -> List[Dict[Any, Any]]:
        """ Rule Logic, returns action proposals """
        # TODO init ctx container

        # Run the query
        if PConst.QUERY_V in rule:
            ProdSem.run(rule[PConst.QUERY_V])

        # TODO bind transform and actions to instances

        # Run any transforms
        transformed = ProdSem.get_results()
        if not bool(transformed):
            return

        if PConst.TRANSFORM_V in rule:
            transformed = ProdSem.run(rule[PConst.TRANSFORM_V])

        # *DELAYED* action results
        # return final passing dictionaries
        results = []
        for data in transformed:
            results.append((data, rule))

        ProdSem.record_results(results)


class LayerAbstraction(SI.AbstractionSemantics):
    def __call__(ProdSem, layer, ctxs=None):
        """ Run a layer, returning actions to perform """
        # rule returns [(data,ProdSem)]
        results = ProdSem.run(layer, ctxs=ctxs, override=PConst.RULE_V)

        logging.warning("Layer results: {}".format(len(results) < 1))
        # Run layer actions
        contexts = []
        if bool(results):
            contexts.append(results[0][0])

        action_results = ProdSem.run(layer[PConst.ACTION_V], ctxs=contexts)
        return action_results


class PipelineAbstraction(SI.AbstractionSemantics):
    def __call__(ProdSem, pipeline, ctxs=None):
        """ Run this pipeline on the given engine for a tick """
        results = ProdSem.run(pipeline, override=PConst.RULE_V)
        # TODO extract semantics
        # Run pipeline actions
        output = []

        return output


class AgendaAbstraction(SI.AbstractionSemantics):
    def __call__(ProdSem, agenda, ctxs=None):
        """ Runs an agenda rule on activated rules """
        assert(isinstance(ctxs, list))
        agenda_settings = ProdSem.run(agenda, ctxs=ctxs, override=PConst.RULE_V)

        # TODO extract semantics
        assert(len(agenda_settings) == 1)
        settings = agenda_settings[0][0]

        # Enact agenda
        resulting_ctxs = ProdSem.run(agenda[PConst.ACTION_V], ctxs=ctxs)
        return resulting_ctxs[0][Agenda.RETURN_NAME_S]


class ContainerAbstraction(SI.AbstractionSemantics):
    def __call__(ProdSem, container, ctxs=None):
        """ Apply the clauses in one move """
        ctxs = ProdSem._initial_ctx_construction(ctxs)

        for x in container.clauses:
            ctxs = ProdSem.run(x, ctxs)

        return ctxs


class TransformAbstraction(SI.AbstractionSemantics):
    """ Takes a context, returns a changed context """
    def __call__(self, transform, ctx:CtxIns, operators:CtxIns):
        # Note: run *all* the transforms at once,
        # To minimise redundent new ctxs
        mutable_ctx = MutableContextInstance.build(ctx)
        for clause in transform.clauses:
            op     = operators[clause.op]
            params = [mutable_ctx[x] for x in clause.params]
            result = op(*params, data=clause.data)
            mutable_ctx[clause.rebind] = result

        # bind ctx with results
        return mutable_ctx.finish()

class ActionAbstraction(SI.AbstractionSemantics):
    """ Takes a context, and the Semantic System / structs  """
    def __call__(action, engine=None, ctxs=None):
        # Get operators

        # get params

        # run operations

        return
