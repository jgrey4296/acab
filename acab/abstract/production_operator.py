"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators

Structure is:
ProductionOperator  : The implementation
ProductionComponent : Pairs the Operator with bindings
ProductionContainer : Groups Components together

"""
import logging as root_logger

from acab.util import OPERATOR_S, STATEMENT_S
from acab import util
from acab.abstract.printing import util as PrU
from acab.abstract.sentence import Sentence
from acab.error.acab_operator_exception import AcabOperatorException

from .value import AcabValue, AcabStatement
from . import type_base as TB

logging = root_logger.getLogger(__name__)

class ProductionOperator(AcabValue):
    """ The Base Operator Class """

    def __init__(self):
        super().__init__(self.__class__.__name__, _type=TB.OPERATOR)

    def __call__(self, *params, data=None, engine=None):
        raise NotImplementedError()


    @property
    def op_str(self):
        return self._value


class ProductionComponent(AcabValue):
    """ Pairs a an operator with some bindings """

    def __init__(self, op_str, params, op_pos=0, data=None, rebind=None, name=None):
        if data is None:
            data = {}

        assert(isinstance(op_str, (Sentence, ProductionOperator)))
        super().__init__(op_str, data=data, name=name, _type=TB.COMPONENT)
        # Parameters of the operation
        self._params = []
        # The value name of the result
        self._rebind = rebind
        # TODO The Position the operator takes: eg: 0: + 1 2, 1: 1 + 2
        self._op_position = op_pos

        self.apply_params(params)

    def __call__(self, ctx, engine):
        """
        Run the operation, on the passed in context and engine
        """
        assert(x.name in ctx for x in self._vars)
        # retrieve op func from active TagEnvs
        op_func = engine.get_operator(self.op)
        # get values from data
        values = self.get_params(ctx)
        # perform action op with data
        return op_func(*values, data=ctx, engine=engine)


    @property
    def op(self):
        return self._value

    @property
    def var_set(self):
        obj = super(ProductionComponent, self).var_set
        for p in self._params:
            tempobj = p.var_set
            obj['in'].update(tempobj['in'])
            obj['in'].update(tempobj['out'])

        if self._rebind is not None:
            obj['out'].add(self._rebind)
        return obj


    def apply_params(self, params):
        """ Safely wrap all passed in parameters as Values, then store """
        safe_params = [AcabValue.safe_make(x) for x in params]
        self._params += safe_params

    def get_params(self, data):
        """ Output a list of bindings from this action """
        output = []
        # TODO: enable currying
        for x in self._params:
            if isinstance(x, Sentence):
                output.append(x.bind(data))
            elif isinstance(x, list):
                output.append([y.bind(data) for y in x])
            elif isinstance(x, AcabValue) and x.is_var:
                if x.is_at_var:
                    output.append(data[util.AT_BIND_S + x.value])
                elif isinstance(data[x.value], list):
                    output.append(data[x.value])
                else:
                    output.append(data[x.value].value)
            else:
                output.append(x.value)
        return output

    def __refine_op_func(self, op_str):
        """ Replace the current op func set with a specific
        op func, used for type refinement """
        self._value = op_str


    def to_local_sentences(self, target=None):
        raise NotImplementedError()

    def verify(self, ctx=None, engine=None):
        """ Verify the Component, retrieving the operator from the engine
        if necessary """
        # $op -> retrieve from ctx
        op = self.op
        if len(op) == 1 and op[0].is_var and ctx is not None:
            op = ctx[op.value]

        if not isinstance(op, ProductionOperator) and engine is not None:
            op = engine.get_operator(op)

        # TODO: should op's be able to be Components and Containers as well?
        if not isinstance(op, ProductionOperator) and engine is not None:
            raise AcabOperatorException(op)

        verified = self.copy()
        verified._value = op

        return verified


class ProductionContainer(AcabStatement):
    """ Production Container: An applicable statement """

    def __init__(self, clauses, params=None, name=None, _type=TB.CONTAINER):
        if clauses is None:
            clauses = []
        super().__init__(clauses, params=params, name=name, _type=_type)

    def __len__(self):
        return len(self.clauses)

    def __call__(self, ctxs=None, engine=None):
        """ Apply the clauses in one move """
        if ctxs is None:
            ctxs = [{}]
        if not isinstance(ctxs, list):
            ctxs = [ctxs]

        for ctx in ctxs:
            for x in self.clauses:
                result = x(ctx, engine)
                if x._rebind is None and isinstance(result, dict):
                    ctx.update(result)
                if x._rebind is not None:
                    ctx[x._rebind.value] = AcabValue.safe_make(result)

        return ctxs

    def __iter__(self):
        for x in self.clauses:
            yield x


    @property
    def clauses(self):
        return self._value

    @property
    def var_set(self):
        """ Return a set of all bindings this container utilizes """
        # ie: Query(a.b.$x? a.q.$w?).var_set -> {'in': [], 'out': [x,w]}
        # Action(+(a.b.$x), -(a.b.$w)).var_set -> {'in': [x,w], 'out': []}
        obj = super(ProductionContainer, self).var_set
        for p in self.clauses:
            if isinstance(p, AcabValue):
                tempobj = p.var_set
                obj['in'].update(tempobj['in'])
                obj['out'].update(tempobj['out'])
        return obj


    def to_local_sentences(self, target=None):
        return [y for x in self.clauses for y in x.to_local_sentences()]

    def verify(self, ctx=None, engine=None):
        for x in self.clauses:
            x.verify(ctx=ctx, engine=engine)

    def pprint_body(self, val):
        return val + PrU.print_container(self)


PrU.register_class(ProductionComponent, PrU.print_operator)
PrU.register_class(ProductionContainer, PrU.print_container)
