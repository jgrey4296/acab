"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators

Structure is:
ProductionOperator  : The implementation
ProductionComponent : Pairs the Operator with bindings
ProductionContainer : Groups Components together

"""
import logging as root_logger

from acab.abstract.printing import util as PrU
from acab.error.acab_operator_exception import AcabOperatorException
from acab.config import AcabConfig

from acab.abstract.core.value import AcabValue, AcabStatement
from acab.abstract.core.sentence import Sentence
from acab.abstract.core import type_base as TB

util = AcabConfig.Get()

OPERATOR_S = util("Parsing.Structure", "OPERATOR_S")
STATEMENT_S = util("Parsing.Structure", "STATEMENT_S")
AT_BIND_S = util("Parsing.Structure", "AT_BIND_S")

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

    def __init__(self, op_str, params, sugared=False, data=None, rebind=None, name=None):
        if data is None:
            data = {}

        assert(isinstance(op_str, (Sentence, ProductionOperator)))
        assert all([isinstance(x, AcabValue) for x in params]), params
        super().__init__(op_str, params=params, data=data, name=name, _type=TB.COMPONENT)
        # The value name of the result
        self._rebind = rebind
        # Sugared: Denotes whether the parse originated from a sugared operator
        # eg: $x ~= /blah/ -> $x
        self._sugared = sugared

    def __call__(self, ctx, engine):
        """
        Run the operation, on the passed in context and engine
        """
        assert(isinstance(ctx, dict))
        assert(x.name in ctx for x in self._params)
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


    def get_params(self, data):
        """ Output a list of bindings from this action,
        Unwraps as necessary
        """
        assert(isinstance(data, dict))
        output = []
        # TODO: enable currying
        for x in self._params:
            if isinstance(x, Sentence):
                output.append(x.bind(data))
            elif isinstance(x, list):
                output.append([y.bind(data) for y in x])
            elif isinstance(x, AcabValue) and x.is_var:
                assert(x.value in data)
                if x.is_at_var:
                    output.append(data[AT_BIND_S + x.value])
                elif isinstance(data[x.value], list):
                    # TODO does this need to unwrap all list values?
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


    def to_abstract_sentences(self, target=None):
        """

        """
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

        # TODO make op cached, rather than this:
        verified = self.copy()
        verified._value = op

        return verified


class ProductionContainer(AcabStatement):
    """ Production Container: An applicable statement of multiple component clauses """

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


    def to_abstract_sentences(self, target=None):
        return [y for x in self.clauses for y in x.to_abstract_sentences()]

    def verify(self, ctx=None, engine=None):
        for x in self.clauses:
            x.verify(ctx=ctx, engine=engine)

    def pprint_body(self, val):
        return val + PrU.print_container(self)


PrU.register_class(ProductionComponent, PrU.print_operator)
PrU.register_class(ProductionContainer, PrU.print_container)
