"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators
"""
import logging as root_logger

from py_rule.util import OPERATOR_S, STATEMENT_S
from py_rule import util
from py_rule.abstract.printing import util as PrU
from py_rule.abstract.sentence import Sentence

from .value import PyRuleValue, PyRuleStatement

logging = root_logger.getLogger(__name__)

class ProductionOperator(PyRuleValue):
    """ The Base Operator Class """

    # A class variable to determine when an operator is instanced
    STATELESS_OP_CLASS = True
    op_dict = {}

    @staticmethod
    def construct_subclass_tree():
        """ Populate a dictionary for auto-generation of parser keywords """
        logging.info("Constructing Operator Subclass list")
        ProductionOperator.op_dict = {}
        found = []
        queue = [ProductionOperator]
        while bool(queue):
            current = queue.pop(0)
            if current in found:
                continue
            found.append(current)
            queue += current.__subclasses__()

        # At this point, all subclasses have been found
        for x in found:
            module_name = x.__module__
            operator_name = x.__name__
            full_name = "{}.{}".format(module_name, operator_name)
            value = x
            if value.STATELESS_OP_CLASS:
                value = value()
            ProductionOperator.op_dict[full_name] = value

        logging.info("Found {} operators".format(len(found)))

    def __init__(self, num_params=2,
                 infix=False,
                 type_str=OPERATOR_S):

        super().__init__(self.__class__.__name__,
                         type_str=type_str)
        # TODO this can be done using subclass DFS
        self._num_params = num_params
        # TODO use infix
        self._infix = infix

    def __call__(self, *params, data=None, engine=None):
        raise NotImplementedError()


    @property
    def op_str(self):
        return self._value


class ProductionComponent(PyRuleValue):
    """ Pairs a an operator with some bindings """

    def __init__(self, op_str, params, data=None, rebind=None,
                 type_str=None, name=None, op_class=ProductionOperator):
        assert(op_class is not None)
        if data is None:
            data = {}
        data.update({util.OP_CLASS_S : op_class})

        super().__init__(op_str, type_str=type_str, data=data, name=name)
        self._params = []
        self._rebind = rebind

        self.apply_params(params)
        self.verify()

    def __call__(self, ctx, engine):
        # lookup op
        self.verify()
        op_func = self._data[util.OP_CLASS_S].op_list[self.op]
        assert(x.name in ctx for x in self._vars)
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
        safe_params = [PyRuleValue.safe_make(x) for x in params]
        self._params += safe_params

    def set_rebind(self, bind):
        """ Set this transform to rebind its result to a different variable """
        assert(isinstance(bind, PyRuleValue))
        assert(bind.is_var and not bind.is_at_var)
        self._rebind = bind
        return self

    def get_params(self, data):
        """ Output a list of bindings from this action """
        output = []
        # TODO: enable currying
        for x in self._params:
            if isinstance(x, Sentence):
                output.append(x.bind(data))
            elif isinstance(x, list):
                output.append([y.bind(data) for y in x])
            elif isinstance(x, PyRuleValue) and x.is_var:
                if x.is_at_var:
                    output.append(data[util.AT_BIND_S + x.value])
                else:
                    output.append(data[x.value].value)
            else:
                output.append(x.value)
        return output

    def __refine_op_func(self, op_str):
        """ Replace the current op func set with a specific
        op func, used for type refinement """
        assert(op_str in self._data[util.OP_CLASS_S].op_list)
        self._value = op_str


    def to_sentence(self, target=None):
        raise NotImplementedError()

    def verify(self, op_constraint=None):
        """ Complains if the operator is not a defined Operator Enum """
        op_dict = {}
        if op_constraint is not None:
            if isinstance(list):
                [op_dict.update(x.op_list) for x in op_constraint]
            elif isinstance(op_constraint, dict):
                op_dict.update(op_constraint)
            elif isinstance(op_constraint, type):
                op_dict.update(op_constraint.op_list)
        else:
            op_dict.update(self._data[util.OP_CLASS_S].op_list)

        if self.op not in op_dict:
            raise AttributeError("Unrecognised operator: {}".format(self.op))


class ProductionContainer(PyRuleStatement):
    """ Production Container: An applicable statement """

    def __init__(self, clauses, params=None, type_str=STATEMENT_S, name=None):
        super().__init__(clauses, params=params, type_str=type_str, name=name)

    def __len__(self):
        return len(self.clauses)

    def __call__(self, ctxs=None, engine=None):
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
                    ctx[x._rebind.value] = PyRuleValue.safe_make(result)

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
            if isinstance(p, PyRuleValue):
                tempobj = p.var_set
                obj['in'].update(tempobj['in'])
                obj['out'].update(tempobj['out'])
        return obj


    def get_variables(self, data):
        # create a new context based on the data based in
        assert(isinstance(data, list))
        new_ctxs = []
        for ctx in data:
            current = {}
            current.update(ctx)
            for x in self._vars:
                current[x] = ctx[x]
            new_ctxs.append(current)

        return new_ctxs

    def to_sentences(self, target=None):
        return [x.to_sentence() for x in self.clauses]

    def verify(self, op_constraint=None):
        if op_constraint is None and util.OP_CLASS_S in self._data:
            op_constraint = self._data[util.OP_CLASS_S]
        for x in self.clauses:
            x.verify(op_constraint=op_constraint)

    def pprint_body(self, val):
        return val + PrU.print_container(self)


PrU.register_class(ProductionComponent, PrU.print_operator)
PrU.register_class(ProductionContainer, PrU.print_container)
