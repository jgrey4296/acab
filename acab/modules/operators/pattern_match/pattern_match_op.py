"""
The actual pattern matcher logic as a transform operator

"""
from acab.core.data.production_abstractions import ProductionOperator

class PatternMatchOp(ProductionOperator):

    def __call__(self, node, *patterns, data=None):
        assert(all([isinstance(x, tuple) for x in patterns]))
        # Given the node, run through patterns until one succeeds
        for patt in patterns:
            if patt[0](node, data=data):
                # TODO: extract binding from data if necessary
                return patt[1](node, data=data)

        raise AcabSemanticException("Uncaught pattern match")
