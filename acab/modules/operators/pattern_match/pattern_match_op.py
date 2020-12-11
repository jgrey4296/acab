"""
The actual pattern matcher logic as a transform operator

"""
from acab.abstract.containers.production_abstractions import ProductionOperator

class PatternMatchOp(ProductionOperator):

    def __call__(self, node, patterns, data=None, engine=None):
        assert(all([isinstance(x, tuple) for x in patterns]))
        # Given the node, run through patterns until one succeeds
        for patt in patterns:
            if patt[0](node, data=data, engine=engine):
                # TODO: extract binding from data if necessary
                return patt[1](node, data=data, engine=engine)

        raise Exception("Uncaught pattern match")
