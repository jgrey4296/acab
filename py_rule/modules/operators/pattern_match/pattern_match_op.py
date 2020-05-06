"""
The actual pattern matcher logic as a transform operator

"""
from py_rule.abstract.transform import TransformOp

class PatternMatchOp(TransformOp):
    def __init__(self):
        super(PatternMatchOp, self).__init__(-1)

    def __call__(self, node, patterns, data=None, engine=None):
        assert(all([isinstance(x, tuple) for x in patterns]))
        # Given the node, run through patterns until one succeeds
        for patt in patterns:
            if patt[0](node):
                # TODO: extract binding from data if necessary
                return patt[1]

        raise Exception("Uncaught pattern match")
