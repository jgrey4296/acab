"""
The actual pattern matcher logic as a transform operator

"""
from py_rule.abstract.transform import TransformOp

class PatternMatchOp(TransformOp):
    def __init__(self):
        super(PatternMatchOp, self).__init__(-1)

    def __call__(self, node, patterns, data=None, engine=None):

        return None
