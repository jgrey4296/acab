""" Operators for manipulating types

Sum type pattern match?
Sum type combination

"""
from acab.core.data.production_abstractions import ActionOperator, AcabOperator

class TypeApply(ActionOperator):
    """
    Update a Ctx with new type assignments
    """


    def __call__(self, *args):
        pass


class TypeUnion(AcabOperator):
    """
    Unify two data structures to get a substitution list
    """

    def __call__(self, *args):
        pass

# TODO implement this, then deprecate query.typematch
@OperatorSugar("τ=")
class TypeMatch(ProductionOperator):
    """ Match a value's type to a passed in sentence """

    def __call__(self, a:Value, ctx:CtxIns, b:Sentence, data=None):
        a_type = a.type

        # TODO this will eventually be some sort of unify
        if a_type == b:
            return True

        for ax, bx in zip(a_type.words, b.words):
            if ax.is_var:
                ax = ctx[ax]
            if bx.is_var:
                bx = ctx[bx]

            if ax == bx:
                continue
            if ax.is_var or bx.is_var:
                continue

            return False

        return True
