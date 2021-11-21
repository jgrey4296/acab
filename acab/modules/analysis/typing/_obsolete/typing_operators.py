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
