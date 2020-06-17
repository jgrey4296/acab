from acab.abstract.module_interface import ModuleInterface

from . import query_operators as QO
from . import query_op_parsers as QOP

class MODULE(ModuleInterface):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):
        pt.add("operator.query.eq", QO.EQ,
               "operator.query.neq", QO.NEQ,
               "operator.query.regmatch", QO.RegMatch,
               "operator.query.elem", QO.ELEM,
               "operator.query.hastag", QO.HasTag,
               "query.annotation.hastag", QOP.tagList)


    def query_parsers(self, pt):
        pass

    def init_strings(self):
        return []
