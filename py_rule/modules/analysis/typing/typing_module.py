from py_rule.abstract.mod_interface import ModuleSpecification
from .parsing import TypeDefParser as TDP
from .parsing import TypeParser as TP


class TypingSpec(ModuleSpecification):
    """ Typing Spec Class, providing entry points
    for an engine and working memory to handle type inference
    """

    def __init__(self):
        super().__init__(types=[],
                         funcs=[])
        # TODO setup value parsers
        # TODO setup statement parsers
        # add functions

    def parse_string(self, s):
        return TP.parseString(s)

    def construct_operators(self):
        # TODO
        return


    def define_layers(self):
        # TODO : add type classes to type checker
        # TODO : add definitions to type checker
        # TODO : add assertions to type checker
        # TODO : add rules to type checker
        # TODO : type_checker validate
        return []
