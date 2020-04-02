from py_rule.abstract.mod_interface import ModuleSpecification

class NetworkSpec(ModuleSpecification):

    def __init__(self):
        super().__init__(parser=TP.main_pattern,
                         types=[],
                         funcs=[])
