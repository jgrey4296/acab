from py_rule.abstract.module_interface import ModuleInterface

class NetworkSpec(ModuleInterface):

    def __init__(self):
        super().__init__(parser=TP.main_pattern,
                         types=[],
                         funcs=[])
