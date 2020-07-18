"""
This Module provides the core components
for basic network communication between
the pipeline and a separate program (eg: Unity)

"""
from acab.abstract.module_interface import ModuleInterface

class MODULE(ModuleInterface):

    def __init__(self):
        super().__init__()

    def parse_string(self, s):
        return TP.parseString(s)

    def assert_parsers(self, pt):
        # TODO: Assert IO Actions
        return

    def query_parsers(self, pt):
        pass

    
MODULE_SPEC = MODULE()
