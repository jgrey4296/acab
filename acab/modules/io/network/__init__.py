"""
This Module provides the core components
for network communication between
the pipeline and a separate program (eg: Unity)

"""
from acab.abstract.module_interface import ModuleInterface

class MODULE(ModuleInterface):

    def __init__(self):
        super().__init__()

    def parse_string(self, s):
        return TP.parseString(s)

    def assert_parsers(self, pt):
        pt.add("value.time", TP.main_pattern)

    def query_parsers(self, pt):
        TP.HOTLOAD_VALUE << pt.query("valbind")
        TP.HOTLOAD_BIND << pt.query("valbind")

MODULE_SPEC = MODULE()
