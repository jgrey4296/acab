from acab.abstract.module_interface import ModuleInterface

from . import pipeline_actions as PA

class MODULE(ModuleInterface):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):
        pt.add("operator.action.load_module", PA.PipelineLoadModule,
               "operator.action.run_layer", PA.PipelineRunLayer,
               "operator.action.run_pipeline", PA.PipelineRunPipeline)

    def query_parsers(self, pt):
        pass

    def init_strings(self):
        return []
