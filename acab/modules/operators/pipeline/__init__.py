from acab.abstract.engine.dsl_fragment import DSL_Fragment

from .pipeline_actions import PipelineLoadModule, PipelineRunLayer, PipelineRunPipeline

class MODULE(DSL_Fragment):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):
        # pt.add("operator.action.load_module", PA.PipelineLoadModule,
        #        "operator.action.run_layer", PA.PipelineRunLayer,
        #        "operator.action.run_pipeline", PA.PipelineRunPipeline)
        pass

    def query_parsers(self, pt):
        pass

    def init_strings(self):
        return []
