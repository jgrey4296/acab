from acab.interfaces.dsl import DSL_Fragment_i

from .pipeline_actions import PipelineLoadModule, PipelineRunLayer, PipelineRunPipeline

class MODULE(DSL_Fragment_i):
    """ The Module Spec for base operators """

    def assert_parsers(self, pt):
        # pt.add("operator.action.load_module", PA.PipelineLoadModule,
        #        "operator.action.run_layer", PA.PipelineRunLayer,
        #        "operator.action.run_pipeline", PA.PipelineRunPipeline)
        pass

    def query_parsers(self, pt):
        pass

