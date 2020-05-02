from py_rule.abstract.module_interface import ModuleInterface

from .agendas import agenda_actions as AA
from .layer import layer_actions as LA
from .pipeline import pipeline_actions as PA
from .comparison import comparison_operators as C
from .action import action_operators as A
from .transform import transform_operators as T
from .pattern_match import pattern_match_op as PMO
from .pattern_match import pattern_match_parser as PMP


class StandardOperators(ModuleInterface):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def construct_operators(self):
        self._construct_comp_ops()
        self._construct_action_ops()
        self._construct_transform_ops()
        self._construct_agenda_actions()
        self._construct_layer_actions()
        self._construct_pipeline_actions()

    def _construct_comp_ops(self):
        C.EQ()
        C.NEQ()
        C.RegMatch()
        C.ELEM()

    def _construct_action_ops(self):
        A.ActionAdd()
        A.ActionPrint()

    def _construct_transform_ops(self):
        T.RegexOp()
        T.FormatOp()
        PMO.PatternMatchOp()

    def _construct_agenda_actions(self):
        AA.AgendaSelect()
        AA.AgendaSort()
        AA.AgendaSet()
        AA.AgendaReturn()

    def _construct_layer_actions(self):
        LA.LayerRunRules()
        LA.LayerRunAgenda()
        LA.LayerPerform()

    def _construct_pipeline_actions(self):
        PA.PipelineRunLayer()
        PA.PipelineRunPipeline()
        PA.PipelineLoadModule()


    def init_strings(self):
        return []

    def insert_hotloads(self, data):
        # TODO: insert PMP into transform hotload
        pass
