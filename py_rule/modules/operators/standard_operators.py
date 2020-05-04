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


    def assert_parsers(self, pt):
        pt.add("operators.comparisons.eq", C.EQ,
               "operators.comparisons.neq", C.NEQ,
               "operators.comparisons.regmatch", C.RegMatch,
               "operators.comparisons.elem", C.ELEM)

        pt.add("operators.transform.ternary.regex", T.RegexOp,
               "operators.transform.binary.format", T.FormatOp,
               "operators.transform.statements.pattern_match", PMP.pattern_match_stmt,
               # Agenda:
               "operators.transform.statements.agenda_sort", AA.AgendaSort,
               "operators.transform.statements.agenda_select", AA.AgendaSelect,
               # Layer:
               "operators.transform.binary.run_agenda", LP.LayerRunAgenda,
               "operators.transform.unary.run_rules", LP.LayerRunRules)

        pt.add("operators.action.add", A.ActionAdd,
               "operators.action.print", A.ActionPrint,
               # Agenda:
               "operators.action.agenda_return", AA.AgendaReturn,
               # Layer:
               "operators.action.layer_perform", LP.LayerPerform,
               # Pipeline
               "operators.action.load_module", PP.PipelineLoadModule,
               "operators.action.run_layer", PP.PipelineRunLayer,
               "operators.action.run_pipeline", PP.PipelineRunPipeline)

    def query_parsers(self, pt):
        pass
    def init_strings(self):
        return []
