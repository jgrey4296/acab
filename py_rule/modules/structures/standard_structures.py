from py_rule.abstract.module_interface import ModuleInterface

from .agenda import AgendaParser as AP
from .layer import LayerParser as LP
from .pipeline import PipelineParser as PP


class StandardStructures(ModuleInterface):
    """ The Module Spec for base structures
    ie: Agenda/Layer/Pipeline
    """

    def __init__(self):
        super().__init__(statement_ps=[AP.parse_point,
                                       LP.parse_point,
                                       PP.parse_point])

    def construct_operators(self):
        pass

    def init_strings(self):
        return []

    def insert_hotloads(self, data):
        # insert parsers into total parser statements
        for x in [AP, LP, PP]:
            x.HOTLOAD_BASIC_SEN << data['basic_sen']
            x.HOTLOAD_QUERY << data['query']
            x.HOTLOAD_TRANSFORM << data['transform']
            x.HOTLOAD_ACTION << data['action']
