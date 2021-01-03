#!/usr/bin/env python3
import acab.abstract.interfaces.semantic_interfaces as SI

@dataclass
class HistoryHandler(SI.SemanticHandler):
    """ TODO Formerly Contexts
    Records the history of an action
    """
    clause_history        : List[Sentence] = field(init=False, default_factory=list)
    instruction_remainder : List[Sentence] = field(init=False, default_factory=list)

    def add(self, clause: Sentence):
        pass

    def set_remainder(self, remainder: List[Sentence]):
        pass
