#!/usr/bin/env python3

import acab.abstract.interfaces.semantic_interfaces as SI

@dataclass
class SequenceMixin(SI.SemanticMixin):
    """ A Sequence of other semantics, used for productions """
    sequence: List['DependentSemantics'] = field(default_factory=list)

    def __post_init__(self):
        # lift all expectations
        self.expectations = set([y for x in self.sequence for y in x.expectations])
