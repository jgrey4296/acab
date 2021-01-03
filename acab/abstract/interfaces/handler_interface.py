#!/usr/bin/env python3

class SemanticHandler(DependentSemantics):
    """ Handler.
    Handlers fulfill contextual expectations of a DependentSemantics
    Handlers can themselves be dependent on other contexts
    eg: read/write, results, listeners, cleanup
    """
    pass



@dataclass
class EngineAccessSemantics(SemanticHandler):
    """ TODO Provide full access to the surrounding engine """
    engine            : Engine               = field(init=False, default_factory=list)

    pass
