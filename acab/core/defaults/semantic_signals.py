#!/usr/bin/env python3
"""
These are the signals which map internal acab data
to semantic actions.

ie: Sentence(data={DS.SEMANTIC_HINT: signals.QUERY}) << ...
will map to the handler_spec in a semantic system using the signal
signals.QUERY, and run the attached handlers

"""

from acab import AcabConfig

config = AcabConfig()

signals = config.attr.Semantic.Signals
