"""
The parser for the REPL

"""
import pyparsing as pp

from py_rule.abstract.parsing import util as PU

# Default: Instructions to pass to an Engine

# TODO: assertions / retractions
# TODO: query
# TODO: run rule/layer/pipeline
# TODO: print state

# Instructions to modify engine
# TODO: save state
# TODO: load state
# TODO: initialise
# TODO: Step to start of prior layer
# TODO: Tick simulation (by amount)

# Instructions to load a module
# TODO: load module
# TODO: unload module?
# TODO: ReRun module init strings

# Misc Instructions
# TODO: perform an output

# TODO: Decompose rule/layer/pipeline
# TODO: Pause on assertion/retraction/rule/layer/pipeline/action

# Help Instructions
# TODO: Type Check all loaded
# TODO: Type Check this string
# TODO: Print Type of String
# TODO: Print stats

parse_point = pp.MatchFirst([])

def parseString(in_string):
    return parse_point.parseString(in_string)[:]
