"""
The parser for the REPL

"""
import pyparsing as pp

from py_rule.abstract.parsing import util as PU
from .repl_commands import ReplE as RE

# Keywords
act_kw       = pp.Literal('act')
all_kw       = pp.Literal('all')
back_kw      = pp.Literal('back')
bindings_kw  = pp.Literal('bindings')
check_kw     = pp.Literal('check')
decompose_kw = pp.Literal('decompose')
exit_kw      = pp.Literal('exit')
help_kw      = pp.Literal('help')
init_kw      = pp.Literal('init')
kb_kw        = pp.Literal('kb')
layer_kw     = pp.Literal('layer')
listen_kw    = pp.Literal('listen')
listeners    = pp.Literal('listeners')
load_kw      = pp.Literal('load')
print_kw     = pp.Literal('print')
reload_kw    = pp.Literal('reload')
remove_kw    = pp.Literal('remove')
rule_kw      = pp.Literal('rule')
run_kw       = pp.Literal('run')
save_kw      = pp.Literal('save')
stats_kw     = pp.Literal('stats')
step_kw      = pp.Literal('step')

# Default: Instructions to pass to an Engine
rest_of_line = pp.regex(".+?$")

multi_line = pp.regex("^λ$")


# assertions / retractions / query
# eg: a.test.statement
# eg2: a.test.query?
base_statement = rest_of_line.copy()

# run rule/layer/pipeline (select appropriate method by its type)
# treat string as query
run_something = run_kw + pp.Optional(all_kw) + rest_of_line
# print trie / query results / selected type
print_state   = print_kw + pp.Or([kb_kw,
                                  bindings_kw,
                                  rest_of_line])

# Instructions to modify engine
# eg: load ~/test/file.trie
state_io_cmds = pp.Or([save_kw, load_kw])
file_path = rest_of_line
# save / load state
state_io = state_io_cmds + file_path

# initialise
# eg: init py_rule.engines.trie_engine.TrieEngine
reinit = init_kw + pp.Optional(rest_of_line)

# step forward or back
# eg: step rule a.test.rule?
step = step_kw + pp.Optional(pp.Or([back_kw,
                                    rule_kw + rest_of_line,
                                    layer_kw + rest_of_line]))

# Instructions to load a module
# load module
# eg: load py_rule.modules.values.numbers
load_mod = load_kw + rest_of_line
# ReRun module init strings
# eg: reload py_rule.modules.values.numbers
re_init_mod = reload_kw + rest_of_line

# Misc Instructions
# perform an action manually
# eg: act print(a.test.print)
manual_act = act_kw + rest_of_line

# Decompose rule/layer/pipeline into bindings
# eg: decompose a.test.$rule?
decompose = decompose_kw + rest_of_line

# Pause on assertion/retraction/rule/layer/pipeline/action
# eg: listen for a.test.assertion
listen_for = listen_kw + for_kw  + rest_of_line
listeners = listeners_kw
remove_listener = remove_kw + listener_kw + rest_of_line

exit_cmd = exit_kw

# Help Instructions
help_cmd = help_kw
# Type Check all loaded / this string
type_check = check_kw + pp.Optional(rest_of_line)
# Print stats
stats = stats_kw

# Actions
save_kw.setParseAction(lambda toks: RE.SAVE)
load_kw.setParseAction(lambda toks: RE.LOAD)
multi_line.setParseAction(lambda toks: (RE.MULTILINE, None))

base_statement.setParseAction  (lambda toks: (RE.PASS, toks[:]))
decompose.setParseAction       (lambda toks: (RE.DECOMPOSE, toks[:]))
exit_cmd.setParseAction        (lambda toks: (RE.EXIT, None))
help_cmd.setParseAction        (lambda toks: (RE.HELP, None))
listen_for.setParseAction      (lambda toks: (RE.LISTEN, toks[:]))
listeners.setParseAction       (lambda toks: (RE.LISTEN, toks[:]))
load_mod.setParseAction        (lambda toks: (toks[0], toks[1:]))
manual_act.setParseAction      (lambda toks: (RE.ACT, toks[:]))
print_state.setParseAction     (lambda toks: (RE.PRINT, toks[:]))
re_init_mod.setParseAction     (lambda toks: (RE.LOAD, toks[1:]))
reinit.setParseAction          (lambda toks: (RE.INIT, toks[:]))
remove_listener.setParseAction (lambda toks: (RE.LISTEN, toks[:]))
run_something.setParseAction   (lambda toks: (RE.RUN, toks[:]))
state_io.setParseAction        (lambda toks: (toks[0], toks[1:]))
stats.setParseAction           (lambda toks: (RE.STATS, None))
step.setParseAction            (lambda toks: (RE.STEP, toks[:]))
type_check.setParseAction      (lambda toks: (RE.CHECK, toks[:]))

# Names

parse_point = pp.MatchFirst([multi_line,
                             run_something,
                             print_state,
                             state_io,
                             reinit,
                             step,
                             load_mod,
                             re_init_mod,
                             manual_act,
                             listen_for,
                             type_check,
                             stats,
                             help_cmd,
                             exit_cmd,
                             rest_of_line])

def parseString(in_string):
    return parse_point.parseString(in_string)[:]
