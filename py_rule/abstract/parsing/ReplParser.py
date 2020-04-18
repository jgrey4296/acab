"""
The parser for the REPL

"""
import pyparsing as pp

from py_rule.abstract.parsing import util as PU
from .repl_commands import ReplE as RE

HOTLOAD_COMMANDS = pp.Forward()


# Keywords
load_kw      = pp.Keyword('load')
save_kw      = pp.Keyword('save')
kb_kw        = pp.Keyword('kb')

for_kw       = PU.s(pp.Keyword('for'))
act_kw       = PU.s(pp.Keyword('act'))
all_kw       = PU.s(pp.Keyword('all'))
back_kw      = PU.s(pp.Keyword('back'))
bindings_kw  = PU.s(pp.Keyword('bindings'))
check_kw     = PU.s(pp.Keyword('check'))
decompose_kw = PU.s(pp.Keyword('decompose'))
exit_kw      = PU.s(pp.Or([pp.Keyword(x) for x in ["exit", "quit"]]))
help_kw      = PU.s(pp.Keyword('help'))
init_kw      = PU.s(pp.Keyword('init'))
layer_kw     = PU.s(pp.Keyword('layer'))
listen_kw    = PU.s(pp.Keyword('listen'))
listener_kw  = PU.s(pp.Keyword('listener'))
listeners_kw = PU.s(pp.Keyword('listeners'))
print_kw     = PU.s(pp.Keyword('print'))
remove_kw    = PU.s(pp.Keyword('remove'))
rule_kw      = PU.s(pp.Keyword('rule'))
run_kw       = PU.s(pp.Keyword('run'))
stats_kw     = PU.s(pp.Keyword('stats'))
step_kw      = PU.s(pp.Keyword('step'))
module_kw    = PU.s(pp.Or([pp.Keyword(x) for x in ['module', 'mod']]))
prompt_kw    = PU.s(pp.Keyword('prompt'))

# Default: Instructions to pass to an Engine
rest_of_line = pp.Regex(".+?$")

multi_line = pp.Regex("^λ$")


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
load_mod = module_kw + rest_of_line

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
listener_remove = remove_kw + listener_kw + rest_of_line

exit_cmd = exit_kw

# Help Instructions
help_cmd = help_kw
# Type Check all loaded / this string
type_check = check_kw + pp.Optional(rest_of_line)
# Print stats
stats = stats_kw

# Set prompt
prompt_cmd = prompt_kw + pp.Word(pp.alphas) + pp.Optional(pp.Word(pp.alphas))

# Actions
save_kw.setParseAction    (lambda toks: RE.SAVE)
load_kw.setParseAction    (lambda toks: RE.LOAD)
multi_line.setParseAction (lambda toks: (RE.MULTILINE, None))
state_io.setParseAction   (lambda toks: (toks[0], toks[1:]))

base_statement.setParseAction  (lambda toks: (RE.PASS, toks[:]))
decompose.setParseAction       (lambda toks: (RE.DECOMPOSE, toks[:]))
listen_for.setParseAction      (lambda toks: (RE.LISTEN, toks[:]))
listener_remove.setParseAction (lambda toks: (RE.LISTEN, toks[:]))
listeners.setParseAction       (lambda toks: (RE.LISTEN, toks[:]))
load_mod.setParseAction        (lambda toks: (RE.MODULE, toks[:]))
manual_act.setParseAction      (lambda toks: (RE.ACT, toks[:]))
print_state.setParseAction     (lambda toks: (RE.PRINT, toks[:]))
reinit.setParseAction          (lambda toks: (RE.INIT, toks[:]))
run_something.setParseAction   (lambda toks: (RE.RUN, toks[:]))
step.setParseAction            (lambda toks: (RE.STEP, toks[:]))
type_check.setParseAction      (lambda toks: (RE.CHECK, toks[:]))
prompt_cmd.setParseAction      (lambda toks: (RE.PROMPT, toks[:]))

exit_cmd.setParseAction        (lambda toks: (RE.EXIT, None))
help_cmd.setParseAction        (lambda toks: (RE.HELP, None))
stats.setParseAction           (lambda toks: (RE.STATS, None))

# Names

parse_point = pp.MatchFirst([multi_line,
                             run_something,
                             print_state,
                             state_io,
                             reinit,
                             step,
                             load_mod,
                             manual_act,
                             listen_for,
                             type_check,
                             stats,
                             help_cmd,
                             exit_cmd,
                             HOTLOAD_COMMANDS,
                             base_statement])

def parseString(in_string):
    return parse_point.parseString(in_string)[0]
