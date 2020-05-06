"""
The parser for the REPL

"""
import pyparsing as pp
import logging as root_logger

from py_rule.abstract.parsing import util as PU
from .repl_commands import ReplE as RE
from .repl_commands import build_command

logging = root_logger.getLogger(__name__)
HOTLOAD_COMMANDS = pp.Forward()

def build_slice(toks):
    result = None
    first  = 0
    second = 0
    if 'first' in toks:
        first = toks['first']
        result = first

    if 'second' in toks:
        second = toks['second'][0]
        result = slice(first, second)

    return result

def build_multiline(toks):
    param = True
    if toks[0][1] == "}":
        param = False

    return build_command(RE.MULTILINE, params=[param])


# Keywords
load_kw      = pp.Keyword('load')
save_kw      = pp.Keyword('save')
wm_kw        = pp.Keyword("wm")
bootstrap_kw = pp.Keyword("bootstrap")
pipeline_kw  = pp.Keyword("pipeline")
action_kw    = pp.Keyword("action")
operator_kw  = pp.Keyword("operator")
agenda_kw    = pp.Keyword("agenda")
binding_kw   = pp.Keyword("binding")
module_kw    = pp.Or([pp.Keyword(x) for x in ['module', 'mod']])

for_kw       = PU.s(pp.Keyword('for'))
act_kw       = PU.s(pp.Keyword('act'))
all_kw       = PU.s(pp.Keyword('all'))
back_kw      = PU.s(pp.Keyword('back'))
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
prompt_kw    = PU.s(pp.Keyword('prompt'))
from_kw      = PU.s(pp.Keyword('from'))

# Default: Instructions to pass to an Engine
empty_line = pp.lineStart + pp.lineEnd
rest_of_line = PU.op(PU.s(pp.White())) + pp.restOfLine

multi_line = pp.Regex("^:[{}]$")
multi_line_pop = pp.Literal(':pop')

# TODO
number = pp.Word(pp.nums)
slice_p = PU.s(pp.Literal('[')) + \
    PU.op(number).setResultsName('first') + \
    PU.op(PU.s(pp.Literal(':')) + number).setResultsName('second') + \
    PU.s(pp.Literal(']'))

param_p = pp.Or([number, slice_p])

# assertions / retractions / query
# eg: a.test.statement
# eg2: a.test.query?
base_statement = rest_of_line.copy()
nop_line = rest_of_line.copy()

# run rule/layer/pipeline (select appropriate method by its type)
# treat string as query
run_something = run_kw + PU.op(PU.BIND + from_kw + rest_of_line)
# print trie / query results / selected type
print_alts = pp.Or([wm_kw, bootstrap_kw, layer_kw, module_kw, pipeline_kw, binding_kw])

print_state   = print_kw + print_alts + PU.op(param_p)

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
load_mod = PU.s(module_kw) + rest_of_line

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
# TODO: add control over stats
stat_words = pp.Or([operator_kw, agenda_kw, rule_kw, pipeline_kw,
                    layer_kw, module_kw, wm_kw, binding_kw])
stats = stats_kw + pp.ZeroOrMore(stat_words)

# Set prompt
prompt_cmd = prompt_kw + rest_of_line

# Actions
slice_p.setParseAction      (build_slice)
number.setParseAction       (lambda toks: int (toks[0]))
rest_of_line.setParseAction (lambda toks: toks[0])
save_kw.setParseAction      (lambda toks: RE.SAVE)
load_kw.setParseAction      (lambda toks: RE.LOAD)
multi_line.setParseAction   (build_multiline)
multi_line_pop.setParseAction(lambda toks: (RE.POP))
empty_line.setParseAction   (lambda toks: build_command(RE.NOP, params=[]))

state_io.setParseAction        (lambda toks: build_command(toks[0], params=toks[1:]))
base_statement.setParseAction  (lambda toks: build_command(RE.PASS, params=toks[:]))
nop_line.setParseAction        (lambda toks: build_command(RE.NOP, params=toks[:]))
decompose.setParseAction       (lambda toks: build_command(RE.DECOMPOSE, params=toks[:]))
listen_for.setParseAction      (lambda toks: build_command(RE.LISTEN, params=toks[:]))
listener_remove.setParseAction (lambda toks: build_command(RE.LISTEN, params=toks[:]))
listeners.setParseAction       (lambda toks: build_command(RE.LISTEN, params=toks[:]))
load_mod.setParseAction        (lambda toks: build_command(RE.MODULE, params=toks[:]))
manual_act.setParseAction      (lambda toks: build_command(RE.ACT, params=toks[:]))
print_state.setParseAction     (lambda toks: build_command(RE.PRINT, params=toks[:]))
reinit.setParseAction          (lambda toks: build_command(RE.INIT, params=toks[:]))
run_something.setParseAction   (lambda toks: build_command(RE.RUN, params=toks[:]))
step.setParseAction            (lambda toks: build_command(RE.STEP, params=toks[:]))
type_check.setParseAction      (lambda toks: build_command(RE.CHECK, params=toks[:]))
prompt_cmd.setParseAction      (lambda toks: build_command(RE.PROMPT, params=toks[:]))
exit_cmd.setParseAction        (lambda toks: build_command(RE.EXIT))
help_cmd.setParseAction        (lambda toks: build_command(RE.HELP, params=[]))
stats.setParseAction           (lambda toks: build_command(RE.STATS, params=toks[:]))

# Names
main_commands = PU.s(pp.Literal(':')) + pp.Or([run_something,
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
                                               prompt_cmd,
                                               HOTLOAD_COMMANDS])



parse_point = pp.MatchFirst([multi_line,
                             main_commands,
                             empty_line,
                             base_statement])

alt_parse_point = pp.MatchFirst([multi_line,
                                 nop_line])

def parseString(in_string, in_multi_line=False):
    if not in_multi_line:
        result = parse_point.parseString(in_string)[0]
    else:
        logging.info("In ML: {}".format(in_multi_line))
        result = alt_parse_point.parseString(in_string)[0]
    return result
