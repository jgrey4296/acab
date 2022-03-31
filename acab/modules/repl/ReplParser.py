"""
The parser for the REPL

"""
import logging as logmod

import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.parsing import consts as PU
from acab.core.parsing import parsers as AP
from acab.modules.repl.util import build_slice

logging = logmod.getLogger(__name__)
config  = AcabConfig()

rst = pp.delimited_list(pp.rest_of_line, delim=pp.White("\n\r"), combine=True).leave_whitespace()

# multi line ##################################################################
MULTI_LINE_START = config.prepare("Module.REPL", "MULTI_LINE_START")()
MULTI_LINE_END   = config.prepare("Module.REPL", "MULTI_LINE_END")()

multi_line_start = pp.Regex(MULTI_LINE_START)
multi_line_end = pp.Regex(MULTI_LINE_END)

multi_line_start.set_parse_action(lambda s,l,t: "multi")
multi_line_end.set_parse_action(lambda s,l,t: "multi")
# ##############################################################
shortcut_config   = config.attr.Module.REPL.shortcuts
short_cmd_parsers = []

# Build the shortcut parsers and their actions
def gen_action(cmd):
    def __action(s, l, t):
        return cmd

    return __action

for cmd in shortcut_config._keys:
    kw      = shortcut_config[cmd]
    s_cmd_p = pp.Keyword(kw)
    s_cmd_p.set_parse_action(gen_action(cmd))
    short_cmd_parsers.append(s_cmd_p)


sugared = pp.Suppress(pp.Literal(":")) + pp.MatchFirst(short_cmd_parsers) + rst

precmd_parser = pp.MatchFirst([multi_line_start,
                               multi_line_end + rst,
                               sugared,
                               rst]).leave_whitespace()

# step kws ####################################################################
back_kw     = pp.Keyword("back")
rule_kw     = pp.Keyword("rule")
layer_kw    = pp.Keyword("layer")
pipe_kw     = pp.Keyword("pipe")
pipeline_kw = pp.Keyword("pipeline")
step_parser = pp.MatchFirst([back_kw,
                             rule_kw + rst,
                             layer_kw,
                             pipe_kw,
                             pipeline_kw])

# stat kws ####################################################################
operator_kw  = pp.MatchFirst([pp.Keyword("ops"),
                              pp.Keyword("operator")])("operator")
module_kw    = pp.MatchFirst([pp.Keyword("mod"),
                              pp.Keyword("module")])("module")
semantic_kw  = pp.Keyword("semantics")("semantics")
printer_kw   = pp.Keyword("printers")("printers")

stats_parser = pp.ZeroOrMore(pp.MatchFirst([operator_kw,
                                            module_kw,
                                            semantic_kw,
                                            printer_kw]))

# listener ####################################################################

# add/remove/list/threshold
add_kw        = pp.Keyword("add")
remove_kw     = pp.Keyword("remove")
list_kw       = pp.Keyword("list")

listen_parser = pp.Or([add_kw, remove_kw, list_kw])


# parser exploration ##########################################################
signals_kw        = pp.Keyword("signals")("signals")
sugar_kw          = pp.Keyword("sugar")("sugar")
debug_kw          = pp.Suppress(pp.Keyword("debug"))
disable_kw        = pp.Keyword("disable")

debug_control     = debug_kw + pp.Or([disable_kw, list_kw, rst])("debug")
parse_info_parser = signals_kw | sugar_kw | debug_control | rst


# context ########################################################################
number = pp.common.integer

slice_p = (PU.s(pp.Literal('[')) +
           PU.op(number)('first') +
           PU.op(pp.Literal(':')("mid") +
                 PU.op(number)("second")) +
           PU.s(pp.Literal(']')))
slice_p.set_parse_action(build_slice)

ctx_kw    = pp.MatchFirst([pp.Keyword("ctx"),
                           pp.Keyword("c")])("context")
ctx       = number("short_context")
ctx_slice = slice_p("context_slice")

# $x, @x
binding = AP.VALBIND

# ctx ([2:])? $x?
ctx_parser = ctx_kw + pp.Optional(ctx | ctx_slice) \
            + pp.ZeroOrMore(binding)("bindings")


# ctx select ##################################################################
ctx_index  = number("subset")
ctx_subset = slice_p("subset")
clear_kw   = pp.Keyword("clear")("clear")
minus_kw   = pp.Keyword("-")("clear")


ctx_select_parser = pp.MatchFirst([ctx_subset,
                                   ctx_index,
                                   clear_kw,
                                   minus_kw,
                                   rst])
