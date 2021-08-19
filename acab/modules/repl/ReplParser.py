"""
The parser for the REPL

"""
import logging as root_logger

import pyparsing as pp
from acab.abstract.config.config import AcabConfig
from acab.abstract.parsing import consts as PU
from acab.abstract.parsing import parsers as AP
from acab.modules.repl.util import build_slice

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

rst = pp.restOfLine()

# multi line ##################################################################
MULTI_LINE_START = config.prepare("Module.REPL", "MULTI_LINE_START")()
MULTI_LINE_END   = config.prepare("Module.REPL", "MULTI_LINE_END")()

multi_line_start = pp.Regex(MULTI_LINE_START)
multi_line_end = pp.Regex(MULTI_LINE_END)

multi_line_start.setParseAction(lambda s,l,t: "multi")
multi_line_end.setParseAction(lambda s,l,t: "multi")
# ##############################################################
# ie: sugar parser:

# TODO put sugar in config?
short_exit = pp.Keyword("q")("quit")
short_exit.setParseAction(lambda s,l,t: "exit")

short_break = pp.Keyword("b")("break")
short_break.setParseAction(lambda s,l,t: "break")

short_echo = pp.Keyword("e")("echo")
short_echo.setParseAction(lambda s,l,t: "echo")

short_save = pp.Keyword("s")("save")
short_save.setParseAction(lambda s,l,t: "save")

short_load = pp.Keyword("l")("load")
short_load.setParseAction(lambda s,l,t: "load")

# "tick"
short_step = pp.Keyword("t")("step")
short_step.setParseAction(lambda s,l,t: "step")

short_context = pp.Keyword("c")("context")
short_context.setParseAction(lambda s,l,t: "ctx")

short_print_context = pp.Keyword("pc")("print_ctx")
short_print_context.setParseAction(lambda s,l,t: "print_ctx")

sugared = pp.Suppress(pp.Literal(":")) + pp.Or([short_exit,
                                                short_break,
                                                short_echo,
                                                short_save,
                                                short_load,
                                                short_step,
                                                short_context,
                                                short_print_context,
                                                ]) + rst

precmd_parser = pp.Suppress(pp.LineStart()) + pp.Or([multi_line_start,
                                                     multi_line_end,
                                                     sugared,
                                                     rst])

# step kws ####################################################################
back_kw     = pp.Keyword("back")
rule_kw     = pp.Keyword("rule")
layer_kw    = pp.Keyword("layer")
pipe_kw     = pp.Keyword("pipe")
pipeline_kw = pp.Keyword("pipeline")
step_parser = pp.Or([back_kw,
                     rule_kw + rst,
                     layer_kw,
                     pipe_kw,
                     pipeline_kw])

# print kws ###################################################################
wm_kw        = pp.Keyword("wm")("wm")
module_kw    = pp.Keyword("module") + pp.Optional(pp.Word(pp.alphas)("mod_target"))
semantic_kw  = pp.Keyword("semantic")("semantic")

# TODO also handle bind without kw?
printer_parser = pp.Or([wm_kw,
                        module_kw,
                        semantic_kw,
                        rst])

# stat kws ####################################################################
operator_kw  = pp.Or(pp.Keyword("ops"),
                     pp.Keyword("operator"))("operator")

stats_parser = pp.ZeroOrMore(pp.Or([operator_kw,
                                    module_kw,
                                    semantic_kw]))

# listener ####################################################################

# add/remove/list/threshold

add_kw        = None
remove_kw     = None
list_kw       = None

listen_parser = pp.Or([])


# parser exploration ##########################################################
bootstrap_kw      = pp.Keyword("bootstrap")("bootstrap")
sugar_kw          = pp.Keyword("sugar")("sugar")

parse_info_parser = pp.Or([bootstrap_kw, sugar_kw, rst])

# result ########################################################################
number = pp.Optional(pp.Literal('-')("minus")) + pp.Word(pp.nums)
number.setParseAction(lambda s, l, t: int(t[-1]) if 'minus' not in t else -1 * int(t[-1]))

slice_p = PU.s(pp.Literal('[')) + \
    PU.op(number)('first') + \
    PU.op(pp.Literal(':')("mid") + \
          PU.op(number)("second")) + \
    PU.s(pp.Literal(']'))

slice_p.setParseAction(build_slice)

# $x, @x
ctx = number("short_context")
ctx_slice = slice_p("context_slice")
binding = AP.VALBIND


# ctx ([2:])? $x?
result_parser = pp.Optional(pp.Or([ctx,
                                   ctx_slice])) \
    + pp.ZeroOrMore(binding)("bindings")

# ctx select ##################################################################
ctx_index  = number("subset")
ctx_subset = slice_p("subset")
clear_kw   = pp.Keyword("clear")("clear")
minus_kw   = pp.Keyword("-")("clear")


ctx_select_parser = pp.Or([ctx_subset,
                           ctx_index,
                           clear_kw,
                           minus_kw,
                           rst])

# force parser ################################################################
query = pp.Word(pp.alphas + ".")("query") + pp.Suppress(pp.Literal("?"))
send_to_parser = rst("send")


force_parser = query + send_to_parser
