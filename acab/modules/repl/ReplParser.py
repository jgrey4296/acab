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

rst = pp.delimitedList(pp.restOfLine, delim=pp.White("\n\r"), combine=True).leaveWhitespace()

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

sugared = pp.Suppress(pp.Literal(":")) + pp.MatchFirst([short_exit,
                                                        short_break,
                                                        short_echo,
                                                        short_save,
                                                        short_load,
                                                        short_step,
                                                        short_context,
                                                        ]) + rst

pwm = pp.Keyword("pwm")("Print Working Memory")
pwm.setParseAction(lambda s,l,t: "print wm")

precmd_parser = pp.MatchFirst([multi_line_start,
                               multi_line_end + rst,
                               pwm,
                               sugared,
                               rst]).leaveWhitespace()

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
operator_kw  = pp.MatchFirst(pp.Keyword("ops"),
                             pp.Keyword("operator"))("operator")
module_kw    = pp.MatchFirst([pp.Keyword("mod"),
                              pp.Keyword("module")])("module")
semantic_kw  = pp.Keyword("semantics")("semantics")

stats_parser = pp.ZeroOrMore(pp.MatchFirst([operator_kw,
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
parse_info_parser = bootstrap_kw | sugar_kw | rst


# context ########################################################################
number = pp.Optional(pp.Literal('-')("minus")) + pp.Word(pp.nums)
number.setParseAction(lambda s, l, t: int(t[-1]) if 'minus' not in t else -1 * int(t[-1]))

slice_p = PU.s(pp.Literal('[')) + \
    PU.op(number)('first') + \
    PU.op(pp.Literal(':')("mid") + \
          PU.op(number)("second")) + \
    PU.s(pp.Literal(']'))

slice_p.setParseAction(build_slice)

ctx_kw = pp.MatchFirst([pp.Keyword("ctx"),
                        pp.Keyword("c")])("context")
ctx = number("short_context")
ctx_slice = slice_p("context_slice")

# $x, @x
binding = AP.VALBIND

# ctx ([2:])? $x?
ctx_parser = ctx_kw + pp.Optional(ctx | ctx_slice) \
            + pp.ZeroOrMore(binding)("bindings")

# print kws ###################################################################
wm_kw        = pp.Keyword("wm")("wm")
mod_target   = pp.Optional(pp.Word(pp.alphas + ".")("mod_target"))

# TODO also handle bind without kw?
printer_parser = pp.MatchFirst([wm_kw,
                                module_kw + mod_target,
                                semantic_kw,
                                ctx_parser,
                                rst])


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

# force parser ################################################################
query = pp.Word(pp.alphas + ".")("query") + pp.Suppress(pp.Literal("?"))
send_to_parser = rst("send")


force_parser = query + pp.Optional(send_to_parser)
