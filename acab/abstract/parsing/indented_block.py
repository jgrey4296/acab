import logging as root_logger

import pyparsing as pp
from pyparsing import (Empty, FollowedBy, OneOrMore, Optional,
                       ParseElementEnhance, ParserElement, col,
                       conditionAsParseAction, matchOnlyAtCol)

logging = root_logger.getLogger(__name__)


class IndentedBlock(ParseElementEnhance):
    """
    Expression to match one or more expressions at a given indentation level.
    Useful for parsing text where structure is implied by indentation (like Python source code).
    """

    def __init__(self, expr: ParserElement, recursive: bool = True):
        super().__init__(expr, savelist=True)
        self._recursive = recursive
        self.callPreparse = True
        self.setWhitespaceChars(" ")

    def parseImpl(self, instring, loc, doActions=True):
        # see if self.expr matches at the current location - if not it will raise an exception
        # and no further work is necessary
        indent_col        = col(loc, instring)
        peer_parse_action = matchOnlyAtCol(indent_col)

        self.expr.parseImpl(instring, loc, doActions=False)

        # peer_expr       = FollowedBy(self.expr).addParseAction(peer_parse_action)
        inner_expr        = (pp.Empty() + self.expr).addParseAction(peer_parse_action)
        inner_expr.setWhitespaceChars(" \t\n")
        inner_expr.setName("indentInner")

        # if self._recursive:
        #     indent_parse_action = conditionAsParseAction(
        #         lambda s, l, t, relative_to_col=indent_col: col(l, s) > relative_to_col
        #     )
        #     indent_expr = FollowedBy(self.expr).addParseAction(indent_parse_action)
        #     inner_expr += Optional(indent_expr + self)

        return OneOrMore(inner_expr).parseImpl(instring, loc, doActions)
