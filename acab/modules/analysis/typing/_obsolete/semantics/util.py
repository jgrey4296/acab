#!/usr/bin/env python3

"""
subtype relation:
a.b.c
a.b.d
a.b.e

type.one:
 c
 d
end

type.two:
 c
end

type.two < type.one < b

"""
def check_node_against_definition(node, definition, ctx):
    """
    a.b.c(::the.type).e
    a.b.c.f

    the.type(::τ):
      e?
      f?
      g.h(::blah)?
    end


    check c against the.type
    """
    pass

def check_node_against_sentence(node, sentence, ctx):
    """
    a query starting at node:
    $x.y.z(::blah)? // :: operator as assignment and subtype relation

    """
    pass

def check_value_against_value(val, val2, ctx):
    """" a < b """

    pass

def check_sentence_against_sentence(sen1, sen2, ctx):
    """ a.b.c < a.b.c """
    pass

def check_subtype(val, a_type, ctx):
    pass
