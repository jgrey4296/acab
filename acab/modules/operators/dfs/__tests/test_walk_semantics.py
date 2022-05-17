#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

import logging as logmod
import unittest
import unittest.mock as mock
import warnings
from os.path import split, splitext

import pyparsing as pp

logging = logmod.getLogger(__name__)


import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    if '@pytest_ar' in globals():
        from acab.core.parsing import debug_funcs as DBF
        DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)


from acab.core.parsing.annotation import ValueRepeatAnnotation
import acab.core.defaults.value_keys as DS
from acab.core.value.instruction import (Instruction, ProductionComponent,
                                         ProductionContainer)
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.modules.engines.configured import exlo
from acab.modules.operators.dfs import parser as DOP

BIND            = DS.BIND
QUERY           = DS.QUERY
SEM_HINT        = DS.SEMANTIC_HINT
TYPE_INSTANCE   = DS.TYPE_INSTANCE
AT_BIND         = DS.AT_BIND
CONSTRAINT      = DS.CONSTRAINT
default_modules = config.prepare("Module.REPL", "MODULES")().split("\n")

class TestWalkSemantics(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

        cls.eng = exlo()
        cls.eng.load_modules(*default_modules, "acab.modules.operators.dfs")

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def tearDown(self):
        self.eng("~a")
        self.eng("~found")
        self.eng("~the")


    def test_parsed_walk_query(self):
        query = DOP.dfs_query.parse_string("ᛦ $x(∈ c.e)?")[0]

        self.eng("a.b.c")
        self.eng("a.b.d")
        self.eng("a.b.e")

        result = self.eng(query)
        self.assertTrue(result)
        self.assertEqual(len(result), 2)
        bound = {ctx.x.name for ctx in result}
        self.assertEqual(bound, {"c", "e"})

    def test_query_walk_only_below_start(self):
        """
        @x ᛦ $y(∈ blah)?

        find a.b.c.test.sub.blah
        and not a.b.e.blah
        """

        self.eng("a.b.c.test.sub.blah")
        self.eng("a.b.d")
        self.eng("a.b.e.blah")

        # Setup a ctx binding 'x' to 'c'
        ctxs       = self.eng("a.b.$x.test?")

        # build a walk instruction
        source_var = AcabValue("x", data={BIND: AT_BIND})
        test_var   = AcabValue("y", data={BIND: True})
        ValueRepeatAnnotation(CONSTRAINT,
                              ProductionComponent(Sentence(["∈"], data={DS.TYPE_INSTANCE: DS.OPERATOR}),
                                                  params=[AcabValue("blah")]))(test_var)

        query_sen = Sentence([source_var, test_var],
                             data={SEM_HINT : "WALK", QUERY: True})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "QUERY"})

        # call walk
        result = self.eng(query, ctxset=ctxs)
        # check results
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0].x, "c")
        self.assertEqual(result[0].y, "blah")

    def test_query_walk_multi_start(self):
        """
        @x ᛦ $y(∈ blah)?
        """
        self.eng("a.b.c.test.sub.blah")
        self.eng("a.b.d.fail.sub.blah")
        self.eng("a.b.e.test.sub.blah")

        # Setup a ctx bound to 'c' and 'e'
        ctxs       = self.eng("a.b.$x.test?")

        # build a walk instruction
        source_var = AcabValue("x", data={BIND: AT_BIND})
        test_var   = AcabValue("y", data={BIND: True})

        ValueRepeatAnnotation(CONSTRAINT,
                              ProductionComponent(Sentence(["∈"], data={DS.TYPE_INSTANCE: DS.OPERATOR}),
                                                  params=[AcabValue("blah")]))(test_var)

        query_sen = Sentence([source_var, test_var],
                             data={SEM_HINT : "WALK", QUERY: True})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "QUERY"})

        # call walk
        result = self.eng(query, ctxset=ctxs)
        # check results
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0].x, "c")
        self.assertEqual(result[0].y, "blah")
        self.assertEqual(result[1].x, "e")
        self.assertEqual(result[1].y, "blah")

    def test_query_walk_no_matches(self):
        """
        @x ᛦ $y(∈ blah)?
        """
        self.eng("a.b.c.test.sub")
        self.eng("a.b.d.fail")
        self.eng("a.b.e.test.sub.fail")

        # Setup a ctx bonud to 'c'
        ctxs       = self.eng("a.b.$x.test?")

        # build a walk instruction
        source_var = AcabValue("x", data={BIND: AT_BIND})
        test_var   = AcabValue("y", data={BIND: True})

        ValueRepeatAnnotation(CONSTRAINT,
                              ProductionComponent(Sentence(["∈"], data={DS.TYPE_INSTANCE: DS.OPERATOR}),
                                                  params=[AcabValue("blah")]))(test_var)

        query_sen = Sentence([source_var, test_var],
                             data={SEM_HINT : "WALK", QUERY: True})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "QUERY"})

        # call walk
        result = self.eng(query, ctxset=ctxs)
        # check results
        self.assertEqual(len(result), 0)

    def test_query_walk_from_root(self):
        """
        ᛦ $y(::blah)?
        """
        self.eng("a.b.c.test.sub.blah")
        self.eng("a.b.d.fail")
        self.eng("a.b.e.test.other.blah")

        # build a walk instruction
        test_var   = AcabValue("y", data={BIND: True})
        ValueRepeatAnnotation(CONSTRAINT,
                              ProductionComponent(Sentence(["∈"], data={DS.TYPE_INSTANCE: DS.OPERATOR}),
                                                  params=[Sentence(["blah"])]))(test_var)

        query_sen = Sentence([test_var], data={SEM_HINT: "WALK", QUERY: True})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "QUERY"})

        # call walk
        result = self.eng(query)
        # check results
        self.assertEqual(len(result), 2)

    def test_query_walk_multi_patterns(self):
        """
        @x ᛦ $y(∈ blah).$z(∈ bloo)?
        """
        self.eng("a.b.c.test.sub.blah.bloo")
        self.eng("a.b.d.blah.fail")
        self.eng("a.b.e.test.sub.bloo.blah")

        # Setup a ctx bonud to 'c'
        ctxs       = self.eng("a.b.$x.test?")

        # build a walk instruction
        source_var = AcabValue("x", data={BIND: AT_BIND})
        test_var   = AcabValue("y", data={BIND: True})
        test_var2  = AcabValue("z", data={BIND: True})

        ValueRepeatAnnotation(CONSTRAINT,
                              ProductionComponent(Sentence(["∈"], data={DS.TYPE_INSTANCE: DS.OPERATOR}),
                                                  params=[Sentence(["blah"])]))(test_var)
        ValueRepeatAnnotation(CONSTRAINT,
                              ProductionComponent(Sentence(["∈"], data={DS.TYPE_INSTANCE: DS.OPERATOR}),
                                                  params=[Sentence(["bloo"])]))(test_var2)


        query_sen = Sentence([source_var, test_var],
                             data={SEM_HINT : "WALK", QUERY: True})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "QUERY"})

        # call walk
        result = self.eng(query, ctxset=ctxs)
        # check results
        self.assertEqual(len(result), 2)


    def test_parsed_act(self):
        """
        the.rule(::ρ):
        | $x |

        @x.test?

        !! found.$x
        end
        """

        # Setup
        self.eng("a.b.c.test")
        self.eng("a.b.d")
        self.eng("a.b.e.test")
        self.eng("~acab")

        # Action to run
        self.eng("""the.rule(::ρ):\n | $x |\n\n @x.test?\n\n !! found.$x\nend""")

        # dfs instruction
        ctxs = self.eng("the.$rule?")
        self.eng("a.$x?", ctxset=ctxs)

        # rule would be: | @x(::node) $a $b |
        inst = DOP.dfs_action.parse_string("@x ᛦ λ$rule")[0]
        self.eng(inst, ctxset=ctxs)

        self.assertTrue(self.eng("found.c?"))
        self.assertTrue(self.eng("found.e?"))

    def test_parsed_rule(self):
        self.eng("""the.rule(::ρ):\n | $x |\n\n @x.test?\n\n !! found.$x\nend""")
        ctxs = self.eng("the.$rule?")
        self.assertTrue(ctxs)

    def test_walk_all_action(self):
        """
        the.rule(::ρ):
        | $x |

        !! found.$x
        end
        """
        # Setup
        self.eng("a.b.c.test")
        self.eng("a.b.d!f")
        self.eng("a.b.e.test")
        self.eng("~acab")

        # Action to run
        self.eng("""the.rule(::ρ):\n | $x |\n\n !! found.$x\nend""".strip())
        # dfs instruction
        ctxs = self.eng("the.$rule?")
        self.eng("$x?", ctxset=ctxs)
        # rule would be: | @x(::node) $a $b |
        inst = DOP.dfs_action.parse_string("@x ᛦ λ$rule")[0]
        self.eng(inst, ctxset=ctxs)
        found = self.eng("found.$x?")

        # a,b,c,d,e,f,test,the,rule
        self.assertEqual(len(found), 9)

    def test_walk_all_action_fail_query(self):
        """
        the.rule(::ρ):
        | $x |

        !! found.$x
        end
        """
        # Setup
        self.eng("a.b.c.test")
        self.eng("a.b.d!f")
        self.eng("a.b.e.test")
        self.eng("~acab")

        # Action to run
        self.eng("""the.rule(::ρ):\n | $x |\n\n @x(∈ c.e.test.f)?\n\n !! found.$x\nend""".strip())
        # dfs instruction
        ctxs = self.eng("the.$rule?")
        self.eng("$x?", ctxset=ctxs)
        # rule would be: | @x(::node) $a $b |
        inst = DOP.dfs_action.parse_string("@x ᛦ λ$rule")[0]
        self.eng(inst, ctxset=ctxs)
        found = self.eng("found.$x?")

        # a,b,c,d,e,f,test,the,rule
        self.assertEqual(len(found), 4)

    def test_walk_all_action_sub_query(self):
        """
        the.rule(::ρ):
        | $x |

        !! found.$x
        end
        """
        # Setup
        self.eng("a.b.c.test")
        self.eng("a.b.d!f")
        self.eng("a.b.e.test")
        self.eng("~acab")

        # Action to run
        self.eng("""the.rule(::ρ):\n | $x |\n\n @x.test?\n\n !! found.$x\nend""".strip())
        # dfs instruction
        ctxs = self.eng("the.$rule?")
        self.eng("$x?", ctxset=ctxs)
        # rule would be: | @x(::node) $a $b |
        inst = DOP.dfs_action.parse_string("@x ᛦ λ$rule")[0]
        self.eng(inst, ctxset=ctxs)
        found = self.eng("found.$x?")

        # a,b,c,d,e,f,test,the,rule
        self.assertEqual(len(found), 2)






    @unittest.skip("Not implemented yet")
    def test_walk_all_action_no_prebind(self):
        """
        the.rule(::ρ):
        | $x |

        !! found.$x
        end
        """
        # TODO to finish
        # Setup
        self.eng("a.b.c.test")
        self.eng("a.b.d!f")
        self.eng("a.b.e.test")
        self.eng("~acab")

        # Action to run
        self.eng("the.rule(::ρ):\n | $x |\n\n !! found.$x\nend")
        # dfs instruction
        ctxs = self.eng("the.$rule?")
        self.eng("$x?", ctxset=ctxs)
        # rule would be: | @x(::node) $a $b |
        inst = DOP.dfs_action.parse_string("@x ᛦ λthe.rule")[0]
        self.eng(inst, ctxset=ctxs)



    @unittest.skip("not done yet")
    def test_parsed_act_without_head(self):
        pass

    @unittest.skip("not done yet")
    def test_parsed_act_rule_loc(self):
        pass
