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
    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)


    import acab.core.defaults.value_keys as DS
    from acab.core.parsing import pyparse_dsl as ppDSL
    from acab.core.util.annotation import ValueRepeatAnnotation
    from acab.core.util.sentences import ProductionComponent
    from acab.core.value.instruction import Instruction, ProductionContainer
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.interfaces.value import ValueFactory as VF
    from acab.modules.engines.configured import exlo
    from acab.modules.operators.dfs import parser as DOP
    from acab.modules.operators.dfs.module import DFSExtension
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser

BIND            = DS.BIND
QUERY           = DS.QUERY
SEM_HINT        = DS.SEMANTIC_HINT
TYPE_INSTANCE   = DS.TYPE_INSTANCE
AT_BIND         = DS.AT_BIND
CONSTRAINT      = DS.CONSTRAINT
default_modules = config.prepare("Module.REPL", "MODULES")().split("\n")

WALK_TYPE  = VF.sen() << config.attr.Type.Primitive.INSTRUCT << config.attr.Semantic.Signals.WALK
QUERY_TYPE = VF.sen() << config.attr.Type.Primitive.INSTRUCT << config.attr.Type.Primitive.CONTAINER << config.attr.Type.Primitive.QUERY

logmod.getLogger("acab").setLevel(logmod.WARN)
logmod.getLogger("acab.modules.operators").setLevel(logmod.DEBUG)

class TestWalkSemantics(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

        cls.eng = exlo()
        cls.eng.load_modules(*default_modules, "acab.modules.operators.dfs")

        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser).register(DFSExtension().build_dsl())
        cls.dsl.build()

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def setUp(self):
        self.eng("~a")
        self.eng("~ran")
        self.eng("~found")
        self.eng("~the")
        self.eng("~walker")
        self.eng("~additional")
        self.eng("~acab")
        self.eng("~types")

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

        # build a walk instruction
        query = self.eng._dsl['sentence.ends'].parse_string("query(::γ):\n a.b.$x.test?\n @x ᛦ $y(∈ blah.bloo)?\nend")[0]
        # call walk
        result = self.eng(query)
        # check results
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0].x, "c")
        self.assertEqual(result[0].y, "blah")
        self.assertEqual(result[0].nodes['y'].node.parent().key(), "sub")

    def test_query_walk_multi_start(self):
        """
        @x ᛦ $y(∈ blah)?
        """
        self.eng("a.b.c.test.sub.blah")
        self.eng("a.b.d.fail.sub.blah")
        self.eng("a.b.e.test.sub.blah")

        # build a walk instruction
        query = self.eng._dsl['sentence.ends'].parse_string("query(::γ):\n a.b.$x.test?\n @x ᛦ $y(∈ blah.bloo)?\nend")[0]
        # call walk
        result = self.eng(query)
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

        # build a walk instruction
        query = self.eng._dsl['sentence.ends'].parse_string("query(::γ):\n a.b.$x.test?\n @x ᛦ $y(∈ blah.bloo)?\nend")[0]
        # call walk
        result = self.eng(query)
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
        query = self.dsl['query.statement'].parse_string("ᛦ $y(∈ blah.bloo)?")[0]
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

        # query = self.eng._dsl['sentence.ends'].parse_string("query(::γ):\n @x ᛦ $y(∈ blah) $z(∈ bloo)?\nend")[0]
        query = self.eng._dsl['query.statement'].parse_string("@x ᛦ $y(∈ blah) $z(∈ bloo)?")[0]
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
        results = self.eng("found.$x?")
        self.assertEqual(len(results), 2)


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

    def test_simple_walk_with_rule(self):
        """
        assert found.$x for all $x's
        """
        # add rule
        self.eng("""walker.rule(::ρ):\n | $x |\n\n @x?\n\n !! found.$x\nend """)

        ctxs       = self.eng("walker.$rules?")
        walk_instr = self.dsl['action.statement'].parse_string("ᛦ λ$rules")[0]
        # trigger walk
        self.eng(walk_instr, ctxset=ctxs)
        # test
        ctx_results = self.eng("found.$x?")
        self.assertEqual(len(ctx_results), 2)

    def test_walk_with_bound_rule(self):
        """
        assert found.$y for all $x(::$y)'s
        """
        # add rule
        self.eng("""walker.rule(::ρ):\n | $x |\n\n @x(::$y)?\n\n !! found.$y\nend """)

        ctxs       = self.eng("walker.$rules?")
        walk_instr = self.dsl['action.statement'].parse_string("ᛦ λ$rules")[0]
        # trigger walk
        self.eng(walk_instr, ctxset=ctxs)
        # test
        ctx_results = self.eng("found.$x?")
        self.assertEqual(len(ctx_results), 2)

    def test_walk_with_bound_rule2(self):
        """
        assert found.$y for all $x(::$y)'s
        with an additional type added
        """
        # add rule
        self.eng("""walker.rule(::ρ):\n | $x |\n\n @x(::$y)?\n\n !! found.$y\nend""")
        self.eng("additional.test(::blah)")
        walk_instr = self.dsl['action.statement'].parse_string("ᛦ λ$rules")[0]
        # trigger walk
        self.eng(walk_instr, ctxset=self.eng("walker.$rules?"))
        # test
        ctx_results = self.eng("found.$x?")
        self.assertEqual(len(ctx_results), 3)

    def test_walk_with_rules_prebound(self):
        """
        run two rules simultaneously in the walk,
        from pre-bound ctxs
        """
        # add rule
        self.eng("""walker.rule(::ρ):\n | $x |\n\n @x?\n\n !! ran.first_rule\nend""")
        self.eng("""walker.other(::ρ):\n | $x |\n\n @x?\n\n !! ran.second_rule\nend""")
        self.eng("additional.test(::blah)")
        walk_instr = self.dsl['action.statement'].parse_string("ᛦ λ$rules")[0]
        # trigger walk
        self.eng(walk_instr, ctxset=self.eng("walker.$rules?"))
        # test
        ctx_results = self.eng("ran.$x?")
        self.assertEqual(len(ctx_results), 2)

    def test_walk_with_rule_retrieval(self):
        """
        assert found.$y for all $x(::$y)'s
        """
        # add rule
        self.eng("""walker.rule(::ρ):\n | $x |\n\n @x(::$y)?\n\n !! found.$y\nend""")
        walk_instr = self.dsl['action.statement'].parse_string("ᛦ λwalker.rule")[0]
        # trigger walk
        self.eng(walk_instr, ctxset=self.eng("walker.$rules?"))
        # test
        ctx_results = self.eng("found.$x?")
        self.assertEqual(len(ctx_results), 2)

    def test_walk_with_rule_retrieval_var_multi(self):
        """
        assert found.$y for all $x(::$y)'s
        with an additional type added
        """
        # add rule
        self.eng("""walker.sen(::ρ):\n | $x |\n\n @x?\n\n !! ran.first\nend""")
        self.eng("""walker.other(::ρ):\n | $x |\n\n @x?\n\n !! ran.second\nend""")
        walk_instr = self.dsl['action.statement'].parse_string("ᛦ λwalker.$x")[0]
        # trigger walk
        self.eng(walk_instr)
        # test
        ctx_results = self.eng("ran.$x?")
        self.assertEqual(len(ctx_results), 2)

    def test_walk_with_rule_retrieval_var_type_constraint(self):
        """
        assert found.$y for all $x(::$y)'s
        with an additional type added
        """
        # add rule
        self.eng("""walker.sen(::ρ):\n | $x |\n\n @x?\n\n !! ran.first\nend""")
        self.eng("""walker.other(::ρ):\n | $x |\n\n @x?\n\n !! ran.second\nend""")
        self.eng("""walker.not_rule""")
        walk_instr = self.dsl['action.statement'].parse_string("ᛦ λwalker.$x(::INSTRUCT.STRUCTURE.RULE)")[0]
        # trigger walk
        self.eng(walk_instr)
        # test
        ctx_results = self.eng("ran.$x?")
        self.assertEqual(len(ctx_results), 2)

    def test_walk_with_rule_retrieval_var_us_existing_bind(self):
        """
        assert found.$y for all $x(::$y)'s
        with an additional type added
        """
        # add rule
        self.eng("""walker.sen(::ρ):\n | $x |\n\n @x(::$y)?\n\n !! found.$y\nend""")
        walk_instr = self.dsl['action.statement'].parse_string("ᛦ λwalker.$x")[0]
        self.eng("a.test.sen")
        # trigger walk
        self.eng(walk_instr, ctxset=self.eng("a.test.$x?"))
        # test
        ctx_results = self.eng("found.$x?")
        self.assertEqual(len(ctx_results), 2)


    @unittest.expectedFailure
    def test_walk_with_rule_retrieval_var_no_match(self):
        """
        assert found.$y for all $x(::$y)'s
        with an additional type added
        """
        # TODO maybe disallow action queries?
        # add rule
        walk_instr = self.dsl['action.statement'].parse_string("ᛦ λwalker.$x")[0]
        # trigger walk
        self.eng(walk_instr, ctxset=self.eng("walker.$rules?"))
        self.assertFalse(True)

    @unittest.skip
    def test_walk_with_operator_act(self):
        """
        In place transform of nodes?

        ᛦ λan.operator
        """
        self.eng("a.b.c\na.b.d\na.b.e")
        breakpoint()
        walk_instr = self.dsl['action.statement'].parse_string(r"ᛦ ~= /(\w)/  ")[0]
        self.eng(walk_instr)


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
