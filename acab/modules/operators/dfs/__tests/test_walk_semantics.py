#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

import logging as root_logger
import unittest
import unittest.mock as mock
from os.path import split, splitext

logging = root_logger.getLogger(__name__)


import acab

config = acab.setup()

from acab.core.data.instruction import ProductionComponent, ProductionContainer
from acab.core.data.value import AcabValue, Sentence
from acab.modules.engines.configured import exlo
from acab.modules.operators.dfs import parser as DOP
from acab.modules.operators.dfs.semantics import DFSSemantics
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.statements import QueryPlusAbstraction
from acab.modules.semantics.values import ExclusionNodeSemantics

BIND          = config.prepare("Value.Structure", "BIND")()
QUERY         = config.prepare("Value.Structure", "QUERY")()
SEM_HINT      = config.prepare("Value.Structure", "SEMANTIC_HINT")()
TYPE_INSTANCE = config.prepare("Value.Structure", "TYPE_INSTANCE")()
AT_BIND       = config.prepare("Value.Structure", "AT_BIND")()
default_modules = config.prepare("Module.REPL", "MODULES")().split("\n")

class TestWalkSemantics(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        root_logger.getLogger('').setLevel(root_logger.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = root_logger.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(root_logger.DEBUG)

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.WARNING)

        logging = root_logger.getLogger(__name__)
        logging.setLevel(root_logger.DEBUG)
        logging.addHandler(console)
        logging.addHandler(file_h)

        cls.eng = exlo()
        cls.eng.load_modules(*default_modules, "acab.modules.operators.dfs.module")

    def tearDown(self):
        self.eng("~a")
        self.eng("~found")
        self.eng("~the")


    def test_parsed_walk_query(self):
        query = DOP.dfs_query.parseString("ᛦ $x(::blah)?")[0]

        self.eng("a.b.c(::blah)")
        self.eng("a.b.d(::blah)")

        result = self.eng(query)
        self.assertTrue(result)
        self.assertEqual(len(result), 2)
        bound = {ctx.x.name for ctx in result}
        self.assertEqual(bound, {"c", "d"})

    def test_query_walk_only_below_start(self):
        """
        @x ᛦ $y(::target)?
        """

        self.eng("a.b.c.test.sub.blah(::target)")
        self.eng("a.b.d")
        self.eng("a.b.e.something(::target)")

        # Setup a ctx bound to 'c'
        ctxs       = self.eng("a.b.$x.test?")

        # build a walk instruction
        source_var = AcabValue.build("x", data={BIND: AT_BIND})
        test_var   = AcabValue.build("y", data={BIND: True,
                                                    TYPE_INSTANCE: Sentence.build(["target"]),
                                                    QUERY : True})
        query_sen = Sentence.build([source_var, test_var],
                                   data={SEM_HINT : "WALK"})
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
        @x ᛦ $y(::target)?
        """
        self.eng("a.b.c.test.sub.blah(::target)")
        self.eng("a.b.d")
        self.eng("a.b.e.test.something(::target)")

        # Setup a ctx bound to 'c' and 'e'
        ctxs       = self.eng("a.b.$x.test?")

        # build a walk instruction
        source_var = AcabValue.build("x", data={BIND: AT_BIND})
        test_var   = AcabValue.build("y", data={BIND: True,
                                                    TYPE_INSTANCE: Sentence.build(["target"]),
                                                    QUERY : True})
        query_sen = Sentence.build([source_var, test_var],
                                   data={SEM_HINT : "WALK"})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "QUERY"})

        # call walk
        result = self.eng(query, ctxset=ctxs)
        # check results
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0].x, "c")
        self.assertEqual(result[0].y, "blah")
        self.assertEqual(result[1].x, "e")
        self.assertEqual(result[1].y, "something")

    def test_query_walk_no_matches(self):
        """
        @x ᛦ $y(::target)?
        """
        self.eng("a.b.c.test.sub.blah(::nothing)")
        self.eng("a.b.d")
        self.eng("a.b.e.test.something(::nothing)")

        # Setup a ctx bonud to 'c'
        ctxs       = self.eng("a.b.$x.test?")

        # build a walk instruction
        source_var = AcabValue.build("x", data={BIND: AT_BIND})
        test_var   = AcabValue.build("y", data={BIND: True,
                                                    TYPE_INSTANCE: Sentence.build(["target"]),
                                                    QUERY : True})
        query_sen = Sentence.build([source_var, test_var],
                                   data={SEM_HINT : "WALK"})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "QUERY"})

        # call walk
        result = self.eng(query, ctxset=ctxs)
        # check results
        self.assertEqual(len(result), 0)

    def test_query_walk_from_root(self):
        """
        ᛦ $y(::target)?
        """
        self.eng("a.b.c.test.sub.blah(::target)")
        self.eng("a.b.d")
        self.eng("a.b.e.test.something(::target)")

        # build a walk instruction
        test_var   = AcabValue.build("y", data={BIND: True,
                                                    TYPE_INSTANCE: Sentence.build(["target"]),
                                                    QUERY : True})
        query_sen = Sentence.build([test_var], data={SEM_HINT: "WALK"})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "QUERY"})

        # call walk
        result = self.eng(query)
        # check results
        self.assertEqual(len(result), 2)

    def test_query_walk_multi_patterns(self):
        """
        @x ᛦ $y(::target) $z(::other)?
        """
        self.eng("a.b.c.test.sub.blah(::target)")
        self.eng("a.b.d")
        self.eng("a.b.e.test.something(::other)")

        # Setup a ctx bonud to 'c'
        ctxs       = self.eng("a.b.$x.test?")

        # build a walk instruction
        source_var = Sentence.build([AcabValue.build("x", data={BIND: AT_BIND})])
        test_var   = AcabValue.build("y", data={BIND: True,
                                                TYPE_INSTANCE: Sentence.build(["target"]),
                                                QUERY : True})

        test_var2  = AcabValue.build("z", data={BIND: True,
                                                TYPE_INSTANCE: Sentence.build(["other"]),
                                                QUERY : True})

        query_sen = Sentence.build([source_var, test_var, test_var2],
                                   data={SEM_HINT : "WALK"})
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
        self.eng("""the.rule(::ρ):\n        | $x |\n\n        @x.test?\n\n        !! found.$x\nend""")

        # dfs instruction
        ctxs = self.eng("the.$rule?")
        self.eng("a.$x?", ctxset=ctxs)
        # rule would be: | @x(::node) $a $b |
        inst = DOP.dfs_action.parseString("@x ᛦ λ$rule")[0]
        self.eng(inst, ctxset=ctxs)

        self.assertTrue(self.eng("found.c?"))
        self.assertTrue(self.eng("found.e?"))

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
        self.eng("""the.rule(::ρ):\n        | $x |\n\n        !! found.$x\nend""".strip())
        # dfs instruction
        ctxs = self.eng("the.$rule?")
        self.eng("$x?", ctxset=ctxs)
        # rule would be: | @x(::node) $a $b |
        inst = DOP.dfs_action.parseString("@x ᛦ λ$rule")[0]
        self.eng(inst, ctxset=ctxs)
        found = self.eng("found.$x?")

        # a,b,c,d,e,f,test,the,rule
        self.assertEqual(len(found), 9)

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
        self.eng("""the.rule(::ρ):\n        | $x |\n\n        !! found.$x\nend""".strip())
        # dfs instruction
        ctxs = self.eng("the.$rule?")
        self.eng("$x?", ctxset=ctxs)
        # rule would be: | @x(::node) $a $b |
        inst = DOP.dfs_action.parseString("@x ᛦ λthe.rule")[0]
        self.eng(inst, ctxset=ctxs)



    @unittest.skip("not done yet")
    def test_parsed_act_without_head(self):
        pass

    @unittest.skip("not done yet")
    def test_parsed_act_rule_loc(self):
        pass
