#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from os.path import splitext, split

import unittest
import unittest.mock as mock

import logging as root_logger
logging = root_logger.getLogger(__name__)


import acab
config = acab.setup()

from acab.modules.operators.dfs.walk_semantics import WalkTrieSemantics
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.values import ExclusionNodeSemantics
from acab.modules.semantics.statements import QueryPlusAbstraction
from acab.modules.engines.configured import exlo
from acab.core.data.instruction import ProductionComponent, ProductionContainer
from acab.core.data.value import Sentence, AcabValue
from acab.modules.operators.dfs import dfs_op_parser as DOP

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
        cls.eng.load_modules(*default_modules, "acab.modules.operators.dfs.dfs_module")

    def tearDown(self):
        self.eng("~a")


    def test_parse_walk_query_instruction(self):
        result = DOP.dfs_query.parseString("ᛦ $x(::blah)?")[0]

        self.assertTrue(result)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(result.data['SEMANTIC_HINT'], 'WALK')
        self.assertTrue(result[-1].data['QUERY'])

    def test_parse_walk_action_instruction(self):
        result = DOP.dfs_action.parseString("ᛦ λa.test.op")[0]

        self.assertTrue(result)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(result.data['SEMANTIC_HINT'], 'WALK')
        self.assertNotIn('QUERY', result[-1].data)

        self.assertEqual(result[0], "_:a.test.op")

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
        source_var = AcabValue.safe_make("x", data={BIND: AT_BIND})
        test_var   = AcabValue.safe_make("y", data={BIND: True,
                                                    TYPE_INSTANCE: Sentence.build(["target"]),
                                                    QUERY : True})
        query_sen = Sentence.build([source_var, test_var],
                                   data={SEM_HINT : "_:WALK"})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "_:QUERY"})

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
        source_var = AcabValue.safe_make("x", data={BIND: AT_BIND})
        test_var   = AcabValue.safe_make("y", data={BIND: True,
                                                    TYPE_INSTANCE: Sentence.build(["target"]),
                                                    QUERY : True})
        query_sen = Sentence.build([source_var, test_var],
                                   data={SEM_HINT : "_:WALK"})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "_:QUERY"})

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
        source_var = AcabValue.safe_make("x", data={BIND: AT_BIND})
        test_var   = AcabValue.safe_make("y", data={BIND: True,
                                                    TYPE_INSTANCE: Sentence.build(["target"]),
                                                    QUERY : True})
        query_sen = Sentence.build([source_var, test_var],
                                   data={SEM_HINT : "_:WALK"})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "_:QUERY"})

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
        test_var   = AcabValue.safe_make("y", data={BIND: True,
                                                    TYPE_INSTANCE: Sentence.build(["target"]),
                                                    QUERY : True})
        query_sen = Sentence.build([test_var], data={SEM_HINT: "_:WALK"})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "_:QUERY"})

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
        source_var = Sentence.build([AcabValue.safe_make("x", data={BIND: AT_BIND})])
        test_var   = AcabValue.safe_make("y", data={BIND: True,
                                                    TYPE_INSTANCE: Sentence.build(["target"]),
                                                    QUERY : True})

        test_var2  = AcabValue.safe_make("z", data={BIND: True,
                                                    TYPE_INSTANCE: Sentence.build(["other"]),
                                                    QUERY : True})

        query_sen = Sentence.build([source_var, test_var, test_var2],
                                   data={SEM_HINT : "_:WALK"})
        query = ProductionContainer(value=[query_sen],
                                    data={SEM_HINT : "_:QUERY"})

        # call walk
        result = self.eng(query, ctxset=ctxs)
        # check results
        self.assertEqual(len(result), 2)


    def test_parsed_act(self):
        # Setup
        self.eng("a.b.c.test")
        self.eng("a.b.d")
        self.eng("a.b.e.test")
        self.eng("~acab")

        # Action to run
        self.eng("""
the.rule(::ρ):
        | $x |

        @x.test?

        !! found.$x
end""".strip())

        # dfs instruction
        ctxs = self.eng("the.$rule?")
        self.eng("a.$x?", ctxset=ctxs)
        # TODO handle without @head
        # TODO handle ᛦ λ$rule $y $z
        # rule would be: | @x(::node) $a $b |
        inst = DOP.dfs_action.parseString("@x ᛦ λ$rule")[0]

        self.eng(inst, ctxset=ctxs)

        self.assertTrue(self.eng("found.c?"))
        self.assertTrue(self.eng("found.e?"))
