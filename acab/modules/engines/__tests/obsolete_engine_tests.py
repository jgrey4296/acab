import unittest
import unittest.mock as mock
from os.path import splitext, split
import logging as logmod
logging = logmod.getLogger(__name__)

from math import isclose

import acab
acab.setup()

from acab.core.value.value import AcabValue
from acab.core.value.instruction import ProductionStructure

from acab.core.engine.engine import Engine
import acab.modules.parsing.exlo.TransformParser as TP
import acab.modules.parsing.exlo.ActionParser as AP


class Engine_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        logmod.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = logmod.StreamHandler()
        console.setLevel(logmod.INFO)
        logmod.getLogger('').addHandler(console)
        logging = logmod.getLogger(__name__)

    def setUp(self):
        self.e = Engine()
        self.e.build()

    def tearDown(self):
        self.e = None

    #----------
    #use testcase snippets
    def test_init(self):
        self.assertIsNotNone(self.e)

    def test_assert(self):
        self.assertEqual(len(self.e._working_memory._structure.root), 0)
        self.e.add('a.b.c')
        self.assertEqual(len(self.e._working_memory._structure.root), 1)

    def test_retract(self):
        self.e.add('a.b.c')
        self.assertEqual(len(self.e._working_memory._structure.root), 1)
        self.e.add('~a')
        self.assertEqual(len(self.e._working_memory._structure.root), 0)

    def test_query(self):
        self.e.add('a.b.c')
        result = self.e.query('a.b.c?')
        self.assertTrue(bool(result))

    def test_query_fail(self):
        self.assertFalse(self.e.query('a.b.c?'))

    def test_query_fail_with_asserted_facts(self):
        self.e.add('a.b.c, a.b.d')
        result = self.e.query('a.b.c?, a.b.e?')
        self.assertFalse(self.e.query('a.b.c?, a.b.e?'))

    def test_query_with_binds(self):
        self.e.add('a.b.c, a.b.d, a.d.c')
        self.assertTrue(self.e.query('a.b.$x?, a.d.$x?'))

    def test_query_with_binds_fail(self):
        self.e.add('a.b.c, a.b.d, a.d.e')
        self.assertFalse(self.e.query('a.b.$x?, a.d.$x?'))

    def test_multi_assert(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        self.assertEqual(len(self.e._working_memory._structure.get_nodes(pred=lambda x: not bool(x))), 3)
        self.assertTrue(self.e.query('a.b.c?, a.b.d?, a.b.e?'))

    def test_multi_retract(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        self.assertEqual(len(self.e._working_memory._structure.get_nodes(pred=lambda x: not bool(x))), 3)
        self.e.add('~a.b.e, ~a.b.d')
        self.assertEqual(len(self.e._working_memory._structure.get_nodes(pred=lambda x: not bool(x))), 1)

    def test_multi_clause_query(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        result = self.e.query('a.b.c?, a.b.d?, a.b.e?')
        self.assertTrue(result)

    def test_rule_assertion(self):
        self.assertFalse(self.e.query('a.test.rule?'))
        self.e.add('a.test.rule: (::ρ)\na.b.c?\n\na.b.d\nend')
        results = self.e.query('a.test.$rule?')
        self.assertTrue(results)
        self.assertIsInstance(results[0]['rule'], ProductionStructure)


    # TODO: in place of action registration, check operators are called appropriately



    @mock.patch('acab.interfaces.working_memory.WorkingMemoryCore', autospec=True)
    @mock.patch('acab.core.semantics.struct_semantics.AcabStructureSemantics', autospec=True)
    def test_init(self, wm_mock, sem_mock):
        engine = Engine(wm_mock)
        wm_mock.assert_called_once_with(None)

    @unittest.skip("TODO")
    @mock.patch('acab.core.value.instruction', autospec=True)
    @mock.patch('acab.interfaces.working_memory.WorkingMemoryCore', autospec=True)
    def test_load_modules(self, wm_mock, op_mock):
        module_mock = mock.Mock()
        engine = Engine(wm_mock, modules=[module_mock])
        op_mock.clear_registrations.assert_called_once()
        engine._working_memory.add_modules.assert_called_once()

    @mock.patch('acab.core.value.instruction', autospec=True)
    @mock.patch('acab.interfaces.working_memory.WorkingMemoryCore', autospec=True)
    def test_reload_modules(self, wm_mock, op_mock):
        engine = Engine(wm_mock)
        engine.build()
        engine._working_memory.construct_parsers_from_fragments.assert_called()

    @unittest.skip("TODO")
    def test_load_file(self):
        pass

    @unittest.skip("TODO")
    def test_save_file(self):
        pass

    @unittest.skip("TODO")
    def test_add_two(self):
        pass

    @unittest.skip("TODO")
    def test_query(self):
        pass

    @unittest.skip("TODO")
    def test_run_thing(self):
        pass


    # TODO to_sentences
