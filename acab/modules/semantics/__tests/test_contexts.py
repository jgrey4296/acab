from os.path import splitext, split
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)

import acab
config = acab.setup()

from acab.abstract.core.values import AcabValue
from acab.modules.semantics.context_set import ContextSet, ContextInstance
from acab.error.acab_semantic_exception import AcabSemanticException

class ContextsTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    #----------
    def test_set_basic(self):
        ctx = ContextSet()
        self.assertIsNotNone(ctx)
        self.assertTrue(bool(ctx))
        self.assertEqual(len(ctx), 1)

    def test_instance_basic(self):
        inst = ContextInstance()
        self.assertEqual(inst.data, {})
        self.assertEqual(inst.nodes, {})

    def test_instance_bindings(self):
        inst = ContextInstance(data={"a" : 2, "b" : 3})
        self.assertEqual(inst.data['a'], 2)
        self.assertEqual(inst.data['b'], 3)

    def test_instance_nodes(self):
        inst = ContextInstance(nodes={"a": 2, "b": 3})
        self.assertEqual(inst.nodes["a"], 2)
        self.assertEqual(inst.nodes["b"], 3)

    def test_instance_copy(self):
        inst = ContextInstance(data={"a": 2, "b": 3})
        inst2 = inst.copy()
        inst2.data["a"] = 5
        self.assertNotEqual(id(inst), id(inst2))
        self.assertNotEqual(inst.uuid, inst2.uuid)
        self.assertEqual(inst.data["a"], 2)
        self.assertEqual(inst2.data["a"], 5)

    def test_set_append(self):
        ctx = ContextSet()
        self.assertTrue(bool(ctx))
        ctx.pop()
        self.assertFalse(bool(ctx))
        self.assertEqual(len(ctx), 0)
        ctx.push(ContextInstance())
        self.assertTrue(bool(ctx))
        self.assertEqual(len(ctx), 1)
        ctx.push(ContextInstance())
        self.assertEqual(len(ctx), 2)

    def test_set_append_2(self):
        ctx = ContextSet()
        ctx.pop()
        self.assertFalse(bool(ctx))
        ctx.push([ContextInstance(),
                  ContextInstance(),
                  ContextInstance()])
        self.assertTrue(bool(ctx))
        self.assertEqual(len(ctx), 3)

    def test_set_fail(self):
        ctx = ContextSet()
        inst = ctx.pop()
        self.assertFalse(bool(ctx))
        ctx.fail(inst, "fail word", "fail_node")
        self.assertFalse(bool(ctx))
        self.assertEqual(inst._failure_word, "fail word")
        self.assertContains(inst.uuid, ctx._purgatory)

    def test_set_iteration(self):
        ctx = ContextSet()
        ctx.pop()
        ctx.push([ContextInstance({"a" : 1}),
                  ContextInstance({"b" : 2}),
                  ContextInstance({"c" : 3})])

        for x,y in zip(ctx, ['a','b','c']):
            self.assertTrue(y in x.data)


    def test_set_alts_binding(self):
        ctx = ContextSet()
        ctx.pop()
        ctx.push([ContextInstance(nodes={'a': "blah"}),
                  ContextInstance(nodes={'a': "bloo"}),
                  ContextInstance(nodes={'a': "blee"})])

        [x.set_current_binding(AcabValue("a")) for x in ctx.active_list()]
        self.assertEqual(len(ctx.active_list()), 3)
        for x,y in zip(ctx.active_list(), ["blah","bloo","blee"]):
            self.assertEqual(x._current, y)

    def test_set_alts_invalid_binding(self):
        ctx = ContextSet()
        ctx.pop()
        ctx.push([ContextInstance(nodes={'a': "blah"}),
                  ContextInstance(nodes={'b': "bloo"}),
                  ContextInstance(nodes={'a': "blee"})])
        with self.assertRaises(AcabSemanticException):
            [x.set_current_binding(AcabValue("a")) for x in ctx.active_list()]

    def test_set_enter_exit(self):
        pass

    def test_set_fail(self):
        pass

    def test_set_pop(self):
        pass

    def test_set_test(self):
        pass

    def test_instance_bind(self):
        pass

    def test_instance_contains(self):
        pass

    def test_instance_getitem(self):
        pass





    # collapse, clear,
    # group_by_type
