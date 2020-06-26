#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from acab.abstract.contexts import Contexts
from acab.util import AT_BIND_S

class ContextTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_basic(self):
        ctx = Contexts()
        self.assertIsNotNone(ctx)
        self.assertFalse(bool(ctx))
        self.assertEqual(len(ctx), 0)

    def test_static_initial(self):
        ctx = Contexts(start_node="test")
        self.assertIsNotNone(ctx)
        self.assertFalse(bool(ctx))
        self.assertEqual(len(ctx), 0)

    def test_append(self):
        ctx = Contexts()
        self.assertFalse(bool(ctx))
        self.assertEqual(len(ctx), 0)
        ctx.append(({}, "test"))
        self.assertTrue(bool(ctx))
        self.assertEqual(len(ctx), 1)
        ctx.append(({}, "test2"))
        self.assertEqual(len(ctx), 2)

    def test_append_2(self):
        ctx = Contexts()
        self.assertFalse(bool(ctx))
        self.assertEqual(len(ctx), 0)
        ctx.append(({}, "test"),
                   ({}, "test2"),
                   ({}, "test3"))
        self.assertTrue(bool(ctx))
        self.assertEqual(len(ctx), 3)

    def test_invert(self):
        ctx = Contexts()
        ctx.force_node_position(target="blah")
        self.assertTrue(bool(ctx))
        ctx.fail()
        self.assertFalse(bool(ctx))

    def test_iteration(self):
        ctx = Contexts()
        ctx.append(({'a': True}, "test"))
        ctx.append(({'b': True}, "test2"))
        ctx.append(({'c': True}, "test3"))

        for x,y in zip(ctx, ['a','b','c']):
            self.assertTrue(y in x.keys())

    def test_force_node_position_target(self):
        ctx = Contexts()
        ctx.append(({'a': True}, "test"))
        ctx.append(({'b': True}, "test2"))
        ctx.append(({'c': True}, "test3"))
        ctx.force_node_position(target="blah")
        self.assertEqual(3, len(ctx))
        for x,y in ctx.pairs():
            self.assertEqual(y, "blah")

    def test_all_alts_binding(self):
        ctx = Contexts()
        ctx.append(({AT_BIND_S + 'a': "blah"}, "test"))
        ctx.append(({AT_BIND_S + 'a': "bloo"}, "test2"))
        ctx.append(({AT_BIND_S + 'a': "blee"}, "test3"))
        ctx.force_node_position(binding="a")
        self.assertEqual(len(ctx), 3)
        for x,y in zip(ctx, ["blah","bloo","blee"]):
            self.assertEqual(y, x[AT_BIND_S + 'a'])

    def test_all_alts_invalidate_binding(self):
        ctx = Contexts()
        ctx.append(({AT_BIND_S + 'a': "blah"}, "test"))
        ctx.append(({AT_BIND_S + 'b': "bloo"}, "test2"))
        ctx.append(({AT_BIND_S + 'a': "blee"}, "test3"))
        ctx.append(({'a' : "awef"}, "test4"))
        ctx.force_node_position(binding="a")
        self.assertEqual(len(ctx), 2)
        for x,y in zip(ctx.pairs(), ["blah", "blee"]):
            self.assertEqual(x[1], y)



if __name__ == "__main__":
    #run python $filename to use this logging setup
    #using python -m unittest $filename won't
    LOGLEVEL = logging.INFO
    logFileName = "log.{}".format(splitext(split(__file__)[1])[0])
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
