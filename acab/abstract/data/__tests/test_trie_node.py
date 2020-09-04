import unittest
from os.path import splitext, split
import logging

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.data.node import AcabNode

ROOT_S = AcabConfig.Get()("Data.Trie", "ROOT_S")

class AcabNodeTests2(unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_node_creation(self):
        the_node = AcabNode("test")
        self.assertIsNotNone(the_node)
        self.assertIsInstance(the_node, AcabNode)
        self.assertEqual(the_node.name, "test")

    def test_root(self):
        root = AcabNode.Root()
        self.assertIsInstance(root, AcabNode)
        self.assertEqual(root.name, ROOT_S)


if __name__ == "__main__":
    #use python $filename to use this logging setup
    LOGLEVEL = logging.INFO
    logFileName = "log.{}".format(splitext(split(__file__)[1])[0])
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
