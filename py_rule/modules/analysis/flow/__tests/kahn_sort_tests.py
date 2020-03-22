#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.modules.analysis.flow.kahn_sort import KahnSort

class KahnSortTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_initial(self):
        simple_edges = [("a","b"), ("b","c")]
        initial = set(["a"])
        success, failure = KahnSort.sort(simple_edges, initial)
        self.assertIsNone(failure)
        self.assertIsNotNone(success)

    def test_failure(self):
        return

    def test_graph_2(self):
        return

    def test_empty_graph(self):
        return

    def test_cycle(self):
        return

    def test_island(self):
        return

    def test_multi_layer(self):
        return

    def test_initial_set_empty(self):
        return

    def test_multi_graph(self):
        return

    def test_duplicate_names(self):
        return


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
