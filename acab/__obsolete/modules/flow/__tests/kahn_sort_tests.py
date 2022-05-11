#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from acab.modules.analysis.flow.kahn_sort import KahnSort

class KahnSortTests(unittest.TestCase):

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

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

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

    @unittest.skip("TODO")
    def test_failure(self):
        return

    @unittest.skip("TODO")
    def test_graph_2(self):
        return

    @unittest.skip("TODO")
    def test_empty_graph(self):
        return

    @unittest.skip("TODO")
    def test_cycle(self):
        return

    @unittest.skip("TODO")
    def test_island(self):
        return

    @unittest.skip("TODO")
    def test_multi_layer(self):
        return

    @unittest.skip("TODO")
    def test_initial_set_empty(self):
        return

    @unittest.skip("TODO")
    def test_multi_graph(self):
        return

    @unittest.skip("TODO")
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
