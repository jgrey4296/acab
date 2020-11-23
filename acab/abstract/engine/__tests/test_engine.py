#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import unittest.mock as mock
import logging as root_logger
logging = root_logger.getLogger(__name__)


from acab.abstract.config.config import AcabConfig
AcabConfig.Get().read("acab/abstract/config")

from acab.abstract.core.core_abstractions import AcabValue
from acab.abstract.engine.working_memory import WorkingMemory
from acab.abstract.engine.engine import Engine

# https://docs.python.org/3/library/unittest.mock.html
# mock.Mock / MagicMock
# create_autospec
# @patch(' ') / with patch.object(...)

class EngineTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    @unittest.skip("obsolete?")
    @mock.patch('acab.abstract.engine.WorkingMemory', autospec=True)
    def test_init(self, wm_mock):
        engine = Engine(wm_mock)
        wm_mock.assert_called_once_with(None)

    @unittest.skip("obsolete?")
    @mock.patch('acab.abstract.engine.ProductionOperator', autospec=True)
    @mock.patch('acab.abstract.engine.WorkingMemory', autospec=True)
    def test_load_modules(self, wm_mock, op_mock):
        module_mock = mock.Mock()
        engine = Engine(wm_mock, modules=[module_mock])
        op_mock.clear_registrations.assert_called_once()
        engine._working_memory.add_modules.assert_called_once()

    @unittest.skip("obsolete?")
    @mock.patch('acab.abstract.engine.ProductionOperator', autospec=True)
    @mock.patch('acab.abstract.engine.WorkingMemory', autospec=True)
    def test_reload_modules(self, wm_mock, op_mock):
        engine = Engine(wm_mock)
        engine.reload_all_modules()
        engine._working_memory.add_modules.assert_called()

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
