# https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from os.path import splitext, split

import unittest
import unittest.mock as mock

import logging

from acab.abstract.engine.engine import Engine
from acab.abstract.core.values import Sentence
from acab.modules.io.importing.actions import ImportQuery
from acab.error.acab_import_exception import AcabImportException

class ImporterTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        self.e = Engine(mock.MagicMock())

    def tearDown(self):
        return 1

    #----------
    # use testcase snippet
    # mock.Mock / MagicMock
    # create_autospec
    # @patch(' ') / with patch.object(...)
    def test_import_init(self):
        self.assertFalse(self.e._loaded_modules)
        im_query = ImportQuery([Sentence.build(["acab","modules","operators","transform"])])
        result = im_query(None, self.e)
        self.assertTrue("acab.modules.operators.transform" in self.e._loaded_modules)

    def test_import_duplicate_guard(self):
        self.assertFalse(self.e._loaded_modules)
        self.e._loaded_modules.add('acab.modules.operators.transform')
        self.assertTrue("acab.modules.operators.transform" in self.e._loaded_modules)
        im_query = ImportQuery([Sentence.build(["acab","modules","operators","transform"])])
        result = im_query(None, self.e)
        self.assertTrue("acab.modules.operators.transform" in self.e._loaded_modules)

    def test_dsl_fragment_load(self):
        self.assertFalse(self.e._loaded_modules)
        im_query = ImportQuery([Sentence.build(["acab","modules","operators","transform"])])

        breakpoint()
        result = im_query(None, self.e)

        self.assertTrue("acab.modules.operators.transform" in self.e._loaded_modules)
        self.assertTrue("acab.modules.operators.transform" in self.e._loaded_DSL_fragments)

    def test_no_dsl_fragment(self):
        self.assertFalse(self.e._loaded_modules)
        im_query = ImportQuery([Sentence.build(["acab","modules","operators", "action"])])
        result = im_query(None, self.e)
        self.assertTrue("acab.modules.operators.action" in self.e._loaded_modules)
        self.assertFalse(self.e._loaded_DSL_fragments)

    @unittest.skip("TODO")
    def test_multi_import(self):
        pass

    def test_no_module_existence(self):
        im_query = ImportQuery([Sentence.build(["non","existent","module"])])
        result = im_query(None, self.e)
        self.assertFalse(result)

    # test_submodule_loading
    # test_manual_operator_dict_load

    # test_alias_creation

    # test_instances_overriding_classes


    # test_tag_env_activation
    # test_tag_env_deactivation

    # test_module_extraction

    # test_import_statement_parse

    # test_import_statement_run

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
