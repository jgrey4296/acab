#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import unittest.mock as mock
import logging as root_logger
logging = root_logger.getLogger(__name__)


from acab.abstract.config.config import AcabConfig
AcabConfig.Get("acab")

from acab.abstract.core.sentence import Sentence
from acab.abstract.core.value import AcabValue, AcabStatement

from acab.abstract.rule import production_operator as PO

util                 = AcabConfig.Get()
BIND_S               = util.value("Value.Structure", "BIND")
OPERATOR_TYPE_PRIM_S = util.value("Type.Primitive", "OPERATOR")

class ProductionOperatorTests(unittest.TestCase):

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
    def test_init_operator(self):
        op = PO.ProductionOperator()
        self.assertIsInstance(op, PO.ProductionOperator)
        # TODO OPERATOR, COMPONENT
        self.assertEqual(op.type, Sentence.build([OPERATOR_TYPE_PRIM_S]))

    def test_component_init(self):
        val = PO.ProductionComponent(Sentence.build(["testop"]), [])
        self.assertIsInstance(val, PO.ProductionComponent)
        self.assertIsInstance(val, AcabValue)

    def test_component_with_params(self):
        val = PO.ProductionComponent(Sentence.build(["testop"]), [AcabValue("a"), AcabValue("b")])
        self.assertIsInstance(val, PO.ProductionComponent)
        self.assertEqual(len(val._params), 2)

    def test_component_op(self):
        val = PO.ProductionComponent(Sentence.build(["testop"]), [])
        self.assertEqual(val.op, Sentence.build(["testop"]))

    def test_component_call(self):
        engine_mock = mock.Mock()
        engine_mock.get_operator = mock.Mock(return_value=lambda x, y, data=None, engine=None: x + y)

        test_sen = Sentence.build(["testop"])
        val = PO.ProductionComponent(test_sen, [AcabValue("a"), AcabValue("b")])
        result = val({}, engine_mock)

        engine_mock.get_operator.assert_called_with(test_sen)
        self.assertEqual(result, "ab")


    def test_apply_parameters(self):
        val = PO.ProductionComponent(Sentence.build(["testop"]), [])
        self.assertEqual(len(val._params), 0)
        val.apply_params(["a","test"])
        self.assertEqual(len(val._params), 2)

    def test_component_empty_var_set(self):
        """
        Component with variables as params should report those variables
        as inputs

        If the component rebinds its completed value, that should be
        reported as an output
        """
        val = PO.ProductionComponent(Sentence.build(["testop"]), [])
        var_set = val.var_set
        self.assertIsInstance(var_set, dict)
        self.assertIn("in", var_set)
        self.assertIn("out", var_set)
        self.assertFalse(bool(var_set['in']))
        self.assertFalse(bool(var_set['out']))

    def test_component_var_set(self):
        """
        Component with variables as params should report those variables
        as inputs

        If the component rebinds its completed value, that should be
        reported as an output
        """
        a_var = AcabValue("test")
        a_var.set_data({BIND_S : True})
        val = PO.ProductionComponent(Sentence.build(["testop"]), [a_var])
        var_set = val.var_set
        self.assertIsInstance(var_set, dict)
        self.assertIn("in", var_set)
        self.assertIn("out", var_set)
        self.assertFalse(bool(var_set['out']))

        self.assertTrue(bool(var_set['in']))
        self.assertIn(a_var, var_set['in'])

    def test_component_var_set_multi(self):
        """
        Component with variables as params should report those variables
        as inputs

        If the component rebinds its completed value, that should be
        reported as an output
        """
        a_var = AcabValue("test")
        a_var.set_data({BIND_S : True})
        b_var = AcabValue("blah")
        b_var.set_data({BIND_S : True})

        val = PO.ProductionComponent(Sentence.build(["testop"]), [a_var, b_var])
        var_set = val.var_set
        self.assertIsInstance(var_set, dict)
        self.assertIn("in", var_set)
        self.assertIn("out", var_set)
        self.assertFalse(bool(var_set['out']))

        self.assertTrue(bool(var_set['in']))
        self.assertIn(a_var, var_set['in'])
        self.assertIn(b_var, var_set['in'])

    def test_component_var_set_rebind(self):
        """
        Component with variables as params should report those variables
        as inputs

        If the component rebinds its completed value, that should be
        reported as an output
        """
        a_var = AcabValue("test")
        a_var.set_data({BIND_S : True})

        val = PO.ProductionComponent(Sentence.build(["testop"]), [], rebind=a_var)
        var_set = val.var_set
        self.assertIs(a_var, val._rebind)
        self.assertIsInstance(var_set, dict)
        self.assertIn("in", var_set)
        self.assertIn("out", var_set)
        self.assertFalse(bool(var_set['in']))

        self.assertTrue(bool(var_set['out']))
        self.assertIn(a_var, var_set['out'])


    def test_get_params_empty(self):
        """
        Component should take a set of bindings, and return its params with those bindings accounted for.
        So {x:2} on a Component(param="$x") -> [2]
        """
        val = PO.ProductionComponent(Sentence.build(["testop"]), [])
        params = val.get_params({})
        self.assertIsInstance(params, list)
        self.assertFalse(bool(params))

    def test_get_params_basic(self):
        a_var = AcabValue("test")
        a_var.set_data({BIND_S : True})

        val = PO.ProductionComponent(Sentence.build(["testop"]), [a_var])
        params = val.get_params({'test' : AcabValue("blah")})
        self.assertTrue(bool(params))
        self.assertEqual(params[0], "blah")

    def test_get_params_missing_binding(self):
        a_var = AcabValue("test")
        a_var.set_data({BIND_S : True})

        val = PO.ProductionComponent(Sentence.build(["testop"]), [a_var])
        with self.assertRaises(AssertionError):
            val.get_params({'blah' : AcabValue("blah")})


    @unittest.skip("TODO")
    def test_get_params_sentence(self):
        pass

    @unittest.skip("TODO")
    def test_get_params_list(self):
        pass

    @unittest.skip("TODO")
    def test_get_params_at_bind(self):
        pass


    @unittest.skip("TODO")
    def test_container_init(self):
        pass


    # TODO no clauses container init
    # TODO one clause container init
    # TODO multi clause container init
    # TODO malformed clause container init

    @unittest.skip("TODO")
    def test_container_call(self):
        pass

    @unittest.skip("TODO")
    def test_container_var_set(self):
        pass

    @unittest.skip("TODO")
    def test_get_vars(self):
        pass

    @unittest.skip("TODO")
    def test_container_to_sentences(self):
        pass


    # TODO container var set
    # TODO container verify

    @unittest.skip("TODO")
    def test_refine_op_func(self):
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
