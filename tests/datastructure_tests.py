import unittest
import logging
from test_context import pyRule
from pyRule.datastructures import Cycle, Game, TimeSpaceTree


class DataStructures_Tests(unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_cycle_init(self):
        self.assertIsNotNone(Cycle)
        aCycle = Cycle([])
        self.assertIsNotNone(aCycle)
        self.assertIsInstance(aCycle, Cycle)
        self.assertEqual(len(aCycle), 0)

    def test_game_init(self):
        self.assertIsNotNone(Game)
        aGame = Game()
        self.assertIsNotNone(aGame)
        self.assertIsInstance(aGame, Game)
        self.assertEqual(aGame.players, 1)
        self.assertEqual(aGame.moves, 2)
        self.assertEqual(aGame.turns, 1)

    def test_timespace_init(self):
        self.assertIsNotNone(TimeSpaceTree)
        aTSTree = TimeSpaceTree()
        self.assertIsNotNone(aTSTree)
        self.assertIsInstance(aTSTree, TimeSpaceTree)


    def test_cycle_simple(self):
        c = Cycle(list("abcd"))
        self.assertEqual(len(c), 4)
        self.assertEqual(c.current(), "a")
        self.assertEqual(c.current(), "a")
        c.increment()
        self.assertEqual(c.current(), "b")
        c.increment()
        self.assertEqual(c.current(), "c")
        self.assertEqual(c(), "c")
        self.assertEqual(c(), "d")
        self.assertEqual(c(), "a")
        self.assertEqual(len(c), 4)

    def test_simple_game(self):
        g = Game(players=1, moves=2, turns=1, preconditions=None)
        g.register_action("a test", (0, 0))
        g.register_action("a second test", (0, 1))
        results = g()
        self.assertEqual(len(results), 1)
        self.assertTrue(any([x in results for x in ["a test", "a second test"]]))

    def test_game_formatting(self):
        g = Game(players=1, moves=2, turns=1, preconditions=None)
        g.register_action("a {first}", (0, 0))
        g.register_action("b {second}", (0, 1))
        results = g(data={"first": "AWEF", "second": "VBNM"})
        self.assertEqual(len(results), 1)
        self.assertTrue(any([x in results for x in ["a AWEF", "b VBNM"]]))

    #todo: test multi turn game

    #todo: test multi player game

    #todo: test various move sizes games

    def test_game_verification(self):
        g = Game(players=2, moves=2, turns=1)
        self.assertFalse(g.verify())
        g.register_action("blah", (0, 0))
        self.assertFalse(g.verify())
        g.register_action("bloo", (0, 1))
        self.assertFalse(g.verify())
        g.register_action("awef", (1, 0))
        g.register_action("aweggg", (1, 1))
        self.assertTrue(g.verify())
        
        
    #todo: test simple TSTree

        

if __name__ == "__main__":
      #use python $filename to use this logging setup
      LOGLEVEL = logging.INFO
      logFileName = "log.DataStructures_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
