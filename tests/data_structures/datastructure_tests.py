import unittest
import logging
from test_context import pyRule
from pyRule.datastructures import Game, TimeSpaceTree


class DataStructures_Tests(unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
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
        self.assertTrue(g.verify())
        self.assertTrue(any([x in results for x in ["a AWEF", "b VBNM"]]))

    def test_game_multi_player(self):
        g = Game(players=2, moves=2, turns=1, preconditions=None)
        g.register_action("a {first}", (0,0))
        g.register_action("a {second}", (0, 1))
        g.register_action("b {first}", (1, 0))
        g.register_action("b {second}", (1, 1))
        self.assertTrue(g.verify())
        results = g(data={"first" : "AWEF", "second": "VBNM"})
        self.assertEqual(len(results), 2)
        self.assertTrue(any([x in results[0] for x in ["a AWEF", "a VBNM"]]))
        self.assertTrue(any([x in results[1] for x in ["b AWEF", "b VBNM"]]))

    def test_game_register_player_actions(self):
        g = Game(players=1, moves=3, turns=2)
        g.register_player_actions(0, ["m0t0", "m1t0", "m2t0", "m0t1", "m1t1", "m2t1"])
        self.assertEqual(g.actions[(0,0,0)], "m0t0")
        self.assertEqual(g.actions[(0,1,0)], "m1t0")
        self.assertEqual(g.actions[(0,2,0)], "m2t0")
        self.assertEqual(g.actions[(0,0,1)], "m0t1")
        self.assertEqual(g.actions[(0,1,1)], "m1t1")
        self.assertEqual(g.actions[(0,2,1)], "m2t1")

    def test_game_register_turn_actions(self):
        g = Game(players=2, moves=2, turns=1)
        g.register_turn_actions(0, ["p0m0", "p0m1", "p1m0", "p1m1"])
        self.assertEqual(g.actions[(0,0,0)], "p0m0")
        self.assertEqual(g.actions[(0,1,0)], "p0m1")
        self.assertEqual(g.actions[(1,0,0)], "p1m0")
        self.assertEqual(g.actions[(1,1,0)], "p1m1")

    def test_game_multi_turn(self):
        g = Game(players=2, moves=2, turns=2)
        g.register_turn_actions(0, ["p0m0", "p0m1", "p1m0", "p1m1"])
        g.register_turn_actions(1, ["p0m0t1", "p0m1t1", "p1m0t1", "p1m1t1"])
        self.assertTrue(g.verify())
        results = g()
        self.assertEqual(len(results), 2*2)
        self.assertTrue(any([x in results[0] for x in ["p0m0", "p0m1"]]))
        self.assertTrue(any([x in results[1] for x in ["p1m0", "p1m1"]]))
        self.assertTrue(any([x in results[2] for x in ["p0m0t1", "p0m1t1"]]))
        self.assertTrue(any([x in results[3] for x in ["p1m0t1", "p1m1t1"]]))

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
