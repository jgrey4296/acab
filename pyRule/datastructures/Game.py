from random import choice
import IPython

class Game:
    """ A Generalised N-Player, M-Move, K-Turn Game """

    def __init__(self, players=1, moves=2, turns=1, preconditions=None, name=None):
        assert(players >= 1)
        assert(moves >= 2)
        assert(turns >= 1)
        
        self.name = name
        self.players = players
        self.moves = moves
        self.turns = turns
        #The entry conditions for the game
        self.preconditions = preconditions
        #The decision tree of actions
        self.decision_tree = {}
        #The registered actions:
        #Action keys are the position tuple (player, move, turn)
        #Action values are strings to format then assert
        self.actions = {}
        #A Mapping for actions to query to get a value,
        #used to decide actions
        #Key: position tuple
        #value: a knowledgebase query for a single value
        self.precondition_utility = {}
        

    def generate_tree(self):
        """ Generate the decision tree from the players, moves, and turns, independent of the actions """
        raise Exception("Not implemented yet")

    def register_action(self, action, positionTuple, query=None):
        """ Register an action with a location of the tuple (player, move, turn) """
        assert(len(positionTuple) >= 2)
        #Auto-add turn if necessary
        if len(positionTuple) == 2:
            positionTuple = (positionTuple[0], positionTuple[1], 0)
        self.actions[positionTuple] = action
        self.precondition_utility[positionTuple] = query

    #todo: register moves for player,
    def register_player_actions(self, player, actions):
        """ Register a players actions. Specify all moves for a turn, then move to the next turn """
        assert(len(actions) == (self.moves * self.turns))
        move = 0
        turn = 0
        for action in actions:
            precon = None
            if isinstance(action, tuple) and len(action) == 2:
                action, precon = action
            self.register_action(action, (player, move, turn), query=precon)
            move += 1
            move = move % self.moves
            if move == 0:
                turn += 1
    
    def register_turn_actions(self, turn, actions):
        """ Register a turns actions. Specify all the moves for a player, then
        move to the next player. Moves can be tuples of (move, precondition) """
        assert(len(actions) == (self.players * self.moves))
        player = 0
        move = 0
        for action in actions:
            precon = None
            if isinstance(action, tuple) and len(action) == 2:
                action, precon = action
            self.register_action(action, (player, move, turn), query=precon)
            move += 1
            move = move % self.moves
            if move == 0:
                player += 1
    
    def verify(self):
        """ Verify that each move for each player has been defined """
        total_moves = self.players * self.moves * self.turns
        enough_moves_defined = len(self.actions) == total_moves

        enough_preconditions_defined = True
        if bool(self.precondition_utility):
            enough_preconditions_defined = len(self.precondition_utility) == total_moves

        return enough_moves_defined and enough_preconditions_defined
        
    
    def __call__(self, knowledgebase=None, data=None):
        """ Run the Game with the provided knowledgebase to query, 
        and a mapping of players. Return a sequence of moves.
        Returns: [ MoveString ]
        """
        if knowledgebase is None:
            return self.play_random(data=data)
        else:
            return self.play_with_assessments(knowledgebase, data)


    def play_random(self, data=None):
        """ Play a sequenece of the game, randomly/without value judgements or queries,
        but will format output based on the input data dict
        """
        assert(data is None or isinstance(data, dict))
        if data is None:
            data = {}
        results = []
        for turn in range(self.turns):
            for player in range(self.players):
                options = [self.actions[(player, i, turn)] for i in range(self.moves)]
                chosen_action = choice(options)
                results.append(chosen_action.format(**data))
        return results
        
    def play_with_assessments(self, knowledgebase, data=None):
        """ Play a sequence of the game, using max utility selection,
        will format any query or action by the data dictionary passed in.
        """
        assert(hasattr(knowledgebase, "query"))
        assert(data is None or isinstance(data, dict))
        if data is None:
            data = {}
        results = []
        for turn in range(self.turns):
            for player in range(self.players):
                moves = [self.actions[(player, i, turn)] for i in range(self.moves)]
                queries = [self.precondition_utility[(player, i, turn)] for i in range(self.moves)]
                #run the queries here
                values = [knowledgebase.query(x.format(**data)) for x in queries]
                combined = zip(values, moves)
                results.append(max(combined)[1].format(**data))
        return results

    def enter(self, knowledgebase):
        """ Passed a knowledgebase that is queryable, return whether this game is currently playable """
        assert(hasattr(knowledgebase, "query"))
        return bool(knowlegebase.query(self.preconditions))
