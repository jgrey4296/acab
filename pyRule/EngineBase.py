class EngineBase:
    """ The base class that wme and trie versions implement the interface of """
    
    def __init__(self, kb_constructor, path=None, init=None):
        self._knowledge_base = kb_constructor(init)
        self._rules = {}
        self._proposed_actions = []
        #to be updated with printed representations of the trie state after each action
        self._prior_states = []
        #named recall states of past tries
        self._recall_states = []
        #Registered custom actions
        self._custom_actions = {}
        if path is not None:
            self.load_file(path)


    def load_file(self, filename):
        raise Exception("Base Engine Stub")
        return

    def _save_state(self, data):
        raise Exception("Base Engine Stub")
    

    def register_action(self, name, func):
        raise Exception("Base Engine Stub")


    def add(self, s):
        raise Exception("Base Engine Stub")


    def retract(self, s):
        raise Exception("Base Engine Stub")


    def registerRules(self, s):
        raise Exception("Base Engine Stub")


    def clear_proposed_actions(self):
        raise Exception("Base Engine Stub")


    def __len__(self):
        raise Exception("Base Engine Stub")


    def _run_rules(self, rule_locations=None, rule_tags=None, policy=None):
        raise Exception("Base Engine Stub")


    def _run_rule(self, rule, propose=False):
        raise Exception("Base Engine Stub")


    def _run_transform(self, ctx, transform):
        raise Exception("Base Engine Stub")


    def _run_actions(self, data, ruleOrActions, propose=False):
        raise Exception("Base Engine Stub")


    def _perform_actions(elf, data, actions):
        raise Exception("Base Engine Stub")


    def _perform_action_by_policy(self, policy):
        raise Exception("Base Engine Stub")


    
