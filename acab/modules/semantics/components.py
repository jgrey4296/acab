
# Components
class DebugListenerComponent():
    """ TODO formerly SubSemantics
    Listeners for triggers
    """
    """ The core used to *debug* WM action, using listeners """
    listeners : set[Any] = field(init=False, default_factory=set)
    listeners_threshold : Fraction = field(init=False, default=Fraction(1,2))
    # TODO use these to enable breakpoint context:
    _production_stack : list['ProductionAbstraction'] = field(init=False, default_factory=list)

    def clear_listeners(self):
        self._listeners = set()

    def register_listeners(self, words):
        self._listeners.update(words)

    def unregister_listeners(self, words):
        self._listeners.difference_update(words)

    def set_listener_threshold(self, a, b):
        self._listener_threshold = Fraction(a,b)

    def score_listener(self, words):
        simple_words = [str(x) if not x.is_var else "$_" for x in words]
        num_in_listener_bag = sum([1 if x in self._listeners else 0 for x in simple_words])
        sentence_fraction = Fraction(num_in_listener_bag, len(simple_words))
        if sentence_fraction >= self._listener_threshold:
            return True

        return False

    def breakpoint(self):
        # TODO: add more listener options: pre, on and post
        breakpoint()




    def add_listeners(self, *words):
        """ Add basic data breakpoints """
        self._working_memory.register_listeners(words)

    def remove_listeners(self, *words):
        """ Remove data breakpoints """
        self._working_memory.unregister_listeners(words)

    def set_listener_threshold(self, a, b):
        """ Specify the number of word matches
        are needed to trigger the breakpoint """
        self._working_memory.set_listener_threshold(a, b)

    def get_listeners(self):
        return self._working_memory._listeners

    def get_listener_threshold(self):
        return self._working_memory._listener_threshold


class MessageComponent(Generic[T]):
    """ Inter-Semantic Communication """
    context      : list[T]        = field(init=False, default_factory=list)
    stack        : list[T]        = field(init=False, default_factory=list)
    queue        : list[T]        = field(init=False, default_factory=list)
    accumulation : dict[str, Any] = field(init=False, default_factory=dict)
    # TODO: defaultdict

    # TODO refine this
    def _add_to_context(self, value):
        if isinstance(value, str):
            self._context.append(value)
        elif isinstance(value, list):
            self._context += value
        else:
            raise Exception("Expected a str or a list")

    def _add_to_accumulation(self, value):
        assert isinstance(value, dict)
        self._accumulation.update(value)

    def _push_stack(self, data, sentinel, params):
        assert isinstance(data, list)
        self._stack.append((self._queue, self._context))

        if sentinel is not None:
            data.append((SemUtil.RET_enum.SENTINEL, data, sentinel, params))

        self._queue = data
        self._context = []

    def _pop_stack(self):
        if not bool(self._queue) and bool(self._stack):
            stack_q, stack_ctx = self._stack.pop()
            self._queue = stack_q
            self._context = stack_ctx


@dataclass
class ListenerComponent():
    """ Maps aribitrary strings to semantics
    rather than mapping values or nodes
    """

    Components : dict[str, ListenerForm] = field(default_factory=dict)

    def call(self, instruction: Sentence) -> None|ListenerForm:
        """
        Take an instruction and return a suitable Component
        """
        pass


class PrinterComponent():
    """ Abstract Class of Print Semantics
    Provides the basic walk through of a value/node/container etc
    to call Components to produce a re-parseable string
    """
    # TODO add expectations of cleanup and message semantics

    def print(self, values: list[SemUtil.Printable]):
        """
        The public print function. Takes a list of values, converts them
        to str's, and combines them using a final-Component or "\n".join
        """
        logging.info("Starting Print on: {}", values)
        if not isinstance(values, list):
            values = [values]

        if overrides is not None:
            self.set_overrides(overrides)

        self._context: list[SemUtil.ContextValue] = []
        self._stack: list[SemUtil.StackValue] = []
        self._queue: list[SemUtil.SemBox] = [
            (SemUtil.RET_enum.PRINTABLE, x, None, None) for x in values
        ]
        self._accumulation: dict[str, Any] = {}

        while bool(self._stack) or bool(self._queue):
            result_instruction, result, result_sentinel = None, None, None
            if not bool(self._queue):
                instruction_tuple = (
                    SemUtil.RET_enum.SENTINEL,
                    "",
                    lambda ps, s, p, a, params: (SemUtil.RET_enum.SIMPLE, str(p), None, None),
                    None,
                )
            else:
                front = self._queue.pop(0)
                instruction_tuple = front

            assert len(instruction_tuple) == 4
            if instruction_tuple[0] is SemUtil.RET_enum.PASS:
                self._pop_stack()
                continue

            instruction_Component = self._instruction_mapping[instruction_tuple[0]]
            logging.info("--------------------")
            logging.info("Running      : {}".format(instruction_tuple))
            logging.info("Queue        : {}".format(len(self._queue)))
            logging.info("Context      : {}".format(" ".join(self._context)))
            logging.info("Accumulation : {}".format(str(self._accumulation)))
            logging.info("Stack        : {}".format(format(len(self._stack))))
            result = instruction_Component(
                self, instruction_tuple[1], instruction_tuple[2], instruction_tuple[3]
            )
            logging.info("Result       : {}".format(str(result)))
            assert isinstance(result, tuple)
            # Insert the result at the head of the queue, so its processed next
            self._queue.insert(0, result)

        return

    def finally(self, *params):
        if final_Component is not None:
            final_val = final_Component(self, self._context, self._accumulation)
            assert isinstance(final_val, str)
        else:
            # Filter out info tuples if necessary for default:
            default_join = self.use(PRINT_SEPARATOR_P)
            final_val = default_join.join(
                [x for x in self._context if isinstance(x, str)]
            )

        return final_val

@dataclass
class HistoryComponent():
    """ TODO Formerly Contexts
    Records the history of an action
    """
    clause_history        : list[Sentence] = field(init=False, default_factory=list)
    instruction_remainder : list[Sentence] = field(init=False, default_factory=list)

    def add(self, clause: Sentence):
        pass

    def set_remainder(self, remainder: list[Sentence]):
        pass




class FailureComponent():

    def __call__(self, *args, **kwargs):
        contexts, clause = args[:2]
        # add all failures back in, if theres a default value
        if QUERY_FALLBACK_S in clause.data and bool(clause.data[QUERY_FALLBACK_S]):
            contexts.promote_failures(clause.data[QUERY_FALLBACK_S])
        else:
            contexts.demote_failures()

