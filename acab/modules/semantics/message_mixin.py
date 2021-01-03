#!/usr/bin/env python3

import acab.abstract.interfaces.semantic_interfaces as SI

class MessageMixin(Generic[T], SI.SemanticMixin):
    """ Inter-Semantic Communication """
    context      : List[T]        = field(init=False, default_factory=list)
    stack        : List[T]        = field(init=False, default_factory=list)
    queue        : List[T]        = field(init=False, default_factory=list)
    accumulation : Dict[str, Any] = field(init=False, default_factory=dict)
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
