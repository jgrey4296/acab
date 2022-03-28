#!/usr/bin/env python3
"""
Variations of HandlerSpecs for specific tasks
"""

import acab

config = acab.GET()

from acab.interfaces import handler_system as HS

class SpecReduce(HS.HandlerSpec):

    def __call__(self, *args, **kwargs):
        if len(args) < 2:
            args = list(args) + [self.struct]

        result = None
        for handler in self.handlers:
            result = handler(result, *args, **kwargs)

        return result


class SpecFirst(HS.HandlerSpec):

    def __call__(self, *args, **kwargs):
        if len(args) < 2:
            args = list(args) + [self.struct]

        for handler in self.handlers:
            result = handler(*args, **kwargs)
            if result is not None:
                return result

class SpecCollect(HS.HandlerSpec):
     def __call__(self, *args, **kwargs):
        if len(args) < 2:
            args = list(args) + [self.struct]

        results = []
        for handler in self.handlers:
            results.append(handler(*args, **kwargs))

        return results


class HandlerBreakpoint(HS.Handler):

    def __call__(self, *args, **kwargs):
        breakpoint()
