"""
A Modified version of topological sorting a graph
(https://en.wikipedia.org/wiki/Topological_sorting)
"""
from collections import defaultdict
import logging as root_logger
logging = root_logger.getLogger(__name__)

class KahnSort:

    @staticmethod
    def sort(edges, initial):
        """
        Expect graph to be a list of edge tuples,
        initial to be a set of nodes
        Returns Either (Layers, Conflict)
        """
        assert(isinstance(edges, list))
        assert(isinstance(initial, set))
        #initialise
        theGraph = KahnSort.make_graph(edges)

        exhausted = set()
        frontier = set(initial)
        maxLayer_map = defaultdict(lambda: 0)
        #explore the frontier
        while bool(frontier):
            current = frontier.pop()
            exhausted.add(current)
            #update the maxLayer count
            maxLayer = max([0] + [maxLayer_map[x] for x in theGraph[current]['in']])
            maxLayer_map[current] = maxLayer + 1
            #follow the edges
            edges = theGraph[current]['out'].copy()
            for x in edges:
                maxLayer_map[x] = max(maxLayer_map[current], maxLayer_map[x])
                theGraph[current]['out'].remove(x)
                theGraph[x]['in'].remove(current)
                if not bool(theGraph[x]['in']):
                    frontier.add(x)

            if not (bool(theGraph[current]['in']) or bool(theGraph[current]['out'])):
                del theGraph[current]

        if bool(theGraph):
            #detect a conflict
            active = set(maxLayer_map.keys()).difference(exhausted)
            activePairs = [(x,maxLayer_map[x]) for x in active]
            #get the smallest active
            smallestActive = min([(None,float("inf"))] + activePairs, key=lambda x: x[1])[0]
            #get its input edges
            smallestInputs = theGraph[smallestActive]['in']
            #get the largest input edge
            ancestors = [(x,maxLayer_map[x]) for x in smallestInputs]
            maxAncestor = max([(None, -1)] + ancestors, key=lambda x: x[1])[0]
            return (None, (maxAncestor, smallestActive))
        else:
            return (maxLayer_map, None)

    @staticmethod
    def make_graph(edges):
        graph = {}
        for a,b in edges:
            if a not in graph:
                graph[a] = {'in': set(), 'out': set()}
            if b not in graph:
                graph[b] = {'in': set(), 'out': set()}
            graph[a]['out'].add(b)
            graph[b]['in'].add(a)

        return graph

