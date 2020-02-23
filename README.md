# Maze algorithm in Clojure

Implementation of Depth-first search (DFS) algorithm in Clojure for generation and solving a maze.

The algorithm is recursive and uses a matrix graph representation 
where 1 denotes a wall between two nodes while 0 denotes a path.

When a maze is initiated all neighbors are connected with walls.

## Generate algorithm

DFS algorithm starts from a given node v. The algorithm takes a random unvisited neighbor node as a new v node and breaks the wall between the two nodes,
while other neighbor unvisited nodes are pushed to stack.
This method repeats until the node v has no neighbors unvisited neighbors.
Then the algorithm takes an unvisited node from the stack and repeats the method.
The algorithm ends when all nodes are visited.

## Solve algorithm

The algorithm starts from a given node v and takes a random unvisited neighbor node as a new v where a path exists.
This method repeats until the destination node d has been reached.
After that, the algoritm prunes dead-end paths in the graph and ends when there is only one path between start and end nodes.

## License

Copyright © 2020 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
