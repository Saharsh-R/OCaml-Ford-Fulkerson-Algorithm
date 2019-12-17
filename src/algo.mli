open Graph

val unvisited_neighbour : int graph-> id -> id list -> id list

type flow = int

val visit_course : int graph -> id -> id list -> id -> (id list * id list)

val var_flow : int graph -> id list -> int

val update_graph : int graph -> id list -> flow -> int graph

val loop_ff: (int graph * flow * id * id) -> (int graph * flow * id * id)