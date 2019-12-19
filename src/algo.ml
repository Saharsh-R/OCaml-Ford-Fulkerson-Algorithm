open Graph 
open Tools
type flow = int

let rec neighbour_sub l = match l with   
  |(id,x)::rest -> id::neighbour_sub rest 
  |[] -> []

let neighbour graph node = 
  neighbour_sub (out_arcs graph node)

let rec unvisited_neighbour_sub visited_list neighbour_list = 
  match (neighbour_list) with 
  | idneighbour::rest -> 
    if (List.mem idneighbour visited_list) then unvisited_neighbour_sub visited_list rest else idneighbour::(unvisited_neighbour_sub visited_list rest)
  | [] -> []

let unvisited_neighbour graph node visited_list = unvisited_neighbour_sub visited_list (neighbour graph node)


let add_visited visited_list element_list = List.append element_list visited_list 

 
(* Find out flow for a given path *)
let rec var_flow_sub graph path flow = 
  match path with 
  |a::b::rest ->( 
      match (find_arc graph a b) with 
      | None -> raise (Graph_error ("Error arc inexistant flow"))
      | Some x -> 
        if (flow = (-1)) 
        then (var_flow_sub graph (b::rest) x) 
        else (
          if(flow > x ) then (var_flow_sub graph (b::rest) x) else (var_flow_sub graph (b::rest) flow )))
  |b::[] -> flow
  |[] -> flow

let var_flow graph path = var_flow_sub graph path (-1);;


let rec visit_course graph current visited_list target = 
  if(current = target && ((List.mem target visited_list) = false)) then (add_visited visited_list [target],  [target])
  else 
    let neighbour1 = unvisited_neighbour graph current visited_list in
    if(neighbour1 = []) 
    then (add_visited visited_list [current],  [])
    else 
      let child_res = 
      (List.fold_left 
        (fun (rand,pth) x -> match (visit_course graph x rand target) with 
          |(randa,pthr) -> (randa, add_visited pthr pth)) 
        (add_visited visited_list [current], []) neighbour1 ) in 
      match (child_res) with
      | (x,[]) -> ((current::x),[])
      | (x,y) -> ((current::x), (current::y))

(* Find if node1 and node2 exist in path *)
let rec continue_path node1 node2 path = 
  match path with
  | a::b::rest -> if ((a=node1) && (b=node2)) then true else (false || (continue_path node1 node2 (b::rest)))
  | a::[] -> false
  | [] -> false

(* Label is current capacity between node1 and node2, flow is the flow value we're trying to send from node1 to node 2 *)
(* Updates edge weight between the two nodes *)
let update_graph_sub graph node1 node2 label path_list flow= 
  if (continue_path  node1 node2 path_list) (*If node1 and node2 edge exist in path*)
  then 
    if((label-flow)=0) then (add_arc graph node2 node1 label) (*if the edge is full*)
    else (add_arc  (add_arc graph node2 node1 (flow)) node1 node2 (label-flow))

  else (add_arc graph node1 node2 label) 

(* Updates the entire graph along the path*)
let update_graph graph path_list flow = 
  let res_graph = clone_nodes graph in
  let new_add_arc a b c d = (update_graph_sub a b c d path_list flow) in 
  e_fold graph new_add_arc res_graph;;

let rec loop_ff (graph,flow,start,ends) = 
  match (visit_course graph start [] ends) with
  | (_,y) ->
    if (y=[]) 
    then (graph,flow,start,ends)
    else (
      let vari_flow = (var_flow graph y) in 
      let new_graph = (update_graph graph y vari_flow) in

      loop_ff (new_graph,(flow + vari_flow),start,ends)
    )
