open Graph 
open Tools

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

type flow = int 

let rec visit_course graph actual visited_list final = 
  if(actual = final && ((List.mem final visited_list) = false)) then (add_visited visited_list [final],  [final])
  else 
    let neighbour1 = unvisited_neighbour graph actual visited_list in
    if(neighbour1 = []) 
    then (add_visited visited_list [actual],  [])
    else 
      let child_res = (List.fold_left (fun (lm,chem) x -> match (visit_course graph x lm final) with |(lma,chemr) -> (lma, add_visited chemr chem)) (add_visited visited_list [actual], []) neighbour1 ) in 
      match (child_res) with
      | (x,[]) -> ((actual::x),[])
      | (x,y) -> ((actual::x), (actual::y))

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

let rec continue_path node1 node2 path_list = 
  match path_list  with
  | a::b::rest -> if ((a=node1) && (b=node2)) then true else (false || (continue_path node1 node2 (b::rest)))
  | a::[] -> false
  | [] -> false

let update_graph_sub graph node1 node2 label path_list flow= 
  if (continue_path  node1 node2 path_list) 
  then 
    if((label-flow)=0) then (add_arc graph node2 node1 label) 
    else (add_arc  (add_arc graph node2 node1 (flow)) node1 node2 (label-flow))

  else (add_arc graph node1 node2 label)

let update_graph graph path_list flow = 
  let res_graph = clone_nodes graph in
  let nv_add_arc a b c d = (update_graph_sub a b c d path_list flow) in 
  e_fold graph nv_add_arc res_graph;;

let rec loop_ff (graph,flow,depart,arrive) = 
  match (visit_course graph depart [] arrive) with
  | (_,y) ->
    if (y=[]) 
    then (graph,flow,depart,arrive)
    else (
      let vari_flow = (var_flow graph y) in 
      let nv_graph = (update_graph graph y vari_flow) in

      loop_ff (nv_graph,(flow + vari_flow),depart,arrive)
    )