
open Graph

let clone_nodes_sub graph1 id = new_node graph1 id


let clone_nodes gr =n_fold gr clone_nodes_sub empty_graph


let gmap_sub graph1 pid node_id p f=


 if (node_exists graph1 pid)=false then

   if (node_exists graph1 node_id) = false then

     (new_arc (new_node (new_node graph1 pid) node_id) pid node_id (f p))

   else (new_arc (new_node graph1 pid) pid node_id (f p))

 else if (node_exists graph1 node_id) = false then

   (new_arc (new_node graph1 node_id) pid node_id (f p))

 else

   (new_arc graph1 pid node_id (f p))


let gmap gr f = let temp a b c d = gmap_sub a b c d f in

 e_fold gr temp (n_fold gr (fun graph id -> new_node graph id ) empty_graph)


let add_arc gr pid node_id n = match (find_arc gr pid node_id) with

 | None -> new_arc gr pid node_id n

 | Some x -> new_arc gr pid node_id (x+n)


