open Gfile
open Tools
open Algo

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  let () = write_file (outfile^"_initial") (graph) and intgraph = (gmap graph (fun x -> int_of_string(x)))in

  (); match (loop_ff (intgraph,0,_source,_sink))with 
  | (a,b,c,d) -> Printf.printf ("\nMaximum flow value starting from node %d to the node %d in this graph is: %d \n\n%!") _source _sink b;
  write_file (outfile^"_result") ((gmap a (fun x -> string_of_int(x))));
  export a (outfile^"_dot_initial");
  export intgraph (outfile^"_dot_result");;