# OCaml-Ford-Fulkerson-Algorithm
Ford Fulkerson Algorithm implementation to find the max flow of a graph.

## Compilation:
cd to the src folder in terminal and type the following command:

`ocamlbuild ftest.native`

It should show a "Finished" message.

You'll need to download ocamlbuild on your system to run this.
  
## Graphs  
I have already included 5 sample graphs in the 'in'; to create more graphs you can use the following web app:
  https://www-m9.ma.tum.de/graph-algorithms/flow-ford-fulkerson/index_en.html#tab_tg


## Running the algorithm

To run a test:
	
type the following command:
	
`./ftest.native path_to_graph_file source_node sink_node out.txt`
   
 It should display the following message: 
 
 `Maximum flow value starting from node x to the node y in this graph is: z`
 
 Note: You might have to create out.txt in the src folder
     
