open Batteries

type input_mode =
  | Sequential
  | Parallel

let default_input_mode = Parallel

type output_mode =
  | Share
  | Duplicate

let default_output_mode = Share

type process_vertex_data = {
  input_mode : input_mode;
  output_mode : output_mode;
  command : string;
  arguments : string list
}

type pipe_vertex_data = {
  input_mode : input_mode;
  output_mode : output_mode
}

type input_vertex_data = {
  input : Unix.file_descr;
  input_mode : input_mode
}

type output_vertex_data = {
  output : Unix.file_descr;
  output_mode : output_mode
}

type vertex_data =
  | Process of process_vertex_data
  | Pipe of pipe_vertex_data
  | Input of input_vertex_data
  | Output of output_vertex_data

type vertex_id = int

type vertex = {
  id : vertex_id;
  data : vertex_data
}

module Vertex = struct
   type t = vertex
   let compare x y = Pervasives.compare x.id y.id
   let hash x = Hashtbl.hash x.id
   let equal x y = x.id = y.id
end

module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)

type graph = {
  graph : G.t;
  mutable next_id : int
}

type +'a node = {
  graph : graph;
  vertex : vertex
} constraint 'a = [< `Process | `Pipe | `Input | `Output]

let create () = {
  graph = G.create ();
  next_id = 0
}

let add_vertex graph vertex_data =
  let vertex = {
    id = graph.next_id;
    data = vertex_data
  } in
  graph.next_id <- graph.next_id + 1;
  G.add_vertex graph.graph vertex;
  vertex

let add_node graph vertex_data =
  let vertex = add_vertex graph vertex_data in
  let node = {
    graph = graph;
    vertex = vertex
  } in
  node

let add_process_thing add_thing graph ?(input_mode = default_input_mode) ?(output_mode = default_output_mode) command arguments =
  let vertex = Process {
    input_mode = input_mode;
    output_mode = output_mode;
    command = command;
    arguments = arguments
  } in
  add_thing graph vertex
let add_process_vertex = add_process_thing add_vertex
let add_process_node = add_process_thing add_node

let add_pipe_thing add_thing graph input_mode output_mode =
  let vertex = Pipe {
    input_mode = input_mode;
    output_mode = output_mode
  } in
  add_thing graph vertex
let add_pipe_vertex = add_pipe_thing add_vertex
let add_pipe_node = add_pipe_thing add_node

let add_input_thing add_thing graph ?(input_mode = default_input_mode) file_descr =
  let vertex = Input {
    input = file_descr;
    input_mode = input_mode
  } in
  add_thing graph vertex
let add_input_vertex = add_input_thing add_vertex
let add_input_node = add_input_thing add_node

let add_output_thing add_thing graph ?(output_mode = default_output_mode) file_descr =
  let vertex = Output {
    output = file_descr;
    output_mode = output_mode
  } in
  add_thing graph vertex
let add_output_vertex = add_output_thing add_vertex
let add_output_node = add_output_thing add_node
 
let (>>) source destination =
  if source.graph != destination.graph then
    failwith ">> can only be used on two nodes of the same graph!"
  else ();
  G.add_edge source.graph.graph source.vertex destination.vertex

let wrap_input_if_needed (graph : graph) input_vertex =
  match input_vertex.data with
  | Process {
    input_mode = input_mode;
    output_mode = _;
    command = _;
    arguments = _
  }
  | Input {
    input = _;
    input_mode = input_mode
  } ->
    if G.out_degree graph.graph input_vertex > 1 then begin
      let pipe_vertex = add_pipe_vertex graph input_mode default_output_mode in
      G.iter_succ (fun output_vertex ->
        G.remove_edge graph.graph input_vertex output_vertex;
        G.add_edge graph.graph pipe_vertex output_vertex
      ) graph.graph input_vertex;
      G.add_edge graph.graph input_vertex pipe_vertex 
    end else ()
  | _ -> ()

let wrap_output_if_needed (graph : graph) output_vertex =
  match output_vertex.data with
  | Process {
    input_mode = _;
    output_mode = output_mode;
    command = _;
    arguments = _
  }
  | Output {
    output = _;
    output_mode = output_mode
  } ->
    if G.in_degree graph.graph output_vertex > 1 then begin
      let pipe_vertex = add_pipe_vertex graph default_input_mode output_mode in
      G.iter_pred (fun input_vertex ->
        G.remove_edge graph.graph input_vertex output_vertex;
        G.add_edge graph.graph input_vertex pipe_vertex
      ) graph.graph output_vertex;
      G.add_edge graph.graph pipe_vertex output_vertex 
    end else ()
  | _ -> ()


let run (graph : graph) =
  graph.graph |> G.iter_vertex (fun vertex ->
    wrap_input_if_needed graph vertex;
    wrap_output_if_needed graph vertex
  ); ()
  (* TODO *)
