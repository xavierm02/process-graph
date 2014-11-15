open Batteries

type input_mode =
  | Sequential
  | Parallel

let default_input_mode = Parallel

type output_mode =
  | Share
  | Duplicate

let default_output_mode = Share

type process_vertex_label= {
  input_mode : input_mode;
  output_mode : output_mode;
  command : string;
  arguments : string list
}

type repeater_vertex_label = {
  input_mode : input_mode;
  output_mode : output_mode
}

type input_vertex_label = {
  input : Unix.file_descr;
  input_mode : input_mode
}

type output_vertex_label = {
  output : Unix.file_descr;
  output_mode : output_mode
}

type vertex_label =
  | Process of process_vertex_label
  | Repeater of repeater_vertex_label
  | Input of input_vertex_label
  | Output of output_vertex_label

type edge_label = {
  mutable input : Unix.file_descr option;
  mutable output : Unix.file_descr option
}

module VL = struct
  type t = vertex_label
end

module EL = struct
  type t = edge_label
  let compare _ _ = 0
  let create () = {
    input = None;
    output = None
  }
  let default = create ()
end

module G = Graph.Imperative.Digraph.AbstractLabeled (VL) (EL)

module V = G.V

type vertex = V.t

module E = G.E

type edge = E.t

type graph = G.t

type +'a node = {
  graph : graph;
  vertex : vertex
} constraint 'a = [< `Process | `Repeater | `Input | `Output]

let create () = G.create ()

let add_vertex graph vertex_data =
  let vertex = V.create vertex_data in
  G.add_vertex graph vertex;
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

let add_repeater_thing add_thing graph input_mode output_mode =
  let vertex = Repeater {
    input_mode = input_mode;
    output_mode = output_mode
  } in
  add_thing graph vertex
let add_repeater_vertex = add_repeater_thing add_vertex
let add_repeater_node = add_repeater_thing add_node

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

let rec add_edge graph src dst =
  let edge_label = EL.create () in
  let edge = E.create src edge_label dst in
  G.add_edge_e graph edge

let (>>) src dst =
  if src.graph != dst.graph then
    failwith ">> can only be used on two nodes of the same graph!"
  else ();
  add_edge src.graph src.vertex dst.vertex

let redirect_pred_edges graph dst new_dst =
  G.iter_pred_e (fun edge ->
    assert (edge |> E.dst = dst);
    G.remove_edge_e graph edge;
    let new_edge = E.create (edge |> E.src) (edge |> E.label) new_dst in
    G.add_edge_e graph new_edge
  ) graph dst

let redirect_succ_edges graph src new_src =
  G.iter_succ_e (fun edge ->
    assert (edge |> E.src = src);
    G.remove_edge_e graph edge;
    let new_edge = E.create new_src (edge |> E.label) (edge |> E.dst) in
    G.add_edge_e graph new_edge
  ) graph src

let wrap_input_if_needed graph src =
  match src |> V.label with
  | Process {
    input_mode = input_mode;
    output_mode = _;
    command = _;
    arguments = _
  }
  | Input {
    input = _;
    input_mode = input_mode
  } -> begin
    if G.out_degree graph src > 1 then begin
      let pipe = add_repeater_vertex graph input_mode default_output_mode in
      redirect_succ_edges graph src pipe;
      add_edge graph src pipe
    end
  end
  | _ -> ()

let wrap_output_if_needed graph dst =
  match dst |> V.label with
  | Process {
    input_mode = _;
    output_mode = output_mode;
    command = _;
    arguments = _
  }
  | Output {
    output = _;
    output_mode = output_mode
  } -> begin
    if G.in_degree graph dst > 1 then begin
      let pipe = add_repeater_vertex graph default_input_mode output_mode in
      redirect_pred_edges graph dst pipe;
      add_edge graph pipe dst
    end
  end
  | _ -> ()

let add_repeater_if_needed graph edge =
  let src = edge |> E.src in
  let dst = edge |> E.dst in
  match src |> V.label, dst |> V.label with
  | Input _, Output _ -> begin
    let pipe = add_repeater_vertex graph default_input_mode default_output_mode in
    G.remove_edge_e graph edge;
    add_edge graph src pipe;
    add_edge graph pipe dst
  end
  | _ -> ()

let get_inputs graph vertex =
  G.fold_pred_e (fun edge input_list ->
    match (edge |> E.label).output with
    | Some output -> output :: input_list
    | None -> failwith "An edge ending at this node has no output!"
  ) graph vertex []

let get_outputs graph vertex =
  G.fold_succ_e (fun edge output_list ->
    match (edge |> E.label).input with
    | Some input -> input :: output_list
    | None -> failwith "An edge starting at this node has no input!"
  ) graph vertex []

let run (graph : graph) =
  graph |> G.iter_vertex (fun vertex ->
    wrap_input_if_needed graph vertex;
    wrap_output_if_needed graph vertex
  );
  graph |> G.iter_edges_e (add_repeater_if_needed graph);
  graph |> G.iter_edges_e (fun edge ->
    let label = edge |> E.label in
    label.input <- None;
    label.output <- None
  );
  graph |> G.iter_edges_e (fun edge ->
    let label = edge |> E.label in
    let src = edge |> E.src in
    let dst = edge |> E.dst in
    match src |> V.label, dst |> V.label with
    | Input _, Output _ -> failwith "Not possible!"
    | Input input, _ -> label.output <- Some input.input
    | _, Output output -> label.input <- Some output.output
    | _, _ -> begin
      let input, output = Unix.pipe () in
      label.input <- Some input;
      label.output <- Some output
    end
  );
  let pids = ref [] in
  graph |> G.iter_vertex (fun vertex ->
    match vertex |> V.label with
    | Process process -> begin
      match get_inputs graph vertex, get_outputs graph vertex with
      | [input], [output] -> begin
        let pid = Unix.create_process process.command (process.command :: process.arguments |> Array.of_list) input output Unix.stderr in
        pids := pid :: !pids
      end
      | _, _ -> failwith "Each process needs exactly one input and one output!"
    end
    | _ -> ()
  )
  (* TODO inputs et ouputs dans noeuds? ou copie active *)

