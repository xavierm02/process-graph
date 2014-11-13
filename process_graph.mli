type input_mode =
  | Sequential
  | Parallel

val default_input_mode : input_mode

type output_mode =
  | Share
  | Duplicate

val default_output_mode : output_mode

type graph

type +'a node
constraint 'a = [< `Process | `Pipe | `Input | `Output]

val create : unit -> graph

val add_process_node : graph -> ?input_mode:input_mode -> ?output_mode:output_mode -> string -> string list -> [`Process] node

val add_pipe_node : graph -> input_mode -> output_mode -> [`Pipe] node

val add_input_node : graph -> ?input_mode:input_mode -> Unix.file_descr -> [`Input] node

val add_output_node : graph -> ?output_mode:output_mode -> Unix.file_descr -> [`Output] node

val (>>) : [`Process | `Pipe | `Input] node -> [`Process | `Pipe | `Output] node -> unit
