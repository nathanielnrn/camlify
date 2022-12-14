(* TODO: Implement according to streamer.mli *)
open Gstreamer
include Option

type file = int
type streamer = int
type tag = string list

(* let pipeline_instance = ref None *)
(* let current_song = ref "" *)

(*Relative location of data directory which contains mp3 files.contents
  in Unix this is "../data/"*)

let sep = Filename.dir_sep
let exe_name = Sys.executable_name

(*[reduce_filepath s n] Takes in a string representing a file path and
  makes it n seps shorter*)
let rec reduce_filepath s n =
  match n with
  | 0 -> s
  | c ->
      reduce_filepath (String.sub s 0 (String.rindex s sep.[0])) (n - 1)

(*Returns path of data dir ending with / i.e [".../data/"]*)
let data_dir_uri =
  if Filename.is_relative exe_name then
    failwith "exe_name shouldn't have relative path"
    (*Creates a path of form [<some_path>/data/]*)
  else reduce_filepath exe_name 2 ^ sep ^ "data" ^ sep

let init_pipeline =
  init ();
  ref (Some (Pipeline.create "audio_pipeline"))

(*Helper function that creates a new streamer of ifle name, only should
  be called if pipeline is not paused*)
let play_new pipeline file_name =
  let file_path =
    data_dir_uri ^ file_name
    |> String.split_on_char ' '
    |> String.concat "%20"
  in

  (*GStreamer initialization and running code See Gstreamer tutorials
    for explanations. Important one is pipeline*)

  (*Replace uri=file:../data/samples-15s.mp3*)
  (*linux: file:///home/nate/cs3110/camlify/camlify/data/sample-15s.mp3*)
  (*windows:
    file:///home/navarro/cs3110/camlify/camlify/data/sample-15s.mp3*)
  pipeline :=
    Some (Pipeline.parse_launch ("playbin uri=file://" ^ file_path));

  (* current_song := file_name; *)
  ignore (Element.set_state (get !pipeline) State_playing);

  let bus = Bus.of_element (get !pipeline) in

  match
    Bus.timed_pop_filtered bus [ Bus.(`End_of_stream); Bus.(`Error) ]
  with
  | { payload = Bus.(`Error s); _ } -> raise (Error s)
  | _ -> ()

let play pipeline file_name =
  match !pipeline with
  | None -> play_new pipeline file_name
  | Some pl -> begin
      match Element.get_state pl with
      | _, Element.State_paused, _ ->
          ignore (Element.set_state pl Element.State_playing)
      | _ -> play_new pipeline file_name
    end

(**Todo: should throw an exception if pipeline not instantiated*)
let pause pipeline =
  match !pipeline with
  | None -> failwith "Should not be called before play"
  | Some pl -> ignore (Element.set_state pl Element.State_paused)

let stop pipeline =
  match !pipeline with
  | Some pl -> ignore (Element.set_state pl Element.State_null)
  | _ -> pipeline := None

let get_pipeline = ref None
