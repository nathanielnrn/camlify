(* TODO: Implement according to streamer.mli *)
open Gstreamer
open Option

type file = int

type streamer = int

type tag = string list

let pipeline_instance = ref None
let current_song = ref ""

(*TODO: implement*)
let tags_of_file f = []

let strings_of_tags l =["To implement"]
(*Relative location of data directory which contains mp3 files.contents
in Unix this is "../data/"*)

let sep = Filename.dir_sep
let exe_name = Sys.executable_name

(*[reduce_filepath s n] Takes in a string representing a file path and makes it n seps shorter*)
let rec reduce_filepath s n = 
  match n with
  |0 -> s
  |c -> reduce_filepath (String.sub s 0 (String.rindex s sep.[0])) (n-1)


  (*Returns path of data dir ending with / i.e [".../data/"]*)
 let data_dir_uri = if (Filename.is_relative exe_name)
  then  failwith "exe_name shouldn't have relative path"
 (*Creates a path of form [<some_path>/data/]*)
  else reduce_filepath exe_name 2 ^ sep ^ "data" ^ sep



  let init_pipeline = 
    init();
    pipeline_instance  := Some (Pipeline.create "audio_pipeline") 


let play file_name =
  if !current_song = file_name then
    ignore (Element.set_state (get !pipeline_instance) State_playing)

  else
    begin
  init_pipeline;

  (*Create file path code*)
  (*Replaces spaces with %20*)
  let file_path = data_dir_uri ^ file_name |> String.split_on_char ' ' |> String.concat "%20" in
  
  
  (*GStreamer initialization and running code
  See Gstreamer tutorials for explanations. Important one is pipeline*)
  init_pipeline;
  
  (*Replace uri=file:../data/samples-15s.mp3*)
  (*linux: file:///home/nate/cs3110/camlify/camlify/data/sample-15s.mp3*)
  (*windows: file:///home/navarro/cs3110/camlify/camlify/data/sample-15s.mp3*)

  pipeline_instance := Some (Pipeline.parse_launch ("playbin uri=file://" ^ file_path));
  current_song := file_name;

  ignore (Element.set_state (get !pipeline_instance) State_playing);


  let bus = Bus.of_element (get !pipeline_instance) in
    
  match Bus.timed_pop_filtered bus [Bus.(`End_of_stream); Bus.(`Error)] with
  |{payload=Bus.(`Error s); _} -> raise (Error s)
  |_ -> ()
end


(**Todo: should throw an exception if pipeline not instantiated*)
  let pause =
    if !pipeline_instance = None then failwith "Should not be called before play"
    else ignore (Element.set_state (get !pipeline_instance) Element.State_paused)
    
    
    (**Todo: should throw an exception if pipeline not instantiated (maybe)?*)
    let stop = 
      if !pipeline_instance = None then print_endline "No current pipeline_instance"
      else ignore (Element.set_state (get !pipeline_instance) Element.State_null);
      pipeline_instance := None

  




(*TODO: See mli file spec*)
  let parse _ = failwith "Not yet implemented"

