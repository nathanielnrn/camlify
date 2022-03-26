(* TODO: Implement according to streamer.mli *)
open Gstreamer
open Option

type file = int

type streamer = int

type tag = string list

let pipeline_instance = ref None
let data_source = ref None
let data_sink = ref None
let audio_convert = ref None
let audio_resample = ref None


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



  (* Start of stream stuff*)


(*Creates an empty pipeline that can then be added to in [play file_name]*)
let init_pipeline = 
init();

(*pointers to be used later*)
data_source := Some (Element_factory.make "filesrc" "source");
data_sink := Some (Element_factory.make "autoaudiosink" "sink");
audio_convert := Some (Element_factory.make "audioconvert" "convert");
audio_resample := Some (Element_factory.make "audioresample" "resampler");

pipeline_instance  := Some (Pipeline.create "audio_pipeline");


Bin.add_many (get !pipeline_instance) [get !data_source; get !audio_convert; get !audio_resample; get !data_sink];
()




let play file_name =

  
  (*Create file path code*)
  (*Replaces spaces with %20*)
  let file_path = data_dir_uri ^ file_name |> String.split_on_char ' ' |> String.concat "%20" in
  
  
  (*GStreamer initialization and running code
  See Gstreamer tutorials for explanations. Important one is pipeline*)
  init_pipeline;
  
  (*Replace uri=file:../data/samples-15s.mp3*)
  (*linux: file:///home/nate/cs3110/camlify/camlify/data/sample-15s.mp3*)
  (*windows: file:///home/navarro/cs3110/camlify/camlify/data/sample-15s.mp3*)
  
  
  
  (* let pipeline_instance = Pipeline.parse_launch ("playbin uri=file://" ^ file_path) in *)
  
  
  Element.set_state (get !pipeline_instance) State_ready;
  
  Element.set_property_string (get !data_source) "location" (file_path);
  Element.link_many [get !data_source; get !audio_convert; get !audio_resample; get !data_sink];
  
  (* Element.set_state (get !pipeline_instance) State_paused; *)

  let ret = Element.set_state (get !pipeline_instance) State_playing in
  print_endline "here";
  
  (* match (print_endline "here"; Element.get_state (get !pipeline_instance)) with
  |(return, curr, pending) -> print_endline @@ Element.string_of_state curr;
  
  if (ret <> Element.State_change_success) then failwith "Could not set state"
  else
    print_endline "Got here"; *)
  
  
  let bus = Bus.of_element (get !pipeline_instance) in

  match Bus.timed_pop_filtered bus [Bus.(`End_of_stream); Bus.(`Error)] with
  |{payload=Bus.(`Error s); _} -> raise (Error s)
  |_ -> ()





(*TODO: See mli file spec*)
  let parse _ = failwith "Not yet implemented"




