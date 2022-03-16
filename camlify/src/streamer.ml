(* TODO: Implement according to streamer.mli *)
open Gstreamer

type file = int

type streamer = int

type tag = string list

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

  (*Not good*)
(* let data_dir_uri = Filename.parent_dir_name ^ Filename.dir_sep ^ "data" ^ Filename.dir_sep *)
let play file_name =
  (*Create file path code*)

  let file_path = data_dir_uri ^ file_name in


  (*GStreamer initialization and running code
  See Gstreamer tutorials for explanations. Important one is pipeline*)
  init();

  (*Replace uri=file:../data/samples-15s.mp3*)
  (*linux: file:///home/nate/cs3110/camlify/camlify/data/sample-15s.mp3*)
  (*windows: file:///home/navarro/cs3110/camlify/camlify/data/sample-15s.mp3*)
  print_endline (Sys.executable_name);
  print_endline (data_dir_uri);
  print_endline(file_path);


  let pipeline = Pipeline.parse_launch ("playbin uri=file://" ^ file_path) in
  
  let _ = Element.set_state pipeline State_playing in

  let bus = Bus.of_element pipeline in

  match Bus.timed_pop_filtered bus [Bus.(`End_of_stream); Bus.(`Error)] with
  |{payload=Bus.(`Error s); _} -> raise (Error s)
  |_ -> ()



  let parse _ = failwith "Not yet implemented"




