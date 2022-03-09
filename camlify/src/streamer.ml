(* TODO: Implement according to streamer.mli *)
open Gstreamer

type file = int

type streamer = int

let play () =
  init();

  let pipeline = Pipeline.parse_launch "playbin uri=file://home/navarro/cs3110/camlify/camlify/data/sample-15s.mp3" in
  
  let _ = Element.set_state pipeline State_playing in

  let bus = Bus.of_element pipeline in

  match Bus.timed_pop_filtered bus [Bus.(`End_of_stream); Bus.(`Error)] with
  |{payload=Bus.(`Error s); _} -> raise (Error s)
  |_ -> ()



  let parse _ = failwith "Not yet implemented"






  
