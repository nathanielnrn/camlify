open Camlify.Music_data

let file = "interface.json"
let message = "Hello!"

let file = "data/interface.json"
let message = "{\"playlists\":[{\"name\":\"frankie\",\"songs\":[{\"name\":\"fly me to the moon\",\"tagged\": true,\"mp3 file\": \"this\"}]}]}" 

let j = Yojson.Basic.from_file "data/interface.json"



(*COPPIED CODE CHECK IF THAT IS OK*)
let () =
  (* Write message to file *)
  let oc = open_out file in (* create or truncate file, return channel *)
    Printf.fprintf oc "%s\n" message; (* write something *)   
    close_out oc;                     (* flush and close the channel *)
  
  (* Read file and display the first line *)
  let ic = open_in file in
    try 
      let line = input_line ic in (* read line, discard \n *)
        print_endline line;       (* write the result to stdout *)
        flush stdout;             (* write on the underlying device now *)
        close_in ic               (* close the input channel *) 
    with e ->                     (* some unexpected exception occurs *)
      close_in_noerr ic;          (* emergency closing *)
      raise e                     (* exit with error: files are closed but
                                     channels are not flushed *)

let x = Yojson.Basic.pretty_to_string j

let () =  print_endline x;

