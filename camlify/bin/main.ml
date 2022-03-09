open Yojson

(* TODO: update with interface to client using terminal,
 * see a2 bin/main.ml for direction *)


let next_song (data: Music_data.t) (st: queue.t) : queue.t = 
  let _ = print_endline ((st |> current_song ^ "\n") in
  let _ = print_string "> " in
  match read_line () with
  | input -> 
    let command = try (parse input) with
    | Empty -> let _ = print_endline "not a valid song name\n" in (Play [])
    | Malformed -> let _ = print_endline "You should \"play\" a song or \"quit\" the mp3...\n" in (Play [])
    in
    match command with
    | Quit -> let _ = (print_endline ("Camlify!")) in let _ = Stdlib.exit 0 in st
    | Go object_phrase -> let exit = (String.concat " " object_phrase) in
      let result = go exit data st in
      match result with
      | Illegal -> let _ = print_endline "please re-enter song name\n" in st
      | Legal n_state -> n_state

let play_song f = 
  let pl = try (f |> Yojson.Basic.from_file |> from_json) with
  | Sys_error x -> let _ = (print_endline ("There is no such file as " ^ f)) in Stdlib.exit 0 in
  let i_state = init_state pl in
  let rec play_song_r pl state = 
    let n_state = next_state pl state in
    play_song_r pl n_state in
  play_song_r pl i_state


let data_dir_prefix = "data" ^ Filename.dir_sep;;

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the MP3 \n";
  print_endline
    "Please enter the name of the song you want to play.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_song (data_dir_prefix ^ file_name ^ ".json")

(* Execute the mp3. *)
let () = main ()