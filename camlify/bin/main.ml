<<<<<<< HEAD
<<<<<<< Updated upstream
open Yojson
=======
(* open Camlify.Music_data
open Camlify.Queue
open Camlify.Streamer *)
(* open Camlify.Command *)
>>>>>>> Stashed changes
=======
(* open Camlify.Music_data
open Camlify.Queue
open Camlify.Streamer *)
open Camlify.Command
(* TODO: update with interface to client using terminal,
 * see a2 bin/main.ml for direction *)


 let step (q: Camlify.Queue.t) =


 let step (q: Camlify.Queue.t) =

  let rec step_r (q: Camlify.Queue.t) : Camlify.Queue.t = 
    print_string "> ";
    
    let cmd : Camlify.Command.command = match read_line () with
    | exception End_of_file -> Quit

    | command -> try (Camlify.Command.parse command) with
      | Camlify.Command.Empty -> let _ = print_endline "Please write anything..." in Idle
      | Camlify.Command.Malformed -> let _ = print_endline "Wrong command input" in Idle in

    | command -> try (parse command) with
      | Empty -> let _ = print_endline "Please write anything..." in Idle
      | Malformed -> let _ = print_endline "Wrong command input" in Idle in


    match cmd with
    | Idle -> (step_r q)

    | Quit -> let _ = print_endline "Bye!" in let _ = Stdlib.exit 0 in (step_r q)

    | Play song_name -> 

      let res = Camlify.Queue.play_song_by_name song_name q in
      begin
      match res with
      | Illegal -> 
        print_endline ("There is no such song as " ^ song_name); 
        (step_r q)

      | Legal new_q -> 
        print_endline ("Playing " ^ song_name ^ "...");
        let _ = Camlify.Streamer.play song_name in 
        (step_r new_q)
      end
    | PlayIndex idx ->
      let res = Camlify.Queue.play_song_by_idx idx q in
      begin
      match res with
      | Illegal ->
        print_endline ("There is no such index as " ^ (string_of_int idx));
        (step_r q)
      | Legal new_q ->
        let new_song_name : string = Camlify.Queue.current_song_name new_q in
      print_endline ("Playing song " ^ new_song_name ^ "...");
        let _ = Camlify.Streamer.play new_song_name in 
        (step_r new_q)
      end

    | CurrentSongName ->
      let song_name = Camlify.Queue.current_song_name q in
      print_endline ("Current song: " ^ song_name);
      (step_r q)

    | CurrentSongIndex -> 
      let song_index = Camlify.Queue.current_song_idx q in
    print_endline ("Current song index: " ^ (string_of_int song_index));
      (step_r q)
    | CurrentPlayList -> failwith "TODO: implement"
    | NextSong ->
      let res = Camlify.Queue.next_song q in
      begin
      match res with
      | Illegal ->
      print_endline ("There is no next song in the queue.");
        (step_r q)
      | Legal new_q ->
        let new_song_name : string = Camlify.Queue.current_song_name new_q in
      print_endline ("Playing song " ^ new_song_name ^ "…");
        let _ = Camlify.Streamer.play new_song_name in 
        (step_r new_q)
      end
    | PreviousSong ->
      let res = Camlify.Queue.prev_song q in
      begin
      match res with
      | Illegal ->
      print_endline ("There is no previous song in the queue.");
        (step_r q)
      | Legal new_q ->
        let new_song_name : string = Camlify.Queue.current_song_name new_q in
        print_endline ("Playing song " ^ new_song_name ^ "…");
        let _ = Camlify.Streamer.play new_song_name in 
        (step_r new_q)
      end
      | _ -> failwith "TODO Add song, remove song"


  in
  step_r q

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

  print_endline "here at least";
  let q = Camlify.Queue.init_state ("Playlist one") in (** hope Camlify.Music_data.init_state returns Camlify.Music_data.t  *)
  (* ANSITerminal.print_string [ ANSITerminal.red ] *)
  print_endline  "\n\nWelcome to Camlify \n";
  print_endline "Commands you can use:";
  print_endline "play [name_of_song.mp3]";
  step q

(* Execute the mp3. *)
let _ = 
  print_endline "here at least2";

  main ()

  let q = Camlify.Queue.init_state ("Playlist one") in (** hope Camlify.Music_data.init_state returns Camlify.Music_data.t  *)
  (* ANSITerminal.print_string [ ANSITerminal.red ] *)
  print_endline  "\n\nWelcome to Camlify \n";
  print_endline "Commands you can use:";
  print_endline "play [name_of_song.mp3]";
  step q

(* Execute the mp3. *)
let _ = main ()

