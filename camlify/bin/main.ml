open Music_data
open Queue
open Streamer

(* TODO: update with interface to client using terminal,
 * see a2 bin/main.ml for direction *)

 let step (q: Queue.t) =
  let rec step_r (q: Queue.t) : Queue.t = 
    print_string "> ";
    cmd : Command.command = match read_line () with
    | exception End_of_file -> Quit
    | command -> try (parse command) with
      | Empty -> let _ = print_endline "Please write anything..." in Idle
      | Malformed -> let _ = print_endline "Wrong command input" in Idle
    in
    match cmd with
    | Idle -> (step_r q)
    | Quit -> let _ = print_endline "Bye!" in let _ = Stdlib.exit 0 in (step_r q)
    | Play song_name -> 
      let res = Queue.play_song_by_name song_name q in
      match res with
      | Illegal -> 
        let _ = print_endline "There is no such song as " ^ song_name in 
        (step_r q)
      | Legal new_q -> 
        let _ = print_endline "Playing " ^ song_name ^ "..." in
        let play_new_song = Streamer.play song_name in 
        (step_r new_q)
    | PlayIndex idx ->
      let res = Queue.play_song_by_idx idx q in
      match res with
      | Illegal ->
        let _ = print_endline "There is no such index as " ^ (string_of_int idx) in
        (step_r q)
      | Legal new_q ->
        let new_song_name : string = current_song_name new_q in
        let _ = print_endline "Playing song " ^ new_song_name ^ "..." in
        let play_new_song = Streamer.play new_song_name in 
        (step_r new_q)
    | CurrentSongName ->
      let song_name = current_song_name q in
      let _ = print_endline "Current song: " ^ song_name in
      (step_r q)
    | CurrentSongIndex -> 
      let song_index = current_song_idx q in
      let _ = print_endline "Current song index: " ^ (string_of_int song_index) in
      (step_r q)
    | CurrentPlayList -> TODO
    | NextSong ->
      let res = Queue.next_song q in
      | Illegal ->
        let _ = print_endline "There is no next song in the queue." in
        (step_r q)
      | Legal new_q ->
        let new_song_name : string = current_song_name new_q in
        let _ = print_endline "Playing song " ^ new_song_name ^ "…" in
        let play_new_song = Streamer.play new_song_name in 
        (step_r new_q)
    | PreviousSong ->
      let res = Queue.prev_song q in
      | Illegal ->
        let _ = print_endline "There is no previous song in the queue." in
        (step_r q)
      | Legal new_q ->
        let new_song_name : string = current_song_name new_q in
        let _ = print_endline "Playing song " ^ new_song_name ^ "…" in
        let play_new_song = Streamer.play new_song_name in 
        (step_r new_q)


  in
  step_r q

let main () =
  let q = Queue.init_state (Music_data.playlist ()) in (** hope Music_data.init_state returns Music_data.t  *)
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the MP3 \n";
  print_endline
    "Commands you can use:";
  print_endline
    "play [name_of_song.mp3]"
  step q;

(* Execute the mp3. *)
let () = main ()