open Music_data
open Queue
open Streamer

(* TODO: update with interface to client using terminal,
 * see a2 bin/main.ml for direction *)

let step (q: Queue.t) =
  let rec step_r (q: Queue.t) : Queue.t = 
    print_string "> ";
    cmd : Command.command = match read_line () with
    | exception End_of_file -> ()
    | command -> parse command in
    match cmd with
    | Quit -> let _ = print_endline "Bye!" in let _ = Stdlib.exit 0 in (step_r queue)
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


  in
  step_r q

let main () =
  let q = Queue.init_state (Music_data.init_state ()) in (** hope Music_data.init_state returns Music_data.t  *)
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the MP3 \n";
  print_endline
    "Commands you can use:";
  print_endline
    "play [name_of_song.mp3]"
  step q;

(* Execute the mp3. *)
let () = main ()