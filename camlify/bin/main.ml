
open Camlify.Music_data
open Camlify.Queue
open Camlify.Streamer 
open Camlify.Command

(* TODO: update with interface to client using terminal,
 * see a2 bin/main.ml for direction *)

let help_message : string = 
  "List of commands :\n \
  help : print this message\n \
  quit : turn off this program\n \
  p [filename.mp3] : plays mp3 file with given filename\n \
  pi [index] : plays mp3 file with given index in current playlist\n \
  pl : displays list of songs in current playlist\n \
  pls : displays list of all playlists\n \
  change_pl [playlist name] : change current playlist into given playlist\n \
  new_pl [playlist name] : create new playlist with given name\n \ 
  name : displays name of current song\n \
  index : displays index of current song in current playlist\n \
  next : plays next song in current playlist\n \
  prev : plays previous song in current playlist\n \
  add [filename.mp3] : add a song named filename.mp3 in current playlist\n \
  rm [filename.mp3 ]: remove song filename.mp3 in current playlist\n"

 let step (q: Camlify.Queue.t) =
  let rec step_r (q: Camlify.Queue.t) : Camlify.Queue.t = 
    print_string "> ";
    
    let cmd : Camlify.Command.command = match read_line () with
    | exception End_of_file -> Quit
    | command -> try (parse command) with
      | Empty -> let _ = print_endline "Please write anything..." in Idle
      | Malformed -> let _ = print_endline "Wrong command input" in Idle in

    match cmd with
    | Idle -> (step_r q)

    | Help -> print_endline help_message; (step_r q)

    | Quit -> print_endline "Bye!"; Stdlib.exit 0;

    | Play song_name -> 

      let res = Camlify.Queue.play_song_by_name song_name q in
      begin
      match res with
      | Illegal -> 
        print_endline ("There is no such song as " ^ song_name); 
        (step_r q)

      | Legal new_q -> 
        print_endline ("Playing " ^ song_name ^ "...");
        let file_name = Camlify.Queue.song_name_to_mp3 song_name in
        let _ = Camlify.Streamer.play file_name in 
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
        let file_name = Camlify.Queue.song_name_to_mp3 new_song_name in
        let _ = Camlify.Streamer.play file_name in  
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
    | CurrentPlayList -> let _ = print_endline (String.concat "\n" (Camlify.Queue.current_playlist q)) in 
      (step_r q)
    | ViewPlaylists -> let _ = print_endline (String.concat "\n" Camlify.Music_data.list_of_playlist) in 
      (step_r q)
    | ChangePlayList pl_name ->
      let res = (Camlify.Queue.select_playlist pl_name q) in
      begin
      match res with
      | Illegal ->
        print_endline ("There is no playlist named " ^ pl_name);
        (step_r q)
      | Legal new_q ->
        print_endline ("Opening playlist " ^ pl_name ^ "…");
        (step_r new_q)
      end
    | CreatePlayList pl_name ->
      let res = (Camlify.Queue.make_new_playlist pl_name q) in
      begin
      match res with
      | Illegal ->
        print_endline ("Failed to create playlist " ^ pl_name);
        (step_r q)
      | Legal new_q ->
        print_endline ("Create new playlist " ^ pl_name ^ "…");
        (step_r new_q)
      end
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
        let file_name = Camlify.Queue.song_name_to_mp3 new_song_name in
        let _ = Camlify.Streamer.play file_name in  
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
        let file_name = Camlify.Queue.song_name_to_mp3 new_song_name in
        let _ = Camlify.Streamer.play file_name in  
        (step_r new_q)
      end
      | AddSong song_name ->
        let res = Camlify.Queue.add_song_to_playlist song_name q in
        begin
        match res with
        | Illegal ->
        print_endline ("There is no song named " ^ song_name);
         (step_r q)
        | Legal new_q ->
         print_endline (song_name ^ " added to current playlist.");
         (step_r new_q)
        end
      | RemoveSong song_name ->
        let res = Camlify.Queue.remove_song_from_playlist song_name q in
        begin
        match res with
        | Illegal ->
        print_endline ("There is no song named " ^ song_name);
         (step_r q)
        | Legal new_q ->
         print_endline (song_name ^ " removed from current playlist.");
         (step_r new_q)
        end
      | _ -> failwith "TODO Add song, remove song"
  in
  step_r q

let rec choose_playlist () : string = 
  print_endline "Choose Playlist:";
  print_endline ((String.concat "\n" Camlify.Music_data.list_of_playlist) ^ "\n");
  print_string "> ";
  let playlist = match read_line () with
    | exception End_of_file -> let _ = print_endline "Empty input :(" in choose_playlist ()
    | some_str -> 
      if List.mem some_str Camlify.Music_data.list_of_playlist 
      then some_str
      else let _ = print_endline "There is no such playlist :(" in choose_playlist ()
  in playlist

let main () =
  (* ANSITerminal.print_string [ ANSITerminal.red ] *)
  print_endline  "\n\nWelcome to Camlify \n";
  let playlist = choose_playlist() in
  let _ = print_endline ("Opening playlist " ^ playlist ^ "...") in
  let q = Camlify.Queue.init_state playlist in

  step q

(* Execute the mp3. *)
let _ = main ()