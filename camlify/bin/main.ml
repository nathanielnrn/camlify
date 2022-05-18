open Camlify.Music_data
open Camlify.Queue
open Camlify.Streamer
open Camlify.Command

(* TODO: update with interface to client using terminal,
 * see a2 bin/main.ml for direction *)

let pipeline = Camlify.Streamer.get_pipeline

let help_message : string =
  "List of commands (note that the commands only run after the song \
   ends):\n\
  \ help : print this message\n\
  \ quit : turn off this program\n\
  \ p [song_name] : plays mp3 file with given filename\n\
  \ pause : pause currently played mp3 file\n\
  \ pi [index] : plays mp3 file with given index in current playlist\n\
  \ pl : displays list of songs in current playlist\n\
  \ pls : displays list of all playlists\n\
  \ change_pl [playlist name] : change current playlist into given \
   playlist\n\
  \ change_l [song_name]: change like state of the song in the json file\n\
  \ change_ar [song_name] : change the artist of the song in the json \
   file.\n\
  \ An additional prompt is given to get the artist of song\n\
  \ change_al [song_name] : change the album of the song in the json \
   file.\n\
  \ An additional prompt is given to get the album of song\n\
  \ change_y [song_name] [year] : change the year of the song in the \
   json file.\n\
  \ add_tag [song_name] : add a tag to the song in the json file.\n\
  \ An additional prompt is given to get the new tag\n\
  \ rm_tag [song_name] : remove a tag to the song in the json file.\n\
  \ An additional prompt is given to get the tag\n\
  \ name : displays name of current song\n\
  \ index : displays index of current song in current playlist\n\
  \ next : plays next song in current playlist\n\
  \ prev : plays previous song in current playlist\n\
  \ shuffle : plays random song in current playlist\n\
  \ add [song_name] : adds [song_name] to currently located playlist\n\
  \ rm[song_name] : removes [song_name] to currently located playlist\n\
  \    play_artist : displays list of artist names and plays selected \
   artist's songs \n\
  \ play_album : displays list of album names and plays selected \
   album's songs \n\
  \ play_year : displays list of years and plays selected year's songs \n\
  \ play_liked : plays all liked songs\n\
  \  play_tag : displays list of tag names and plays selected tag's \
   songs \n\
  \ \n\
  \  "

let remove_dup lst = List.sort_uniq compare lst
let remove_empty lst = List.filter (fun x -> String.length x > 0) lst
let remove_zero lst = List.filter (fun x -> int_of_string x > 0) lst

let rec step_r (q : Camlify.Queue.t) : Camlify.Queue.t =
  let get_cmd : Camlify.Command.command =
    match read_line () with
    | exception End_of_file -> Quit
    | command -> begin
        try parse' command with
        | Empty ->
            let _ = print_endline "Please write anything..." in
            Idle
        | Malformed ->
            let _ = print_endline "Wrong command input" in
            Idle
      end
  in

  print_string "> ";

  match get_cmd with
  | Idle -> step_r q
  | Help ->
      print_endline help_message;
      step_r q
  | Quit ->
      print_endline "Bye!";
      Stdlib.exit 0
  | Play song_name -> h_play song_name q
  | Pause ->
      let song_name = current_song_name q in
      print_endline ("Pausing " ^ song_name ^ "...");
      Camlify.Streamer.pause pipeline;
      step_r q
  | Stop ->
      let song_name = current_song_name q in
      print_endline ("Stopping " ^ song_name ^ "...");
      Camlify.Streamer.stop pipeline;
      step_r q
  (* |TODO: Update command to match: Continue -> let song_name =
     Camlify.Queue.current_song_name q in print_endline ("Continuing " ^
     song_name ^ "..."); Camlify.Streamer.continue pipeline; (step_r
     q) *)
  | PlayIndex idx -> h_play_index idx q
  | CurrentSongName ->
      let song_name = current_song_name q in
      print_endline ("Current song: " ^ song_name);
      step_r q
  | CurrentSongIndex ->
      let song_index = current_song_idx q in
      print_endline ("Current song index: " ^ string_of_int song_index);
      step_r q
  | CurrentPlayList ->
      print_endline (String.concat "\n" (current_playlist q));
      step_r q
  | ViewPlaylists ->
      print_endline (String.concat "\n" list_of_playlist);
      step_r q
  | ChangePlayList pl_name -> h_change_playList pl_name q
  | ChangeSongLike song_name ->
      change_song_liked song_name (not (read_song_liked song_name));
      (match read_song_liked song_name with
      | true -> print_endline (song_name ^ " is liked")
      | false -> print_endline (song_name ^ " is unliked"));
      step_r q
  | ChangeSongArtist song_name ->
      print_endline "What is the name of the artist?";
      print_string "> ";
      let artist = read_line () in
      change_song_artist song_name artist;
      step_r q
  | ChangeSongAlbum song_name ->
      print_endline "What is the name of the album?";
      print_string "> ";
      let album = read_line () in
      change_song_album song_name album;
      step_r q
  | ChangeSongYear (song_name, year) ->
      print_endline
        ("The year of " ^ song_name ^ " has been changed to "
       ^ string_of_int year);
      change_song_year song_name year;
      step_r q
  | AddSongTag song_name ->
      print_endline "What is a new tag?";
      print_string "> ";
      let tag = read_line () in
      add_song_tag song_name tag;
      step_r q
  | RemoveSongTag song_name ->
      print_endline "What is the tag?";
      print_string "> ";
      let tag = read_line () in
      remove_song_tag song_name tag;
      step_r q
  | CreatePlayList pl_name -> h_create_playList pl_name q
  | NextSong ->
      let res = next_song q in
      h_next_song res q
  | PreviousSong ->
      let res = prev_song q in
      h_previous_song res q
  | Shuffle ->
      let res = random_song q in
      h_shuffle res q
  | AddSong song_name ->
      let res = add_song_to_playlist song_name q in
      h_add_song song_name res q
  | RemoveSong (song_name : string) ->
      let res = remove_song_from_playlist song_name q in
      h_remove_song song_name res q
  | PlayArtist ->
      print_endline "Names of all artists in this player :";
      all_songs
      |> List.map read_song_artist
      |> remove_dup |> remove_empty |> String.concat ", "
      |> print_endline;
      print_string "Select artist \n >";
      let artist = read_line () in
      let res = select_playlist_by_artist artist q in
      h_play_artist artist res q
  | PlayAlbum ->
      print_endline "Names of all albums in this player :";
      all_songs
      |> List.map read_song_album
      |> remove_dup |> remove_empty |> String.concat ", "
      |> print_endline;
      print_string "Select album \n >";
      let album = read_line () in
      let res = select_playlist_by_album album q in
      h_play_album album res q
  | PlayYear ->
      print_endline "List of years of songs in this player :";
      all_songs |> List.map read_song_year |> remove_dup
      |> List.map string_of_int |> remove_zero |> String.concat ", "
      |> print_endline;
      print_string "Select year \n >";
      let year = read_line () in
      if year = "" then (
        print_endline "You didn't type anything";
        step_r q)
      else
        let res =
          try select_playlist_by_year (int_of_string year) q with
          | _ -> Illegal
        in
        h_play_year year res q
  | PlayLiked ->
      let res = select_playlist_by_liked q in
      h_play_liked res q
  | PlayTag ->
      print_endline "Names of all tags in this player :";
      all_songs |> List.map read_tags |> List.flatten |> remove_dup
      |> remove_empty |> String.concat ", " |> print_endline;
      print_string "Select tag \n > ";
      let tag = read_line () in
      let res = select_playlist_by_tag tag q in
      h_play_tag tag res q
  | _ -> failwith "TODO?"

and h_play song_name q =
  let res = play_song_by_name song_name q in
  match res with
  | Illegal ->
      print_endline ("There is no such song as " ^ song_name);
      step_r q
  | Legal new_q ->
      print_endline ("Playing " ^ song_name ^ "...");
      (* TODO: should this logic be here? Might be better in either in
         queue or streamer? Verdict: should probably add a string ref in
         streamer*)
      if song_name = current_song_name q then ()
      else Camlify.Streamer.stop pipeline;
      let file_name = read_song_mp3_file song_name in
      ignore (Thread.create (Camlify.Streamer.play pipeline) file_name);
      step_r new_q

and h_play_index idx q =
  let res = play_song_by_idx idx q in
  match res with
  | Illegal ->
      print_endline ("There is no such index as " ^ string_of_int idx);
      step_r q
  | Legal new_q ->
      let new_song_name : string = current_song_name new_q in
      print_endline ("Playing song " ^ new_song_name ^ "...");
      let file_name = read_song_mp3_file new_song_name in
      Camlify.Streamer.play pipeline file_name;
      step_r new_q

and h_change_playList pl_name q =
  let res = select_playlist pl_name q in
  match res with
  | Illegal ->
      print_endline ("There is no playlist named " ^ pl_name);
      step_r q
  | Legal new_q ->
      print_endline ("Opening playlist " ^ pl_name ^ "…");
      step_r new_q

and h_create_playList pl_name q =
  let res = make_new_playlist pl_name q in
  match res with
  | Illegal ->
      print_endline ("Failed to create playlist " ^ pl_name);
      step_r q
  | Legal new_q ->
      print_endline ("Create new playlist " ^ pl_name ^ "…");
      step_r new_q

and h_next_song res q =
  match res with
  | Illegal ->
      print_endline "There is no next song in the queue.";
      step_r q
  | Legal new_q ->
      let new_song_name : string = current_song_name new_q in
      print_endline ("Playing song " ^ new_song_name ^ "…");
      let file_name = read_song_mp3_file new_song_name in
      Camlify.Streamer.stop pipeline;
      ignore (Thread.create (Camlify.Streamer.play pipeline) file_name);
      step_r new_q

and h_previous_song res q =
  match res with
  | Illegal ->
      print_endline "There is no previous song in the queue.";
      step_r q
  | Legal new_q ->
      let new_song_name : string = current_song_name new_q in
      print_endline ("Playing song " ^ new_song_name ^ "…");
      let file_name = read_song_mp3_file new_song_name in
      Camlify.Streamer.stop pipeline;
      ignore (Thread.create (Camlify.Streamer.play pipeline) file_name);
      step_r new_q

and h_shuffle res q =
  match res with
  | Illegal ->
      print_endline "There is no song in the queue.";
      step_r q
  | Legal new_q ->
      let new_song_name : string = current_song_name new_q in
      print_endline ("Playing song " ^ new_song_name ^ "…");
      let file_name = read_song_mp3_file new_song_name in
      Camlify.Streamer.stop pipeline;
      ignore (Thread.create (Camlify.Streamer.play pipeline) file_name);
      step_r new_q

and h_add_song song_name res q =
  match res with
  | Illegal ->
      print_endline ("There is no song named " ^ song_name);
      step_r q
  | Legal new_q ->
      Camlify.Music_data.add_song_to_playlist
        (current_playlist_name new_q)
        song_name;
      print_endline (song_name ^ " added to current playlist.");
      step_r new_q

and h_remove_song song_name res q =
  match res with
  | Illegal ->
      print_endline ("There is no song named " ^ song_name);
      step_r q
  | Legal new_q ->
      delete_song_from_playlist
        (current_playlist_name new_q : string)
        (song_name : string);
      print_endline (song_name ^ " removed from current playlist.");
      step_r new_q

and h_play_artist artist res q =
  match res with
  | Illegal ->
      print_endline ("Artist named " ^ artist ^ " doesn't exist");
      step_r q
  | Legal new_q ->
      print_endline ("Playing songs by " ^ artist ^ "...");
      step_r new_q

and h_play_album album res q =
  match res with
  | Illegal ->
      print_endline ("Album named " ^ album ^ " doesn't exist");
      step_r q
  | Legal new_q ->
      print_endline ("Playing songs in " ^ album ^ "...");
      step_r new_q

and h_play_year year res q =
  match res with
  | Illegal ->
      print_endline ("Songs in year " ^ year ^ " doesn't exist");
      step_r q
  | Legal new_q ->
      print_endline ("Playing songs from " ^ year ^ "...");
      step_r new_q

and h_play_liked res q =
  match res with
  | Illegal ->
      print_endline "No songs are liked";
      step_r q
  | Legal new_q ->
      print_endline "Playing liked songs...";
      step_r new_q

and h_play_tag tag res q =
  match res with
  | Illegal ->
      print_endline ("Tag named " ^ tag ^ " doesn't exist");
      step_r q
  | Legal new_q ->
      print_endline ("Playing songs with tag " ^ tag ^ "...");
      step_r new_q

(*add [filename.mp3] : add a song named filename.mp3 in current playlist
  rm [filename.mp3 ]: remove song filename.mp3 in current playlist*)
(* new_pl [playlist name] : create new playlist with given name *)
let step (q : Camlify.Queue.t) = step_r q

let rec choose_playlist () : string =
  print_endline "Choose Playlist:";
  print_endline (String.concat "\n" list_of_playlist ^ "\n");
  print_string "> ";
  let playlist =
    match read_line () with
    | exception End_of_file ->
        let _ = print_endline "Empty input :(" in
        choose_playlist ()
    | some_str ->
        if List.mem some_str list_of_playlist then some_str
        else
          let _ = print_endline "There is no such playlist :(" in
          choose_playlist ()
  in
  playlist

(* let main () = (* ANSITerminal.print_string [ ANSITerminal.red ] *)
   print_endline "\n\nWelcome to Camlify \n"; let playlist =
   choose_playlist () in print_endline ("Opening playlist " ^ playlist ^
   "..."); let q = Camlify.Queue.init_state playlist in step q *)

let main () =
  (* ANSITerminal.print_string [ ANSITerminal.red ] *)
  print_endline "\n\nWelcome to Camlify \n";
  let playlist = choose_playlist () in
  print_endline ("Opening playlist " ^ playlist ^ "...");
  let q = init_state playlist in
  let res = play_song_by_idx 0 q in
  match res with
  | Illegal ->
      print_endline ("There is no such index as " ^ string_of_int 0);
      step q
  | Legal new_q ->
      let new_song_name : string = current_song_name new_q in
      print_endline ("Playing song " ^ new_song_name ^ "...");
      let file_name = read_song_mp3_file new_song_name in
      ignore (Thread.create (Camlify.Streamer.play pipeline) file_name);
      step new_q

(* Execute the mp3. *)
let _ =
  load_data ();
  main ()
