<<<<<<< HEAD

open Yojson
=======
(* open Camlify.Music_data
open Camlify.Queue
open Camlify.Streamer *)
(* open Camlify.Command *)
>>>>>>> Stashed changes
=======
(* open Camlify.Music_data
=======

=======
>>>>>>> 805a313ec7f132d1f859b2cbd4d0adbd89049195
open Camlify.Music_data
>>>>>>> 820a90b5d722e406bb6ea47dd6b2828384e8cbe3
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
  \ p [filename.mp3] : plays mp3 file with given filename\n\
  \ pause : pause currently played mp3 file\n\
  \ pi [index] : plays mp3 file with given index in current playlist\n\
  \ pl : displays list of songs in current playlist\n\
  \ pls : displays list of all playlists\n\
  \ change_pl [playlist name] : change current playlist into given \
   playlist\n\
  \ change_l [filename.mp3] : change like of the song in the json file\n\
  \ change_ar [filename.mp3] : change the artist of the song in the \
   json file.\n\
  \ An additional prompt is given to get the artist of song\n\
  \ change_al [filename.mp3] : change the album of the song in the \
   json file.\n\
  \ An additional prompt is given to get the album of song\n\
  \ change_y [filename.mp3] [year] : change the year of the song in \
   the json file.\n\
  \ add_tag [filename.mp3] : add a tag to the song in the json file.\n\
  \ An additional prompt is given to get the new tag\n\
  \ rm_tag [filename.mp3] : remove a tag to the song in the json file.\n\
  \ An additional prompt is given to get the tag\n\
  \ name : displays name of current song\n\
  \ index : displays index of current song in current playlist\n\
  \ next : plays next song in current playlist\n\
  \ prev : plays previous song in current playlist\n\
  \ \n\
  \  play_artist : displays list of artist names and plays selected \
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

<<<<<<< HEAD
  (*add [filename.mp3] : add a song named filename.mp3 in current playlist\n \
  rm [filename.mp3 ]: remove song filename.mp3 in current playlist*)
  (* new_pl [playlist name] : create new playlist with given name\n \  *)
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

    | Help -> print_endline help_message; (step_r q)
=======
(*add [filename.mp3] : add a song named filename.mp3 in current
  playlist\n \ rm [filename.mp3 ]: remove song filename.mp3 in current
  playlist*)
(* new_pl [playlist name] : create new playlist with given name\n \ *)
let step (q : Camlify.Queue.t) =
  let rec step_r (q : Camlify.Queue.t) : Camlify.Queue.t =
    print_string "> ";
>>>>>>> 805a313ec7f132d1f859b2cbd4d0adbd89049195

    let cmd : Camlify.Command.command =
      match read_line () with
      | exception End_of_file -> Quit
      | command -> (
          try parse command with
          | Empty ->
              let _ = print_endline "Please write anything..." in
              Idle
          | Malformed ->
              let _ = print_endline "Wrong command input" in
              Idle)
    in

    match cmd with
    | Idle -> step_r q
    | Help ->
        print_endline help_message;
        step_r q
    | Quit ->
        print_endline "Bye!";
        Stdlib.exit 0
    | Play song_name -> (
        let res = Camlify.Queue.play_song_by_name song_name q in
        match res with
        | Illegal ->
            print_endline ("There is no such song as " ^ song_name);
            step_r q
        | Legal new_q ->
            print_endline ("Playing " ^ song_name ^ "...");
            (* TODO: should this logic be here? Might be better in
               either in queue or streamer? Verdict: should probably add
               a string ref in streamer*)
            if song_name = Camlify.Queue.current_song_name q then ()
            else Camlify.Streamer.stop pipeline;
            let file_name =
              Camlify.Music_data.read_song_mp3_file song_name
            in
            ignore
              (Thread.create (Camlify.Streamer.play pipeline) file_name);
            step_r new_q)
    | Pause ->
        let song_name = Camlify.Queue.current_song_name q in
        print_endline ("Pausing " ^ song_name ^ "...");
        Camlify.Streamer.pause pipeline;
        step_r q
    | Stop ->
        let song_name = Camlify.Queue.current_song_name q in
        print_endline ("Stopping " ^ song_name ^ "...");
        Camlify.Streamer.stop pipeline;
        step_r q
    | PlayIndex idx -> (
        let res = Camlify.Queue.play_song_by_idx idx q in
        match res with
        | Illegal ->
            print_endline
              ("There is no such index as " ^ string_of_int idx);
            step_r q
        | Legal new_q ->
            let new_song_name : string =
              Camlify.Queue.current_song_name new_q
            in
            print_endline ("Playing song " ^ new_song_name ^ "...");
            let file_name =
              Camlify.Music_data.read_song_mp3_file new_song_name
            in
            ignore
              (Thread.create (Camlify.Streamer.play pipeline) file_name);
            step_r new_q)
    | CurrentSongName ->
        let song_name = Camlify.Queue.current_song_name q in
        print_endline ("Current song: " ^ song_name);
        step_r q
    | CurrentSongIndex ->
        let song_index = Camlify.Queue.current_song_idx q in
        print_endline ("Current song index: " ^ string_of_int song_index);
        step_r q
    | CurrentPlayList ->
        print_endline
          (String.concat "\n" (Camlify.Queue.current_playlist q));
        step_r q
    | ViewPlaylists ->
        print_endline
          (String.concat "\n" Camlify.Music_data.list_of_playlist);
        step_r q
    | ChangePlayList pl_name -> (
        let res = Camlify.Queue.select_playlist pl_name q in
        match res with
        | Illegal ->
            print_endline ("There is no playlist named " ^ pl_name);
            step_r q
        | Legal new_q ->
            print_endline ("Opening playlist " ^ pl_name ^ "…");
            step_r new_q)
    | ChangeSongLike ((song_name : playlist_name), (liked : bool)) ->
        Camlify.Music_data.change_song_liked song_name liked;
        step_r q
    | ChangeSongArtist song_name ->
        print_endline "What is the name of the artist?";
        print_string "> ";
        let artist = read_line () in
        Camlify.Music_data.change_song_artist song_name artist;
        step_r q
    | ChangeSongAlbum song_name ->
<<<<<<< HEAD
<<<<<<< HEAD
        let _ = print_endline "What is the name of the album?" in 
        let _ = print "> " in
=======
        print_endline "What is the name of the album?";
        print_string "> ";
>>>>>>> ec42c6930d343aee32793a6404ffb44df2e7dfd6
=======
        print_endline "What is the name of the album?";
        print_string "> ";
>>>>>>> 805a313ec7f132d1f859b2cbd4d0adbd89049195
        let album = read_line () in
        change_song_album song_name album;
        step_r q
    | ChangeSongYear (song_name, year) ->
        Camlify.Music_data.change_song_year song_name year;
        step_r q
    | AddSongTag song_name ->
        print_endline "What is a new tag?";
        print_string "> ";
        let tag = read_line () in
        Camlify.Music_data.add_song_tag song_name tag;
        step_r q
    | RemoveSongTag song_name ->
        print_endline "What is the tag?";
        print_string "> ";
        let tag = read_line () in
        Camlify.Music_data.remove_song_tag song_name tag;
        step_r q
    | CreatePlayList pl_name -> (
        let res = Camlify.Queue.make_new_playlist pl_name q in
        match res with
        | Illegal ->
            print_endline ("Failed to create playlist " ^ pl_name);
            step_r q
        | Legal new_q ->
            print_endline ("Create new playlist " ^ pl_name ^ "…");
            step_r new_q)
    | NextSong -> (
        let res = Camlify.Queue.next_song q in
        match res with
        | Illegal ->
            print_endline "There is no next song in the queue.";
            step_r q
        | Legal new_q ->
            let new_song_name : string =
              Camlify.Queue.current_song_name new_q
            in
            print_endline ("Playing song " ^ new_song_name ^ "…");
            let file_name =
              Camlify.Music_data.read_song_mp3_file new_song_name
            in
            Camlify.Streamer.stop pipeline;
            ignore
              (Thread.create (Camlify.Streamer.play pipeline) file_name);
            step_r new_q)
    | PreviousSong -> (
        let res = Camlify.Queue.prev_song q in
        match res with
        | Illegal ->
            print_endline "There is no previous song in the queue.";
            step_r q
        | Legal new_q ->
            let new_song_name : string =
              Camlify.Queue.current_song_name new_q
            in
            print_endline ("Playing song " ^ new_song_name ^ "…");
            let file_name =
              Camlify.Music_data.read_song_mp3_file new_song_name
            in
            ignore
              (Thread.create (Camlify.Streamer.play pipeline) file_name);
            step_r new_q)
    | AddSong song_name -> (
        let res = Camlify.Queue.add_song_to_playlist song_name q in
        match res with
        | Illegal ->
            print_endline ("There is no song named " ^ song_name);
            step_r q
        | Legal new_q ->
            Camlify.Music_data.add_song_to_playlist
              (Camlify.Queue.current_playlist_name new_q)
              song_name;
            print_endline (song_name ^ " added to current playlist.");
            step_r new_q)
    | RemoveSong (song_name : string) -> (
        let res = Camlify.Queue.remove_song_from_playlist song_name q in
        match res with
        | Illegal ->
            print_endline ("There is no song named " ^ song_name);
            step_r q
        | Legal new_q ->
            Camlify.Music_data.delete_song_from_playlist
              (Camlify.Queue.current_playlist_name new_q : string)
              (song_name : string);
            print_endline (song_name ^ " removed from current playlist.");
            step_r new_q)
    | PlayArtist -> begin
        print_endline "Names of all artists in this player :";
        print_endline
          (String.concat ", "
             (List.map Camlify.Music_data.read_song_artist
                Camlify.Music_data.all_songs));
        print_endline "Select artist";
        print_string "> ";
        let artist = read_line () in
        let res = Camlify.Queue.select_playlist_by_artist artist q in
        match res with
        | Illegal ->
            print_endline ("Artist named " ^ artist ^ " doesn't exist");
            step_r q
        | Legal new_q ->
            print_endline ("Playing songs by " ^ artist ^ "...");
            ignore (step_r new_q);
            step_r q
      end
    | PlayAlbum -> begin
        print_endline "Names of all albums in this player :";
        print_endline
          (String.concat ", "
             (List.map Camlify.Music_data.read_song_album
                Camlify.Music_data.all_songs));
        print_endline "Select album";
        print_string "> ";
        let album = read_line () in
        let res = Camlify.Queue.select_playlist_by_album album q in
        match res with
        | Illegal ->
            print_endline ("Album named " ^ album ^ " doesn't exist");
            step_r q
        | Legal new_q ->
            print_endline ("Playing songs in " ^ album ^ "...");
            ignore (step_r new_q);
            step_r q
      end
    | PlayYear -> begin
        print_endline "List of years of songs in this player :";
        print_endline
          (String.concat ", "
             (List.map string_of_int
                (List.map Camlify.Music_data.read_song_year
                   Camlify.Music_data.all_songs)));
        print_endline "Select year";
        print_string "> ";
        let year = read_line () in
        let res =
          Camlify.Queue.select_playlist_by_year (int_of_string year) q
        in
        match res with
        | Illegal ->
            print_endline ("Songs in year " ^ year ^ " doesn't exist");
            step_r q
        | Legal new_q ->
            print_endline ("Playing songs from " ^ year ^ "...");
            ignore (step_r new_q);
            step_r q
      end
    | PlayLiked -> begin
        let res = Camlify.Queue.select_playlist_by_liked q in
        match res with
        | Illegal ->
            print_endline "No songs are liked";
            step_r q
        | Legal new_q ->
            print_endline "Playing liked songs...";
            ignore (step_r new_q);
            step_r q
      end
    | PlayTag -> begin
        print_endline "Names of all tags in this player :";
        print_endline
          (String.concat ", "
             (remove_dup
                (List.flatten
                   (List.map Camlify.Music_data.read_tags
                      Camlify.Music_data.all_songs))));
        print_endline "Select tag";
        print_string "> ";
        let tag = read_line () in
        let res = Camlify.Queue.select_playlist_by_tag tag q in
        match res with
        | Illegal ->
            print_endline ("Tag named " ^ tag ^ " doesn't exist");
            step_r q
        | Legal new_q ->
            print_endline ("Playing songs with tag " ^ tag ^ "...");
            ignore (step_r new_q);
            step_r q
      end
    | _ -> failwith "TODO?"
  in
  step_r q

let rec choose_playlist () : string =
  print_endline "Choose Playlist:";
  print_endline
    (String.concat "\n" Camlify.Music_data.list_of_playlist ^ "\n");
  print_string "> ";
  let playlist =
    match read_line () with
    | exception End_of_file ->
        let _ = print_endline "Empty input :(" in
        choose_playlist ()
    | some_str ->
        if List.mem some_str Camlify.Music_data.list_of_playlist then
          some_str
        else
          let _ = print_endline "There is no such playlist :(" in
          choose_playlist ()
  in
  playlist

let main () =
  (* ANSITerminal.print_string [ ANSITerminal.red ] *)
  print_endline "\n\nWelcome to Camlify \n";
  let playlist = choose_playlist () in
  print_endline ("Opening playlist " ^ playlist ^ "...");
  let q = Camlify.Queue.init_state playlist in

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

