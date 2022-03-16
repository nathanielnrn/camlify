(* TODO: Implement according to music_data.mli *)
open Yojson.Basic.Util

type song = {
  name : string;
  liked : bool;
  artist : string option;
  album : string option;
  year : int option;
  tagged : string option;
}

type playlist = {name : string ; songs : song list}

type interface = {
  all_songs : song list;
  playlists : playlist list
}

(*converts from exit object to ocaml exit type*)
let song_from_json song =
  {
    name = song |> member "name" |> to_string;
    liked = ((song |> member "liked" |> to_string) = "true");
    artist = (if ((song |> member "artist") = `Null) then None else Some of 
    (song |> member "artist"|> to_string));
    album = if ((song |> member "album") = `Null) then None else Some of 
    (song |> member "album"|> to_string);
    year = if ((song |> member "year") = `Null) then None else Some of 
    (song |> member "year"|> to_int);
    tagged = if ((song |> member "tagged") = `Null) then None else Some of 
    (song |> member "tagged"|> to_string);
  }

let playlist_from_json playlist =
  {
    name = room |> member "name" |> to_string;
    songs = room |> member "exits" |> to_list |> List.map song_from_json;
  }

let from_json json =
  {
    songs = json |> member "songs" |> to_list |> List.map song_from_json;
    playlists = json |> member "playlists" |> to_list |> List.map playlist_from_json;
  }


let rewrite_json = ()

  (*type t = {x: int; y: int} [@@deriving to_yojson]
  type u = {s: string; pos: t} [@@deriving to_yojson]
  let () = print_endline (Yojson.Safe.pretty_to_string (u_to_yojson {s= "hello"; pos={x= 1; y= 2}}))  *)