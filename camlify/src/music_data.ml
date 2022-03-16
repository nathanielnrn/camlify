(* TODO: Implement according to music_data.mli *)
open Yojson.Basic.Util

let file = "data/interface.json"

let data = Yojson.Basic.from_file file

type song = {
  name : string;
  liked : bool;
  mp3_file : string;
  artist : string option;
  album : string option;
  year : int option;
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
    liked =  song |> member "liked" |> to_bool;
    mp3_file = song |> member "mp3 file" |> to_string;
    artist = if ((song |> member "artist") = `Null) then None else 
      song |> member "artist" |> to_string_option;
    album = if ((song |> member "album") = `Null) then None else 
      song |> member "album" |> to_string_option;
    year = if ((song |> member "year") = `Null) then None else 
      song |> member "year" |> to_int_option;

  }

let playlist_from_json playlist =
  {
    name = playlist |> member "name" |> to_string;
    songs = playlist |> member "songs" |> to_list |> List.map song_from_json;
  }

let from_json json =
  {
    all_songs = json |> member "songs" |> to_list |> List.map song_from_json;
    playlists = json |> member "playlists" |> to_list |> List.map playlist_from_json;
  }


let rec to_song (song : song) : Yojson.t = 
  `Assoc (("name", `String song.name ) :: 
  ("liked", `Bool song.liked ) 
  :: ("mp3 file", `String song.mp3_file )
  :: ("artist", match song.artist with 
  | None -> `Null
  | Some artist -> `String artist )
  :: ("album", match song.album with 
  | None -> `Null
  | Some album -> `String album )
  :: ("year", match song.year with 
  | None -> `Null
  | Some year -> `Int year )
  ::[])

let rec to_playlist (playlist : playlist) : Yojson.t = 
    `Assoc (("name", `String playlist.name ) :: 
    ("songs", `List (playlist.songs|> List.map to_song)) 
    ::[])
  
let rec to_interface (interface: interface) : Yojson.t = 
    `Assoc (("all songs", `List (interface.all_songs |> List.map to_song)) :: 
    ("playlists", `List (interface.playlists|> List.map to_playlist)) 
    ::[])

let song1 ={
      name = "fly me to the moon";
      liked = true;
      mp3_file = "yeet";
      artist = None;
      album = None;
      year = None;
    }

let song2 ={
  name = "fly me to the caml";
  liked = true;
  mp3_file = "yeet";
  artist = None;
  album = None;
  year = None;
  }
let playlist1 = {name = "bangers";
songs = [song1;song2]

}

let x : interface = {all_songs = [song1;song2];
playlists = [playlist1]}

let to_x = to_interface x

let pushed = Yojson.pretty_to_string to_x

let () =  
  (* Write message to file *)
  let oc = open_out file in (* create or truncate file, return channel *)
    Printf.fprintf oc "%s\n" pushed; (* write something *)   
    close_out oc;;

let slist_to_snames (slist : song list) = 
  List.map  (fun (s : song)-> s.name) slist

let plist_to_pnames (plist : playlist list) = 
  List.map  (fun (p : playlist)-> p.name) plist

let rec playlist_selector (plist : playlist list) (pname : string)= 
  match plist with
  | [] -> []
  | h::t when h.name = pname -> slist_to_snames h.songs
  | h::t -> playlist_selector t pname
       

(**[select_playlist playlist_name] returns the list of song names that the 
  playlist of playlist_name contains*)
let select_playlist pname = 
  let j = Yojson.Basic.from_file file in
    let iface = from_json j in
      playlist_selector iface.playlists pname      
   
(**[list_of_playlist] is a list of all playlist names*)
let list_of_playlist : string list = 
  let j = Yojson.Basic.from_file file in
    let iface = from_json j in
      plist_to_pnames iface.playlists
 
      
(*song list to name of somg lists*)      
let all_songs : string list = 
  let j = Yojson.Basic.from_file file in
    let iface = from_json j in
      slist_to_snames iface.all_songs
  
(*let rec to_interface (interface : interface) : Yojson.Basic.t = 
  `Assoc [("playlists",(match interface.playlists with
  | [] -> `List []
  | h::t -> [List]*)
  
(* let file = "interface.json" *)
let message = "Hello!"
let test_write =  
  (* Write message to file *)
  let oc = open_out file in (* create or truncate file, return channel *)
    Printf.fprintf oc "%s\n" message; (* write something *)   
    close_out oc; 

(*let rewrite_json = ()

  type t = {x: int; y: int} [@@deriving to_yojson]
  type u = {s: string; pos: t} [@@deriving to_yojson]
  let () = print_endline (Yojson.Safe.pretty_to_string (u_to_yojson {s= "hello"; pos={x= 1; y= 2}}))  *)