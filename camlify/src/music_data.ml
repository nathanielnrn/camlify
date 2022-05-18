(* TODO: Implement according to music_data.mli *)
open Yojson.Basic
open Yojson.Basic.Util

exception UnknownSong of string
exception UnknownInformation of string
exception UnknownPlaylist of string

let file = ref "data/data.json"

let setfile s =
  let j =
    Yojson.Basic.from_file
      (file := s;
       !file)
  in
  (fun j' -> ()) j

type song = {
  name : string;
  liked : bool;
  mp3_file : string;
  artist : string option;
  album : string option;
  year : int option;
  tags : string list;
}

type playlist = {
  name : string;
  songs : song list;
}

type interface = {
  all_songs : song list;
  playlists : playlist list;
}

(*converts from exit object to ocaml exit type*)
let song_from_json song =
  {
    name = song |> member "name" |> to_string;
    liked = song |> member "liked" |> to_bool;
    mp3_file = song |> member "mp3 file" |> to_string;
    artist =
      (if song |> member "artist" = `Null then None
      else song |> member "artist" |> to_string_option);
    album =
      (if song |> member "album" = `Null then None
      else song |> member "album" |> to_string_option);
    year =
      (if song |> member "year" = `Null then None
      else song |> member "year" |> to_int_option);
    tags = song |> member "tags" |> to_list |> List.map to_string;
  }

let playlist_from_json playlist =
  {
    name = playlist |> member "name" |> to_string;
    songs =
      playlist |> member "songs" |> to_list |> List.map song_from_json;
  }

let from_json json =
  {
    all_songs =
      json |> member "all songs" |> to_list |> List.map song_from_json;
    playlists =
      json |> member "playlists" |> to_list
      |> List.map playlist_from_json;
  }

let rec to_song (song : song) : Yojson.t =
  `Assoc
    [
      ("name", `String song.name);
      ("liked", `Bool song.liked);
      ("mp3 file", `String song.mp3_file);
      ( "artist",
        match song.artist with
        | None -> `Null
        | Some artist -> `String artist );
      ( "album",
        match song.album with
        | None -> `Null
        | Some album -> `String album );
      ( "year",
        match song.year with
        | None -> `Null
        | Some year -> `Int year );
      ("tags", `List (List.map (fun s -> `String s) song.tags));
    ]

let rec to_playlist (playlist : playlist) : Yojson.t =
  `Assoc
    [
      ("name", `String playlist.name);
      ("songs", `List (playlist.songs |> List.map to_song));
    ]

let rec to_interface (interface : interface) : Yojson.t =
  `Assoc
    [
      ("all songs", `List (interface.all_songs |> List.map to_song));
      ("playlists", `List (interface.playlists |> List.map to_playlist));
    ]

let update_json iface =
  let pushed = Yojson.pretty_to_string (to_interface iface) in
  let out_chan = open_out !file in
  Printf.fprintf out_chan "%s\n" pushed;
  close_out out_chan

let slist_to_snames (slist : song list) =
  List.map (fun (s : song) -> s.name) slist

let plist_to_pnames (plist : playlist list) =
  List.map (fun (p : playlist) -> p.name) plist

let rec playlist_selector (plist : playlist list) (pname : string) =
  match plist with
  | [] -> raise (UnknownPlaylist pname)
  | h :: t when h.name = pname -> slist_to_snames h.songs
  | h :: t -> playlist_selector t pname

(**[select_playlist playlist_name] returns the list of song names that
   the playlist of playlist_name contains*)
let select_playlist pname =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  playlist_selector iface.playlists pname

let get_mp3 lst = List.map (fun x -> x.mp3_file) lst

(*song list to mp3 file of song lists*)
let all_songs_mp3 () : string list =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  get_mp3 iface.all_songs

(**[list_of_playlist] is a list of all playlist names*)
let list_of_playlist () : string list =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  plist_to_pnames iface.playlists

(*song list to name of song lists*)
let all_songs () : string list =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  slist_to_snames iface.all_songs

(*song list to name of song lists*)
let all_songs_objects () : song list =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  iface.all_songs

let rec song_name_to_mp3_file song_name (lst : song list) =
  match lst with
  | [] -> raise (UnknownSong song_name)
  | h :: t when h.name = song_name -> h.mp3_file
  | h :: t -> song_name_to_mp3_file song_name t

let read_song_liked song =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  let rec song_liked song (songlst : song list) =
    match songlst with
    | [] -> raise (UnknownSong song)
    | h :: t when h.name = song -> h.liked
    | h :: t -> song_liked song t
  in
  song_liked song iface.all_songs

(*[read_song_component f song] is f on the song*)
let read_song_component f song =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  let rec read_song song (songlst : song list) =
    match songlst with
    | [] -> raise (UnknownSong song)
    | h :: t when h.name = song -> f h
    | h :: t -> read_song song t
  in
  read_song song iface.all_songs

let option_to_useful_str component song =
  match component with
  | Some s -> s
  | None -> ""

let option_to_useful_int component song =
  match component with
  | Some s -> s
  | None -> 0

let read_song_mp3_file song =
  read_song_component (fun sng -> sng.mp3_file) song

let read_song_artist song =
  option_to_useful_str
    (read_song_component (fun sng -> sng.artist) song)
    song

let read_song_album song =
  option_to_useful_str
    (read_song_component (fun sng -> sng.album) song)
    song

let read_song_year song =
  option_to_useful_int
    (read_song_component (fun sng -> sng.year) song)
    song

let read_tags song = read_song_component (fun sng -> sng.tags) song

let rec delete_song (songs : song list) song =
  match songs with
  | [] -> raise (UnknownSong song)
  | h :: t when h.name = song -> t
  | h :: t -> h :: delete_song t song

(* just a helper for delete_song_from_playlist*)
let rec delete_playlist (playlists : playlist list) playlist song =
  match playlists with
  | [] -> raise (UnknownPlaylist playlist)
  | h :: t when h.name = playlist ->
      { h with songs = delete_song h.songs song } :: t
  | h :: t -> h :: delete_playlist t playlist song

let delete_song_from_playlist playlist song =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  let newiface =
    {
      iface with
      playlists = delete_playlist iface.playlists playlist song;
    }
  in
  update_json newiface

let rec add_song_playlist (playlists : playlist list) playlist song =
  match playlists with
  | [] -> raise (UnknownPlaylist playlist)
  | h :: t when h.name = playlist ->
      { h with songs = h.songs @ [ song ] } :: t
  | h :: t -> h :: add_song_playlist t playlist song

let add_song_to_playlist playlist song =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  let newiface =
    {
      iface with
      playlists =
        add_song_playlist iface.playlists playlist
          (try
             List.find
               (fun (sng : song) -> sng.name = song)
               iface.all_songs
           with
          | _ -> raise (UnknownSong song));
    }
  in
  update_json newiface

(*makes a trivial playlist from a playlist name*)
let default_playlist playlist = { name = playlist; songs = [] }

let add_playlist playlist =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  let newiface =
    {
      iface with
      playlists = iface.playlists @ [ default_playlist playlist ];
    }
  in
  update_json newiface

let rec modify_song f (songlst : song list) song =
  match songlst with
  | [] -> raise (UnknownSong song)
  | h :: t when h.name = song -> f h :: t
  | h :: t -> h :: modify_song f t song

(*modifies a song without raising an exception*)
let rec modify_song_no_exp f (songlst : song list) song =
  match songlst with
  | [] -> []
  | h :: t when h.name = song -> f h :: t
  | h :: t -> h :: modify_song_no_exp f t song

let rec update_playlists f (plist : playlist list) song =
  match plist with
  | [] -> []
  | h :: t ->
      { h with songs = modify_song_no_exp f h.songs song }
      :: update_playlists f t song

let modify_song_and_write f song =
  let j = Yojson.Basic.from_file !file in
  let iface = from_json j in
  let newiface =
    {
      all_songs = modify_song f iface.all_songs song;
      playlists = update_playlists f iface.playlists song;
    }
  in
  update_json newiface

let change_song_liked song like =
  modify_song_and_write (fun sng -> { sng with liked = like }) song

let change_song_artist song artist =
  modify_song_and_write
    (fun sng -> { sng with artist = Some artist })
    song

let change_song_album song album =
  modify_song_and_write
    (fun sng -> { sng with album = Some album })
    song

let change_song_year song year =
  modify_song_and_write (fun sng -> { sng with year = Some year }) song

let add_song_tag song tag =
  modify_song_and_write
    (fun sng -> { sng with tags = sng.tags @ [ tag ] })
    song

let remove_song_tag song tag =
  modify_song_and_write
    (fun sng ->
      { sng with tags = List.filter (fun t -> t <> tag) sng.tags })
    song

(* Helper function which traverses to /data and returns a string list of
   .mp3 files string in list is of form '<song_name>.mp3'*)

let get_dir_songs () : string list =
  let data_path = Streamer.data_dir_uri in
  let song_names = Sys.readdir data_path |> Array.to_list in
  List.filter
    (fun file_name -> Filename.extension file_name = ".mp3")
    song_names

(*pair of (song name, mp3 name) from a get_dir_song list*)
let seperate_song_mp3 dat =
  let rec sep i s acc =
    if i >= String.length s then failwith "got to end of string"
    else
      let c = String.get s i in
      if c = '.' then acc else sep (i + 1) s (acc ^ Char.escaped c)
  in
  (sep 0 dat "", dat)

(*creates a list of songs from list of data strings from get_dir_song*)
let rec songs_to_interface slist : song list =
  match slist with
  | (sname, mp3) :: t ->
      {
        name = sname;
        liked = false;
        mp3_file = mp3;
        artist = None;
        album = None;
        year = None;
        tags = [];
      }
      :: songs_to_interface t
  | [] -> []

(*initial playlist list (TODO: set later to [])*)
let init_playlists slist = [ { name = "All_songs"; songs = slist } ]

let interface_from_song_list slist : interface =
  { all_songs = slist; playlists = init_playlists slist }

let load_data () =
  get_dir_songs ()
  |> List.map seperate_song_mp3
  |> songs_to_interface |> interface_from_song_list |> update_json

let original_test_interface =
  from_json (Yojson.Basic.from_file "data/archived.json")

let reset () =
  let pushed =
    Yojson.pretty_to_string (to_interface original_test_interface)
  in
  (let out_chan = open_out "data/interface.json" in
   Printf.fprintf out_chan "%s\n" pushed;
   close_out out_chan);
  let out_chan = open_out "data/writable.json" in
  Printf.fprintf out_chan "%s\n" pushed;
  close_out out_chan
