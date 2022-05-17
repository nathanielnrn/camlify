(* (* TODO: Implement according to music_data.mli *) open
   Yojson.Basic.Util

   let file = "data/interface.json" let data = Yojson.Basic.from_file
   "data/interface.json"

   type song = { name : string; liked : bool; mp3_file : string; artist
   : string option; album : string option; year : int option; }

   type playlist = { name : string; songs : song list; }

   type interface = { all_songs : song list; playlists : playlist list;
   }

   (*converts from exit object to ocaml exit type*) let song_from_json
   song = { name = song |> member "name" |> to_string; liked = song |>
   member "liked" |> to_bool; mp3_file = song |> member "mp3 file" |>
   to_string; artist = (if song |> member "artist" = `Null then None
   else song |> member "artist" |> to_string_option); album = (if song
   |> member "album" = `Null then None else song |> member "album" |>
   to_string_option); year = (if song |> member "year" = `Null then None
   else song |> member "year" |> to_int_option); }

   let playlist_from_json playlist = { name = playlist |> member "name"
   |> to_string; songs = playlist |> member "songs" |> to_list |>
   List.map song_from_json; }

   let from_json json = { all_songs = json |> member "songs" |> to_list
   |> List.map song_from_json; playlists = json |> member "playlists" |>
   to_list |> List.map playlist_from_json; }

   let rec to_song (song : song) : Yojson.t = `Assoc [ ("name", `String
   song.name); ("liked", `Bool song.liked); ("mp3 file", `String
   song.mp3_file); ( "artist", match song.artist with | None -> `Null |
   Some artist -> `String artist ); ( "album", match song.album with |
   None -> `Null | Some album -> `String album ); ( "year", match
   song.year with | None -> `Null | Some year -> `Int year ); ]

   let rec to_playlist (playlist : playlist) : Yojson.t = `Assoc [
   ("name", `String playlist.name); ("songs", `List (playlist.songs |>
   List.map to_song)); ]

   let rec to_interface (interface : interface) : Yojson.t = `Assoc [
   ("all songs", `List (interface.all_songs |> List.map to_song));
   ("playlists", `List (interface.playlists |> List.map to_playlist)); ]

   let message = "Hello!" let file' = "data/data.json"

   let message =
   "{\"playlists\":[{\"name\":\"frankie\",\"songs\":[{\"name\":\"fly
   me\n\ \ to the moon\",\"tagged\": true,\"mp3 file\": \"this\"}]}]}"

   let j = Yojson.Basic.from_file "data/interface.json"

   let song1 = { name = "fly me to the moon"; liked = true; mp3_file =
   "yeet"; artist = None; album = None; year = None; tags = []; }

   let song2 = { name = "fly me to the caml"; liked = true; mp3_file =
   "yeet"; artist = None; album = None; year = None; tags = []; }

   let playlist1 = { name = "bangers"; songs = [ song1; song2 ] }

   let x : interface = { all_songs = [ song1; song2 ]; playlists = [
   playlist1 ] }

   let to_x = to_interface x let pushed = Yojson.pretty_to_string
   to_x *)
