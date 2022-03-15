(* TODO: Populate. This module reads and writes to files in data/ directory.
 * These files contain info on playlists, settings, etc. *)


type song
(**represents a song*)


type playlist
(**represents a playlist*)


type interface
(**represents all the song and playlist data available*)

val from_json : Yojson.Basic.t -> interface
(** [from_json j] is the data that [j] represents. Requires: [j] is
    a valid JSON music data representation.*)

val select_playlist : string -> string list
(**[select_playlist playlist_name] returns the list of song names that the 
playlist of playlist_name contains*)
   
val list_of_playlist : string list
(**[list_of_playlist] is a list of all playlist names*)
   
val all_songs : string list
(**[all_songs] is a list of all song names*)