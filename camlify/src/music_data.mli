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

val test_write : unit