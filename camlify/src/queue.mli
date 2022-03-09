(* TODO: Populate. This file is the "state" of the player.
 * Current song being played, list of previous songs, next songs, etc. *)


type t 
(** The abstract type of values representing current state of music player*)

val init_state : t


val current_song_id : t -> string
(** [current_song st] is the identifier of the song the user is currently playing*)

val following_songs : t -> string list
(** [following_songs st] is the list of songs that are left to play on the playlist*)

val played_songs : t-> string list
(** [played_songs st] is the list of songs that have previously been played on the playlist*)

val remove_dup : 'a list -> 'a list
(**[remove_dup lst] is a helper function that removes duplicates in a lst*)

type result = 
| Legal of t
|Illegal of t

val play_song_by_name : string -> t 
(** [play_song_in_playlist song_name] is *)

val play_song_by_idx : int -> t

val add_song_to_playlist : string -> t

val remove_song_from_playlist : string -> t

val next_song : t 

val back : t