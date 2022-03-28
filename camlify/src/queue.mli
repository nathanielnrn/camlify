
(* TODO: Populate. This file is the "state" of the player.
 * Current song being played, list of previous songs, next songs, etc. *)

 type t
 (** The abstract type of values representing current state of music player*)
 
 val init_state : string -> t
 (**[init_state playlist_name] is the initial state of the playlist. In that state
 the song is set to the first song of the current playlist.*)
 

 val current_song_name : t -> string
 (** [current_song_name st] is the identifier of the name of the song the user is
  currently playing*)
 
 val current_song_idx : t -> int
 (**[current_song_idx st] is the identifier of the index of the song the user is
  currently playing*)
 
 val current_playlist : t -> string list
 (**[current_playlist st] is the identifier of the current playlist*)
 
 (**The type representing the result of an attempted change of song*)
 type result = 
 | Legal of t
 |Illegal
 
 val play_song_by_name : string -> t -> result
 (** [play_song_in_playlist song_name st] is [r] if attempting to play [song_name]
   in state [st] results in [r]. If [song_name] exists in [current_playlist], then
   [r] is [Legal st'], where in [st'] the current_song_name and current_song_idx
   is updated to the corresponding name and index of [song_name]. Otherwise,
   the result is [Illegal]*)
 
 val play_song_by_idx : int -> t -> result
 (** [play_song_by_idx song_idx st] is [r] if attempting to play song of
   [song_idx] in state [st] results in [r]. If [song_idx] is smaller than the
   length of [current_playlist], then [r] is [Legal st'], where in [st'] the 
   current_song_name and current_song_idx is updated to the corresponding name and
   index of [song_idx]. Otherwise, the result is [Illegal]*)
 
 val next_song : t -> result 
 (**[next_song st] is [r] if attempting to play next song in playlist in state 
   [st] results in [r]. If the length of current_playlist is greater than 0, then
   [r] is [Legal st'], where in [st'] the current_song updated to the song of 
   next index. Otherwise, the result is [Illegal]. If the current_song is the 
   last song on playlist, the next_song loops around to the first song in 
   playlist*)
 
 val prev_song : t -> result
 (**[prev_song st] is [r] if attempting to play previous song in playlist in state 
   [st] results in [r]. If the length of current_playlist is greater than 0, then
   [r] is [Legal st'], where in [st'] the current_song updated to the song of 
   previous index. Otherwise, the result is [Illegal]. If the current_song is the 
   first song on playlist, the prev_song loops around to the last song in 
   playlist*)
 
 val random_song : t -> result
 (**[random_song st] is [r] if attempting to play random song in playlist in state 
   [st] results in [r]. If the length of current_playlist is greater than 0, then
   [r] is [Legal st'], where in [st'] the current_song updated to the song of 
   random index. Otherwise, the result is [Illegal].*)
 
 val add_song_to_playlist : string -> t -> result
 (**[add_song_to_playlist song_name  st] is [r] if attempting to add song to
    playlist in playlist in state [st] results in [r]. If song_name is a song in
    all_songs, then [r] is [Legal st'], where in [st'] the song is added to the
    playlist. Otherwise, the result is [Illegal]*)
 
 val remove_song_from_playlist : string -> t -> result
 (**[remove_song_from_playlist song_name st] is [r] if attempting to remove
    song from playlist in playlist in state [st] results in [r]. If song_name is
    a song in current_playlist, then [r] is [Legal st'], where in [st'] the song is
    added to the playlist. Otherwise, the result is [Illegal]*)
 
 val make_new_playlist : string -> t -> result
 (**[make_new_playlist playlist_name st] is [r] if attempting to make a new 
   playlist in state [st] results in [r]. If playlist_name is not in list_of_playlist,
  then [r] is [Legal st'], where in [st'] the new empty playlist is added to
  list_of_playlist. Otherwise, the result is [Illegal]*)
 
 val select_playlist : string -> t -> result
 (**[select_playlist playlist_name st] is [r] if attempting to select an 
   existing playlist in state [st] results in [r]. If playlist_name is in 
  list_of_playlist, then [r] is [Legal st'], where in [st'] the playlist_name 
  selected with first song as current_song_name. Otherwise, the result is 
  [Illegal]*)
 
val select_playlist_by_artist : string -> t -> result
 (**[select_playlist_by_artist artist st] is [r] if attempting to select an 
   artist's songs in state [st] results in [r]. If artist is an existing artist of
  list_of_all_songs, then [r] is [Legal st'], where in [st'] the artist's songs 
  selected with first song as current_song_name. Otherwise, the result is 
  [Illegal]*)

val select_playlist_by_album : string -> t -> result
  (**[select_playlist_by_album album st] is [r] if attempting to select an 
    album's songs in state [st] results in [r]. If album is an existing album of
   list_of_all_songs, then [r] is [Legal st'], where in [st'] the album's songs 
   selected with first song as current_song_name. Otherwise, the result is 
   [Illegal]*)

val select_playlist_by_year : int -> t -> result
 (**[select_playlist_by_year year st] is [r] if attempting to select an 
   year's songs in state [st] results in [r]. If year is an existing year of
  list_of_all_songs, then [r] is [Legal st'], where in [st'] the year's songs 
  selected with first song as current_song_name. Otherwise, the result is 
  [Illegal]*)

val select_playlist_by_liked : t -> result
 (**[select_playlist_by_liked st] is [r] if attempting to select  
   liked songs in state [st] results in [r]. If liked songs exist in 
  list_of_all_songs, then [r] is [Legal st'], where in [st'] the liked songs 
  selected with first song as current_song_name. Otherwise, the result is 
  [Illegal]*)

val select_playlist_by_tag : string -> t -> result
  (**[select_playlist_by_tag tag st] is [r] if attempting to select a
    tag's songs in state [st] results in [r]. If tag is an existing tag of
   list_of_all_songs, then [r] is [Legal st'], where in [st'] the tag's songs 
   selected with first song as current_song_name. Otherwise, the result is 
   [Illegal]*)

 val delete_playlist : string -> t -> result
 (**[delete_playlist playlist_name  st] is [r] if attempting to delete a 
   playlist in state [st] results in [r]. If playlist_name is in list_of_playlist,
  then [r] is [Legal st'], where in [st'] the playlist is removed from
  list_of_playlist. Otherwise, the result is [Illegal]*)
 
 val add_new_song : string -> t -> result
 (**[add_new_song song_name  st] is [r] if attempting to add a new song in 
  all_songs in state [st] results in [r]. If song_name is not in list_of_all_songs,
  then [r] is [Legal st'], where in [st'] the new song is added to
  list_of_all_songs. Otherwise, the result is [Illegal]*)
 
 val remove_song_from_all_songs : string  -> t -> result
  (**[remove_new_song song_name  st] is [r] if attempting to remove an existing
  song in all_songs in state [st] results in [r]. If song_name is in 
  list_of_all_songs, then [r] is [Legal st'], where in [st'] the song is removed
  from list_of_all_songs. Otherwise, the result is [Illegal]*)

