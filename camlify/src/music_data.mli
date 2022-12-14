(**This module reads and writes to files in data/ directory. * These
   files contain info on playlists, settings, etc. *)

exception UnknownSong of string
(**raised when the inputed song is unknown*)

exception UnknownPlaylist of string
(**raised when the inputed playlist is unknown*)

exception UnknownInformation of string
(**raised when the song does not contain the information requested*)

exception EmptyFile
(**raised when an empty file is trying to be used*)

val setfile : string -> unit
(**[set file file_name] changes the file that music_data reads from. For
   testing purposes. Raises [EmptyFile] for empty files*)

val setfile' : string -> unit
(**[setfile' file_name] does the same as sefile without raising
   [EmptyFile]*)

val select_playlist : string -> string list
(**[select_playlist playlist_name] returns the list of song names that
   the playlist of playlist_name contains. Raises [UnknownPlaylist] for
   a playlist that does not exist *)

val list_of_playlist : unit -> string list
(**[list_of_playlist ()] is a list of all playlist names*)

val all_songs : unit -> string list
(**[all_songs] is a list of all song names*)

val read_song_liked : string -> bool
(**[read_song_liked song_name] returns the bool value of liked of
   song_name. ex) read_song_liked Reptilia liked returns false. Raises
   [UnknownSong] for a song that does not exist*)

val read_song_mp3_file : string -> string
(**[read_song_mp3_file song_name] returns the field of song_name. Raises
   [UnknownSong] for a song that does not exist*)

val read_song_artist : string -> string
(**[read_song_artist song_name] returns the artist of song_name. Raises
   [UnknownSong] for a song that does not exist returns the empty string
   if the artist for the song was not specified in the json file*)

val read_song_album : string -> string
(**[read_song_album song_name] returns the album of song_name. Raises
   [UnknownSong] for a song that does not exist returns the empty string
   if the album for the song was not specified in the json file*)

val read_song_year : string -> int
(**[read_song_year song_name] returns the year of song_name. Raises
   [UnknownSong] for a song that does not exist returns 0 if the year
   for the song was not specified in the json file*)

val read_tags : string -> string list
(**[read_tags song_name] returns the list of tags associated with
   [song_name]. Raises [UnknownSong] for a song that does not exist
   returns the empty list if the tags for the song was not specified in
   the json file*)

val add_song_to_playlist : string -> string -> unit
(** [add_song_to_playlist playlist song] adds [song] to [playlist] in
    the json file. Raises [UnknownSong] if [song] is not in the "all
    songs" in json Raises [UnknownPlaylist]*)

val delete_song_from_playlist : string -> string -> unit
(**[delete_song_from_playlist playlist song] deletes song from playlist
   in json raises [UnknownSong] and [UnknownPlaylist]*)

val add_playlist : string -> unit
(**[add playlist pname] adds a playlist with no songs to the json file
   with name [pname]*)

val change_song_liked : string -> bool -> unit
(**[change_song_liked song_name liked_state ] modifies the liked field
   of [song_name] to [liked_state]. ex) change_song_liked Reptilia true
   changes the liked field of Reptilia to true.*)

val change_song_artist : string -> string -> unit
(**[change_song_artist song_name new_artist] modifies the artist of
   [song_name] to [new_artist].*)

val change_song_album : string -> string -> unit
(**[change_song_album song_name new_album] modifies the album of
   song_name to new_album.*)

val change_song_year : string -> int -> unit
(**[change_song_artist song_name new_year] modifies the year of
   song_name to new_year.*)

val add_song_tag : string -> string -> unit
(**[add_song_tag song_name new_tag] adds new_tag to the tags of
   song_name.*)

val remove_song_tag : string -> string -> unit
(**[remove_song_tag song_name tag_to_remove] removes tag_to_remove from
   the tags of song_name.*)

val load_data : unit -> unit
(**[load_data ()] writes to data.json file all the information obtained
   from the data*)

val reset : unit -> unit
(**[reset ()] resets the json files for testing*)
