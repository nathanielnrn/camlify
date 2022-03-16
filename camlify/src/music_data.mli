(* TODO: Populate. This module reads and writes to files in data/ directory.
 * These files contain info on playlists, settings, etc. *)


val select_playlist : string -> string list
(**[select_playlist playlist_name] returns the list of song names that the 
playlist of playlist_name contains 
For now, the function requiers an existing playlist and deals with a non
existing playlist by returning the empty list*)
   
val list_of_playlist : string list
(**[list_of_playlist] is a list of all playlist names*)
   
val all_songs : string list
(**[all_songs] is a list of all song names*)
<<<<<<< HEAD
=======

>>>>>>> music_data
