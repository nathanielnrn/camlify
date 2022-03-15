(* These are values queue.ml needs from music_data.ml copy and paste these specifications *)

 val select_playlist : string -> string list
 (**[select_playlist playlist_name] returns the list of song names that the 
 playlist of playlist_name contains*)

 val list_of_playlist : string list
 (**[list_of_playlist] is a list of all playlist names*)

 val all_songs : string list
 (**[all_songs] is a list of all song names*)