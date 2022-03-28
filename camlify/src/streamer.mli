(* TODO: Populate. This file take in a file using gstreamer and
 * makes audio play on a computer *)

 type file

 type streamer
 
 type tag

 (*Returns tags of files*)
 val tags_of_file : file -> tag list

 val strings_of_tags : tag list -> string list

 (*Plays audio based on string of a file. File must be of form ["<name>.mp3"] and is expected to be in the /data/ directory*)
 val play : string -> unit

 (**Pauses currently playing song, throws an exception otherwise (TBD what kind)*)
 val pause : unit

 (**Stops currently playing stream or does nothing because there is none*)
 val stop : unit


 (*[parse song] Takes in an mp3 file and parses data.
 Outputs this data in standardized format*)
 val parse  : 'a -> 'b
