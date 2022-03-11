(* TODO: Populate. This file take in a file using gstreamer and
 * makes audio play on a computer *)

 type file

 type streamer
 
 type tags

 (*Returns tags of files*)
 val tags_of_file : file -> tags

 (*Plays audio based on file*)
 val play : file -> unit


 (*[parse song] Takes in an mp3 file and parses data.
 Outputs this data in standardized format*)
 val parse  : 'a -> 'b
