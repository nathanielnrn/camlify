open Gstreamer
open Option
(* TODO: Populate. This file take in a file using gstreamer and
 * makes audio play on a computer *)


 (**Behavior -> functions take in a ref option and mutates that ref to contain latest and greatest pipeline*)



 

 (* Returns tags of files
 val tags_of_file : file -> tag list

 val strings_of_tags : tag list -> string list *)

 (** [play pipeline file_name] Plays audio based on string of a file. file_name must be of form ["<name>.mp3"] and is expected to be in the /data/ directory
 mutates the passed in pipeline
 Example: Should be called in main via
 (Thread.create (Camlify.Streamer.play pipeline) file_name)*)
 val play : Element.t Option.t ref-> string -> unit

 (** [pause pipeline] Pauses currently playing from pipeline, throws an exception otherwise (TBD what kind)
 Does not (seem) to need to be in a thread*)
 val pause : Element.t Option.t ref -> unit

 (**[stop pipeline] stops currently playing stream or does nothing because there is none
 Does not (seem) to need to be in a thread*)

 val stop : Element.t Option.t ref -> unit

 val get_pipeline : Element.t Option.t ref


 (*TODO: [parse song] Takes in an mp3 file and parses data.
 Outputs this data in standardized format*)
