(** This module allows us to make calls to the gstreamer library and
    makes audio play on a computer.

    Behavior -> functions take in a ref option and mutates that ref to
    contain latest and greatest pipeline. *)

open Gstreamer
open Option

val play : Element.t Option.t ref -> string -> unit
(** [play pipeline file_name] Plays audio based on string of a file.
    file_name must be of form ["<name>.mp3"] and is expected to be in
    the /data/ directory. This call Mutates the passed in pipeline
    Example: Should be called in main via (Thread.create
    (Camlify.Streamer.play pipeline) file_name)*)

val pause : Element.t Option.t ref -> unit
(** [pause pipeline] Pauses currently playing from pipeline, throws an
    exception otherwise (TBD what kind) Does not (seem) to need to be in
    a thread*)

val stop : Element.t Option.t ref -> unit
(**[stop pipeline] stops currently playing stream or does nothing
   because there is none. Does not (seem) to need to be in a thread*)

val get_pipeline : Element.t Option.t ref
(**Returns an empty pipeline (not instantiated yet)*)

val data_dir_uri : string
(** Returns path of data dir ending with / i.e [".../data/"] based on
    execution path *)
