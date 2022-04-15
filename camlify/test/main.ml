open OUnit2
open Camlify.Queue
open Camlify.Music_data

(*Tests queue functions*)
let queue_tests =
  [

  ]

  (*Tests music_data functions*)
  let music_data_tests = 
    [

    ]

    let suite = "test suite for Camlify"
    >::: List.flatten [queue_tests; music_data_tests]

    let _ = run_test_tt_main suite