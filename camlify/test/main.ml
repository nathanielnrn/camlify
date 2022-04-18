open OUnit2
open Camlify.Queue
open Camlify.Music_data

let init_state_song_name_test
    (name : string)
    (playlist_name : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (init_state playlist_name |> current_song_name)
    ~printer:String.escaped

(*Tests queue functions*)
let queue_tests =
  [
    init_state_song_name_test
      "First song of Playlist one is \"All Falls Down\"" "Playlist one"
      "All Falls Down";
  ]

(*Tests music_data functions*)
let music_data_tests = []

let suite =
  "test suite for Camlify"
  >::: List.flatten [ queue_tests; music_data_tests ]

let _ = run_test_tt_main suite
