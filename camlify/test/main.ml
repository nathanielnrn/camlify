open OUnit2
open Camlify.Queue
open Camlify.Music_data

(**A general test
   case.[test called_function expected_output
  printer_function]
   creates a test case named [name], and tests for [called_input] =
   [expected_output]. Turns output to string using printer_function*)
let test
    (name : string)
    called_function
    expected_output
    printer_function : test =
  name >:: fun _ ->
  assert_equal called_function expected_output ~printer:printer_function

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
    init_state_song_name_test
      "First song of Playlist zero is \"All Falls Down\""
      "Playlist zero" "All Falls Down";
    ( "current_playlist__name (init_state Playlist one) = \"Playlist \
       one\""
    >:: fun _ ->
      assert_equal "Playlist one"
        (init_state "Playlist one" |> current_playlist_name) );
    ( "current_song_idx (init_state Playlist one) = 0" >:: fun _ ->
      assert_equal 0 (init_state "Playlist one" |> current_song_idx) );
    ( "current_playlist (init_state Playlist one) = [\"All Falls \
       Down\"; \"Break My Heart\"; \"Reptilia\"; \"Sample 15s\"]"
    >:: fun _ ->
      assert_equal
        [ "All Falls Down"; "Break My Heart"; "Reptilia"; "Sample 15s" ]
        (init_state "Playlist one" |> current_playlist) );
  ]

let list_to_message (lst : string list) : string =
  (lst
  |> List.map (fun s -> String.escaped s)
  |> List.fold_left (fun acc s -> acc ^ " " ^ s ^ " ;") "{")
  ^ " }"

let test_select_playlist name playlist_name expected_output =
  test name (select_playlist playlist_name) expected_output

(*Tests music_data functions*)
let music_data_tests =
  [
    test_select_playlist "testing Playlist one song names correct"
      "Playlist one"
      [ "All Falls Down"; "Break My Heart"; "Reptilia"; "Sample 15s" ]
      list_to_message;
  ]

let suite =
  "test suite for Camlify"
  >::: List.flatten [ queue_tests; music_data_tests ]

let _ = run_test_tt_main suite
