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
    expected_output
    called_function
    printer_function : test =
  name >:: fun _ ->
  assert_equal expected_output called_function ~printer:printer_function

let list_to_message (lst : string list) : string =
  (lst
  |> List.map (fun s -> String.escaped s)
  |> List.fold_left (fun acc s -> acc ^ " " ^ s ^ " ;") "{")
  ^ " }"

(* Default expectation of output is an int *)
let queue_test (name : string) expected_output called_function printer_f
    : test =
  name >:: fun _ ->
  assert_equal expected_output called_function ~printer:printer_f

let init_state_song_name_test
    (name : string)
    (playlist_name : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (init_state playlist_name |> current_song_name)
    ~printer:String.escaped

let result_to_t result =
  match result with
  | Legal t -> t
  | _ -> raise (Failure "Something's wrong")

let next st = st |> next_song |> result_to_t
let prev st = st |> prev_song |> result_to_t

(* let rec next_count n nxt = next_count (n-1) (next nxt) *)

(*Tests queue functions*)
let queue_tests =
  [
    init_state_song_name_test
      "First song of Playlist one is \"All Falls Down\"" "Playlist one"
      "All Falls Down";
    init_state_song_name_test
      "First song of Playlist zero is \"All Falls Down\""
      "Playlist zero" "All Falls Down";
    test
      "current_playlist__name (init_state Playlist one) = \"Playlist \
       one\""
      (init_state "Playlist one" |> current_playlist_name)
      "Playlist one" Fun.id;
    queue_test
      "current_playlist__name (init_state Playlist one) = \"Playlist \
       one\""
      "Playlist_one"
      (init_state "Playlist one" |> current_playlist_name)
      Fun.id;
    queue_test "current_song_idx (init_state Playlist one) = 0" 0
      (init_state "Playlist one" |> current_song_idx)
      string_of_int;
    queue_test
      "current_playlist (init_state Playlist one) = [\"All Falls \
       Down\"; \"Break My Heart\"; \"Reptilia\"; \"Sample 15s\"]"
      [ "All Falls Down"; "Break My Heart"; "Reptilia"; "Sample 15s" ]
      (init_state "Playlist one" |> current_playlist)
      list_to_message;
    queue_test
      "current_song_idx(play_song_by_name(\"Reptilia\" (init_state \
       Playlist one))) = 2"
      2
      (init_state "Playlist one"
      |> play_song_by_name "Reptilia"
      |> result_to_t |> current_song_idx)
      string_of_int;
    queue_test
      "current_song_idx(play_song_by_name(\"Reptilia\" (init_state \
       Playlist one))) = 2"
      2
      (init_state "Playlist one"
      |> play_song_by_name "Reptilia"
      |> result_to_t |> current_song_idx)
      string_of_int;
    queue_test
      "current_song_idx(next_song (next_song(next_song(init_state \
       Playlist one))) = 3"
      3
      (init_state "Playlist one"
      |> next |> next |> next |> current_song_idx)
      string_of_int;
    queue_test
      "current_song_idx(next_song(next_song \
       (next_song(next_song(init_state Playlist one)))) = 0"
      0
      (init_state "Playlist one"
      |> next |> next |> next |> next |> current_song_idx)
      string_of_int;
    queue_test
      "current_song_idx(next_song(next_song(next_song(next_song \
       (next_song(next_song(init_state Playlist one)))))) = 2"
      2
      (init_state "Playlist one"
      |> next |> next |> next |> next |> next |> next
      |> current_song_idx)
      string_of_int;
    queue_test
      "current_song_idx(prev_song (prev_song(prev_song(init_state \
       Playlist one))) = 1"
      1
      (init_state "Playlist one"
      |> prev |> prev |> prev |> current_song_idx)
      string_of_int;
    queue_test
      "current_song_idx(prev_song(prev_song(prev_song \
       (prev_song(prev_song(init_state Playlist one))))) = 3"
      3
      (init_state "Playlist one"
      |> prev |> prev |> prev |> prev |> prev |> current_song_idx)
      string_of_int;
    queue_test
      "current_song_name(prev_song(prev_song(prev_song \
       (prev_song(prev_song(init_state Playlist one))))) = 3"
      "Sample 15s"
      (init_state "Playlist one"
      |> prev |> prev |> prev |> prev |> prev |> current_song_name)
      Fun.id;
  ]

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
