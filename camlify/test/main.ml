open OUnit2
open Camlify.Queue
open Camlify.Music_data

(**A general test
   case.[test expected_output called_function 
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

<<<<<<< HEAD
=======
let list_to_message_int (lst : int list) : string =
  (lst
  |> List.map (fun i -> string_of_int i)
  |> List.fold_left (fun acc s -> acc ^ " " ^ s ^ " ;") "{")
  ^ " }"

>>>>>>> 9c197c5... adds some test cases
let list_to_message (lst : string list) : string =
  (lst
  |> List.map (fun s -> String.escaped s)
  |> List.fold_left (fun acc s -> acc ^ " " ^ s ^ " ;") "{")
  ^ " }"
<<<<<<< HEAD

(* Default expectation of output is an int *)
let queue_test (name : string) expected_output called_function printer_f
    : test =
  name >:: fun _ ->
  assert_equal expected_output called_function ~printer:printer_f
=======
>>>>>>> 9c197c5... adds some test cases

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
    queue_test
      "select_playlist_by_artist \"Dua Lipa\" (init_state Playlist \
       one) = [\"Break My Heart\"]"
      [ "Break My Heart" ]
      (init_state "Playlist one"
      |> select_playlist_by_artist "Dua Lipa"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "select_playlist_by_album \"Ka\" (init_state Playlist one) = \
       [\"All Falls Down\"]"
      [ "All Falls Down" ]
      (init_state "Playlist one"
      |> select_playlist_by_album "Ka"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "select_playlist_by_liked (init_state Playlist one) = [\"All \
       Falls Down\"]"
      [
        "Break My Heart";
        "Reptilia";
        "fly me to the moon";
        "fly me to the caml";
      ]
      (init_state "Playlist one"
      |> select_playlist_by_liked |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "select_playlist_by_year 2004 (init_state Playlist one) = [\"All \
       Falls Down\"]"
      [ "All Falls Down" ]
      (init_state "Playlist one"
      |> select_playlist_by_year 2004
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "select_playlist_by_tag \"old\" (init_state Playlist one) = \
       [\"All Falls Down\"; \"Reptilia\";\"Sample 15s\";\"fly me to \
       the moon\"]"
      [
        "All Falls Down"; "Reptilia"; "Sample 15s"; "fly me to the moon";
      ]
      (init_state "Playlist one"
      |> select_playlist_by_tag "old"
      |> result_to_t |> current_playlist)
      list_to_message;
  ]

let test_select_playlist name expected_output playlist_name =
  test name expected_output
    (Camlify.Music_data.select_playlist playlist_name)

let test_list_of_playlist name list_of_playlists =
  test name list_of_playlists list_of_playlist

let test_all_songs name list_of_songs =
  test name list_of_songs all_songs

let test_read_songs_liked name liked song_name =
  test name liked (read_song_liked song_name) string_of_bool

let test_read_song_mp3_file name expected_mp3_file song_name =
  test name expected_mp3_file
    (read_song_mp3_file song_name)
    String.escaped

let test_read_song_artist name expected_artist song_name =
  test name expected_artist (read_song_artist song_name) String.escaped

let test_read_song_album name expected_album song_name =
  test name expected_album (read_song_album song_name) String.escaped

let read_song_year name expected_year song_name =
  test name expected_year (read_song_year song_name) string_of_int

let test_read_tags name expected_tags song_name =
  test name expected_tags (read_tags song_name) list_to_message

(*test cases not done*)

(*Tests music_data functions*)
let music_data_tests =
  [
    test_select_playlist "testing Playlist one song names correct"
      [ "All Falls Down"; "Break My Heart"; "Reptilia"; "Sample 15s" ]
      "Playlist one" list_to_message;
    test_list_of_playlist "list of playlist returns correctly"
      [ "Playlist zero"; "Playlist one" ]
      list_to_message;
    test_all_songs "all songs"
      [
        "All Falls Down";
        "Break My Heart";
        "Reptilia";
        "Sample 15s";
        "fly me to the moon";
        "fly me to the caml";
      ]
      list_to_message;
  ]

let suite =
  "test suite for Camlify"
  >::: List.flatten [ queue_tests; music_data_tests ]

let _ = run_test_tt_main suite
