open OUnit2
open Camlify.Queue
open Camlify.Music_data
open Camlify.Streamer
open Camlify.Command

(** Testing is largely implemented as follows: Glass box testing was
    used for the most part, largely because our test cases use dynamic
    data that we need to know the contents of in order to properly test
    for.

    This made it more difficult to use black box/property testing. To
    that end we had 2 test benches that had different data sets, and
    manually input our expected result in our tests.

    Our Music_Data, Command, and Queue module are all tested using Ounit
    in this test file, while our Main (interface) and Streamer modules
    were user tested by us. Main and Streamer could not feasibly be
    tested using Ounit (as far as we are aware) due to main being
    responsible for outputting to terminal, while Streamer largely makes
    calls to external libraries, making testing difficult. This being
    said, some helper functions in Streamer are tested, mainly as a
    result of test driven development.

    Our tests ensure that Queue and Music_Data, the modules responsible
    for manipulating and changing the state of our data and music by
    testing for edge cases, expected cases, and by specifically catching
    known bugs that arose during implementation. *)

(*TODO: Add Command tests*)

let _ = reset ()
let _ = setfile "data/interface.json"

(**A general test
   case.[test name expected_output called_function 
  printer_function]
   creates a test case named [name], and tests for [called_input] =
   [expected_output]. Turns output to string using [printer_function]*)
let test
    (name : string)
    expected_output
    called_function
    printer_function : test =
  name >:: fun _ ->
  assert_equal expected_output called_function ~printer:printer_function

let list_to_message_int (lst : int list) : string =
  (lst
  |> List.map (fun i -> string_of_int i)
  |> List.fold_left (fun acc s -> acc ^ " " ^ s ^ " ;") "{")
  ^ " }"

let list_to_message (lst : string list) : string =
  (lst
  |> List.map (fun s -> String.escaped s)
  |> List.fold_left (fun acc s -> acc ^ "\n    " ^ s ^ " ;") "{")
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
    queue_test "Playlist one\n   name is \"Playlist one\""
      "Playlist one"
      (init_state "Playlist one" |> current_playlist_name)
      Fun.id;
    queue_test "Current song of initial\n   state 0" 0
      (init_state "Playlist one" |> current_song_idx)
      string_of_int;
    queue_test "Current playlist of playlist one"
      [ "All Falls Down"; "Break My Heart"; "Reptilia"; "Sample 15s" ]
      (init_state "Playlist one" |> current_playlist)
      list_to_message;
    queue_test "Songs with artist Dua Lipa is Break My Heart"
      [ "Break My Heart" ]
      (init_state "Playlist one"
      |> select_playlist_by_artist "Dua Lipa"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test "Songs\n   in album Du is Break My Heart"
      [ "Break My Heart" ]
      (init_state "Playlist one"
      |> select_playlist_by_album "Du"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test "Songs from year 2004\n   is All Falls Down"
      [ "All Falls Down" ]
      (init_state "Playlist one"
      |> select_playlist_by_year 2004
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "Songs that are liked are Break My Heart,\n\
      \   Reptilia, fly me to  the moon, fly me to the caml"
      [
        "Break My Heart";
        "Reptilia";
        "fly me to the moon";
        "fly me to the caml";
      ]
      (init_state "Playlist one"
      |> select_playlist_by_liked |> result_to_t |> current_playlist)
      list_to_message;
    queue_test "Songs with the tag\n   banger is Reptilia"
      [ "Reptilia" ]
      (init_state "Playlist one"
      |> select_playlist_by_tag "banger"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "Songs with the tag good is Sample 15s,\n   fly me to the moon"
      [ "Sample 15s"; "fly me to the moon" ]
      (init_state "Playlist one"
      |> select_playlist_by_tag "good"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "Songs\n   with the tag good is Break My Heart, Sample 15s"
      [ "Break My Heart"; "Sample 15s" ]
      (init_state "Playlist one"
      |> select_playlist_by_tag "new"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "Songs with the tag good is All Falls Down, Reptilia, Sample  15s,\n\
      \   fly me to the moon"
      [
        "All Falls Down"; "Reptilia"; "Sample 15s"; "fly me to the moon";
      ]
      (init_state "Playlist one"
      |> select_playlist_by_tag "old"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "Songs with the tag good is All Falls\n   Down, Sample 15s"
      [ "All Falls Down"; "Sample 15s" ]
      (init_state "Playlist one"
      |> select_playlist_by_tag "rap"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "Songs with the tag\n   good is Break My Heart, Sample 15s"
      [ "Break My Heart"; "Sample 15s" ]
      (init_state "Playlist one"
      |> select_playlist_by_tag "sad"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test "Current\n   song of play by name 'Reptilia' is 2" 2
      (init_state "Playlist one"
      |> play_song_by_name "Reptilia"
      |> result_to_t |> current_song_idx)
      string_of_int;
    queue_test "Current song idx is 1" 1
      (init_state "Playlist one"
      |> play_song_by_idx 1 |> result_to_t |> current_song_idx)
      string_of_int;
    queue_test "Song id of 3 nexts is 3" 3
      (init_state "Playlist one"
      |> next |> next |> next |> current_song_idx)
      string_of_int;
    queue_test "song id loops after 4\n   nexts " 0
      (init_state "Playlist one"
      |> next |> next |> next |> next |> current_song_idx)
      string_of_int;
    queue_test "song id loops after 6\n   nexts" 2
      (init_state "Playlist one"
      |> next |> next |> next |> next |> next |> next
      |> current_song_idx)
      string_of_int;
    queue_test "Prev\n   song loops and is 1" 1
      (init_state "Playlist one"
      |> prev |> prev |> prev |> current_song_idx)
      string_of_int;
    queue_test "Prev song loops\n   3 3" 3
      (init_state "Playlist one"
      |> prev |> prev |> prev |> prev |> prev |> current_song_idx)
      string_of_int;
    queue_test "Prev loop name\n   is correct" "Sample 15s"
      (init_state "Playlist one"
      |> prev |> prev |> prev |> prev |> prev |> current_song_name)
      Fun.id;
    queue_test
      "select_playlist_by_artist\n\
      \  \"Dua Lipa\" (init_state Playlist\n\
      \   one) = [\"Break My  Heart\"]" [ "Break My Heart" ]
      (init_state "Playlist one"
      |> select_playlist_by_artist "Dua Lipa"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "select_playlist_by_liked (init_state Playlist one) = [\"All  \
       Falls\n\
      \   Down\"]"
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
      "select_playlist_by_year 2004\n\
      \ (init_state Playlist one) = [\"All Falls Down\"]"
      [ "All Falls Down" ]
      (init_state "Playlist one"
      |> select_playlist_by_year 2004
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "select_playlist_by_tag \"old\" (init_state Playlist one) =  \
       [\"All\n\
      \   Falls Down\"; \"Reptilia\";\"Sample 15s\";\"fly me to the\n\
      \ moon\"]"
      [
        "All Falls Down"; "Reptilia"; "Sample 15s"; "fly me to the moon";
      ]
      (init_state "Playlist one"
      |> select_playlist_by_tag "old"
      |> result_to_t |> current_playlist)
      list_to_message;
    queue_test
      "select_playlist_by_album \"Ka\" (init_state Playlist one) =  \
       [\"All\n\
      \   Falls Down\"]" [ "All Falls Down" ]
      (init_state "Playlist one"
      |> select_playlist_by_album "Ka"
      |> result_to_t |> current_playlist)
      list_to_message;
  ]

(* test exceptions *)
let test_raise_exception name f expected_exception =
  name >:: fun _ -> assert_raises expected_exception f

let test_select_playlist name expected_output playlist_name =
  test name expected_output (select_playlist playlist_name)

let test_list_of_playlist name list_of_playlists =
  test name list_of_playlists (list_of_playlist ())

let test_all_songs name list_of_songs =
  test name list_of_songs (all_songs ())

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

let test_read_song_year name expected_year song_name =
  test name expected_year (read_song_year song_name) string_of_int

let test_read_tags name expected_tags song_name =
  test name expected_tags (read_tags song_name) list_to_message

(*test cases not done*)
(*Tests music_data functions*)

(**test "dynamic dir loading" [ "" ] (get_dir_songs ()) list_to_message;*)
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
    test_read_song_artist "All Falls Down" "Kanye" "All Falls Down";
    test_read_song_artist "Break My Heart" "Dua Lipa" "Break My Heart";
    test_read_song_artist "Sample 15s" "" "Sample 15s";
    test_read_song_album "Sample 15s" "" "Sample 15s";
    test_read_song_album "All Falls Down" "Ka" "All Falls Down";
    test_read_song_album "Break My Heart" "Du" "Break My Heart";
    test_read_songs_liked "Break My Heart" true "Break My Heart";
    test_read_songs_liked "Sample 15s" false "Sample 15s";
    test_read_song_year "Break My Heart" 2020 "Break My Heart";
    test_read_song_year "Sample 15s" 0 "Sample 15s";
    test_read_tags "All Falls Down" [ "old"; "rap" ] "All Falls Down";
    test_read_tags "Break My Heart"
      [ "new"; "sad"; ""; {|["great"]|} ]
      "Break My Heart";
    test_read_tags "Fly me to the caml" [] "fly me to the caml";
    test_read_song_mp3_file "Reptilia" "reptilia.mp3" "Reptilia";
    test_read_song_mp3_file "Break My Heart" "break_my_heart.mp3"
      "Break My Heart";
    test_raise_exception "unknown playlist from select_playlist"
      (fun () -> select_playlist "non_existent_playlist")
      (UnknownPlaylist "non_existent_playlist");
    test_raise_exception "read song liked"
      (fun () -> read_song_liked "non_existent_liked_song")
      (UnknownSong "non_existent_liked_song");
    test_raise_exception "unknown song mp3"
      (fun () -> read_song_mp3_file "non_existent_mp3_song")
      (UnknownSong "non_existent_mp3_song");
    test_raise_exception "unknown song artist"
      (fun () -> read_song_artist "non_existent_artist_song")
      (UnknownSong "non_existent_artist_song");
    test_raise_exception "unknown song album"
      (fun () -> read_song_album "non_existent_album_song")
      (UnknownSong "non_existent_album_song");
    test_raise_exception "unknown song year"
      (fun () -> read_song_year "non_existent_year_song")
      (UnknownSong "non_existent_year_song");
    test_raise_exception "unknown song tags"
      (fun () -> read_tags "non_existent_tags_song")
      (UnknownSong "non_existent_tags_song");
  ]

let streamer_tests =
  [
    test "data dir uri"
      "/home/navarro/cs3110/camlify/_build/default/camlify/data/"
      data_dir_uri Fun.id;
    test "get_pipeline" true
      (Option.is_none !get_pipeline)
      string_of_bool;
    test_raise_exception "pause fails correctly"
      (fun () -> pause get_pipeline)
      (Failure "Should not be called before play");
    test "stop works" true
      (stop get_pipeline;
       Option.is_none !get_pipeline)
      string_of_bool;
  ]

let test_command
    (command_string : string)
    (command : Camlify.Command.command) : test =
  "parsing " ^ command_string >:: fun _ ->
  assert_equal command (parse command_string)

let test_bad_command (command_string : string) : test =
  "bad command " ^ command_string >:: fun _ ->
  assert_raises Malformed (fun _ -> parse command_string)

let test_empty_command (command_string : string) : test =
  "bad command " ^ command_string >:: fun _ ->
  assert_raises Empty (fun _ -> parse command_string)

let command_tests =
  [
    test_empty_command "";
    test_bad_command "a";
    test_bad_command "1";
    test_bad_command "!#@";
    test_command "quit" Quit;
    test_bad_command "quit All Falls Down";
    test_command "p All Falls Down" (Play "All Falls Down");
    test_bad_command "p";
    test_command "pause" Pause;
    test_bad_command "pause song name";
    test_command "s" Stop;
    test_bad_command "s song name";
    test_command "pi 7" (PlayIndex 7);
    test_bad_command "pi huh";
    test_bad_command "pi";
    test_command "name" CurrentSongName;
    test_bad_command "name     ";
    test_bad_command "name skk 77";
    test_bad_command "name   s   ";
    test_command "index" CurrentSongIndex;
    test_bad_command "index 4";
    test_command "pl" CurrentPlayList;
    test_bad_command "pl new playlist";
    test_bad_command "pl 3";
    test_command "change_pl playist 2" (ChangePlayList "playist 2");
    test_bad_command "change_pl";
    test_command "change_ar 22 t" (ChangeSongArtist "22 t");
    test_bad_command "change_ar";
    test_command "change_al qwerty" (ChangeSongAlbum "qwerty");
    test_bad_command "change_al";
    test_command "change_y name 2001" (ChangeSongYear ("name", 2001));
    test_bad_command "change_y";
    test_bad_command "change_y name s";
    test_command "add_tag happy" (AddSongTag "happy");
    test_command "add_tag   " (AddSongTag "  ");
    test_bad_command "add_tag";
    test_command "rm_tag songg" (RemoveSongTag "songg");
    test_command "rm_tag    " (RemoveSongTag "   ");
    test_bad_command "rm_tag";
    test_command "new_pl playlisttwo" (CreatePlayList "playlisttwo");
    test_bad_command "new_pl";
    test_command "next" NextSong;
    test_bad_command "next All Falls";
    test_command "prev" PreviousSong;
    test_bad_command "prev All Falls";
    test_command "shuffle" Shuffle;
    test_bad_command "shuffle songg";
    test_command "add sample" (AddSong "sample");
    test_command "add sample 3" (AddSong "sample 3");
    test_bad_command "add";
    test_command "rm sample" (RemoveSong "sample");
    test_bad_command "rm";
    test_command "pls" ViewPlaylists;
    test_bad_command "pls song1";
    test_command "play_artist" PlayArtist;
    test_bad_command "play_artist art";
    test_command "play_album" PlayAlbum;
    test_bad_command "play_album al";
    test_command "play_year" PlayYear;
    test_bad_command "play_year 2111";
    test_command "play_liked" PlayLiked;
    test_bad_command "play_liked liked";
    test_command "play_tag" PlayTag;
    test_bad_command "play_tag sad";
    test_command "help" Help;
    test_bad_command "help ewfd";
    test_command "new_playlist hi" (NewPlaylist "hi");
    test_bad_command "new_playlist";
    test_command "new_playlist multiple words"
      (NewPlaylist "multiple words");
  ]

(**[test_change_read name writer song_name value reader output printer]
   is a test named [name], calls [writer song_name value], and then
   checks for [reader song_name] = [output]. prints error message with
   the printer*)
let test_change_read name writer song_name value reader output printer =
  test name output
    (writer song_name value;
     reader song_name)
    printer

let music_data__writer_tests =
  setfile "data/writable.json";
  let tlst =
    [
      test "adding fly me to the moon to Playlist one"
        [
          "All Falls Down";
          "Break My Heart";
          "Reptilia";
          "Sample 15s";
          "fly me to the caml";
        ]
        (add_song_to_playlist "Playlist one" "fly me to the caml";
         select_playlist "Playlist one")
        list_to_message;
      test "adding playlist hi"
        [ "Playlist zero"; "Playlist one"; "hi" ]
        (add_playlist "hi";
         list_of_playlist ())
        list_to_message;
      test "deleting All falls down from playlist zero"
        (delete_song_from_playlist "Playlist zero" "All Falls Down";
         select_playlist "Playlist zero")
        [] list_to_message;
      test "deleting Reptilia from playlist zero"
        (delete_song_from_playlist "Playlist zero" "Reptilia";
         select_playlist "Playlist zero")
        [ "All Falls Down" ] list_to_message;
      test "deleting Break My Heart from playlist zero"
        (delete_song_from_playlist "Playlist zero" "Break My Heart";
         select_playlist "Playlist zero")
        [ "All Falls Down"; "Reptilia" ]
        list_to_message;
      test_change_read "Liking all falls down" change_song_liked
        "All Falls Down" true read_song_liked true string_of_bool;
      test_change_read "Disliking all falls down" change_song_liked
        "All Falls Down" false read_song_liked false string_of_bool;
      test_change_read "Diego wrote Break My Heart" change_song_artist
        "Break My Heart" "Diego" read_song_artist "Diego" String.escaped;
      test_change_read "Nathaniel wrote Reptilia" change_song_artist
        "Reptilia" "Nathaniel" read_song_artist "Nathaniel"
        String.escaped;
      test_change_read "Change All Falls Down album to Super Bangers"
        change_song_album "All Falls Down" "Super Bangers"
        read_song_album "Super Bangers" String.escaped;
      test_change_read
        "Change fly me to the moon album to Super Duper Bangers"
        change_song_album "fly me to the moon" "Super Duper Bangers"
        read_song_album "Super Duper Bangers" String.escaped;
      test_change_read "Making fly me to the moon very very old"
        change_song_year "fly me to the moon" 42 read_song_year 42
        string_of_int;
      test_change_read "Making fly me to the caml in the future"
        change_song_year "fly me to the caml" 3022 read_song_year 3022
        string_of_int;
      test_change_read "adding tag to fly me to the moon" add_song_tag
        "fly me to the moon" "jazzy" read_tags [ "good"; "jazzy" ]
        list_to_message;
      test_change_read "Liking all falls down" remove_song_tag
        "fly me to the moon" "old" read_tags [ "good" ] list_to_message;
    ]
  in
  setfile "data/interface.json";
  tlst

let suite =
  "test suite for Camlify"
  >::: List.flatten
         [
           queue_tests;
           music_data_tests;
           streamer_tests;
           command_tests;
           music_data__writer_tests;
         ]

let _ = run_test_tt_main suite
