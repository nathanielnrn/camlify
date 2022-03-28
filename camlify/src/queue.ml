type t =
{
  current_song_name : string;
  current_song_mp3_file : string;
  current_song_idx : int;
  current_playlist_name : string;
  current_playlist : string list;
  list_of_playlist : string list;
  list_of_all_songs : string list;
}

let rec remove_element element lst = 
  match lst with
  |[] -> []
  |h::t -> if h = element then remove_element element t else h::remove_element element t


let init_state selected_playlist=
  {
    current_song_name = List.nth (Music_data.select_playlist selected_playlist) 0;
    current_song_mp3_file = Music_data.read_song_mp3_file (List.nth (Music_data.select_playlist selected_playlist) 0);
    current_song_idx = 0;
    current_playlist_name = selected_playlist;
    current_playlist = Music_data.select_playlist selected_playlist;
    list_of_playlist = Music_data.list_of_playlist;
    list_of_all_songs = Music_data.all_songs;
  }

let current_song_name st = st.current_song_name

let current_song_idx st = st.current_song_idx

let current_playlist st = st.current_playlist

type result = 
|Legal of t
|Illegal

let remove_dup lst = List.sort_uniq compare lst

let rec find_idx e lst = match lst with
|[]-> 0
|h::t -> if h=e then 0 else 1+find_idx e t

let play_song_by_name song_name st =  
  try
    List.find (fun x -> x = song_name) (st.current_playlist)
    |> fun x ->
      Legal{
        current_song_name = song_name;
        current_song_mp3_file = Music_data.read_song_mp3_file song_name;
        current_song_idx = find_idx song_name st.current_playlist;
        current_playlist_name = st.current_playlist_name;
        current_playlist = st.current_playlist;
        list_of_playlist = st.list_of_playlist;
        list_of_all_songs = st.list_of_all_songs
      }
    with Not_found -> Illegal

let play_song_by_idx idx st = 
  try 
  List.nth st.current_playlist idx |> fun x ->
    Legal{
      current_song_name = x;
      current_song_mp3_file = Music_data.read_song_mp3_file x;
      current_song_idx = idx;
      current_playlist_name = st.current_playlist_name;
      current_playlist = st.current_playlist;
      list_of_playlist = st.list_of_playlist;
      list_of_all_songs = st.list_of_all_songs
    }
  with Failure x -> Illegal


let next_song st =  if play_song_by_idx (current_song_idx st +1) st = Illegal
  then play_song_by_idx 0 st
else play_song_by_idx (current_song_idx st +1) st

let prev_song st = if play_song_by_idx (current_song_idx st -1) st = Illegal
  then play_song_by_idx (List.length st.current_playlist -1) st
else play_song_by_idx (current_song_idx st -1) st

let random_song st = play_song_by_idx (Random.int (List.length st.current_playlist)) st

let add_song_to_playlist song_name st = 
    try 
    List.find (fun x -> x = song_name) (st.list_of_all_songs)
    |> fun x -> Legal {
    current_song_name = st.current_song_name;
    current_song_mp3_file = Music_data.read_song_mp3_file st.current_song_name;
    current_song_idx = st.current_song_idx;
    current_playlist_name = st.current_playlist_name;
    current_playlist = st.current_playlist @ [song_name];
    list_of_playlist = st.list_of_playlist;
    list_of_all_songs = st.list_of_all_songs;
  }
with Failure x -> Illegal

let remove_song_from_playlist song_name st = 
  try
    List.find (fun x -> x = song_name) (st.current_playlist)
    |> fun x -> Legal{
      current_song_name = st.current_song_name;
      current_song_mp3_file = Music_data.read_song_mp3_file st.current_song_name;
      current_song_idx = st.current_song_idx;
      current_playlist_name = st.current_playlist_name;
      current_playlist = remove_element song_name st.current_playlist;
      list_of_playlist = st.list_of_playlist;
      list_of_all_songs = st.list_of_all_songs;
    }
  with Failure x -> Illegal

let make_new_playlist playlist_name st =
  try
    if List.exists(fun x -> x = playlist_name) (st.list_of_playlist) then
      Illegal else Legal {current_song_name = st.current_song_name;
      current_song_mp3_file = Music_data.read_song_mp3_file st.current_song_name;
      current_song_idx = st.current_song_idx;
      current_playlist_name = playlist_name;
      current_playlist = [];
      list_of_playlist = st.list_of_playlist @ [playlist_name];
      list_of_all_songs = st.list_of_all_songs}
  with 
  |Not_found -> Legal {
      current_song_name = st.current_song_name;
      current_song_mp3_file = Music_data.read_song_mp3_file st.current_song_name;
      current_song_idx = st.current_song_idx;
      current_playlist_name = playlist_name;
      current_playlist = [];
      list_of_playlist = st.list_of_playlist @ [playlist_name];
      list_of_all_songs = st.list_of_all_songs
  }

let select_playlist playlist_name st =
  try List.find (fun x -> x = playlist_name) (st.list_of_playlist)
  |> fun x -> Legal{
    current_song_name = List.nth (Music_data.select_playlist playlist_name) 0;
    current_song_mp3_file = Music_data.read_song_mp3_file st.current_song_name;
    current_song_idx = 0;
    current_playlist_name = playlist_name;
    current_playlist = Music_data.select_playlist playlist_name;
    list_of_playlist = st.list_of_playlist;
    list_of_all_songs = st.list_of_all_songs;
  }
with Failure x -> Illegal

let delete_playlist playlist_name st =
  try List.find (fun x -> x = playlist_name) (st.list_of_playlist)
  |> fun x -> Legal{
    current_song_name = st.current_song_name;
    current_song_mp3_file = Music_data.read_song_mp3_file st.current_song_name;
    current_song_idx = st.current_song_idx;
    current_playlist_name = st.current_playlist_name;
    current_playlist = st.current_playlist;
    list_of_playlist = remove_element x st.list_of_playlist;
    list_of_all_songs = st.list_of_all_songs;
  }
with Failure x -> Illegal

let add_new_song song_name st =
  try List.find (fun x -> x = song_name) (st.list_of_all_songs)
  |> fun x -> Illegal
with Failure x -> Legal{
  current_song_name = st.current_song_name;
  current_song_mp3_file = Music_data.read_song_mp3_file st.current_song_name;
    current_song_idx = st.current_song_idx;
    current_playlist_name = st.current_playlist_name;
    current_playlist = st.current_playlist;
    list_of_playlist = st.list_of_playlist;
    list_of_all_songs = st.list_of_all_songs @ [song_name];
}

let remove_song_from_all_songs song_name st =
  try List.find (fun x -> x = song_name) (st.list_of_all_songs)
  |> fun x -> Legal{
  current_song_name = st.current_song_name;
  current_song_mp3_file = Music_data.read_song_mp3_file st.current_song_name;
    current_song_idx = st.current_song_idx;
    current_playlist_name = st.current_playlist_name;
    current_playlist = st.current_playlist;
    list_of_playlist = st.list_of_playlist;
    list_of_all_songs = remove_element x st.list_of_all_songs;
}
with Failure x -> Illegal

