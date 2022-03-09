type t = {
  current_song_id : string;
  current_song_idx : int;
  played_songs : string list;
  following_songs: string list;
  playlist : string list;
}

let init_state music_data=
  {
    current_song_id = MusicData.initial_song music_data;
    current_song_idx = 0;
    played_songs =  [];
    following_songs = MusicData.playlist;
    playlist = MusicData.playlist;
  }

let current_song_id st = st.current_song_id
let played_songs st = st.played_songs

type result = 
|Legal of t
|Illegal

let remove_dup lst = List.sort_uniq compare lst


let play_song_by_name song_id music_data st =  
  try
    List.find (fun x -> x = song_id) (MusicData.songs music_data st.current_song_id)
    |> fun x ->
      Legal{
        current_song_id = song_id;
        current_song_idx = MusicData.song_idx music_data current_song_id x;
        played_songs = List.cons
        (MusicData.next_song music_data current_song_id x)
        (played_songs st) |> remove_dup;
        following_songs = st.following_songs;
        playlist = st.playlist;
      }
    with Not_found -> Illegal

let play_song_by_idx idx music_data st = 
  try 
  List.nth (MusicData.songs music_data) idx |> fun x ->
    Legal{
      current_song_id = x;
      current_song_idx = idx;
      played_songs = List.cons
      (MusicData.next_song music_data current_song_id x)
      (played_songs st) |> remove_dup;
      following_songs = st.following_songs;
      playlist = st.playlist;
    }
  with Failure x -> Illegal

let add_song_to_playlist song music_data st = 
    try 
    List.find (fun x -> x = song.song_id) (MusicData.songs music_data st.current_song_id)
    |> fun x -> Legal st
with Not_found -> Legal {
    current_song_id = st.current_song_id;
    current_song_idx = st.current_song_idx;
    played_songs = st.played_songs;
    following_songs = st.following_songs :: [song.song_id];
    playlist = st.playlist :: [song.song_id];

}

let remove_song_from_playlist song 

let next_song music_data st =  play_song_by_idx (st.current_song_idx+1) music_data st

let back music_data st = play_song_by_idx (st.current_song_idx-1) music_data st