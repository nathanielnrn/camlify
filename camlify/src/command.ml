type song_name = string
type playlist_name = string
type index = int

exception Empty
exception Malformed

type command =
  | Play of song_name
  | Pause
  | Stop
  | PlayIndex of index
  | CurrentSongName
  | CurrentSongIndex
  | ViewPlaylists
  | NewPlaylist of playlist_name
  | CurrentPlayList
  | ChangePlayList of playlist_name
  | CreatePlayList of playlist_name
  | NextSong
  | PreviousSong
  | Shuffle
  | AddSong of song_name
  | RemoveSong of song_name
  | ChangeSongLike of song_name
  | ChangeSongArtist of song_name
  | ChangeSongAlbum of song_name
  | ChangeSongYear of song_name * int
  | AddSongTag of song_name
  | RemoveSongTag of song_name
  | PlayArtist
  | PlayAlbum
  | PlayYear
  | PlayLiked
  | PlayTag
  | Help
  | Quit
  | Idle

let make_play_index i =
  try PlayIndex (int_of_string i) with
  | Failure _ -> raise Malformed

let make_change_song_year songn y =
  try ChangeSongYear (String.concat " " [ songn ], int_of_string y) with
  | Failure _ -> raise Malformed

(*Uses pattern matching versus else if trees to continue implementing
  compare to [parse] below and match correct output with match*)
let parse (str : string) : command =
  if str = "" then raise Empty
  else
    match String.split_on_char ' ' str with
    | [] -> raise Malformed
    | h :: t -> begin
        match (h, t) with
        | "quit", [] -> Quit
        | "p", _ :: _ -> Play (String.concat " " t)
        | "pause", [] -> Pause
        | "s", [] -> Stop
        | "pi", [ i ] -> make_play_index i
        | "name", [] -> CurrentSongName
        | "index", [] -> CurrentSongIndex
        | "pl", [] -> CurrentPlayList
        | "change_pl", _ :: _ -> ChangePlayList (String.concat " " t)
        | "change_l", _ :: _ -> ChangeSongLike (String.concat " " t)
        | "change_ar", _ :: _ -> ChangeSongArtist (String.concat " " t)
        | "change_al", _ :: _ -> ChangeSongAlbum (String.concat " " t)
        | "change_y", songn :: [ y ] -> make_change_song_year songn y
        | "add_tag", _ :: _ -> AddSongTag (String.concat " " t)
        | "rm_tag", _ :: _ -> RemoveSongTag (String.concat " " t)
        | "new_pl", _ :: _ -> CreatePlayList (String.concat " " t)
        | "next", [] -> NextSong
        | "prev", [] -> PreviousSong
        | "shuffle", [] -> Shuffle
        | "add", _ :: _ -> AddSong (String.concat " " t)
        | "rm", _ :: _ -> RemoveSong (String.concat " " t)
        | "pls", [] -> ViewPlaylists
        | "play_artist", [] -> PlayArtist
        | "play_album", [] -> PlayAlbum
        | "play_year", [] -> PlayYear
        | "play_liked", [] -> PlayLiked
        | "play_tag", [] -> PlayTag
        | "help", [] -> Help
        | "new_playlist", _ :: _ -> NewPlaylist (String.concat " " t)
        | _ -> raise Malformed
      end