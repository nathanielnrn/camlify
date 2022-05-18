(**Module for command types and parsing of said types*)

type song_name
type playlist_name
type index
(**Raised for empty strings*)
exception Empty
(**All other malformed commands raise this*)
exception Malformed

(**Defines command type*)
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

val parse : string -> command
(**Uses pattern matching to parse strings and convert them into commands*)