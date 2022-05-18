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

val parse' : string -> command
(**Uses pattern matching vs else if trees to continue implementing
   compare to [parse] below and match correct output with match*)