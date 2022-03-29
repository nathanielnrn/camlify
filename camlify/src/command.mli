type song_name = string

type playlist_name = string

type index = int

exception Empty
exception Malformed

type command = 
    | Play of song_name
    | PlayIndex of index
    | CurrentSongName
    | CurrentSongIndex

    | ViewPlaylists
    | CurrentPlayList
    | ChangePlayList of playlist_name
    | CreatePlayList of playlist_name
    | NextSong
    | PreviousSong
    | AddSong of song_name
    | RemoveSong of song_name
    | Help
    | Quit
    | Idle

val parse : string -> command
