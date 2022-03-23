type song_name = string
type index = int

exception Empty
exception Malformed

type command = 
    | Play of song_name
    | PlayIndex of index
    | CurrentSongName
    | CurrentSongIndex
    | CurrentPlayList
    | NextSong
    | PreviousSong
    | AddSong
    | Quit
    | Idle

val parse : string -> command
