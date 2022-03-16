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

let parse (str : string) : command = 
    if String.length str = 0 then raise Empty else
    match (String.split_on_char ' ' str) with
    | [] -> raise Malformed
    | hd::tl -> 
        if String.equal hd "quit" then
            if List.length tl != 0 then raise Malformed 
            else Quit
        else if String.equal hd "play" then
            if List.length tl != 1 then raise Malformed
            else Play tl[0]
        else if String.equal hd "play_index" then
            if List.length tl != 1 then raise Malformed 
            else PlayIndex (int_of_string tl[0])
        else if String.equal hd "cname" then
            if List.length tl != 0 then raise Malformed 
            else CurrentSongName
        else if String.equal hd "cidx" then
            if List.length tl != 0 then raise Malformed 
            else CurrentSongIndex
        else if String.equal hd "cpl" then
            if List.length tl != 0 then raise Malformed 
            else CurrentPlayList
        else if String.equal hd "next" then
            if List.length tl != 0 then raise Malformed 
            else NextSong
        else if String.equal hd "prev" then
            if List.length tl != 0 then raise Malformed 
            else PreviousSong
        else if String.equal hd "add" then
            if List.length tl != 1 then raise Malformed 
            else AddSong
        else raise Malformed