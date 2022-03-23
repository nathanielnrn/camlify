print_endline "here at least4";

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

let parse (str : string) : command = 
    if String.length str = 0 then raise Empty else
    match (String.split_on_char ' ' str) with
    | [] -> raise Malformed
    | hd :: tl -> 
        if String.equal hd "quit" then
            if List.length tl != 0 then raise Malformed 
            else Quit
        else if String.equal hd "p" then
            if List.length tl == 0 then raise Malformed
            else (Play (String.concat " " tl))
        else if String.equal hd "pi" then
            if List.length tl != 1 then raise Malformed 
            else PlayIndex (int_of_string (List.hd tl))
        else if String.equal hd "name" then
            if List.length tl != 0 then raise Malformed 
            else CurrentSongName
        else if String.equal hd "index" then
            if List.length tl != 0 then raise Malformed 
            else CurrentSongIndex
        else if String.equal hd "pl" then
            if List.length tl != 0 then raise Malformed 
            else CurrentPlayList
        else if String.equal hd "change_pl" then
            if List.length tl == 0 then raise Malformed 
            else ChangePlayList (String.concat " " tl)
        else if String.equal hd "new_pl" then
            if List.length tl == 0 then raise Malformed 
            else CreatePlayList (String.concat " " tl)

        else if String.equal hd "next" then
            if List.length tl != 0 then raise Malformed 
            else NextSong
        else if String.equal hd "prev" then
            if List.length tl != 0 then raise Malformed 
            else PreviousSong
        else if String.equal hd "add" then
            if List.length tl == 0 then raise Malformed 
            else (AddSong (String.concat " " tl))
        else if String.equal hd "rm" then
            if List.length tl == 0 then raise Malformed 
            else (RemoveSong (String.concat " " tl))
        else if String.equal hd "pls" then
            if List.length tl != 0 then raise Malformed 
            else ViewPlaylists
        else if String.equal hd "help" then
            if List.length tl != 0 then raise Malformed 
            else Help
        else raise Malformed