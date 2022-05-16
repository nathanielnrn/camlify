type song_name = string
type playlist_name = string
type index = int

exception Empty
exception Malformed

type command = 
    | Play of song_name
    | Pause
    | Stop
    | Continue
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
    | ChangeSongLike of song_name* bool
    | ChangeSongArtist of song_name
    | ChangeSongAlbum of song_name
    | ChangeSongYear of song_name* int
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


let parse (str : string) : command =
  if String.length str = 0 then raise Empty
  else
    match String.split_on_char ' ' str with
    | [] -> raise Malformed
    | hd :: tl ->
        if String.equal hd "quit" then
          if List.length tl != 0 then raise Malformed else Quit
        else if String.equal hd "p" then
          if List.length tl == 0 then raise Malformed
          else Play (String.concat " " tl)
        else if String.equal hd "pause" then
          if List.length tl != 0 then raise Malformed else Pause
        else if String.equal hd "s" then
            if List.length tl != 0 then raise Malformed 
            else Stop
        else if String.equal hd "c" then
            if List.length tl != 0 then raise Malformed 
            else Stop

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
        else if String.equal hd "change_l" then
          if
            List.length tl < 2
            || List.length tl > 0
               && List.nth tl (List.length tl - 1) <> "true"
               && List.nth tl (List.length tl - 1) <> "false"
          then raise Malformed
          else
            let liked = List.nth tl (List.length tl - 1) in
            ChangeSongLike
              ( String.concat " " (List.filter (fun x -> x <> liked) tl),
                String.equal liked "true" )
        else if String.equal hd "change_ar" then
          if List.length tl == 0 then raise Malformed
          else ChangeSongArtist (String.concat " " tl)
        else if String.equal hd "change_al" then
          if List.length tl == 0 then raise Malformed
          else ChangeSongAlbum (String.concat " " tl)
        else if String.equal hd "change_y" then
          if List.length tl < 2 then raise Malformed
          else
            let year = List.nth tl (List.length tl - 1) in
            let year_int =
              try int_of_string year with
              | _ -> raise Malformed
            in
            ChangeSongYear
              ( String.concat " " (List.filter (fun x -> x <> year) tl),
                year_int )
        else if String.equal hd "add_tag" then
          if List.length tl == 0 then raise Malformed
          else AddSongTag (String.concat " " tl)
        else if String.equal hd "rm_tag" then
          if List.length tl == 0 then raise Malformed
          else RemoveSongTag (String.concat " " tl)
        else if String.equal hd "new_pl" then
          if List.length tl == 0 then raise Malformed
          else CreatePlayList (String.concat " " tl)
        else if String.equal hd "next" then
          if List.length tl != 0 then raise Malformed else NextSong
        else if String.equal hd "prev" then
          if List.length tl != 0 then raise Malformed else PreviousSong
        else if String.equal hd "shuffle" then
          if List.length tl != 0 then raise Malformed else Shuffle
        else if String.equal hd "add" then
          if List.length tl == 0 then raise Malformed
          else AddSong (String.concat " " tl)
        else if String.equal hd "rm" then
          if List.length tl == 0 then raise Malformed
          else RemoveSong (String.concat " " tl)
        else if String.equal hd "pls" then
          if List.length tl != 0 then raise Malformed else ViewPlaylists
        else if String.equal hd "play_artist" then
          if List.length tl != 0 then raise Malformed else PlayArtist
        else if String.equal hd "play_album" then
          if List.length tl != 0 then raise Malformed else PlayAlbum
        else if String.equal hd "play_year" then
          if List.length tl != 0 then raise Malformed else PlayYear
        else if String.equal hd "play_liked" then
          if List.length tl != 0 then raise Malformed else PlayLiked
        else if String.equal hd "play_tag" then
          if List.length tl != 0 then raise Malformed else PlayTag
        else if String.equal hd "help" then
          if List.length tl != 0 then raise Malformed else Help
        else raise Malformed