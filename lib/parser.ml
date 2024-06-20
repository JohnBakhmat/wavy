let ( let* ) x f = Result.bind x f

type music_info =
  { filepath : string
  ; artist : string option
  ; album : string option
  ; title : string option
  }

let cons_uniq xs x = if List.mem x xs then xs else x :: xs
let deduplicate_left xs = List.rev (List.fold_left cons_uniq [] xs)

let parse_file (filepath : string) : (music_info, string) result =
  let* _ = if Sys.file_exists filepath then Ok () else Error "File not found" in
  let* _ = if Sys.is_regular_file filepath then Ok () else Error "Its not a file" in
  let info =
    filepath
    |> Metadata.parse_file
    |> List.filter_map (fun (k, v) ->
      match k with
      | "title" | "artist" | "album" -> Some (k, v)
      | _ -> None)
    |> deduplicate_left
    |> List.fold_left
         (fun r (k, v) ->
           match k with
           | "album" -> { r with album = Some v }
           | "title" -> { r with title = Some v }
           | "artist" -> { r with artist = Some v }
           | _ -> r)
         { album = None; artist = None; title = None; filepath }
  in
  Ok info
;;
