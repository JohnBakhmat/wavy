let ( let* ) x f = Result.bind x f

open Riot
open Riot.Task

type music_info =
  { filepath : string
  ; artist : string option
  ; album : string option
  ; title : string option
  }

let cons_uniq xs x = if List.mem x xs then xs else x :: xs
let deduplicate_left xs = List.rev (List.fold_left cons_uniq [] xs)

let parse_file (filepath : string)
  : (music_info, [> `File_not_found | `Not_a_file ]) result
  =
  let* _ = if Sys.file_exists filepath then Ok () else Error `File_not_found in
  let* _ = if Sys.is_regular_file filepath then Ok () else Error `Not_a_file in
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

let parse_all (paths : string list) =
  let tasks =
    paths
    |> List.map (fun path ->
      async (fun () ->
        Logger.info (fun f -> f "Starting to parse %s" path);
        parse_file path))
  in
  let task_results =
    tasks
    |> List.mapi (fun i task ->
      let res = await ~timeout:100_000L task in
      match res with
      | Ok x ->
        Logger.debug (fun f ->
          f "Successfully parsed %s" ((Result.get_ok x).title |> Option.value ~default:""));
        Ok x
      | Error `Timeout ->
        Logger.debug (fun f -> f "Request timed out %d" i);
        Error `Timeout
      | x ->
        Logger.debug (fun f -> f "Unexpected error");
        x)
  in
  let results =
    task_results
    |> List.map (fun res ->
      match res with
      | Ok music_res -> music_res
      | Error x -> Error x)
  in
  results
;;
