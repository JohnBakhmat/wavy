let ( let* ) x f = Result.bind x f

open Riot

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

type Message.t += Worker_shutdown
type Message.t += Worker_parse of string
type Message.t += Parse_res of (music_info, string) result

let rec parse_worker caller =
  (match receive_any () with
   | Worker_shutdown -> shutdown ()
   | Worker_parse path ->
     Format.printf "Worker %a starting to parse %s\n" Pid.pp (self ()) path;
     let result = parse_file path in
     let title = Option.value (Result.get_ok result).title ~default:"" in
     Format.printf "Worker %a has parsed %s\n" Pid.pp (self ()) title;
     send caller (Parse_res result)
   | _ -> print_endline "Got something not sure what");
  parse_worker caller
;;

let parse_all (paths : string list) =
  let this = self () in
  let workers = List.init 3 (fun _ -> spawn (fun _ -> parse_worker this)) in
  paths
  |> List.iteri
     @@ fun id path ->
     let worker = List.nth workers (id mod 3) in
     send worker (Worker_parse path);
     match receive_any () with
     | Parse_res res ->
       (match res with
        | Ok music -> print_endline (Option.value music.title ~default:"")
        | Error err -> print_endline err)
     | _ -> print_endline "parse_all Unexpected message"
;;
