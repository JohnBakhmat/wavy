exception Dir_not_found

open Riot
open Parser

type Message.t += Check_fs of string

let enter_dir (path : string) : unit =
  if Sys.is_directory path then Sys.chdir path else raise Dir_not_found
;;

let rec walk_dir (path : string) : string list =
  let dir_entries = Sys.readdir path in
  let entries = Array.to_list dir_entries in
  let process_entry (entry_name : string) =
    let entry = String.concat "/" [ path; entry_name ] in
    if Sys.is_directory entry then walk_dir entry else [ entry ]
  in
  List.flatten (List.map process_entry entries)
;;

let is_supported (file : string) : bool =
  let supported_extensions = [ "mp3"; "wav"; "flac" ] in
  let supported (ext : string) = String.ends_with ~suffix:ext file in
  List.fold_left ( || ) false (List.map supported supported_extensions)
;;

let get_library (path : string) =
  Logger.debug (fun f -> f "Starting to parse library");
  let paths = walk_dir path in
  Logger.debug (fun f -> f "Got paths");
  let entries = Parser.parse_all paths in
  Logger.debug (fun f -> f "Parsed");
  let successful = entries |> List.filter (fun entry -> Result.is_ok entry) in
  let unwrapped = successful |> List.map (fun entry -> Result.get_ok entry) in
  unwrapped
;;

let main () =
  (match[@warning "-8"] receive_any () with
   | Check_fs path ->
     let lib = get_library path in
     let str = lib |> List.map (fun x -> x.filepath) |> List.fold_left ( ^ ) "" in
     Logger.debug (fun f -> f "Library: %s" str);
     ()
   | _ -> Logger.debug (fun f -> f "Huh wtf is this message?"));
  shutdown ()
;;
