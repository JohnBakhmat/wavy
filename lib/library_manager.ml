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
  print_endline "Starting to parse library";
  let paths = walk_dir path in
  print_endline "Got paths";
  let entries = Parser.parse_all paths in
  print_endline "Parsed";
  let successful = entries |> List.filter (fun entry -> Result.is_ok entry) in
  let unwrapped = successful |> List.map (fun entry -> Result.get_ok entry) in
  unwrapped
;;

let main () =
  (match[@warning "-8"] receive_any () with
   | Check_fs path ->
     let lib = get_library path in
     lib |> List.map (fun x -> x.filepath) |> List.fold_left ( ^ ) "" |> print_endline
   | _ -> print_endline "Huh wtf is this message?");
  shutdown ()
;;
