exception Dir_not_found

open Riot

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
  let paths = walk_dir path in
  let audio = Parser.parse_all paths in
  audio
;;

let main () =
  (match[@warning "-8"] receive_any () with
   | Check_fs path -> get_library path
   | _ -> print_endline "Huh wtf is this message?");
  shutdown()
