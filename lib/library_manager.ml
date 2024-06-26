(*open Parser*)

exception Dir_not_found

open Riot

type Message.t += Check_fs

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

let parse_all (_files : string list) =
  let _pid = spawn (fun () -> print_endline "Sup, Meg!") in
  ()
;;

let rec main () =
  (match[@warning "-8"] receive_any () with
   | Check_fs -> print_endline "Starting filesystem walk"
   | _ -> print_endline "Huh wtf is this message?");
  main ()
;;
