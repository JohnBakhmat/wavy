exception Dir_not_found

let enter_dir (path : string) : unit =
  if Sys.is_directory path then Sys.chdir path else raise Dir_not_found
;;

let process_file (path : string) =
  let _ = Parser.parse_file path in
  [ path ]
;;

let rec walk_dir (path : string) : string list =
  let dir_entries = Sys.readdir path in
  let entries = Array.to_list dir_entries in
  let process_entry (entry_name : string) =
    let entry = String.concat "/" [ path; entry_name ] in
    if Sys.is_directory entry then walk_dir entry else process_file entry
  in
  List.flatten (List.map process_entry entries)
;;

let is_supported (file : string) : bool =
  let supported_extensions = [ "mp3"; "wav"; "flac" ] in
  let supported (ext : string) = String.ends_with ~suffix:ext file in
  List.fold_left ( || ) false (List.map supported supported_extensions)
;;

let main () =
  enter_dir "/Users/johnbakhmat/Downloads/Bishu - microcelebrity LP- 2024 FLAC/";
  let entries = walk_dir (Sys.getcwd()) in
  let supported_entries = List.filter is_supported entries in
  let res = String.concat "\n" supported_entries in
  print_endline res
;;
