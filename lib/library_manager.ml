exception Dir_not_found

let enter_dir(path:string):unit = 
        match Sys.is_directory path with
        | false -> raise Dir_not_found;
        | true -> Sys.chdir path;
;;

let process_file (path:string)=
        [path]
;;


let rec walk_dir (path:string):string list = 
        let dir_entries = Sys.readdir(path) in
        let entries = Array.to_list(dir_entries) in
        let process_entry (entry_name:string) = 
                let entry = String.concat "/" [path;entry_name] in
                if Sys.is_directory entry then walk_dir(entry) else process_file(entry) 
        in
        List.flatten (List.map process_entry entries)
;;

let is_supported (file:string):bool =
        let supported_extensions = ["mp3";"wav";"flac"] in
        let supported (ext:string) =  String.ends_with ~suffix:ext file in
        List.fold_left (||) false (List.map supported supported_extensions)
;;
                
let main () = 
        let entries = walk_dir (Sys.getcwd ()) in
        let supported_entries = List.filter is_supported entries in
        let res = String.concat "\n" supported_entries in
        print_endline res;
;;
