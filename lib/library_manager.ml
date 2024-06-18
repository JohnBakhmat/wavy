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
                
let main () = 
        let dir = Sys.getcwd() in
        let entries = walk_dir dir in
        let res = String.concat "\n" entries in
        print_endline res;
;;
