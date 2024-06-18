exception Dir_not_found

let enter_dir(path:string) = 
        match Sys.is_directory path with
        | false -> raise Dir_not_found;
        | true -> Sys.chdir path;
;;
                
let main () = 
        print_endline (Sys.getcwd());
        enter_dir "../";
        print_endline (Sys.getcwd());
;;
