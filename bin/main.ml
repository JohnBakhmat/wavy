open Wavy

let () =
  Riot.run
  @@ fun () ->
  let open Riot in
  let lib_mngr_pid = spawn Library_manager.main in
  send lib_mngr_pid Library_manager.Check_fs;
  ()
;;
