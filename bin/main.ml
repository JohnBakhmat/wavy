open Wavy

let () =
  Riot.run
  @@ fun () ->
  let open Riot in

  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);

  let lib_mngr_pid = spawn Library_manager.main in
  send
    lib_mngr_pid
    (Library_manager.Check_fs
       "/Users/johnbakhmat/Downloads/Bishu - microcelebrity LP- 2024 FLAC/");
  wait_pids [ lib_mngr_pid ];
  ()
;;
