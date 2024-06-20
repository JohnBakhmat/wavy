type music_info = {
  filepath : string;
  artist : string option;
  album : string option;
  title : string option;
}

val parse_file : string -> (music_info, string) result

