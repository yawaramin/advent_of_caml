let fold_file_lines ?(test = false) name accum func =
  In_channel.with_open_bin
    (name ^ if test then ".test" else ".input")
    (In_channel.fold_lines func accum)

let print_parts p1 p2 =
  Printf.printf "\nPart 1: %d\nPart 2: %d\n" (p1 ()) (p2 ())
