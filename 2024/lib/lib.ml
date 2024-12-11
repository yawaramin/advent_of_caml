let fold_file_lines ?(test = false) name accum func =
  In_channel.with_open_bin
    (name ^ if test then ".test" else ".input")
    (In_channel.fold_lines func accum)
