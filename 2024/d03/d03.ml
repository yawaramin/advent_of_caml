let test = false
let mul_r = Str.regexp {|mul(\([0-9]?[0-9]?[0-9]?\),\([0-9]?[0-9]?[0-9]?\))|}

let rec add_muls_to ?(pos = 0) sum line =
  match Str.search_forward mul_r line pos with
  | pos ->
      let product =
        int_of_string (Str.matched_group 1 line)
        * int_of_string (Str.matched_group 2 line)
      in
      add_muls_to ~pos:(succ pos) (sum + product) line
  | exception Not_found -> sum

let p1 () = Lib.fold_file_lines ~test __FILE__ 0 add_muls_to

(* 159833790 *)
let () = print_int (p1 ())
