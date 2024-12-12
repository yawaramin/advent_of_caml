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

let mul_do_dont =
  Str.regexp
    {|\(mul(\([0-9]?[0-9]?[0-9]?\),\([0-9]?[0-9]?[0-9]?\))\)\|\(do()\)\|\(don't()\)|}

let rec add_muls_enabled ~pos (sum, enabled) line =
  match Str.search_forward mul_do_dont line pos with
  | pos -> (
      let pos = succ pos in
      match Str.matched_group 0 line with
      | "do()" -> add_muls_enabled ~pos (sum, true) line
      | "don't()" -> add_muls_enabled ~pos (sum, false) line
      | _ ->
          let product =
            if enabled then
              int_of_string (Str.matched_group 2 line)
              * int_of_string (Str.matched_group 3 line)
            else 0
          in
          add_muls_enabled ~pos (sum + product, enabled) line)
  | exception Not_found -> (sum, enabled)

let p2 () =
  fst (Lib.fold_file_lines ~test __FILE__ (0, true) (add_muls_enabled ~pos:0))

(* Part 1: 159833790
   Part 2: 89349241 *)
let () = Lib.print_parts p1 p2
