let between min num max = min <= num && num <= max

let safe_diffs diff1 diff2 =
  (between ~-3 diff1 ~-1 && between ~-3 diff2 ~-1)
  || (between 1 diff1 3 && between 1 diff2 3)

let rec safe ~dampen diff = function
  | level1 :: level2 :: tl ->
      let new_diff = level1 - level2 in
      if safe_diffs diff new_diff then safe ~dampen new_diff (level2 :: tl)
      else
        dampen
        && (safe ~dampen:false diff (level1 :: tl)
           || safe ~dampen:false new_diff (level2 :: tl))
  | _ -> true

let safe_report ~dampen = function
  | num1 :: num2 :: tl -> safe ~dampen (num1 - num2) (num2 :: tl)
  | _ -> assert false

let calc_safe ?(dampen = false) num_safe = function
  | "" -> num_safe
  | line ->
      let report = line |> String.split_on_char ' ' |> List.map int_of_string in
      num_safe + if safe_report ~dampen report then 1 else 0

let p1 () = Lib.fold_file_lines __FILE__ 0 calc_safe
let p2 () = Lib.fold_file_lines __FILE__ 0 (calc_safe ~dampen:true)

(* Part 1: 246
   Part 2: 318 *)
let () = Lib.print_parts p1 p2
