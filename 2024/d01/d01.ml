let data_txt = "./2024/d01/data.txt"

let add_to_lists ((list1, list2) as lists) = function
  | "" -> lists
  | line ->
      Scanf.sscanf line "%d %d" (fun num1 num2 ->
          (num1 :: list1, num2 :: list2))

let p1 () =
  let list1, list2 =
    In_channel.with_open_bin data_txt
      (In_channel.fold_lines add_to_lists ([], []))
  in
  let list1 = List.sort Int.compare list1
  and list2 = List.sort Int.compare list2 in
  list2
  |> List.map2 (fun num1 num2 -> abs (num1 - num2)) list1
  |> List.fold_left ( + ) 0

(* Part 1: 1388114 *)
let () = Printf.printf "Part 1: %d\n" (p1 ())

module IntMap = Map.Make (Int)

let incr_count num =
  IntMap.update num (function Some n -> Some (succ n) | None -> Some 1)

let add_to_maps ((map1, map2) as maps) = function
  | "" -> maps
  | line ->
      Scanf.sscanf line "%d %d" (fun num1 num2 ->
          (incr_count num1 map1, incr_count num2 map2))

let p2 () =
  let map1, map2 =
    In_channel.with_open_bin data_txt
      (In_channel.fold_lines add_to_maps (IntMap.empty, IntMap.empty))
  in
  let calc_similarity left_num left_count similarity =
    let right_count =
      map2 |> IntMap.find_opt left_num |> Option.value ~default:0
    in
    similarity + (left_count * left_num * right_count)
  in
  IntMap.fold calc_similarity map1 0

(* 23529853 *)
let () = Printf.printf "Part 2: %d\n" (p2 ())
