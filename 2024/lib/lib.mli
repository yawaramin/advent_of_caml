val fold_file_lines : ?test:bool -> string -> 'a -> ('a -> string -> 'a) -> 'a
(** [fold_file_lines ?test name accum func] is the result of folding over the
    lines of the given input file with the folding function [func] and
    accumulator [accum].

    @param test
      determines whether to open the test file or the real input file. If
      [test], then the file that is actually opened is [name ^ ".test"].
      Otherwise the file [name ^ ".input"] is opened. *)

val print_parts : (unit -> int) -> (unit -> int) -> unit
