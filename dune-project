(lang dune 3.6)

(name aoc)

(version v0.0.1)

(generate_opam_files true)

(license GPL-3.0-or-later)

(authors "Yawar Amin <yawar.amin@gmail.com>")

(maintainers "Yawar Amin <yawar.amin@gmail.com>")

(source
 (github yawaramin/aoc))

(package
 (name aoc)
 (synopsis "Advent of Code")
 (description "Advent of Code")
 (documentation "TBD")
 (allow_empty)
 (tags (org:yawaramin))
 (depends
  (ocaml
   (>= 5.2.1))))
