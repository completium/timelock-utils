(lang dune 2.7)
(using menhir 2.0)
(name timelock-utils)
(allow_approximate_merlin)
(generate_opam_files true)

(maintainers "Completium team <contact@completium.com>")
(authors "Completium team <contact@completium.com>")
(homepage "https://completium.com")
(bug_reports "https://github.com/edukera/archetype-lang/issues")

(package
 (name timelock-utils)
 (synopsis "Timelock")
 (description "\
Archetype is a general purpose language to develop smart contracts
on the Tezos blockchain, with a specific focus on contract security
")
 (depends
   (ocaml (>= 4.12.0))
   (hex (>= 1.4.0))
   (tezos-hacl-glue (>= 11.1))
   (tezos-crypto (>= 11.1))
  )
)
