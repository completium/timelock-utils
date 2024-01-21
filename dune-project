(lang dune 2.7)
(using menhir 2.0)
(name timelock-utils)
(allow_approximate_merlin)
(generate_opam_files true)

(maintainers "Completium team <contact@completium.com>")
(authors "Completium team <contact@completium.com>")
(homepage "https://completium.com")
(bug_reports "https://github.com/completium/timelock-utils/issues")

(package
 (name timelock-utils)
 (synopsis "Timelock")
 (description "\
Utils to generate chest and chest_key from packed data bytes
")
 (depends
   (ocaml (>= 4.12.0))
   (hex (>= 1.4.0))
   (octez-libs (>= 18.1))
   (yojson (>= 1.7.0))
  )
)
