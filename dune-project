(lang dune 3.16)

(name tinyrust)

(using menhir 3.0)

(generate_opam_files true)

(source
 (github dalps/tinyrust))

(authors "Federico Dal Pio Luogo")

(maintainers "Federico Dal Pio Luogo")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tinyrust)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
