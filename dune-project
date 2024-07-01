(lang dune 3.16)

(name wavy)
(version cd5973f-dirty)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "John Bakhmat")

(maintainers "John Bakhmat")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name wavy)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune metadata)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
