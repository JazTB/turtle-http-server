(lang dune 3.17)

(name TurtleHttpServer)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "JazTb <JazTB@example.com>")

(maintainers "JazTB <JazTB@example.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name TurtleHttpServer)
 (synopsis "A http server for controlling cc:tweaked turtles.")
 (description "Allows automation of lua tasks on turtles for ComputerCraft:Tweaked (a Minecraft Java mod).")
 (depends ocaml yojson sqlite3 lwt cohttp cohttp-lwt-unix)
 (tags
  ("Minecraft")))
