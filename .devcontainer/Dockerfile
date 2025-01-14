FROM ocaml/opam:debian-ocaml-5.2

RUN sudo apt update && sudo apt install gh

RUN opam install -y dune merlin ocaml-lsp-server odoc ocamlformat ocamlformat-rpc utop menhir ppx_inline_test
