# Installation Instructions

## Run the following commands in the terminal:
```
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc ANSITerminal pyml owl

(
  If there's an issue with owl installation, run the following: 
  opam depext conf-openblas.0.2.0
)

make final_demo
```