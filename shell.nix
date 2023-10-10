{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  # nativeBuildInputs is usually what you want -- tools you need to run
  nativeBuildInputs = with pkgs; [ bubblewrap gcc curl unzip opam ];

  shellHook =
    ''
      echo "Hello shell"
      opam init
      eval $(opam env)
      opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release
    '';
  }
