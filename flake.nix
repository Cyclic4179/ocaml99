{
  description = "OCaml exploring, 99 problems";

  inputs = {
    # Convenience functions for writing flakes
    flake-utils.url = "github:numtide/flake-utils";
    # Precisely filter files copied to the nix store
    #nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, flake-utils, nixpkgs }:
    # Construct an output set that supports a number of default systems
    flake-utils.lib.eachDefaultSystem (system:
    let
        # Legacy packages that have not been converted to flakes
        legacyPackages = nixpkgs.legacyPackages.${system};
        # OCaml packages available on nixpkgs
        ocamlPackages = legacyPackages.ocamlPackages;
        # Library functions from nixpkgs
        #lib = legacyPackages.lib;
    in
    {
      devShells = {
        default = legacyPackages.mkShell {
            # Development tools
            packages = [
              # Source file formatting
              #legacyPackages.nixpkgs-fmt
              #legacyPackages.ocamlformat
              # For `dune build --watch ...`
              legacyPackages.fswatch
              # For `dune build @doc`
              ocamlPackages.odoc
              # OCaml editor support
              #ocamlPackages.ocaml-lsp
              # Nicely formatted types on hover
              ocamlPackages.ocamlformat-rpc-lib
              # Fancy REPL thing
              ocamlPackages.utop
            ];
            shellHook = ''
              exec zsh
            '';
          };
        };
      });
}
