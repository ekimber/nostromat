{
  description = "A cljs flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;        
        # nodeDependencies = (pkgs.callPackage ./default.nix {}).nodeDependencies;
    in {
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = with pkgs; [ clojure nodejs yarn openjdk19_headless ];
      };
    };
}
