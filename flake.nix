{
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/aef39c369f49b0cd0b7ecc17e190f50c3965e53c";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;
      pkg = pkgs.hspkgs.callCabal2nix "ki-effectful" self { };
    in {
      devShell."x86_64-linux" = pkgs.hspkgs.shellFor {
        packages = p: [ pkg ];
        buildInputs = with pkgs; [
          cabal-install
          haskell-language-server
          fourmolu
        ];
      };
    };
}
