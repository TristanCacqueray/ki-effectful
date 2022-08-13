{
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/cc4beb99e1c29a6109a1867c0c3f178b2b27c34c";
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
