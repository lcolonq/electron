{
  description = "electron";
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    emacs-overlay.url = github:nix-community/emacs-overlay;
  };
  outputs = { self, nixpkgs, emacs-overlay }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [emacs-overlay.overlay];
      };
      emacsPackages = pkgs.emacsPackagesFor pkgs.emacs;
      emacs = emacsPackages.emacsWithPackages (epkgs: with epkgs; [
        dash
        s
        f
        ht
        request
        xmlgen
      ]);
      shell = pkgs.mkShell {
        NIX_HARDENING_ENABLE = "";
        buildInputs = [
          emacs
          pkgs.libtool
          pkgs.raylib
        ];
      };
    in {
      defaultPackage.x86_64-linux = shell;
      packages.x86_64-linux = {
        inherit
          shell
        ;
      };
    };
}
