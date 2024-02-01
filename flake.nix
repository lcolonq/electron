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
      deps = [
        emacs
        pkgs.libtool
        pkgs.raylib
      ];
      shell = pkgs.mkShell {
        NIX_HARDENING_ENABLE = "";
        buildInputs = deps;
      };
      uwtrbtpimicasy-lib = pkgs.stdenv.mkDerivation {
        name = "uwtrbtpimicasy";
        src = ./.;
        buildInputs = deps;
        buildPhase = ''
          make
        '';
        installPhase = ''
          mkdir -p $out
          cp -r assets $out
          cp uwtrbtpimicasy.el electron.so $out
        '';
      };
      uwtrbtpimicasy = pkgs.writeShellScriptBin "uwtrbtpimicasy" ''
        ${emacs}/bin/emacs --batch --load="${uwtrbtpimicasy-lib}/electron.so" --load="${uwtrbtpimicasy-lib}/uwtrbtpimicasy.el"
      '';
    in {
      devShells.x86_64-linux.default = shell;
      packages.x86_64-linux = {
        inherit uwtrbtpimicasy-lib uwtrbtpimicasy;
        default = uwtrbtpimicasy;
      };
    };
}
