{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.clojure
    pkgs.nodejs
    pkgs.pnpm
    pkgs.jdk21_headless
  ];
}
