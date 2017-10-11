{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./tubes.nix {}
