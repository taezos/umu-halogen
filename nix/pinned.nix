let
  pkgs = import <nixpkgs> { };
in
import (
  pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "64cf1e79dd77378046bf802e84c432ccf205a62f";
    sha256 = "1h64p66a38ca5m7a7m0fgpdpk494fchigczrhl0mwfrfvcv8q67w";
  }
)

# https://github.com/NixOS/nixpkgs/archive/64cf1e79dd77378046bf802e84c432ccf205a62f.tar.gz
