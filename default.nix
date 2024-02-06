let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-23.11") {};
in {
  aeternityEnv = pkgs.stdenv.mkDerivation {
    name = "aeternity";
    ## required to compile the C parts of cuckoo
    hardeningDisable = [ "format" ];
    nativeBuildInputs = [ pkgs.cmake ];
    buildInputs = [
      ## base
      pkgs.stdenv
      ## erlang
      pkgs.erlang_26 # OTP 26.1.2
      ## crypto
      pkgs.libsodium
      ## rocksdb build deps
      pkgs.automake
      pkgs.autoconf
      pkgs.libtool
      pkgs.which
      ## emcl's dependencies
      pkgs.gmp
    ];
    ## required to start the node locally
    shellHooks = ''
      ulimit -n 24576
    '';
  };
}
