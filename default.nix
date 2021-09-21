let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/860b56be91fb874d48e23a950815969a7b832fbc.tar.gz") {};
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
      pkgs.erlangR23 # OTP 23.3.4.4
      ## crypto
      pkgs.libsodium
      ## rocksdb build deps
      pkgs.automake
      pkgs.autoconf
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
