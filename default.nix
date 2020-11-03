let
  stable = import (fetchTarball { # 19.09
    url = https://github.com/NixOS/nixpkgs-channels/archive/a22b0189002.tar.gz;
    sha256 = "0rgd0cbxg9mrzb830hgjlvy134ivpfcnkyhbnlvvn8vl4y20zqmz";
  }) {};
in {
  aeternityEnv = stable.stdenv.mkDerivation {
    name = "aeternity";
    ## required to compile the C parts of cuckoo
    hardeningDisable = [ "format" ];
    nativeBuildInputs = [ stable.cmake ];
    buildInputs = [
      ## base
      stable.stdenv
      ## erlang
      stable.erlangR21 # OTP 21.3.5.2
      ## crypto
      stable.libsodium
      ## rocksdb build deps
      stable.automake
      stable.autoconf
      stable.which
      ## emcl's dependencies
      stable.gmp
    ];
    ## required to start the node locally
    shellHooks = ''
      ulimit -n 24576
    '';
  };
}
