let
  stable = import (fetchTarball { # 19.09
    url = https://github.com/NixOS/nixpkgs-channels/archive/a22b0189002.tar.gz;
    sha256 = "0rgd0cbxg9mrzb830hgjlvy134ivpfcnkyhbnlvvn8vl4y20zqmz";
  }) {};
in {
  aeternityEnv = stable.stdenv.mkDerivation {
    name = "aeternity-webhook";
    buildInputs = [
      ## base
      stable.stdenv
      ## curl
      stable.curl
    ];
  };
}
