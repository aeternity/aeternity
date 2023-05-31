let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/860b56be91fb874d48e23a950815969a7b832fbc.tar.gz") {};
in {
  aeternityEnv = pkgs.stdenv.mkDerivation {
    name = "aeternity-webhook";
    buildInputs = [
      ## base
      pkgs.stdenv
      ## curl
      pkgs.curl
    ];
  };
}
