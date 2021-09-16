let
  pkgs = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs/";                       
    ref = "refs/heads/nixos-unstable";                     
    rev = "860b56be91fb874d48e23a950815969a7b832fbc";                                           
  }) {};  
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
