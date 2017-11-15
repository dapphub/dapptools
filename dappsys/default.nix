dapp: let load = x: import x dapp; in {

  ## Standards
  erc20    = load ./erc20/dapp.nix;

  ## Mixins
  ds-auth  = load ./ds-auth/dapp.nix;
  ds-exec  = load ./ds-exec/dapp.nix;
  ds-math  = load ./ds-math/dapp.nix;
  ds-note  = load ./ds-note/dapp.nix;
  ds-stop  = load ./ds-stop/dapp.nix;
  ds-test  = load ./ds-test/dapp.nix;
  ds-warp  = load ./ds-warp/dapp.nix;

  ## Boxes
  ds-cache = load ./ds-cache/dapp.nix;
  ds-chief = load ./ds-chief/dapp.nix;
  ds-group = load ./ds-group/dapp.nix;
  ds-guard = load ./ds-guard/dapp.nix;
  ds-proxy = load ./ds-proxy/dapp.nix;
  ds-roles = load ./ds-roles/dapp.nix;
  ds-thing = load ./ds-thing/dapp.nix;
  ds-token = load ./ds-token/dapp.nix;
  ds-value = load ./ds-value/dapp.nix;
  ds-vault = load ./ds-vault/dapp.nix;
}
