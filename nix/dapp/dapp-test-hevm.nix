{ pkgs }: pkgs.bashScript {
  name = "dapp2-test-hevm";
  deps = with pkgs; [findutils hevm];
  text = ''
    find "''${DAPP_OUT?}" -type f -name '*.sol.json' -print0 |
      xargs -0 -n1 -I{} hevm dapp-test --json-file={} --dapp-root=. "$@"
  '';
}
