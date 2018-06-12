# This file acts like the main file of an entire <nixpkgs>.
# It imports a full nixpkgs from our pinned submodule,
# and extends it with our overlay.
#
# This is meant to be used as a channel.

{ pkgsPath ? null
, overlays ? []
, system ? null
}:

let
  ## Horrible; copied from nixpkgs to get extra overlays to work with our overlay.
  homeDir = builtins.getEnv "HOME";
  try = x: def: let res = builtins.tryEval x; in if res.success then res.value else def;
  extra-overlays = with builtins; let
    isDir = path: pathExists (path + "/.");
    pathOverlays = try <nixpkgs-overlays> "";
    homeOverlaysFile = homeDir + "/.config/nixpkgs/overlays.nix";
    homeOverlaysDir = homeDir + "/.config/nixpkgs/overlays";
    overlays = path:
      # check if the path is a directory or a file
      if isDir path then
        # it's a directory, so the set of overlays from the directory, ordered lexicographically
        let content = readDir path; in
        map (n: import (path + ("/" + n)))
          (builtins.filter (n: builtins.match ".*\\.nix" n != null || pathExists (path + ("/" + n + "/default.nix")))
            (attrNames content))
      else
        # it's a file, so the result is the contents of the file itself
        import path;
  in
    if pathOverlays != "" && pathExists pathOverlays then overlays pathOverlays
    else if pathExists homeOverlaysFile && pathExists homeOverlaysDir then
      throw ''
        Nixpkgs overlays can be specified with ${homeOverlaysFile} or ${homeOverlaysDir}, but not both.
        Please remove one of them and try again.
      ''
    else if pathExists homeOverlaysFile then
      if isDir homeOverlaysFile then
        throw (homeOverlaysFile + " should be a file")
      else overlays homeOverlaysFile
    else if pathExists homeOverlaysDir then
      if !(isDir homeOverlaysDir) then
        throw (homeOverlaysDir + " should be a directory")
      else overlays homeOverlaysDir
    else [];

in (
  (import ./nixpkgs) ({
    overlays = [
      (import ./nix/overlay.nix { flavor = "stable"; })
    ] ++ extra-overlays;
  } // (
    if system != null then { inherit system; } else {}
  ))
) // {
  master = (import ./nixpkgs) ({
    overlays = [
      (import ./nix/overlay.nix { flavor = "master"; })
    ] ++ extra-overlays;
  } // (
    if system != null then { inherit system; } else {}
  ));
}
