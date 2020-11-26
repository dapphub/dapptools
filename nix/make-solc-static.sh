#!/usr/bin/env bash
### helper script for constructing the solc-static nix attrs

curl https://binaries.soliditylang.org/macosx-amd64/list.json > macos.json
curl https://binaries.soliditylang.org/linux-amd64/list.json > linux.json

# extract some fields from the json and replace solc_x.y.z with solc_x_y_z
get_info() {
  < $1 jq -r '.builds[]
    | "solc_\(.version) = { version = \"\(.version)\"; path = \"\(.path)\";"
    | sub("solc_(?<x>[0-9]+).(?<y>[0-9]+).(?<z>[0-9]+)"; "solc_\(.x)_\(.y)_\(.z)")'
}

# the actual binary urls
linux_urls="$(< linux.json jq -r '.builds[] | "https://binaries.soliditylang.org/linux-amd64/\(.path)"')"
macos_urls="$(< macos.json jq -r '.builds[] | "https://binaries.soliditylang.org/macosx-amd64/\(.path)"')"

get_hashes() {
  for url in $1; do
    sha256=$(nix-prefetch-url --type sha256 "$url")
    echo "sha256 = "'"'${sha256}'"'"; };"
  done
}

linux="$(paste -d' ' <(get_info linux.json) <(get_hashes "$linux_urls"))"
macos="$(paste -d' ' <(get_info macos.json) <(get_hashes "$macos_urls"))"
# now paste this into solc-static-versions.nix
echo "x86_64-linux  = {"
echo "$linux"
echo "};"
echo "x86_64-darwin  = {"
echo "$macos"
echo "};"
