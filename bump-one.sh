#!/usr/bin/env bash
set -eu

exec > >(sed "s/^/${1/\//\\\/}: /") 2> >(sed >&2 "s/^/${1/\//\\\/}: /")

GET() {
  curl -s -H "Authorization: token ${GITHUB_TOKEN?Need OAuth token}" "$@"
}

echo "bumping"
pkg="$1"
name=$(basename "$pkg")
tag=$(GET https://api.github.com/repos/"$pkg"/tags?per_page=1 | jshon -e 0)
version=$(jshon -e name -u <<<"$tag")
taghash=$(jshon -e commit -e sha -u <<<"$tag")
head=$(GET https://api.github.com/repos/"$pkg"/commits?per_page=1 | jshon -e 0)
headhash=$(jshon -e sha -u <<<"$head")

echo "stable $version, master $headhash"

tagsha256=$(
  nix-prefetch-url \
    --unpack \
    https://github.com/"$pkg"/archive/"$version".tar.gz 2>/dev/null)

headsha256=$(
  nix-prefetch-url \
    --unpack \
    https://github.com/"$pkg"/archive/"$headhash".tar.gz 2>/dev/null)

tree=$(GET https://api.github.com/repos/"$pkg"/git/trees/"$taghash")
nix=$(jq -r <<<"$tree" '.tree | .[] | select(.path == "default.nix") | .url')
GET "$nix" | jshon -e content -u | base64 -d > overlay/upstream/stable/$name.nix

tree=$(GET https://api.github.com/repos/"$pkg"/git/trees/"$headhash")
nix=$(jq -r <<<"$tree" '.tree | .[] | select(.path == "default.nix") | .url')
GET "$nix" | jshon -e content -u | base64 -d > overlay/upstream/master/$name.nix

(
  flock 9 || exit 1
  json=$(cat bump.json)
  json=$(
    jshon <<<"$json" -n {} \
      -s "${version#v}" -i version \
      -n {} \
      -s "$headhash" -i rev \
      -s "$headsha256" -i sha256 \
      -s "$(dirname "$pkg")" -i owner \
      -s "$name" -i repo \
      -i master \
      -n {} \
      -s "$taghash" -i rev \
      -s "$tagsha256" -i sha256 \
      -s "$(dirname "$pkg")" -i owner \
      -s "$name" -i repo \
      -i stable \
      -i "$name"
  )
  echo "$json" >bump.json
) 9>/tmp/dapphub-bump.lock

echo "done"
