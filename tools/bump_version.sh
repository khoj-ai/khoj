#!/bin/zsh

# Copy current version to project root
echo $PWD
cp src/interface/obsidian/versions.json .
cp src/interface/obsidian/manifest.json .

# Induce hatch to compute next version number
# remove .dev[commits-since-tag] version suffix from hatch computed version number
next_version=$(touch bump.txt && git add bump.txt && hatch version | sed 's/\.dev.*//g')
git rm --cached -- bump.txt && rm bump.txt

# Bump Obsidian plugins to next version
cd src/interface/obsidian
sed -E -i.bak "s/version\": \"(.*)\",/version\": \"$next_version\",/" package.json
sed -E -i.bak "s/version\": \"(.*)\"/version\": \"$next_version\"/" manifest.json
npm run version  # updates versions.json
rm *.bak

# Bump Emacs package to next version
cd ../emacs
sed -E -i.bak "s/^;; Version: (.*)/;; Version: $next_version/" khoj.el
rm *.bak

# Restore State
cd ../../../
