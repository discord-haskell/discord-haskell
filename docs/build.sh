git clone https://$GIT_USER:$GIT_PASS@github.com/jano017/Discord.hs.git -b gh-pages _site/
cp -r $(stack path --local-doc-root --nix)/discord-* docs/build
stack exec --nix docs build
cd _site && git add . && git commit --allow-empty -m "Auto build docs for $TRAVIS_COMMIT" && git push --quiet
