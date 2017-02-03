FROM haskell:8
ARG  base_url='https://gitlab.com/jkoike/Discord.hs'
RUN  apt-get update && \
     apt-get install -y curl && \
     rm -rf /var/lib/apt/lists/*
RUN  curl -o stack.yaml       $base_url/raw/master/stack.yaml && \
     curl -o discord-hs.cabal $base_url/raw/master/discord-hs.cabal && \
     stack install --install-ghc --only-dependencies --haddock
