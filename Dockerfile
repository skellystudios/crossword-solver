FROM haskell
RUN cabal update ; cabal install hashmap
WORKDIR /opt/solver/src
