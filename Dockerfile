FROM haskell
RUN cabal update ; cabal install hashmap;
RUN cabal update ; cabal install memoize;
WORKDIR /opt/solver/src
