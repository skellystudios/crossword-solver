FROM haskell
RUN cabal update
RUN cabal install hashmap
ADD ./data /opt/solver/data
ADD ./src /opt/solver/src
WORKDIR /opt/solver/src
CMD ghci Thesaurus.hs