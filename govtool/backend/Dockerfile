FROM haskell:9.2-buster
WORKDIR /src
COPY . .
RUN cabal update && cabal configure && cabal build
RUN cp dist-newstyle/build/x86_64-linux/ghc-9.2.8/vva-be-0.1.0.0/x/vva-be/build/vva-be/vva-be /usr/local/bin
