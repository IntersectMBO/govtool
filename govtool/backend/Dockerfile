FROM 733019650473.dkr.ecr.eu-west-1.amazonaws.com/backend-base:39906fb
WORKDIR /src
COPY . .
RUN cabal build
RUN cp dist-newstyle/build/x86_64-linux/ghc-9.2.8/vva-be-0.1.0.0/x/vva-be/build/vva-be/vva-be /usr/local/bin
