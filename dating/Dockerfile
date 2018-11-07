FROM haskell:8.2.2

RUN apt-get update && apt-get install -y \
  libpq-dev libgsl0-dev liblapack-dev libatlas-base-dev

RUN mkdir -p /app/user
WORKDIR /app/user
COPY ./stack.yaml ./package.yaml ./*.cabal ./

RUN export PATH=$(stack path --local-bin):$PATH
RUN stack setup
RUN stack build --only-dependencies --fast
RUN stack install ghcid
