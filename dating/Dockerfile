FROM haskell:8.2.2

RUN apt-get update && apt-get install -y \
  libpq-dev

RUN mkdir -p /app/user
WORKDIR /app/user
COPY ./stack.yaml ./package.yaml ./*.cabal ./

RUN export PATH=$(stack path --local-bin):$PATH
RUN stack setup
RUN stack build --only-dependencies
RUN stack install ghcid
