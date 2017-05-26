FROM mitchty/alpine-ghc:large
RUN sed -i -e 's/v3\.5/edge/g' /etc/apk/repositories
RUN apk add --update bzip2-dev ncurses-static readline-dev readline-static \
&& ln -s /usr/lib/libncursesw.a /usr/lib/libncurses.a
WORKDIR /src
ADD hsevm.cabal /src/
RUN cabal update && cabal install --only-dependencies
ADD . /src
RUN cabal configure --disable-executable-dynamic --ghc-option=-optl-static --ghc-option=-optl-pthread && cabal build