FROM mitchty/alpine-ghc:large
RUN sed -i -e 's/v3\.5/edge/g' /etc/apk/repositories
RUN apk add --update bzip2-dev readline-dev readline-static \
&& ln -s /usr/lib/libncursesw.so.6.0 /usr/lib/libncurses.so \
&& ln -s /usr/lib/libncursesw.so.6.0 /usr/lib/libncursesw.so
WORKDIR /src
ADD hsevm.cabal /src/
RUN cabal update && cabal install happy && cabal install --only-dependencies --disable-executable-dynamic
ADD . /src
RUN apk add --update ncurses-static \
&& ln -s /usr/lib/libncursesw.a /usr/lib/libncurses.a \
&& cabal configure --disable-executable-dynamic --ghc-option=-optl-static --ghc-option=-optl-pthread \
&& cabal build \
&& cp dist/build/hsevm-cli/hsevm-cli /bin/hsevm
