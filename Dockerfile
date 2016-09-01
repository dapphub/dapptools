FROM ubuntu:16.04
RUN apt-get update && apt-get install -y build-essential curl git
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash
RUN apt-get update && apt-get install -y nodejs
RUN git clone -b 20131105 --depth=1 https://github.com/keenerd/jshon
RUN apt-get update && apt-get install -y libjansson-dev
RUN make -C jshon all install
RUN apt-get update && apt-get install -y bc
RUN apt-get update && apt-get install -y shellcheck
RUN apt-get update && apt-get install -y cmake
RUN git clone -b v31 --depth=1 https://github.com/SimonKagstrom/kcov
RUN apt-get update && apt-get install -y pkg-config
RUN apt-get update && apt-get install -y libz-dev
RUN apt-get update && apt-get install -y libcurl4-openssl-dev
RUN apt-get update && apt-get install -y libelf-dev
RUN apt-get update && apt-get install -y libdw-dev
RUN apt-get update && apt-get install -y python
RUN cd kcov && cmake . && make all install
