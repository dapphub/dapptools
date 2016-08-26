FROM ubuntu:16.04
RUN apt-get update && apt-get install -y build-essential curl git
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash
RUN apt-get update && apt-get install -y nodejs
RUN git clone -b 20131105 --depth=1 https://github.com/keenerd/jshon
RUN apt-get update && apt-get install -y libjansson-dev
RUN make -C jshon all install
