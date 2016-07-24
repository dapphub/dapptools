FROM ubuntu:16.04
RUN apt-get update && apt-get install -y curl jshon
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash
RUN apt-get update && apt-get install -y nodejs
