# Would love to use alpine, but it doesn't appear that libgccjit is available yet
FROM ubuntu:20.10

MAINTAINER thornjad <jademichael@jmthornton.net>

WORKDIR /opt/emacs
COPY . /opt/emacs

# Shut up debconf, otherwise it will try to set up postfix for no good reason
ENV DEBIAN_FRONTEND=noninteractive
# Fix "Couldn't register with accessibility bus" error message
ENV NO_AT_BRIDGE=1

RUN apt-get update \
  && apt-get install -y \
  curl git zsh \
  gnupg-agent libgnutls28-dev \
  gcc-10 libgccjit0 libgccjit-10-dev \
  libjansson4 libjansson-dev

# Cheats' way of ensuring we get all the build deps for Emacs without
# specifying them ourselves. Enable source packages then tell apt to
# get all the deps for whatever Emacs version Ubuntu supports by
# default. Thanks to masteringemacs.org for this hack.
RUN sed -i 's/# deb-src/deb-src/' /etc/apt/sources.list \
  && apt-get update \
  && apt-get build-dep -y emacs

# required by asEnvUser
RUN git clone https://github.com/ncopa/su-exec.git /tmp/su-exec \
  && cd /tmp/su-exec \
  && make \
  && chmod 770 su-exec \
  && mv ./su-exec /usr/local/sbin

# asEnvUser lets us create and use a given user
ADD https://raw.githubusercontent.com/JAremko/docker-emacs/master/asEnvUser /tmp
RUN cp /tmp/asEnvUser /usr/local/sbin
RUN chown root /usr/local/sbin/asEnvUser \
  && chmod 700  /usr/local/sbin/asEnvUser

ENV CC="gcc-10"

RUN ./autogen.sh \
  && ./configure --build="$(dpkg-architecture --query DEB_BUILD_GNU_TYPE)" \
              --with-native-compilation \
              --with-modules \
              --with-dbus \
              --with-gnutls \
              --with-xml2 \
  && make -j "$(nproc --all)" NATIVE_FAST_BOOT=1 \
  && make install

# TODO Now dependencies for tools we use in Aero
# FROM rust:latest
# RUN rustup component add rls rust-analysis rust-src && \
#   cargo install ripgrep

# FROM node:latest
# RUN  npm i -g sass-lint eslint tern coffeescript cofeelint \
#               bash-language-server javascript-typescript-langserver

# FROM python:latest
# RUN  pip3 install python-language-server "ptvsd>=4.2"

ENV UNAME="aero" \
    GNAME="aero" \
    UHOME="/home/aero" \
    UID="1000" \
    GID="1000" \
    WORKSPACE="/mnt/workspace" \
    SHELL="/usr/bin/zsh"

ENTRYPOINT ["asEnvUser"]
CMD ["zsh", "-c", "emacs; /usr/bin/zsh"]
