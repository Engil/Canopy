FROM ocaml/dev:release-debian-9_ocaml-4.02.3
MAINTAINER canopy
ENV OPAMYES 1
RUN sudo apt-get update
RUN opam pin add dolog https://github.com/UnixJunkie/dolog.git\#no_unix
RUN opam pin add bin_prot https://github.com/hannesm/bin_prot.git\#113.33.00+xen
RUN opam pin add crc https://github.com/xapi-project/ocaml-crc.git
RUN opam install ptime
RUN opam pin add syndic https://github.com/Cumulus/Syndic.git\#ptime
COPY . /src
RUN sudo chown -R opam:opam /src
WORKDIR /src
RUN mkdir disk
RUN opam config exec -- mirage configure --xen
RUN opam config exec -- make
