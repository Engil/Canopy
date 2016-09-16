FROM ocaml/opam:debian-stable_ocaml-4.03.0
MAINTAINER canopy
ENV OPAMYES 1
RUN sudo apt-get update
RUN sudo apt-get -yy install npm
RUN sudo ln -s `which nodejs` /usr/bin/node
RUN sudo npm install -g less browserify
RUN cd /home/opam/opam-repository && git pull && opam update
RUN opam remote add mirage-dev git://github.com/mirage/mirage-dev.git
RUN opam update -u
ADD package.json README.md config.ml /src/
WORKDIR /src
ADD tls /src/tls
RUN sudo chown -R opam:opam /src; sudo chmod -R 700 /src
ENV TMP /tmp
RUN opam install -y -j2 mirage
RUN opam config exec -- mirage configure --no-assets-compilation
COPY . /src
ADD assets /src/assets
RUN sudo chown -R opam:opam /src; sudo chmod -R 700 /src
RUN opam config exec -- mirage configure --no-opam --no-depext
RUN opam config exec -- make
EXPOSE 8080
ENTRYPOINT ["opam", "config", "exec", "--", "./mir-canopy"]
