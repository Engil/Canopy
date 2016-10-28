FROM ocaml/opam:debian-stable_ocaml-4.03.0
MAINTAINER canopy
ENV OPAMYES 1
RUN sudo apt-get update
RUN sudo apt-get -yy install npm
RUN sudo ln -s `which nodejs` /usr/bin/node
RUN sudo npm install -g less browserify
RUN cd /home/opam/opam-repository && git pull && opam update
ADD package.json README.md config.ml /src/
WORKDIR /src
ADD tls /src/tls
RUN sudo chown -R opam:opam /src; sudo chmod -R 700 /src
ENV TMP /tmp
RUN opam install -y -j2 mirage
COPY . /src
ADD assets /src/assets
RUN sudo chown -R opam:opam /src; sudo chmod -R 700 /src
RUN opam config exec -- mirage configure
RUN opam config exec -- make
RUN sudo mkdir /tmp/assets ; sudo chown opam:opam /tmp/assets ; ./populate.sh /tmp/assets
EXPOSE 8080
ENTRYPOINT ["opam", "config", "exec", "--", "./mir-canopy"]
