# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.245.2/containers/rust/.devcontainer/base.Dockerfile

# [Choice] Debian OS version (use bullseye on local arm64/Apple Silicon): buster, bullseye

FROM arm64v8/ubuntu:latest
RUN apt update -y && apt install -y software-properties-common
RUN add-apt-repository ppa:avsm/ppa
RUN apt install -y ocaml opam python3 cmake llvm llvm-14-dev
RUN opam init --auto-setup --yes
RUN opam install ocaml-lsp-server -y
RUN opam install ocamlformat -y
RUN opam install menhir -y
RUN opam install ppx_deriving -y
RUN opam install dune -y
RUN opam install utop -y
