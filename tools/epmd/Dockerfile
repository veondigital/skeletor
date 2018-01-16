FROM debian:stretch

RUN apt-get update -y -q
RUN apt-get dist-upgrade -y -q
RUN apt-get install -y erlang-nox

#ENV ERL_EPMD_ADDRESS
ENV ERL_EPMD_PORT 4369

ENTRYPOINT set -x && \
           epmd -port $ERL_EPMD_PORT \
                -relaxed_command_check

