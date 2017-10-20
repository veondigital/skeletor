FROM debian:stretch

LABEL maintainer="{{author_name}} <{{author_email}}>"
LABEL description="{{description}}"

ARG ERLANG_VSN=19.3
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -qq
RUN apt-get dist-upgrade -y
RUN apt-get install -y build-essential \
                       libncurses-dev \
                       libssl1.0-dev \
                       libexpat-dev \
                       curl \
                       git
RUN apt-get clean

WORKDIR /usr/local/src
RUN curl -O http://erlang.org/download/otp_src_$ERLANG_VSN.tar.gz
RUN tar xzf otp_src_$ERLANG_VSN.tar.gz

WORKDIR /usr/local/src/otp_src_$ERLANG_VSN
RUN ./configure --prefix=/usr/local
RUN make && make install

ADD . /opt/{{name}}_src
WORKDIR /opt/{{name}}_src
RUN ./rebar3 as prod do release
RUN mv /opt/{{name}}_src/_build/prod/rel/{{name}} /opt/

WORKDIR /opt/{{name}}
RUN rm -rf /opt/{{name}}_src /usr/local/src/otp_src_$ERLANG_VSN*

ENTRYPOINT ["/opt/{{name}}/bin/start-prod.sh", "console"]
