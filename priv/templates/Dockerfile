FROM debian:stretch

LABEL maintainer="{{author_name}} <{{author_email}}>"
LABEL description="{{description}}"

RUN apt-get update -qq
RUN apt-get dist-upgrade -y
RUN apt-get install -y build-essential \
                       libncurses-dev \
                       libssl1.0-dev \
                       libexpat-dev \
                       curl \
                       git

RUN mkdir -p /usr/local/erlang
WORKDIR /usr/local/erlang
RUN curl -O http://erlang.org/download/otp_src_19.3.tar.gz
RUN tar xzf otp_src_19.3.tar.gz

WORKDIR /usr/local/erlang/otp_src_19.3
RUN ./configure --prefix=/usr/local
RUN make && make install

ADD . /opt/{{name}}_src
WORKDIR /opt/{{name}}_src
RUN ./rebar3 as prod do release
RUN mv /opt/{{name}}_src/_build/prod/rel/{{name}} /opt/

ENTRYPOINT ["/opt/{{name}}/bin/start-prod.sh", "console"]
