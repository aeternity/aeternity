FROM erlang:20.2 as builder

ADD . /app
RUN cd /app && make prod-build

# Put epoch node in second stage container
FROM ubuntu:16.04

# OpenSSL is shared lib dependency
RUN apt-get update && apt-get -qy install openssl \
    && rm -rf /var/lib/apt/lists/*

# Epoch app won't run as root for security reasons
RUN useradd --create-home --shell /bin/bash epoch

# Deploy application code from builder container
COPY --from=builder /app/_build/prod/rel/epoch /home/epoch/node
RUN chown -R epoch:epoch /home/epoch

# Switch to non-root user
USER epoch

ENV SHELL /bin/bash
ENV PEER_ADDRESS 127.0.0.1

WORKDIR /home/epoch/node

# Erl handle SIGQUIT instead of the default SIGINT
STOPSIGNAL SIGQUIT

EXPOSE 3013

COPY ./docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
