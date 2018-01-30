FROM erlang:20.2 as builder

ADD . /app
RUN cd /app && make prod-build

# Put epoch node in second stage container
FROM ubuntu:16.04

# OpenSSL is shared lib dependency
RUN apt-get -qq update && apt-get -qq -y install openssl curl \
    && rm -rf /var/lib/apt/lists/*

# Deploy application code from builder container
COPY --from=builder /app/_build/prod/rel/epoch /home/epoch/node

# Deploy TestNet user configuration template
COPY --from=builder /app/apps/aeutils/test/data/epoch_testnet.yaml /home/epoch/node/epoch_testnet.yaml

# Epoch app won't run as root for security reasons
RUN useradd --shell /bin/bash epoch \
    && chown -R epoch:epoch /home/epoch

# Switch to non-root user
USER epoch
ENV SHELL /bin/bash

WORKDIR /home/epoch/node

# Erl handle SIGQUIT instead of the default SIGINT
STOPSIGNAL SIGQUIT

EXPOSE 3013 3113 3114

COPY ./docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
