FROM aetrnty/builder as builder

ADD . /app
RUN cd /app && make prod-build

# Put epoch node in second stage container
FROM ubuntu:16.04

# Deploy application code from builder container
COPY --from=builder /app/_build/prod/rel/epoch /home/epoch/node

# Copy custom built libs
# Note the `ldconfig` requirement which is implicitly run by openssl install
COPY --from=builder /usr/local/lib /usr/local/lib

# OpenSSL is shared lib dependency
RUN apt-get -qq update && apt-get -qq -y install openssl curl \
    && ldconfig \
    && rm -rf /var/lib/apt/lists/*

# Epoch app won't run as root for security reasons
RUN useradd --shell /bin/bash epoch \
    && chown -R epoch:epoch /home/epoch

# Switch to non-root user
USER epoch
ENV SHELL /bin/bash

# Create data directories in advance so that volumes can be mounted in there
# see https://github.com/moby/moby/issues/2259 for more about this nasty hack
RUN mkdir -p /home/epoch/node/data/mnesia \
    && mkdir -p /home/epoch/node/keys

WORKDIR /home/epoch/node

# Erl handle SIGQUIT instead of the default SIGINT
STOPSIGNAL SIGQUIT

EXPOSE 3013 3113 3114

COPY ./docker/entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
