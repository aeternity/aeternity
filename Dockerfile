FROM aeternity/builder as builder

# Add required files to download and compile only the dependencies
ADD rebar.config rebar.lock Makefile rebar3 rebar.config.script VERSION /app/
RUN cd /app && make prod-compile-deps
# Add the whole project and compile epoch itself.
ADD . /app
RUN cd /app && make prod-build

# Put epoch node in second stage container
FROM ubuntu:18.04

# Deploy application code from builder container
COPY --from=builder /app/_build/prod/rel/epoch /home/epoch/node

# OpenSSL is shared lib dependency
RUN apt-get -qq update && apt-get -qq -y install libssl1.0.0 curl libsodium23 \
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

EXPOSE 3013 3014 3015 3113 3114

COPY ./docker/entrypoint.sh /docker-entrypoint.sh
COPY ./docker/healthcheck.sh /healthcheck.sh

ENTRYPOINT ["/docker-entrypoint.sh"]
HEALTHCHECK --timeout=3s CMD /healthcheck.sh
