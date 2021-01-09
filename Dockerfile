FROM aeternity/builder:otp21 as builder

# Add required files to download and compile only the dependencies
ADD rebar.config rebar.lock Makefile rebar3 rebar.config.script VERSION /app/
ENV ERLANG_ROCKSDB_OPTS "-DWITH_BUNDLE_LZ4=ON -DWITH_BUNDLE_SNAPPY=ON"
RUN cd /app && make prod-compile-deps
# Add the whole project and compile aeternity itself.
ADD . /app
RUN cd /app && make prod-build

# Put aeternity node in second stage container
FROM ubuntu:18.04

# Deploy application code from builder container
COPY --from=builder /app/_build/prod/rel/aeternity /home/aeternity/node

# OpenSSL is shared lib dependency
RUN apt-get -qq update && apt-get -qq -y install libssl1.0.0 curl libsodium23 libgmp10 \
    && ldconfig \
    && rm -rf /var/lib/apt/lists/*

# Aeternity app won't run as root for security reasons
RUN useradd --shell /bin/bash aeternity \
    && chown -R aeternity:aeternity /home/aeternity

# Switch to non-root user
USER aeternity
ENV SHELL /bin/bash

# Create data directories in advance so that volumes can be mounted in there
# see https://github.com/moby/moby/issues/2259 for more about this nasty hack
RUN mkdir -p /home/aeternity/node/data/mnesia \
    && mkdir -p /home/aeternity/node/keys

WORKDIR /home/aeternity/node

EXPOSE 3013 3014 3015 3113

COPY ./docker/healthcheck.sh /healthcheck.sh
HEALTHCHECK --timeout=3s CMD /healthcheck.sh

CMD ["bin/aeternity", "console", "-noinput"]
