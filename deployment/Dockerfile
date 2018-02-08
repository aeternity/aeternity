FROM ubuntu:16.04

RUN apt-get -qq update \
    && apt-get -qq -y install git curl \
        autoconf build-essential ncurses-dev libssl-dev \
        python-pip python-dev software-properties-common \
        default-jre-headless \
    && apt-add-repository ppa:ansible/ansible \
    && apt-get -qq update \
    && apt-get -qq -y install ansible \
    && rm -rf /var/lib/apt/lists/*

ENV OTP_VERSION="20.2.2"
RUN OTP_DOWNLOAD_URL="https://github.com/erlang/otp/archive/OTP-${OTP_VERSION}.tar.gz" \
    && curl -fsSL -o otp-src.tar.gz "$OTP_DOWNLOAD_URL" \
    && mkdir otp-src \
    && tar -zxf otp-src.tar.gz -C otp-src --strip-components=1 \
    && cd otp-src \
    && export ERL_TOP=`pwd` \
    && ./otp_build autoconf && ./configure && make -j$(nproc) && make install

ENV LIBSODIUM_VERSION=1.0.16
RUN LIBSODIUM_DOWNLOAD_URL="https://github.com/jedisct1/libsodium/releases/download/${LIBSODIUM_VERSION}/libsodium-${LIBSODIUM_VERSION}.tar.gz" \
    && curl -fsSL -o libsodium-src.tar.gz "$LIBSODIUM_DOWNLOAD_URL" \
    && mkdir libsodium-src \
    && tar -zxf libsodium-src.tar.gz -C libsodium-src --strip-components=1 \
    && cd libsodium-src \
    && ./configure && make -j$(nproc) && make install && ldconfig

RUN pip install virtualenv

# Add non-root user in case some app won't run as root
RUN useradd --shell /bin/bash builder --create-home

ENV SHELL=/bin/bash
