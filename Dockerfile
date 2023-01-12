# syntax=docker/dockerfile:1

FROM ubuntu:22.04 AS builder

# Download the GNU Emacs tarball
WORKDIR /usr/src
ARG EMACS_VERSION=28.2
RUN --mount=type=cache,target=/var/lib/apt/lists,sharing=locked --mount=type=cache,target=/var/cache/apt/archives,sharing=locked \
    apt-get update \
    && apt-get install -y --no-install-recommends curl ca-certificates
RUN curl -O http://mirrors.kernel.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.gz
RUN tar -x -f emacs-${EMACS_VERSION}.tar.gz

# Build and install the GNU Emacs
WORKDIR /usr/src/emacs-${EMACS_VERSION}
ARG BUILD_PACKAGES="\
    build-essential \
    pkg-config \
    libgnutls28-dev \
    libncurses-dev \
    zlib1g-dev \
    libgccjit-11-dev \
    python3 \
    "
RUN --mount=type=cache,target=/var/lib/apt/lists,sharing=locked --mount=type=cache,target=/var/cache/apt/archives,sharing=locked \
    apt-get install -y --no-install-recommends ${BUILD_PACKAGES}
RUN ./configure --prefix=/opt/emacs --with-native-compilation
RUN make
RUN make install

FROM ubuntu:22.04

# Install runtime packages
ARG RUNTIME_PACKAGES="\
    tzdata \
    language-pack-en \
    ca-certificates \
    build-essential \
    libgccjit-11-dev \
    sudo \
    curl \
    python3 \
    git \
    "
RUN --mount=type=cache,target=/var/lib/apt/lists,sharing=locked --mount=type=cache,target=/var/cache/apt/archives,sharing=locked \
    apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends ${RUNTIME_PACKAGES}
COPY --from=builder /opt/emacs /opt/emacs

# Add a runttime user
ARG USERNAME=emacs
ARG USER_UID=1000
ARG USER_GID=${USER_UID}
RUN groupadd -g ${USER_GID} ${USERNAME} \
    && useradd -lm -g ${USER_GID} -u ${USER_UID} ${USERNAME} \
    && echo "${USERNAME} ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/${USERNAME} \
    && chmod 0400 /etc/sudoers.d/${USERNAME}

USER ${USERNAME}
WORKDIR /workspace
ENTRYPOINT [ "/opt/emacs/bin/emacs" ]
ENV LANG=en-US.UTF-8
ENV PATH=/opt/emacs/bin:$PATH

COPY --chown=${USERNAME}:${USERNAME} init.el /home/${USERNAME}/.emacs.d/init.el
RUN /opt/emacs/bin/emacs --script /home/${USERNAME}/.emacs.d/init.el
