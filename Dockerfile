# syntax=docker/dockerfile:1.2

FROM ubuntu:20.04 AS base

ARG USERNAME=emacs
ARG USER_UID=1000
ARG USER_GID=${USER_UID}

RUN --mount=type=cache,target=/var/lib/apt/lists apt-get update \
        && apt-get install --no-install-recommends -y tzdata language-pack-en \
        && apt-get install --no-install-recommends -y ca-certificates emacs git \
        && groupadd -g ${USER_GID} ${USERNAME} \
        && useradd -lm -g ${USER_GID} -u ${USER_UID} ${USERNAME}

FROM base AS builder

WORKDIR /workspace/.emacs.d
COPY init.el .
RUN mkdir local

FROM base

COPY --from=builder --chown=${USERNAME}:${USERNAME} /workspace/.emacs.d /home/${USERNAME}/.emacs.d
VOLUME /home/${USERNAME}/.emacs.d/local

USER ${USERNAME}
ENV LANG=en-US.UTF-8

WORKDIR /workspace
ENTRYPOINT [ "/usr/bin/emacs" ]
