# syntax=docker/dockerfile:1.2

FROM ubuntu:20.04 AS base

RUN --mount=type=cache,target=/var/lib/apt/lists apt-get update \
    && apt-get install --no-install-recommends -y tzdata language-pack-en \
    && apt-get install --no-install-recommends -y ca-certificates curl \
    && curl -fsSL https://deb.nodesource.com/setup_16.x | bash \
    && apt-get install --no-install-recommends -y emacs sudo git python3 nodejs

FROM base AS builder

WORKDIR /workspace

COPY init.el .
RUN emacs --script init.el

FROM base

ARG USERNAME=emacs
ARG USER_UID=1000
ARG USER_GID=${USER_UID}
RUN groupadd -g ${USER_GID} ${USERNAME} \
    && useradd -lm -g ${USER_GID} -u ${USER_UID} ${USERNAME} \
    && echo "${USERNAME} ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/${USERNAME} \
    && chmod 0400 /etc/sudoers.d/${USERNAME}

COPY --from=builder --chown=${USERNAME}:${USERNAME} /root/.emacs.d /home/${USERNAME}/.emacs.d
COPY --chown=${USERNAME}:${USERNAME} init.el /home/${USERNAME}/.emacs.d

USER ${USERNAME}
WORKDIR /workspace

ENV LANG=en-US.UTF-8
ENTRYPOINT [ "/usr/bin/emacs" ]
