# syntax=docker/dockerfile:1.2

FROM ubuntu:20.04 AS base

ARG USERNAME=emacs
ARG USER_UID=1000
ARG USER_GID=${USER_UID}

RUN --mount=type=cache,target=/var/lib/apt/lists apt-get update \
        && apt-get install --no-install-recommends -y tzdata language-pack-en \
        && apt-get install --no-install-recommends -y ca-certificates emacs \
        && groupadd -g ${USER_GID} ${USERNAME} \
        && useradd -lm -g ${USER_GID} -u ${USER_UID} ${USERNAME}

FROM base AS builder

USER ${USERNAME}
WORKDIR /home/${USERNAME}/.emacs.d
COPY init.el .
RUN emacs --batch -l init.el

FROM base

WORKDIR /home/${USERNAME}

COPY --from=builder --chown=${USERNAME}:${USERNAME} /home/${USERNAME}/.emacs.d .emacs.d

USER ${USERNAME}
ENV LANG=en-US.UTF-8
CMD [ "emacs" ]
