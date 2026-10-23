#!/usr/bin/env bash
# Install emacs-nox and the words of $EXTRA_PACKAGES, called by action.yml.
#
# A slow Azure mirror once took 8 minutes for 18.7 MB (PR #10). The index
# update and the package download are each tried up to 3 times, cut off
# after 40 seconds apiece: 4 minutes at the worst, inside the calling step's
# 5. Only those run under `timeout`: killing dpkg mid-install leaves it
# needing repair. The install reads the downloaded files, so it is quick
# and has no timeout of its own.
set -euo pipefail

retry() {
  local attempt
  for attempt in 1 2 3; do
    sudo timeout 40 "$@" && return 0
    echo "attempt $attempt of '$*' failed" >&2
  done
  return 1
}

packages="emacs-nox ${EXTRA_PACKAGES:-}"

# By default a transient network failure makes update exit 0, so ask for a
# failure to be reported and retried. Emacs needs nothing from the repos
# the runner image adds (packages.microsoft.com), so when the retries are
# spent go on with the lists apt has: the download below is the gate.
retry apt-get -o APT::Update::Error-Mode=any update \
  || echo "warning: apt-get update kept failing; using the existing package lists" >&2
# shellcheck disable=SC2086 # packages is a word list
retry apt-get install --download-only --no-install-recommends -y $packages
# shellcheck disable=SC2086
sudo apt-get install --no-install-recommends -y $packages
