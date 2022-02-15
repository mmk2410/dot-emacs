#!/bin/sh

set -euo pipefail

PUBLISH_SCRIPT_PATH=./publish/publish.el
EMACS_CONF_DIR=~/.emacs.d
CURRENT_DIR=$(pwd)

function goBack() {
    cd "$CURRENT_DIR"
}
trap goBack EXIT

cd "$EMACS_CONF_DIR" || exit
emacs -Q --script ./publish/publish.el
