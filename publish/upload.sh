#!/bin/bash

# Deploy using rsync
rsync \
    --archive \
    --verbose \
    --compress \
    --chown=marcel:www-data \
    --delete \
    --progress \
    /tmp/dot-emacs-publish/ \
    tolkien:/var/www/config.mmk2410.org/
