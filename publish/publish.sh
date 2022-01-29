#!/bin/sh

# emacs -Q --script ./publish.el
emacs -q --batch -l ./publish.el --funcall mmk2410/dot-emacs-publish
