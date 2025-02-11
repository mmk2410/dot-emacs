;;; init.el --- My personal Emacs configuration

;; Source:  https://gitlab.com/mmk2410/dot-emacs
;; Authors: Marcel Kapfer
;; Created: 2020-02-03 19:26, 2025-02-11 19:12
;; License: This file is licensed under the GPLv3 License

;; This file only loads and evaluates my Emacs configuration
;; kept in config.org using Org Babel.
;; And it counts startup time.

(message "Welcome to Emacs, starting up now.")

(defvar mmk2410/init-el-start-time (current-time))

(require 'package)
(require 'org)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(message "Total init.el load time: %.2fs" (float-time (time-subtract (current-time) mmk2410/init-el-start-time)))
