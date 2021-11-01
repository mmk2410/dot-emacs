;;; init.el --- My personal Emacs configuration

;; Source:  https://gitlab.com/mmk2410/dot-emacs
;; Authors: Marcel Kapfer
;; Created: 2020-02-03 19:26
;; License: This file is licensed under the GPLv3 License

;; This file only loads and evaluates my Emacs configuration
;; kept in config.org using Org Babel.
;; And it counts startup time.

;; Little welcome message
(message "Welcome to Emacs, starting now up.")

;; Save the time when init.el was started.
(defvar mmk2410/init-el-start-time (current-time))

;; Enable package management.
(package-initialize)

;; First add the required package archives and initialize it.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Configure package management basics.
;; Prefer newer packages.
(setq-default load-prefer-newer t)
;; Don't load packages automatically on startup.
(setq-default package-enable-at-startup nil)

;; Install / enable use-package for handling following dependencies.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Always ensure that packages are installed on the current system.
(setq use-package-always-ensure t)

;; Use/load latest Org mode
(use-package org
  :pin gnu)

;; Load config.org file with org-babel
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;; Message the total startup time
(message "Total init.el load time: %.2fs" (float-time (time-subtract (current-time) mmk2410/init-el-start-time)))
