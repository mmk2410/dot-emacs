;;; tab-bar-helpers.el --- Helpers for using tab-bar mode -*- lexical-binding: t; -*-

;;; Commentary:
;;; Personal helpers and shortcuts to work more effectively with tab-bar mode.

;;; Code:

(defun mmk2410/tab-bar-new-tab (name func)
  "Create new tab using FUNC with name NAME."
  (when (eq nil tab-bar-mode)
    (tab-bar-mode))
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (funcall func))

(defun mmk2410/tab-bar-tab-exists (name)
  "Check whether a tab named NAME exists."
  (member name
          (mapcar #'(lambda (tab) (alist-get 'name tab))
                  (tab-bar-tabs))))

(defun mmk2410/tab-bar-switch-or-create (name func)
  "Switch to tab with name NAME or create one using FUNC."
  (if (mmk2410/tab-bar-tab-exists name)
      (tab-bar-switch-to-tab name)
    (mmk2410/tab-bar-new-tab name func)))

(defun mmk2410/tab-bar-new-named-tab (name)
  "Create new tab with named NAME."
  (interactive "sTab Name: ")
  (tab-bar-new-tab)
  (tab-bar-rename-tab name))

(defun mmk2410/tab-bar-run-elfeed ()
  "Switch to or start elfeed."
  (interactive)
  (mmk2410/tab-bar-switch-or-create "RSS" #'elfeed))

(defun mmk2410/tab-bar-run-mail ()
  "Switch to or start mu4e."
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Mail"
   #'(lambda ()
       (mu4e-context-switch :name "Private")
       (mu4e))))

(defun mmk2410/tab-bar-run-irc ()
  "Switch to or start ERC session."
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "IRC"
   #'(lambda ()
       (mmk2410/erc-connect)
       (sit-for 1)
       (switch-to-buffer "Libera.Chat"))))

(defun mmk2410/tab-bar-run-agenda ()
  "Switch to or open agenda tab."
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Agenda"
   #'(lambda ()
       (org-agenda nil "d"))))

(defun mmk2410/tab-bar-run-agenda-journal ()
  "Switch to or open agenda+journal tab."
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Agenda"
   #'(lambda ()
       (org-agenda nil "d")
       (sit-for 1)
       (split-window-horizontally)
       (other-window 1)
       (org-roam-dailies-goto-today))))

(defun mmk2410/tab-bar-run-journal ()
  "Switch to or create org-roam daily journal."
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Journal"
   #'org-roam-dailies-goto-today))

(defun mmk2410/tab-bar-run-mastodon ()
  "Switch to or create a tab running mastodon.el."
  (interactive)
  (mmk2410/tab-bar-switch-or-create "Mastodon" #'mastodon))

(defun mmk2410/tab-bar-run-vterm ()
  "Switch to or create a tab running a shell using vterm."
  (interactive)
  (mmk2410/tab-bar-switch-or-create "term" #'vterm))

(defhydra mmk2410/tab-bar (:color teal :hint nil)
  "
  ^Apps^               ^Org^                     ^Helpers^            ^Misc
--^^^^^^---------------------------------------------------------------------------
  _e_: RSS (Elfeed)    _a_: Agenda               _RET_: Search        _q_: Cancel
  _m_: Mail            _A_: Agenda + Journal     _SPC_: New
  _i_: IRC (erc)       _j_: Journal              _f_: Previous Tab
  _M_: Mastodon                                _Q_: Close Tab
  _t_: vterm

"
  ;; Apps
  ("e" mmk2410/tab-bar-run-elfeed)
  ("m" mmk2410/tab-bar-run-mail)
  ("i" mmk2410/tab-bar-run-irc)
  ("M" mmk2410/tab-bar-run-mastodon)
  ("t" mmk2410/tab-bar-run-vterm)
  ;; Org
  ("a" mmk2410/tab-bar-run-agenda)
  ("A" mmk2410/tab-bar-run-agenda-journal)
  ("j" mmk2410/tab-bar-run-journal)
  ;; Helpers
  ("RET" tab-bar-select-tab-by-name)
  ("SPC" mmk2410/tab-bar-new-named-tab)
  ("f" tab-bar-switch-to-recent-tab)
  ("Q" tab-bar-close-tab)
  ;; Misc
  ("q" nil))

(provide 'mmk2410-tab-bar-helpers)
;;; mmk2410-tab-bar-helpers.el ends here
