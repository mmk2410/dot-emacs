;;; tab-bar-helpers.el -*- lexical-binding: t; -*-

(defun mmk2410/tab-bar-new-tab (name func)
  (when (eq nil tab-bar-mode)
    (tab-bar-mode))
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (funcall func))

(defun mmk2410/tab-bar-tab-exists (name)
  (member name
          (mapcar #'(lambda (tab) (alist-get 'name tab))
                  (tab-bar-tabs))))

(defun mmk2410/tab-bar-switch-or-create (name func)
  (if (mmk2410/tab-bar-tab-exists name)
      (tab-bar-switch-to-tab name)
    (mmk2410/tab-bar-new-tab name func)))

(defun mmk2410/tab-bar-run-elfeed ()
  (interactive)
  (mmk2410/tab-bar-switch-or-create "RSS" #'elfeed))

(defun mmk2410/tab-bar-new-named-tab (name)
  (interactive "sTab Name: ")
  (tab-bar-new-tab)
  (tab-bar-rename-tab name))

(defun mmk2410/tab-bar-run-mail ()
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Mail"
   #'(lambda ()
       (mu4e-context-switch :name "Private")
       (mu4e))))

(defun mmk2410/tab-bar-run-irc ()
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "IRC"
   #'(lambda ()
       (mmk2410/erc-connect)
       (sit-for 1)
       (switch-to-buffer "Libera.Chat"))))

(defun mmk2410/tab-bar-run-agenda ()
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Agenda"
   #'(lambda ()
       (org-agenda nil "a"))))

(defun mmk2410/tab-bar-run-journal ()
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Journal"
   #'org-journal-open-current-journal-file))

(defun mmk2410/tab-bar-run-projects ()
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Projects"
   #'(lambda ()
       (find-file "~/org/work.projects.org"))))

(defhydra mmk2410/tab-bar (:color teal :hint nil)
  "
  ^Apps^               ^Helpers^        ^Misc
--^^^^^^---------------------------------------------
  _a_: Agenda          _RET_: Search    _q_: Cancel
  _e_: RSS (Elfeed)    _SPC_: New
  _i_: IRC (erc)       _f_: Previous
  _j_: Journal         _Q_: Close
  _m_: Mail
  _p_: Projects
"
  ("a" mmk2410/tab-bar-run-agenda)
  ("e" mmk2410/tab-bar-run-elfeed)
  ("i" mmk2410/tab-bar-run-irc)
  ("j" mmk2410/tab-bar-run-journal)
  ("m" mmk2410/tab-bar-run-mail)
  ("p" mmk2410/tab-bar-run-projects)
  ("RET" tab-bar-select-tab-by-name)
  ("SPC" mmk2410/tab-bar-new-named-tab)
  ("f" tab-bar-switch-to-recent-tab)
  ("Q" tab-bar-close-tab)
  ("q" nil))

(provide 'mmk2410-tab-bar-helpers)

;;; tab-bar-helpers.el ends here
