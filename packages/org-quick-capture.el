;;; org-quick-capture.el --- Open a org capture frame and close it directly afterwards. -*- lexical-binding: t; -*-

;;; Commentary:

;;; The code is grabbed from Reddit
;;; (https://www.reddit.com/r/emacs/comments/74gkeq/system_wide_org_capture/)
;;; and was written by u/lebitso (https://www.reddit.com/user/lebitso)
;;; with the help of an anoymous other user.

;;; Code:

(require 'org)
(require 'org-capture)

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame."
  (if (equal "Org Capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame when finished."
  (when (and (equal "Org Capture" (frame-parameter nil 'name))
	     (not (eq this-command 'org-capture-refile)))
    (delete-frame)))

(defadvice org-capture-refile
  (after delete-capture-frame activate)
  "Advise org-refile to close the frame when finished."
  (delete-frame))

(defun org-quick-capture ()
  "Run 'org-capture' in an own capture frame."
  (select-frame-by-name "Org Capture")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture))

(provide 'org-quick-capture)
;;; org-quick-capture.el ends here
