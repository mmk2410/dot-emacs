;;; linkding.el --- Linkding Integration -*- lexical-binding: t -*-

;;; Commentary:
;;; Integration with Linkding.

;;; Code:
(require 'json)
(require 'plz)

(defcustom linkding-host ""
  "Hostname of the Linkding instance."
  :type '(string)
  :group 'linkding)

(defcustom linkding-user ""
  "Linkding user name."
  :type '(string)
  :group 'linkding)

(defun linkding--get-api-key ()
  "Get Linkding API key from auth store."
  (let ((result (auth-source-search :host linkding-host :user linkding-user)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(defun linkding--build-headers ()
  "Build headers for Linkding Lab API request."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Token " (linkding--get-api-key)))))

(defun linkding-add-bookmark (url)
  "Add URL as bookmark to Linkding."
  (interactive "sURL: ")
  (plz 'post (concat "https://" linkding-host "/api/bookmarks/")
    :headers (linkding--build-headers)
    :body (json-encode `(("url" . ,url)
                         ("title" . "")
                         ("description" . "")
                         ("notes" . "")
                         ("is_archived" . false)
                         ("unread" . false)
                         ("shared" . false)
                         ("tag_names" . [])))))

(provide 'linkding)
;;; linkding.el ends here
