;;; scribbles.el --- Scribbles Integration -*- lexical-binding: t -*-

;;; Commentary:
;;; Integration with my Scribbles microblog on mmk2410.org

;;; Code:
(require 'json)
(require 'plz)

(defun scribbles--get-api-key ()
  "Get Scribbles API key from auth store."
  (let ((result (auth-source-search :host "mmk2410.org" :user "scribbles")))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(defun scribbles--build-headers ()
  "Build headers for Scribbles Lab API request."
  `(("Content-Type" . "application/json")
    ("Api-Key" . ,(scribbles--get-api-key))))

(defun scribbles-post (message)
  "Post MESSAGE to Scribbles."
  (interactive "sScribble: ")
  (plz 'post "https://mmk2410.org/my/api/v1/scribble"
    :headers (scribbles--build-headers)
    :body (json-encode `(("text" . ,message)))))

;;; scribbles.el ends here
