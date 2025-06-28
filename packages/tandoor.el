;;; tandoor.el --- Tandoor Integration -*- lexical-binding: t -*-

;;; Commentary:
;;; Integration with Tandoor.

;;; Code:
(require 'json)
(require 'plz)

(defcustom tandoor-host ""
  "Hostname of the Tandoor instance."
  :type '(string)
  :group 'tandoor)

(defun tandoor--get-api-key ()
  "Get Tandoor API key from auth store."
  (let ((result (auth-source-search :host tandoor-host)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(defun tandoor--build-headers ()
  "Build headers for Tandoor API request."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Bearer " (tandoor--get-api-key)))))

(defun tandoor-shopping-list-add-entry (food amount unit)
  "Add AMOUNT UNIT FOOD to Tandoor shopping list."
  (interactive "sFood to add: \nsAmount: \nsUnit: ")
  (plz 'post (concat "https://" tandoor-host "/api/shopping-list-entry/")
    :headers (tandoor--build-headers)
    :body (json-encode `(("food" ("name" . ,food))
                         ("unit" ("name" . ,unit))
                         ("amount" . ,amount)))
    :as 'string
    :then (lambda (res) (message "Entry added successfully."))
    :else (lambda (res) (message "Failed to add entry."))))

(provide 'tandoor)
;;; tandoor.el ends here
