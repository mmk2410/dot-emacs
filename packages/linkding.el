;;; linkding.el --- Linkding Integration -*- lexical-binding: t -*-

;;; Commentary:
;;; Integration with Linkding.

;;; Code:
(require 'json)
(require 'plz)
(require 'shr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Customisable Variables ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom linkding-host ""
  "Hostname of the Linkding instance."
  :type '(string)
  :group 'linkding)

(defcustom linkding-user ""
  "Linkding user name."
  :type '(string)
  :group 'linkding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; API functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun linkding--api-get-bookmarks ()
  "Get bookmarks from Linkding."
  (plz 'get (concat "https://" linkding-host "/api/bookmarks/")
    :headers (linkding--build-headers)
    :as 'json-read))

(defun linkding--api-get-bookmarks-from-bundle (bundle)
  "Get bookmarks from BUNDLE id from Linkding."
  (plz 'get (concat "https://" linkding-host "/api/bookmarks/?bundle=" (number-to-string bundle))
    :headers (linkding--build-headers)
    :as 'json-read))

(defun linkding--api-post-bookmark (url)
  "Add URL as bookmark to Linkding."
  (plz 'post (concat "https://" linkding-host "/api/bookmarks/")
    :headers (linkding--build-headers)
    :body (json-encode `(("url" . ,url)
                         ("title" . "")
                         ("description" . "")
                         ("notes" . "")
                         ("is_archived" . false)
                         ("unread" . false)
                         ("shared" . false)
                         ("tag_names" . [])))
    :as 'string
    :then (lambda (_) (message "URL stored successfully."))
    :else (lambda (_) (message "Failed to store URL."))))

(defun linkding--api-archive-bookmark (id)
  "Archive bookmark by its ID."
  (plz 'post (concat "https://" linkding-host "/api/bookmarks/" (number-to-string id) "/archive/")
    :headers (linkding--build-headers)
    :as 'string
    :then (lambda (_) (message "Bookmark archived successfully."))
    :else (lambda (_) (message "Failed to archive bookmark."))))

(defun linkding--api-get-bundles ()
  "Get bundles from Linkding."
  (plz 'get (concat "https://" linkding-host "/api/bundles/")
    :headers (linkding--build-headers)
    :as 'json-read))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; UI functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun linkding-add-bookmark (url)
  "Add URL as bookmark to Linkding."
  (interactive "sURL: ")
  (linkding--api-post-bookmark url))

(defun linkding-add-bookmark-at-point ()
  "Add URL at point as bookmark to Linkding."
  (interactive)
  (linkding--api-post-bookmark (shr-url-at-point nil)))

(defun linkding--format-bookmark-for-completing-read (bookmark)
  "Format BOOKMARK for displaying in completing read."
  (let ((title (alist-get 'title bookmark))
        (hostname (url-host (url-generic-parse-url (alist-get 'url bookmark))))
        (tags (string-replace
                 "-" " "
                 (string-join (alist-get 'tag_names bookmark) ", "))))
    (if (string-empty-p tags)
        (format "%s  (%s)" title hostname)
      (format "%s  (%s | %s)" title hostname tags))))

(defun linkding--user-select-bookmark (bookmarks)
  "Let the user select a bookmark from BOOKMARKS."
  (let ((bookmarks (seq-map
                    (lambda (bookmark)
                      `(,(linkding--format-bookmark-for-completing-read bookmark) . ,bookmark))
                    bookmarks)))
    (alist-get
     (completing-read "Select a bookmark: " bookmarks)
     bookmarks nil nil #'equal)))

(defun linkding-open-bookmark-eww ()
  "Open a bookmark using Emacs EWW."
  (interactive)
  (eww-browse-url (alist-get
                   'url
                   (linkding--user-select-bookmark
                    (alist-get 'results (linkding--api-get-bookmarks)))) t))

(defun linkding-open-bookmark-generic ()
  "Open a bookmark using default browser."
  (interactive)
  (browse-url (alist-get
                   'url
                   (linkding--user-select-bookmark
                    (alist-get 'results (linkding--api-get-bookmarks))))))

(defun linkding--user-select-bundle ()
  "Let the user select a bookmark bundle."
  (let ((bundles (seq-map
                  (lambda (bundle) `(,(alist-get 'name bundle) ,(alist-get 'id bundle)))
                  (alist-get 'results (linkding--api-get-bundles)))))
    (car (alist-get
          (completing-read "Select a bundle: " bundles)
          bundles nil nil #'equal))))

(defun linkding-open-bundled-bookmark-eww ()
  "Open a bookmark from a bundle using Emacs EWW."
  (interactive)
  (let ((bundle (linkding--user-select-bundle)))
    (eww-browse-url (alist-get
                   'url
                   (linkding--user-select-bookmark
                    (alist-get 'results (linkding--api-get-bookmarks-from-bundle bundle)))) t)))

(defun linkding-open-bundled-bookmark-generic ()
  "Open a bookmark from a bundle using the default browser."
  (interactive)
  (let ((bundle (linkding--user-select-bundle)))
    (browse-url (alist-get
                   'url
                   (linkding--user-select-bookmark
                    (alist-get 'results (linkding--api-get-bookmarks-from-bundle bundle)))))))

(defun linkding-archive-bookmark ()
  "Archive bookmark."
  (interactive)
  (linkding--api-archive-bookmark
   (alist-get 'id
              (linkding--user-select-bookmark
               (alist-get 'results (linkding--api-get-bookmarks))))))

(defun linkding-archive-bundled-bookmark ()
  "Archive bookmark from a bundle."
  (interactive)
  (let ((bundle (linkding--user-select-bundle)))
    (linkding--api-archive-bookmark
     (alist-get 'id
                (linkding--user-select-bookmark
                 (alist-get 'results (linkding--api-get-bookmarks-from-bundle bundle)))))))

(provide 'linkding)
;;; linkding.el ends here
