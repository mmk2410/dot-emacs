;;; qwint.el --- Qwint Helpers -*- lexical-binding: t -*-

;;; Commentary:
;;; Helper functions for working on Qwint.

;;; Code:
(require 'json)
(require 'plz)
(require 'org)
(require 'ox-md)

(defun qwint--forgejo-get-api-key ()
  "Get Forgejo API key from auth store."
  (let ((result (auth-source-search :host "git.qwint.de" :user "marcel")))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(defun qwint--forgejo-build-headers ()
  "Build headers for Forgejo API request."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "token " (qwint--forgejo-get-api-key)))))

(defun qwint--forgejo-get-issue (repo issue)
  "Get ISSUE in REPO from the Forgejo API."
  (plz 'get (concat "https://git.qwint.de/api/v1/repos/qwint/" repo "/issues/" issue)
        :headers (qwint--forgejo-build-headers)
        :as 'json-read))

(defun qwint--get-assignee-list-from-issue (issue)
  "Get assignees as list for ISSUE (result of qwint--forgejo-get-issue)."
  (seq-reduce
   (lambda (list assignee) (let ((username (alist-get 'username assignee)))
                             (if (string= list "")
                                 username
                               (concat list ", " username))))
   (alist-get 'assignees issue)
   ""))

(defun qwint-set-assignees-from-forgejo ()
  "Set assignees from Forgejo.

Requires the issue URL to be set as property FORGEJO_URL."
  (interactive)
  (let* ((url (symbol-name (org-property-or-variable-value 'FORGEJO_URL)))
         (url-parts (reverse (split-string url "/")))
         (issue (car url-parts))
         (repo (nth 2 url-parts))
         (assignees (qwint--get-assignee-list-from-issue
                     (qwint--forgejo-get-issue repo issue))))
    (org-set-property "ASSIGNEES" assignees)))

(defun qwint-copy-org-region-as-markdown ()
  "Copy Org region as markdown.

Copied from https://mbork.pl/2021-05-02_Org-mode_to_Markdown_via_the_clipboard."
  (interactive)
  (if (use-region-p)
      (let* ((region (buffer-substring-no-properties
                      (region-beginning)
                      (region-end)))
             (markdown (org-export-string-as region 'md t '(:with-toc nil))))
        (gui-set-selection 'CLIPBOARD markdown))))

(defun qwint-add-link-to-org-item (url)
  "Add URL to org heading and as a property."
  (interactive "sURL: ")
  (let* ((url-parts (reverse (split-string url "/")))
         (title (concat (nth 2 url-parts) "#" (car url-parts ))))
    (org-edit-headline
     (concat (nth 4 (org-heading-components)) " ([[" url "][" title "]])"))
    (org-set-property "FORGEJO_URL" url)
    (org-align-tags)))

(defun qwint-init-org-capture-template ()
  "Initialise and provide org capture template for tasks."
  (add-to-list 'org-capture-templates '("Q" "Qwint Capture Templates"))
  (add-to-list 'org-capture-templates
               `("Qi" "Qwint Issue" entry (here)
                 (file ,(expand-file-name
                         "./packages/qwint/org-capture-template-task.org"
                         (file-name-directory (or load-file-name buffer-file-name))))
                 :empty-lines 1
                 :immediate-finish t))
  (add-to-list 'org-capture-templates
               '("Qn" "Qwint Note" item (here)
                 "- %U %?"
                 :immediate-finish t
                 :empty-lines-after 1)))

(provide 'qwint)
;;; qwint.el ends here
