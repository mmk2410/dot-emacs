(setq mmk2410/dot-emacs-publish-publishing-dir "/tmp/dot-emacs-publish/")

(setq mmk2410/dot-emacs-publish-html-head-extra
      (concat "<link rel=\"stylesheet\" href=\"normalize.css\">\n"
              "<link rel=\"stylesheet\" href=\"layout.css\">\n"
              "<link rel=\"stylesheet\" href=\"code.css\">\n"
              "<link rel=\"stylesheet\" href=\"font.css\">"
              "<script defer data-domain=\"config.mmk2410.org\" src=\"https://stats.mmk2410.org/js/plausible.js\"></script>"))

(setq mmk2410/dot-emacs-publish-html-preamble
      (concat "<a id=\"title\" href=\"https://mmk2410.org\">"
              "Marcel Kapfer"
              "</a>"))

(when (file-directory-p mmk2410/dot-emacs-publish-publishing-dir)
  (delete-directory mmk2410/dot-emacs-publish-publishing-dir t))
(mkdir mmk2410/dot-emacs-publish-publishing-dir)

(setq-default load-prefer-newer t)
(setq package-user-dir (expand-file-name "./.packages"))
(package-initialize)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-refresh-contents)
(package-install 'htmlize)
(add-to-list 'load-path "~/.emacs.d/elpa")

(require 'org)
(require 'ox-publish)
(require 'htmlize)

(setq org-html-htmlize-output-type 'css)

(setq org-publish-project-alist
      `(("dot-emacs:org"
         :base-directory "~/.emacs.d"
         :publishing-directory ,mmk2410/dot-emacs-publish-publishing-dir
         :exclude ".*"
         :include ("config.org")
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :html-doctype "html5"
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-head-extra ,mmk2410/dot-emacs-publish-html-head-extra
         :html-html5-fancy t
         :html-preamble ,mmk2410/dot-emacs-publish-html-preamble
         :html-self-link-headlines t
         :html-validation-link nil
         )
        ("dot-emacs:static"
         :base-directory "~/.emacs.d/publish/assets"
         :publishing-directory ,mmk2410/dot-emacs-publish-publishing-dir
         :base-extension "css\\|woff\\|woff2\\|ico"
         :publishing-function org-publish-attachment
         :recursive t)))

(defun mmk2410/dot-emacs-publish-additional ()
  (rename-file
   (concat mmk2410/dot-emacs-publish-publishing-dir "config.html")
   (concat mmk2410/dot-emacs-publish-publishing-dir "index.html")))

(defun mmk2410/dot-emacs-publish ()
  (interactive)
  (org-publish-all t)
  (mmk2410/dot-emacs-publish-additional))