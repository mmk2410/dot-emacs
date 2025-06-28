;;; edam.el --- Eamcs DAM -*- lexical-binding: t -*-

;;; Commentary:
;;; A simple work-in-progress photography DAM in Emacs

;;; Code:
(require 'dired)

(defun edam-set-rating-file-at-point (rating)
  "Set image RATING."
  (interactive "nRating: ")
  (let ((args (cond ((= rating 1) "-Rating=1 -RatingPercent=1")
                    ((= rating 2) "-Rating=2 -RatingPercent=25")
                    ((= rating 3) "-Rating=3 -RatingPercent=50")
                    ((= rating 4) "-Rating=4 -RatingPercent=75")
                    ((= rating 5) "-Rating=5 -RatingPercent=99")
                    (t (error "Not a valid rating")))))
    (dired-do-shell-command
     (concat "exiftool " args)
     (dired-get-marked-files))))

(provide 'edam)
;;; edam.el ends here
