;;; me-tramp-adb-x.el --- Extra tweaks -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-08-05
;; Last modified: 2025-08-05

;;; Commentary:

;;; Code:


;;;###autoload(with-eval-after-load 'tramp-adb (require 'me-tramp-adb-x))

(setq tramp-adb-ls-date-time-regexp
      (rx (= 2 digit) ":" (= 2 digit) (? ":" (= 2 digit) (? "." (+ digit))) (? (+ space) (any "+" "-") (= 4 digit)))
      tramp-adb-ls-date-regexp
      (rx
       blank (regexp tramp-adb-ls-date-year-regexp)
       blank (regexp tramp-adb-ls-date-time-regexp)
       blank)
      tramp-adb-ls-toolbox-regexp
      (rx
       bol (* blank) (group (+ (any ".-" alpha)))
       (? (+ blank) (+ digit))
       (* blank) (group (+ (not blank)))
       (+ blank) (group (+ (not blank)))
       (+ blank) (group (+ digit)
                        blank (regexp tramp-adb-ls-date-time-regexp))
       blank (group (* nonl)) eol))


;; Special case of a minimal Busybox compile
(advice-add
 'tramp-adb-get-ls-command :around
 (satch-defun +tramp-adb-get-ls-command:try-another-command-a (orig vec)
   (let ((ret (funcall orig vec)))
     (if (and (member ret '("ls --color=never --full-time" "ls"))
              (tramp-adb-send-command-and-check vec (concat "ls --color=never --full-time -al " (tramp-get-remote-null-device vec))))
         "ls --color=never --full-time"
       ret))))

(provide 'me-tramp-adb-x)
;;; me-tramp-adb-x.el ends here
