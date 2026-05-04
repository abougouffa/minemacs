;;; me-spellcheck.el --- Spell check mode, use `jinx' or fallback to `flyspell' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2026-05-04
;; Last modified: 2026-05-04

;;; Commentary:

;;; Code:

;;;###autoload
(define-minor-mode +spellcheck-mode
  "Spell checking mode, with ARG.
Based on `jinx-mode' if available. Falls back to the built-in
`flyspell-mode'."
  :init-value nil
  (let ((arg (if +spellcheck-mode 1 -1)))
    (if (and (fboundp 'jinx-mode) (+jinx-load-module))
        (jinx-mode arg)
      (flyspell-mode arg))))

;;;###autoload
(defun +spellcheck-correct ()
  "Correct word at point."
  (interactive)
  (cond ((bound-and-true-p jinx-mode)
         (call-interactively #'jinx-correct))
        ((fboundp 'flyspell-correct-at-point)
         (call-interactively #'flyspell-correct-at-point))
        (t (call-interactively #'ispell-word))))

;;;###autoload
(progn
  (add-hook 'text-mode-hook #'+spellcheck-mode)
  (with-eval-after-load 'git-commit
    (add-hook 'git-commit-mode-hook #'+spellcheck-mode)))



(provide 'me-spellcheck)
;;; me-spellcheck.el ends here
