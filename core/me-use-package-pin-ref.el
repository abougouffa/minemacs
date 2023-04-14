;; me-use-package-pin-ref.el --- Extend use-package to allow straight-x package pinning -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;; Add support for pinning versions of individual packages. See:
;; github.com/radian-software/straight.el#how-do-i-pin-package-versions-or-use-only-tagged-releases
(with-eval-after-load 'straight
  ;; Add a profile (and lockfile) for stable package revisions.
  (add-to-list 'straight-profiles '(pinned . "pinned.el"))
  (require 'straight-x))

;; Allow pinning versions from `use-package' using the `:pin-ref' keyword
(with-eval-after-load 'use-package
  (add-to-list 'use-package-keywords :pin-ref)

  (defun use-package-normalize/:pin-ref (name-symbol keyword args)
    (use-package-only-one (symbol-name keyword) args
      (lambda (label arg)
        (cond
         ((stringp arg) arg)
         ((symbolp arg) (symbol-name arg))
         (t (use-package-error ":pin-ref wants a commit hash or a ref."))))))

  (defun use-package-handler/:pin-ref (name-symbol keyword ref rest state)
    (let ((body (use-package-process-keywords name-symbol rest state)))
      (if (null ref)
          body
        (use-package-concat
         `((let ((straight-current-profile 'pinned))
            (push '(,(symbol-name name-symbol) . ,ref) straight-x-pinned-packages)
            ,(macroexp-progn body))))))))

(provide 'me-use-package-pin-ref)
