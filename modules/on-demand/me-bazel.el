;;; me-bazel.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-06-04
;; Last modified: 2025-06-04

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-bazel
  :auto-mode '(("/\\(?:\\(?:bazel\\)?\\.bazelrc\\)\\'" . bazelrc-mode)
               ("/Android\\.bp\\'" . bazel-build-mode)
               ("/\\(?:BUILD\\(?:\\.bazel\\)?\\)\\'" . bazel-build-mode)
               ("/MODULE\\.bazel\\'" . bazel-module-mode)
               ("/.+\\.bzl\\'" . bazel-starlark-mode)
               ("/\\.bazelignore\\'" . bazelignore-mode)
               ("/\\.bazeliskrc\\'" . bazeliskrc-mode)
               ("/\\(?:WORKSPACE\\(?:\\.b\\(?:azel\\|zlmod\\)\\)?\\)\\'" . bazel-workspace-mode)))


(use-package bazel
  :straight t
  :mode ("Android\\.bp\\'" . bazel-mode))


(provide 'on-demand/me-bazel)
;;; me-bazel.el ends here
