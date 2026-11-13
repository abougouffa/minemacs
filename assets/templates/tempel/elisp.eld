;; -*- mode: lisp-data; coding: utf-8 -*-
;; Tempel templates converted from YASnippet snippets in ~/.emacs.d/snippets/emacs-lisp-mode/
;; by yas2tempel on 2026-11-13.

emacs-lisp-mode

;; __elpkg
;; WARNING: Nested fields flattened into field 1
(__elpkg ";;; " (list 'l (file-name-nondirectory buffer-file-name)) (p " --- Description") " -*- lexical-binding: t; -*-" n
 ";;" > n
 ";; Copyright (C) " (list 'l (format-time-string "%Y")) " " (list 'l (format "%s" (or user-full-name ""))) > n
 ";;" > n
 ";; Author: " (list 'l (format "%s" (or user-full-name ""))) " <" (list 'l (format "%s" (or user-mail-address ""))) ">" > n
 ";; Maintainer: " (list 'l (format "%s" (or user-full-name ""))) " <" (list 'l (format "%s" (or user-mail-address ""))) ">" > n
 ";; Created: " (list 'l (format-time-string "%B %d, %Y")) > n
 ";; Modified: " (list 'l (format-time-string "%B %d, %Y")) > n
 ";; Version: 0.0.1" > n
 ";; Keywords: " (list 'l (format "%s" (or (progn (require 'finder) (mapconcat #'symbol-name (mapcar #'car finder-known-keywords) " ")) ""))) > n
 ";; Homepage: https://github.com/" (list 'l (format "%s" (or (cl-find-if-not #'string-empty-p (list (string-trim (shell-command-to-string "git config github.user")) user-login-name)) ""))) "/" (list 'l (file-name-base buffer-file-name)) > n
 ";; Package-Requires: ((emacs \"28.1\"))" > n
 ";;" > n
 ";; This file is not part of GNU Emacs." > n
 ";;" > n
 ";;; Commentary:" > n
 ";;" > n
 ";;  Description" > n
 ";;" > n
 ";;; Code:" > n
 n>
 (list 'l (format "%s" (or % ""))) q n
 n
 "(provide '" (list 'l (file-name-base buffer-file-name)) ")" > n
 ";;; " (list 'l (file-name-nondirectory buffer-file-name)) " ends here" >)

;; meobs
(meobs ";;; " `(p ,(file-name-base (or buffer-file-name "me-package")) f1) ".el --- " (p "Description") " -*- lexical-binding: t; -*-" n
 n
 ";; Copyright (C) 2022-" (list 'l (concat (format-time-string "%Y") "  " user-full-name)) > n
 n
 ";; Author: " (list 'l (format "%s  (rot13 %S)" user-full-name (rot13 user-mail-address))) > n
 ";; Created: " (list 'l (format-time-string "%Y-%m-%d")) > n
 ";; Last modified: " (list 'l (format-time-string "%Y-%m-%d")) > n
 n
 ";;; Commentary:" > n
 n
 ";;; Code:" > n
 n
 q n
 n
 "(provide 'obsolete/" f1 ")" > n
 ";;; " f1 ".el ends here" > n
 )

;; meon
(meon ";;; " `(p ,(file-name-base (or buffer-file-name "me-package")) f1) ".el --- " (p "Description") " -*- lexical-binding: t; -*-" n
 n
 ";; Copyright (C) 2022-" (list 'l (concat (format-time-string "%Y") "  " user-full-name)) > n
 n
 ";; Author: " (list 'l (format "%s  (rot13 %S)" user-full-name (rot13 user-mail-address))) > n
 ";; Created: " (list 'l (format-time-string "%Y-%m-%d")) > n
 ";; Last modified: " (list 'l (format-time-string "%Y-%m-%d")) > n
 n
 ";;; Commentary:" > n
 n
 ";;; Code:" > n
 n
 ";;;###autoload" > n
 "(minemacs-register-on-demand-module '" f1 > n
 "  :auto-mode '())" > n
 n
 q n
 n
 "(provide 'on-demand/" f1 ")" > n
 ";;; " f1 ".el ends here" > n
 )

