;;; me-embedded.el --- Embedded systems stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Emacs package with utilities for embedded development with OpenOCD
(use-package embed
  :straight (:host github :repo "xal-0/embed-el"))


;; A set of Emacs modes for various Yocto/Bitbake file formats
(use-package bitbake
  :straight (bitbake-modes :host bitbucket :repo "olanilsson/bitbake-modes")
  :hook (bitbake-mode . bitbake-electric-mode)
  :config
  (require 'bitbake-insert)
  (require 'bitbake-electric)
  :init
  (defun +bitbake-poky-sources (build-dir &optional include-native)
    "Get all source directories for BUILD-DIR. Optionally INCLUDE-NATIVE."
    ;; From the build-dir "yocto-ws/build-MACHINE/", this will extract all source
    ;; directories of these formats:
    ;; yocto-ws/build-MACHINE/tmp/work/aarch64-rdk-linux/procps/3.3.16-r0/git/
    ;; yocto-ws/build-MACHINE/tmp/work/aarch64-rdk-linux/ppp/2.4.7-r0/ppp-2.4.7/
    (let* (result
           (base-dir (expand-file-name "tmp/work/" build-dir))
           (arch-dirs (seq-filter #'file-directory-p (directory-files base-dir t "[^.][^.]?\\'"))))
      (dolist (arch-dir arch-dirs)
        (let* ((package-dirs (directory-files arch-dir t "[^.][^.]?\\'"))
               (package-dirs (if include-native package-dirs (seq-filter (lambda (dir) (not (string-suffix-p "-native" dir))) package-dirs))))
          (dolist (package-dir package-dirs)
            (let ((ver-dirs (directory-files package-dir t "[^.][^.]?\\'")))
              (dolist (ver-dir ver-dirs)
                (let* ((ver (string-trim-right (file-name-nondirectory (directory-file-name ver-dir)) "-r[[:digit:]]*$"))
                       (dir-git (expand-file-name "git/" ver-dir))
                       (dir-non-git (expand-file-name (format "%s-%s/" (file-name-nondirectory (directory-file-name package-dir)) ver) ver-dir)))
                  (cond ((file-directory-p dir-git)
                         (push dir-git result))
                        ((file-directory-p dir-non-git)
                         (push dir-non-git result)))))))))
      result))

  (defun +widget-choose-completion (prompt items &optional _event)
    "Same interface as `widget-choose' but uses `completing-read' under the hood."
    (let ((choice (completing-read (format "%s: " prompt) (mapcar #'car items))))
      (alist-get choice items nil nil #'equal)))

  ;; `bitbake' uses `widget-choose' to choose, but I prefer `completing-read',
  ;; so lets overwrite it!
  (satch-advice-add
   '(bitbake-recipe-build-dir bitbake-recipe-build-dir-dired) :around
   (satch-defun +widget-choose--use-completion-read (fn &rest args)
     (cl-letf (((symbol-function 'widget-choose) #'+widget-choose-completion))
       (apply fn args))))

  (defun +bitbake-insert-poky-sources (build-dir)
    "Insert poky source directories for BUILD-DIR."
    (interactive "DSelect the build directory: ")
    (insert (string-join (+bitbake-poky-sources build-dir) "\n"))))


;; A `treesit'-based Bitbake major mode
(use-package bitbake-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter))


;; PlatformIO integration for Emacs
(push 'projectile straight-built-in-pseudo-packages)

(use-package platformio-mode
  :straight t)

(cl-callf2 remove 'projectile straight-built-in-pseudo-packages)


;; Quickly jump to Intel's x86 documentation from Emacs
(use-package x86-lookup
  :straight t
  :custom
  ;; Get manual from intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
  (x86-lookup-pdf (concat minemacs-local-dir "intel-64-and-ia32-volumes-1234.pdf"))
  (x86-lookup-cache-directory (concat minemacs-cache-dir "x86-lookup/"))
  :config
  (unless (file-exists-p x86-lookup-pdf)
    (url-copy-file "https://cdrdv2.intel.com/v1/dl/getContent/671200" x86-lookup-pdf t)))


(provide 'me-embedded)

;;; me-embedded.el ends here
