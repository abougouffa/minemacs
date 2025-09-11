;;; me-bitbake.el --- Interface for ADB commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-22
;; Last modified: 2025-09-11

;;; Commentary:

;;; Code:


;;;###autoload
(defun +bitbake-poky-sources (build-dir &optional include-native)
  "Get all source directories for BUILD-DIR. Optionally INCLUDE-NATIVE."
  ;; From the build-dir "yocto-ws/build-MACHINE/", this will extract all source
  ;; directories of these formats:
  ;; yocto-ws/build-MACHINE/tmp/work/aarch64-rdk-linux/procps/3.3.16-r0/git/
  ;; yocto-ws/build-MACHINE/tmp/work/aarch64-rdk-linux/ppp/2.4.7-r0/ppp-2.4.7/
  (let* (result
         (base-dir (expand-file-name "tmp/work/" build-dir))
         (arch-dirs (seq-filter #'file-directory-p (directory-files base-dir t directory-files-no-dot-files-regexp))))
    (dolist (arch-dir arch-dirs)
      (let* ((package-dirs (directory-files arch-dir t directory-files-no-dot-files-regexp))
             (package-dirs (if include-native package-dirs (seq-filter (lambda (dir) (not (string-suffix-p "-native" dir))) package-dirs))))
        (dolist (package-dir package-dirs)
          (let ((ver-dirs (directory-files package-dir t directory-files-no-dot-files-regexp)))
            (dolist (ver-dir ver-dirs)
              (let* ((ver (string-trim-right (file-name-nondirectory (directory-file-name ver-dir)) "-r[[:digit:]]*$"))
                     (dir-git (expand-file-name "git/" ver-dir))
                     (dir-non-git (expand-file-name (format "%s-%s/" (file-name-nondirectory (directory-file-name package-dir)) ver) ver-dir)))
                (cond ((file-directory-p dir-git)
                       (push dir-git result))
                      ((file-directory-p dir-non-git)
                       (push dir-non-git result)))))))))
    result))

;;;###autoload
(defun +bitbake-insert-poky-sources (build-dir)
  "Insert poky source directories for BUILD-DIR."
  (interactive "DSelect the build directory: ")
  (insert (string-join (+bitbake-poky-sources build-dir) "\n")))


(provide 'me-bitbake)
;;; me-bitbake.el ends here
