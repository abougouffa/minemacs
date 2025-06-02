;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Copyright Leo Butler (leo.butler@umanitoba.ca) 2020
;; Released under the terms of GPLv3+
;;
;; Usage: in *ielm* execute
;; (load "rtest-imaxima.el")
;; (imaxima-rtests :program "/path/to/maxima/script" :lisps '(list of enabled lisps))
;;
;; Description:
;; imaxima-run-test		: batches a single regression test file using a single lisp
;; imaxima-run-test-in-shell	: launches a fresh emacs session, sets the imaxima-maxima-program, loads this file into the session and then runs imaxima-run-test
;; imaxima-rtests		: loops over a list of regression test files and lisps and calls imaxima-run-test-in-shell on each pair (file lisp)

(require 'imaxima)

(let ((imaxima-enabled-lisps '(gcl ecl sbcl clisp))
      (imaxima-rtest-files '("./rtest_imaxima.mac"))
      (this-file "./rtest-imaxima.el"))
  (cl-defun imaxima-run-test
      (file lisp)
    (let* ((kill-buffer-query-functions '())
	   (imaxima-maxima-options (format "%s --lisp=%s" imaxima-maxima-options lisp))
	   (imaxima-use-maxima-mode-flag nil)
	   (rtest-cmd (format "batch(\"%s\",'test);\n" file))
	   (do-it (lambda ()
		    (comint-send-string (get-buffer-process "*imaxima*") rtest-cmd))))
      (message imaxima-maxima-options)
      (message imaxima-maxima-program)
      (add-hook 'imaxima-startup-hook do-it t)
      (save-excursion (imaxima))
      (remove-hook 'imaxima-startup-hook do-it)
      ))
  (cl-defun imaxima-run-test-in-shell (file lisp)
    (let ((cmd (format "emacs --no-desktop --execute='(progn (defvar imaxima-maxima-program \"%s\") (load \"%s\") (imaxima-run-test \"%s\" \"%s\"))'" imaxima-maxima-program this-file file lisp))
	  (out (format "*rtest-imaxima-%s*" lisp))
	  (err (format "*rtest-imaxima-%s-err*" lisp)))
    (async-shell-command cmd out err)))
  (cl-defun imaxima-rtests
      (&key ((:program imaxima-maxima-program)) (files imaxima-rtest-files) (lisps imaxima-enabled-lisps))
    (mapcar #'(lambda (lisp) (mapcar #'(lambda (file) (imaxima-run-test-in-shell file lisp)) files)) lisps))
  )

; end of rtest-imaxima.el
