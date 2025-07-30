;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((project-vc-name . "MinEmacs")))
 (emacs-lisp-mode . ((time-stamp-pattern . "^;; Last modified: %%$")
                     (time-stamp-format . "%04Y-%02m-%02d")
                     (eval . (and (fboundp 'apheleia-mode) (apheleia-mode 1)))
                     (copyright-query . nil)
                     (eval . (add-hook 'before-save-hook #'copyright-update nil 'local))))
 (text-mode . ((+git-commit-prefix-in-project . conventional))))
