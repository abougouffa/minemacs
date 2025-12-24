;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-12-15
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;; Debug Adapter Protocol for Emacs
(use-package dape
  :ensure t
  :hook
  (dape-compile . kill-buffer) ; Kill compile buffer on build success
  (dape-display-source . pulse-momentary-highlight-one-line) ; Pulse source line (performance hit)
  (dape-stopped . dape-info) ; To display info and/or repl buffers on stopped
  (dape-stopped . dape-repl)
  (dape-start . (lambda () (project-save-some-buffers t))) ; Save buffers on startup, useful for interpreted languages
  :config
  (dape-breakpoint-load) ; Load breakpoints on startup, with laziness
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save) ; Save breakpoints on quit

  ;; When using `gud' and `dape' in the same session, the last to be loaded will
  ;; overwrite the C-x C-a keybinding, this command let us switch between the two
  (defun +dape-gud-switch-keybinding ()
    "Switch binding between GUD and dape."
    (interactive)
    (require 'gud)
    (if (not (equal dape-key-prefix gud-key-prefix))
        (message "No switching is necessary, GUD uses `%s' while Dape uses `%s'" (key-description gud-key-prefix) (key-description dape-key-prefix))
      (let ((type (read-answer "Set to [d]ape or [g]ud? " '(("dape" ?d "Set the keybinding to `dape-global-map'") ("gud" ?g "Set the keybinding to `gud-global-map'")))))
        (if (equal type "dape")
            (global-set-key dape-key-prefix dape-global-map)
          (global-set-key gud-key-prefix gud-global-map))
        (message "Set to %s" type)))))


;; `dape' integration for cortex-debug (https://github.com/Marus/cortex-debug)
(use-package dape-cortex-debug
  :vc (:url "https://github.com/svaante/dape-cortex-debug")
  :after dape
  :demand)


;; A compiler output viewer
(use-package rmsbolt
  :ensure t
  :hook (rmsbolt-mode . (lambda () (when (derived-mode-p 'asm-mode) (flymake-mode-off))))
  :hook (rmsbolt-mode . +rmsbolt-set-command-form-compilaiton-db)
  :custom
  (rmsbolt-asm-format nil) ; don't impose a format, use the tool's defaults
  :config
  (defun +rmsbolt-set-command-form-compilaiton-db ()
    (when-let* ((dir-cmd (+guess-args-from-compilation-db (buffer-file-name))))
      (setq-local rmsbolt-command (cdr dir-cmd)
                  rmsbolt-default-directory (car dir-cmd)))))


;; Compiler Explorer clone (fork of `rmsbolt' optimized for C/C++)
(use-package beardbolt
  :vc (:url "https://github.com/joaotavora/beardbolt")
  :hook (beardbolt--asm-mode . flymake-mode-off)
  :config
  (add-to-list 'beardbolt-languages '(rust-ts-mode beardbolt--rust-setup))
  (defvar-local +beardbolt-default-directory nil)
  (advice-add 'beardbolt--guess-from-ccj :override #'+beardbolt--guess-from-ccj:override-a)
  (advice-add 'beardbolt-compile :around #'+beardbolt--set-compilation-dir:around-a)

  (defun +beardbolt--guess-from-ccj:override-a ()
    (when-let* ((dir-cmd (+guess-args-from-compilation-db (buffer-file-name))))
      (list (cdr dir-cmd) beardbolt-ccj-extra-flags)))

  (defun +beardbolt--set-compilation-dir:around-a (origfn &rest args)
    (let* ((ccj (+compilation-db-get-entry (buffer-file-name)))
           (default-directory (or (plist-get ccj :directory) default-directory)))
      (apply origfn args))))


;; Use "objdump" to display disassembled executable and object files
(use-package objdump-disassemble
  :vc (:url "https://github.com/abougouffa/objdump-disassemble")
  :hook (minemacs-first-file . global-objdump-disassemble-mode))


(provide 'me-debug)

;;; me-debug.el ends here
