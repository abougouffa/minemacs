;;; me-editor.el --- Editing stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package yasnippet
  :straight t
  :hook (minemacs-lazy . yas-global-mode)
  :init
  (defvar yas-verbosity 2)
  :custom
  (yas-triggers-in-field t))

(use-package cape-yasnippet
  :straight (:host github :repo "elken/cape-yasnippet")
  :hook ((prog-mode text-mode conf-mode) . +cape-yasnippet--setup-h)
  :after cape yasnippet
  :defines +cape-yasnippet--setup-h
  :config
  ;; To avoid auto-expanding snippets
  (plist-put cape-yasnippet--properties :exit-function #'always)
  (defvar-local +capf--list nil)
  (defun +cape-yasnippet--setup-h ()
    (run-with-timer
     5 nil ;; give some time to other hooks, useful when adding backends to `completion-at-point-functions'
     (lambda ()
       (when (and (bound-and-true-p yas-minor-mode)
                  (not (compiled-function-p completion-at-point-functions)))
         (setq-local
          +capf--list (seq-filter #'functionp ;; to filter the potential `t' member
                                  (append
                                   '(cape-yasnippet) completion-at-point-functions))
          completion-at-point-functions (apply #'cape-super-capf +capf--list)))))))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package unicode-fonts
  :straight t
  :hook (minemacs-after-startup . unicode-fonts-setup)
  :config
  (when (daemonp)
    (add-hook
     'server-after-make-frame-hook
     (defun +unicode-fonts--setup-once-h ()
       (when (display-graphic-p)
         (unicode-fonts-setup)
         (remove-hook
          'server-after-make-frame-hook
          #'+unicode-fonts--setup-once-h))))))

(use-package ligature
  :straight t
  :after minemacs-loaded
  :hook (prog-mode . ligature-mode)
  :when (and (>= emacs-major-version 28) (+emacs-features-p 'harfbuzz 'cairo))
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                "\\\\" "://")))

(use-package page-break-lines
  :straight t
  :hook ((prog-mode text-mode) . page-break-lines-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :straight t
  :hook (prog-mode . highlight-numbers-mode))

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :config
  ;; Default `smartparens' configuration (example, do not complete single quote)
  (require 'smartparens-config)
  (with-eval-after-load 'evil-mc
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars))))))

(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  ;; Pulse for evil commands
  (goggles-define undo primitive-undo evil-undo)
  (goggles-define yank yank yank-pop evil-yank evil-yank-line)
  (goggles-define kill kill-region)
  (goggles-define delete delete-region evil-delete evil-delete-line))

(provide 'me-editor)
