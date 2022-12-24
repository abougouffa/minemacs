;;; me-editor.el --- Editing stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package yasnippet
  :straight t
  :defer t
  :init
  (defvar yas-verbosity 2))

(use-package cape-yasnippet
  :straight (:host github :repo "elken/cape-yasnippet")
  :hook ((prog-mode org-mode markdown-mode latex-mode tex-mode TeX-mode LaTeX-mode) . +cape-yasnippet--setup-h)
  :defines +cape-yasnippet--setup-h
  :config
  (defun +cape-yasnippet--setup-h ()
    (yas-minor-mode 1)
    (setq-local completion-at-point-functions
                (cons #'cape-yasnippet
                      completion-at-point-functions))))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package unicode-fonts
  :straight t
  :after minemacs-loaded
  :config
  (unicode-fonts-setup)

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
