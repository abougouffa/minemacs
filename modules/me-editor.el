;; -*- lexical-binding: t; -*-

;; Visual Undo
(use-package vundo
  :straight t
  :defer t
  :general
  (me-map "ov" '(vundo :which-key "Visual Undo"))
  :custom
  (vundo-compact-display t)
  (vundo-window-max-height 6)
  (vundo-glyph-alist
   '((selected-node   . ?●)
     (node            . ?○)
     (vertical-stem   . ?│)
     (branch          . ?├)
     (last-branch     . ?╰)
     (horizontal-stem . ?─))))


(use-package undo-fu
  :straight t
  :after minemacs-loaded
  :config
  (with-eval-after-load 'evil
    (evil-set-undo-system 'undo-fu)))


(use-package undo-fu-session
  :straight t
  :after undo-fu
  :custom
  (undo-fu-session-compression 'zst)
  (undo-fu-session-directory (expand-file-name "undo-fu-session" minemacs-local-dir))
  :config
  (global-undo-fu-session-mode 1))


(use-package tempel
  :straight t
  :custom
  (tempel-trigger-prefix "<") ;; Require trigger prefix before template name when completing.
  (tempel-path (expand-file-name "templates/*.eld" minemacs-root-dir))
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  :config
  (global-tempel-abbrev-mode))


(use-package unicode-fonts
  :straight t
  :after minemacs-loaded
  :config
  (unicode-fonts-setup))


(use-package ligature
  :straight t
  :after minemacs-loaded
  :hook (prog-mode . ligature-mode)
  :when (and (>= emacs-major-version 28) feat/harfbuzz feat/cairo)
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
  :after minemacs-loaded
  :config
  (global-page-break-lines-mode))


(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package highlight-numbers
  :straight t
  :hook (prog-mode . highlight-numbers-mode))


(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :hook (text-mode . smartparens-mode)
  :config
  (with-eval-after-load 'evil-mc
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars))))))


;; Default `smartparens' configuration (example, do not complete single quote)
(use-package smartparens-config
  :after smartparens)


(use-package hideshowvis
  :hook (prog-mode . hideshowvis-symbols))


(when (<= emacs-major-version 28)
  (use-package good-scroll
    :straight t
    :config
    (good-scroll-mode 1)))


(provide 'me-editor)
