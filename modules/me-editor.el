;; -*- lexical-binding: t; -*-

;; Visual Undo
(use-package vundo
  :straight t
  :defer t
  :general
  (me-map "ov" '(vundo :which-key "Visual Undo"))
  :config
  (setq vundo-compact-display t
        vundo-window-max-height 6
        vundo-glyph-alist
        '((selected-node   . ?●)
          (node            . ?○)
          (vertical-stem   . ?│)
          (branch          . ?├)
          (last-branch     . ?╰)
          (horizontal-stem . ?─))))


(use-package undo-fu
  :straight t
  :config
  (with-eval-after-load 'evil
    (evil-set-undo-system 'undo-fu)))


(use-package undo-fu-session
  :straight t
  :after undo-fu
  :config
  (setq undo-fu-session-compression 'zst
        undo-fu-session-directory (expand-file-name "undo-fu-session" minemacs-var-dir))
  (global-undo-fu-session-mode 1))


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

  (when nil
    (with-eval-after-load 'evil-collection
      ;; Make evil-mc cooperate with smartparens better
      (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
        (unless (memq (car sp--mc/cursor-specific-vars) vars)
          (setcdr (assq :default evil-mc-cursor-variables)
                  (append vars sp--mc/cursor-specific-vars)))))))


(when (<= emacs-major-version 28)
  (use-package good-scroll
    :straight t
    :config
    (good-scroll-mode 1)))


(provide 'me-editor)
