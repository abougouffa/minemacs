;;; me-editor.el --- Editing stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package tempel
  :straight t
  :custom
  (tempel-trigger-prefix "<") ;; Require trigger prefix before template name when completing.
  (tempel-path (concat minemacs-root-dir "templates/tempel/*.eld"))
  :bind (("M-\"" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         :map tempel-map
         ("TAB" . tempel-next)
         ("<backtab>" . tempel-previous))
  :hook ((prog-mode text-mode) . +tempel-setup-capf-h)
  :hook (prog-mode . tempel-abbrev-mode)
  :config
  (defun +tempel-setup-capf-h ()
    (add-hook 'completion-at-point-functions #'tempel-complete -100 t)))

(use-package tempel-collection
  :straight t
  :after tempel
  :demand t)

(use-package header2
  :straight t
  :hook (prog-mode . auto-make-header)
  :custom
  (header-date-format t)
  (make-header-hook
   '(
     ;; header-mode-line
     header-title
     header-blank
     header-file-name
     header-description
     ;; header-status
     header-author
     header-maintainer
     header-copyright
     header-creation-date
     ;; header-rcs-id
     header-version
     ;; header-pkg-requires
     ;; header-sccs
     header-modification-date
     header-modification-author
     header-update-count
     ;; header-url
     ;; header-doc-url
     ;; header-keywords
     ;; header-compatibility
     header-blank
     ;; header-lib-requires
     header-end-line
     header-commentary
     header-blank
     header-blank
     header-blank
     header-end-line
     ;; header-history
     ;; header-blank
     ;; header-blank
     ;; header-rcs-log
     ;; header-end-line
     header-free-software
     header-code
     header-eof)))

(use-package unicode-fonts
  :straight t
  :hook (minemacs-after-startup . +unicode-fonts-setup)
  :config
  (defun +unicode-fonts-setup ()
    "Prefer the `:unicode-font-family' from `minemacs-fonts'."
    (when-let ((frame (selected-frame)))
      (when (display-multi-font-p frame)
        (with-selected-frame frame
          (when-let ((unicode-font-family (plist-get minemacs-fonts :unicode-font-family)))
            (dolist (unicode-block unicode-fonts-block-font-mapping)
              (push unicode-font-family (cadr unicode-block))))
          (unicode-fonts-setup))))))

(unless (and (>= emacs-major-version 28) (+emacs-features-p 'harfbuzz 'cairo))
  (push 'ligature minemacs-disabled-packages))

(use-package ligature
  :straight t
  :after minemacs-loaded
  :hook (prog-mode . ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all "Cascadia Code" ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :straight t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  ;; Original "\\_<[[:digit:]].*?\\_>"
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :init
  ;; From Doom Emacs, disable expensive navigation features.
  (+setq-hook! smartparens-mode
    sp-navigate-skip-match nil
    sp-navigate-consider-sgml-tags nil)
  :config
  (with-eval-after-load 'evil-mc
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars))))))

;; Default `smartparens' configuration (for example, do not complete a single
;; quote)
(use-package smartparens-config
  :after smartparens
  :demand t)

(use-package expand-region
  :straight t
  :init
  (+vmap! "v" #'er/expand-region))

(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  ;; Pulse for evil commands
  (goggles-define undo primitive-undo evil-undo)
  (goggles-define yank yank yank-pop evil-yank evil-yank-line)
  (goggles-define kill kill-region)
  (goggles-define delete delete-region evil-delete evil-delete-line))

(use-package drag-stuff
  :straight t
  :init
  (keymap-global-set "M-<up>" 'drag-stuff-up)
  (keymap-global-set "M-<down>" 'drag-stuff-down)
  (keymap-global-set "M-<left>" 'drag-stuff-left)
  (keymap-global-set "M-<right>" 'drag-stuff-right))


(provide 'me-editor)

;;; me-editor.el ends here
