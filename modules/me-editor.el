;;; me-editor.el --- Editing stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package yasnippet
  :straight t
  :hook (minemacs-lazy . yas-global-mode)
  :init
  (defvar yas-verbosity 2)
  :custom
  (yas-snippet-dirs nil)
  (yas-triggers-in-field t))

(use-package cape-yasnippet
  :straight (:host github :repo "elken/cape-yasnippet")
  :after cape yasnippet
  :config
  ;; To avoid auto-expanding snippets
  (plist-put cape-yasnippet--properties :exit-function #'always)
  (defun +cape-yasnippet--setup-h ()
    (when (and (bound-and-true-p yas-minor-mode))
      (add-to-list 'completion-at-point-functions #'cape-yasnippet t)))

  (dolist (mode-hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook mode-hook #'+cape-yasnippet--setup-h)))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package doom-snippets
  :straight (:host github :repo "hlissner/doom-snippets" :files ("*.el" "*"))
  :after yasnippet)

(use-package license-snippets
  :straight t
  :after yasnippet
  :config
  (license-snippets-init))

(use-package pcache
  :straight t
  :defer t
  :init
  (setq pcache-directory (concat minemacs-local-dir "pcache/")))

(use-package unicode-fonts
  :straight t
  :hook (minemacs-after-startup . +unicode-fonts-initialize)
  :config
  (defun +unicode-fonts-initialize ()
    "Set up `unicode-fonts' to eventually run; accommodating the daemon, if necessary."
    (if (display-graphic-p)
        (+unicode-fonts-setup-font (selected-frame))
      (add-hook 'after-make-frame-functions #'+unicode-fonts-setup-font)))

  (defun +unicode-fonts-setup-font (&optional frame)
    "Initialize `unicode-fonts', if in a GUI session.
If doom-unicode-font is set, add it as preferred font for all unicode blocks."
    (when (and frame (display-graphic-p frame))
      (with-selected-frame frame
        (when-let ((unicode-font-family (plist-get minemacs-fonts :unicode-font-family)))
          (dolist (unicode-block unicode-fonts-block-font-mapping)
            (push unicode-font-family (cadr unicode-block))))
        (unicode-fonts-setup)))))

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
