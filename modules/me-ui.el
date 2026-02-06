;;; me-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-20
;; Last modified: 2026-02-06

;;; Commentary:

;;; Code:

;; Nerd Font icons for Emacs
(use-package nerd-icons
  :straight t
  :hook (minemacs-build-functions . nerd-icons-install-fonts)
  :init
  ;; Ensure installing the font
  (when (and (display-graphic-p) (not (+font-installed-p "Symbols Nerd Font Mono")))
    (nerd-icons-install-fonts t))
  :config
  ;; Show .m files as Matlab/Octave files instead of Objective-C
  (setcdr (assoc "m" nerd-icons-extension-icon-alist) '(nerd-icons-devicon "nf-dev-matlab" :face nerd-icons-orange)))


;; A megapack of themes for Emacs
(use-package doom-themes
  :straight t
  :config
  (with-eval-after-load 'org
    (doom-themes-org-config))
  ;; Enable blinking modeline on errors (`visible-bell')
  (+with-delayed-1! (doom-themes-visual-bell-config))

  ;; BUG: Fix face-inheritance cycle doomemacs/themes#875
  (setcdr (assoc 'gnus-group-news-low-empty doom-themes-base-faces) '(:inherit 'gnus-group-mail-1-empty :weight 'normal)))


;; Highly legible minimalist themes with precise typography
(use-package doric-themes
  :straight t)


;; Vim-like tab bar
(use-package vim-tab-bar
  :straight t
  :hook
  (minemacs-after-startup . vim-tab-bar-mode)
  (server-after-make-frame . vim-tab-bar--apply)
  :custom
  (vim-tab-bar-show-groups t))


;; Light, modern and opinionated mode-line for MinEmacs
(use-package minemacs-modeline
  :init
  (minemacs-modeline-mode 1))


;; Integrate `nerd-icons' with `ibuffer'
(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; Integrate `nerd-icons' with `archive-mode', `tar-mode', `dired-mode', and `ztree'
(use-package nerd-icons-multimodal
  :straight (:host github :repo "abougouffa/nerd-icons-multimodal")
  :hook ((archive-mode tar-mode dired-mode ztree-mode vc-dir-mode) . nerd-icons-multimodal-mode))


;; Extra font lock rules for a more colourful `dired'
(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode)
  :config
  (cl-callf append diredfl-compressed-extensions '(".zst" ".rar" ".7z" ".cab" ".arc" ".zoo")))


;; Format buffers visually without modification
(use-package virtual-format
  :straight (:host github :repo "abougouffa/virtual-format"))


;; Display typographical ligatures in major modes
(use-package ligature
  :straight t
  :when (and (featurep 'feat/harfbuzz) (featurep 'feat/cairo) (version<= "1.16.0" cairo-version-string))
  :hook
  (prog-mode . ligature-mode)
  (minemacs-after-setup-fonts . ligature-generate-ligatures)
  :config
  ;; A fine-tuned list of per-language ligatures, constructed from:
  ;; - Iosevka: https://typeof.net/Iosevka/customizer
  ;; - Fira Code: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
  ;; - Cascadia Code: https://github.com/microsoft/cascadia-code/wiki/Coding-ligature-coverage
  (defvar +ligature-common-prog
    `("<<" ">>" ">>=" "<<=" "<=" ">=" "::" ":::" "..=" "::<" "==" ;; "<<<" ">>>" buggy
      "*=" "+=" "<|" "<|>" "|>" "++" "+++" "&&" "||" "/=" "--" "#!" "::="
      "#[" "]#" "{|" "|}" "__"))

  (defvar +ligature-c-like ; C, C++, C#, Java, Rust, JS, PHP, Go, V
    `("!=" "<>" "/*" "*/" "//" "///" "^=" "|=" "?." "??" "<~>"))

  (defvar +ligature-html ; HTML, XML, JS, PHP
    `("</" "</>" "/>" "<!--" "<!---" "www"))

  (defvar +ligature-brackets ; Ruby, PHP, Julia, ML, Haskell, Raku, Dafny, Swift, Idris, PHP
    `("<>" "<:" ":=" "*+" "<*" "<*>" "*>" "<." "<.>" ".>" "+*" "=*"
      "=:" ":>" "(*" "*)" "/*" "*/"))

  (defvar +ligature-js `(,@+ligature-c-like ,@+ligature-html "!==" "!===" "==="))

  (defvar +ligature-lisp `(";;" ";;;"))

  (defvar +ligature-markdown
    `("##" "###" "####" "#####" "######" "--" "---" "----" "-----" "------"))

  (defvar +ligature-functional ; ML, Ocaml, F#, Idris, Coq, Haskell, Elm, PureScript
    `(,@+ligature-brackets "~~" "~-" "<>" "\\/" "/\\" "|-" "-|" "[|" "|]" "%%" "<$" "<$>" "$>" "=/="))

  (defvar +ligature-arrows
    `("<-" "->" "<<-" "->>" "<--" "-->" "<---" "--->"
      "=>" "<==" "==>" "<===" "===>" "<<=" "=>>" "<<==" "==>>" "<->" "<=>"
      "<~~" "~~>" "<-->" "<--->" "<---->" "<==>" "<===>" "<====>"))

  (defvar +ligature-arrows-extra
    '("-<<" "-<" "-<-" "->-" ">-" ">>-" "=<<" "=<" "=<=" "=>="
      "<<==>>" "|-|-|" "|=|=|" "/=/" "=<<=" "=>>=" "-<<-" "->>-" "||-" "-||"
      "<=//" "//=>" "<=|" "|=>" "<-|" "|->" "<-<<" ">>->" "<=<<" ">>=>"
      "__|__" "/==/==/" "//==//==//" "|==|==|" "||==||==||" "<==<==<" ">==>==>"
      "<<==<<==<<" ">>==>>==>>" "|--|--|" "||--||--||" "<--<--<" ">-->-->"
      "<<--<<--<<" ">>-->>-->>"))

  (ligature-set-ligatures 't '("ff" "ffi" "Fl" "Tl" "fi" "fj" "fl" "ft" "www"))
  (ligature-set-ligatures '(prog-mode conf-mode) `(,@+ligature-common-prog ,@+ligature-arrows ,@+ligature-arrows-extra))
  (ligature-set-ligatures '(text-mode) `(,@+ligature-arrows ,@+ligature-arrows-extra))
  (ligature-set-ligatures '(js-mode typescript-mode typescript-ts-mode php-ts-mode php-mode) +ligature-js)
  (ligature-set-ligatures '(julia-mode julia-ts-mode ess-julia-mode ruby-mode ruby-ts-mode php-mode) +ligature-brackets)
  (ligature-set-ligatures '(markdown-mode markdown-ts-mode) +ligature-markdown)
  (ligature-set-ligatures '(html-mode nxml-mode) +ligature-html)
  (ligature-set-ligatures
   '( emacs-lisp-mode lisp-mode lisp-data-mode common-lisp-mode
      hy-mode scheme-mode geiser-mode)
   +ligature-lisp)
  (ligature-set-ligatures
   '( c-mode c++-mode opencl-c-mode cuda-mode llvm-ts-mode java-mode
      java-ts-mode csharp-mode csharp-ts-mode rust-mode rust-ts-mode
      go-mode go-ts-mode go-mod-ts-mode v-mode v-ts-mode zig-mode zig-ts-mode)
   +ligature-c-like)
  (ligature-set-ligatures
   '( haskell-mode haskell-ts-mode elm-mode elm-ts-mode purescript-mode
      purescript-ts-mode ml-mode caml-mode tuareg-mode fsharp-mode fstar-mode
      fsharp-ts-mode dafny-mode swift-mode coq-mode idris-mode)
   +ligature-functional))


;; A collection of opinionated keyboard-driven transient menus for various Emacs modes
(use-package casual-suite
  :straight t
  :bind ("C-o" . casual-editkit-main-tmenu)
  :bind (("M-g a" . casual-avy-tmenu))
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu))
  :bind (:map Man-mode-map ("C-o" . casual-man-tmenu))
  :bind (:map bookmark-bmenu-mode-map ("C-o" . casual-bookmarks-tmenu))
  :bind (:map calendar-mode-map ("C-o" . casual-calendar))
  :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu))
  :bind (:map eshell-mode-map ("C-o" . casual-eshell-tmenu))
  :bind (:map help-mode-map ("C-o" . casual-help-tmenu))
  :bind (:map ibuffer-mode-map ("C-o" . casual-ibuffer-tmenu))
  :bind (:map image-mode-map ("C-o" . casual-image-tmenu))
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu))
  :bind (:map makefile-mode-map ("C-o" . casual-make-tmenu))
  :bind (:map org-agenda-mode-map ("C-o" . casual-agenda-tmenu))
  :bind (:map reb-mode-map ("C-o" . casual-re-builder-tmenu))
  :bind (:map symbol-overlay-map ("C-o" . casual-symbol-overlay-tmenu))
  :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))


(provide 'me-ui)

;;; me-ui.el ends here
