;;; me-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-20
;; Last modified: 2025-07-28

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
  (+with-delayed-1! (doom-themes-visual-bell-config)))


;; Light, modern and opinionated mode-line for MinEmacs
(use-package me-modeline
  :init
  (me-modeline-mode 1))


;; Display "^L" page breaks as tidy horizontal lines
(use-package page-break-lines
  :straight t
  :hook ((prog-mode text-mode special-mode) . page-break-lines-mode))


;; Pulse highlight on demand or after select functions
(use-package pulsar
  :straight (:host github :repo "protesilaos/pulsar")
  :hook (minemacs-first-file . pulsar-global-mode)
  :custom
  (pulsar-iterations 6)
  (pulsar-pulse-on-window-change t)
  (pulsar-pulse-region-functions pulsar-pulse-region-common-functions)
  (pulsar-region-face 'pulsar-green)
  (pulsar-highlight-face 'pulsar-cyan)
  (pulsar-region-change-face 'pulsar-red)
  (pulsar-window-change-face 'pulsar-yellow)
  :config
  (cl-callf append pulsar-pulse-functions '(what-cursor-position)))


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


;; Extra colors for `Info-mode'
(use-package info-colors
  :straight t
  :hook (Info-selection . info-colors-fontify-node))


;; Format buffers visually without modification
(use-package virtual-format
  :straight (:host github :repo "abougouffa/virtual-format"))


;; A collection of opinionated keyboard-driven user interfaces for various built-in Emacs modes
(use-package casual
  :straight (:host github :repo "kickingvegas/casual")
  :bind ("C-o" . casual-editkit-main-tmenu)
  :bind (:package bookmark :map bookmark-bmenu-mode-map ("C-o" . casual-bookmarks-tmenu))
  :bind (:package calc :map calc-mode-map ("C-o" . casual-calc-tmenu))
  :bind (:package calendar :map calendar-mode-map ("C-o" . casual-calendar))
  :bind (:package dired :map dired-mode-map ("C-o" . casual-dired-tmenu))
  :bind (:package esh-mode :map eshell-mode-map ("C-o" . casual-eshell-tmenu))
  :bind (:package ibuffer :map ibuffer-mode-map ("C-o" . casual-ibuffer-tmenu))
  :bind (:package image-mode :map image-mode-map ("C-o" . casual-image-tmenu))
  :bind (:package info :map Info-mode-map ("C-o" . casual-info-tmenu))
  :bind (:package isearch :map isearch-mode-map ("C-o" . casual-isearch-tmenu))
  :bind (:package make-mode :map makefile-mode-map ("C-o" . casual-make-tmenu))
  :bind (:package man :map Man-mode-map ("C-o" . casual-man-tmenu))
  :bind (:package help-mode :map help-mode-map ("C-o" . casual-help-tmenu))
  :bind (:package org-agenda :map org-agenda-mode-map ("C-o" . casual-agenda-tmenu))
  :bind (:package re-builder :map reb-mode-map ("C-o" . casual-re-builder-tmenu)))


;; An opinionated `transient' menu for `avy'
(use-package casual-avy
  :straight t
  :bind ("M-g a" . casual-avy-tmenu))


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


;; Effortlessly persist and restore your Emacs sessions
(use-package easysession
  :straight t
  :hook (minemacs-lazy . easysession-save-mode))


(provide 'me-ui)

;;; me-ui.el ends here
