;;; me-ligature.el --- Ligatures -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package ligature
  :straight t
  :when (+emacs-options-p 'harfbuzz 'cairo)
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
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>" ":::" "::="
     "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!==" "!!." ">=>" ">>=" ">>>"
     ">>-" ">->" "->>" "-->" "---" "-<<" "<~~" "<~>" "<*>" "<||" "<|>" "<$>"
     "<==" "<=>" "<=<" "<->" "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###"
     "#_(" "..<" "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|" "[|" "]#" "::"
     ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:" ">=" ">>" ">-" "-~" "-|" "->"
     "--" "-<" "<~" "<*" "<|" "<:" "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#["
     "#:" "#=" "#!" "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)" "\\\\" "://")))


(provide 'obsolete/me-ligature)
;;; me-ligature.el ends here
