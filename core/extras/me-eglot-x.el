;;; me-eglot-x.el --- Eglot customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-25
;; Last modified: 2025-07-05

;;; Commentary:

;;; Code:

;;;###autoload(with-eval-after-load 'eglot (require 'me-eglot-x))

(require 'me-eglot-ltex)

(setq-default
 eglot-workspace-configuration
 `(:harper-ls
   ( :userDictPath ,(expand-file-name (+directory-ensure minemacs-local-dir "harper-ls/dictionary.txt"))
     :fileDictPath ,(expand-file-name (+directory-ensure minemacs-local-dir "harper-ls/file-dictionaries/"))
     :linters ( :SpellCheck t
                :SpelledNumbers :json-false
                :AnA t
                :SentenceCapitalization t
                :UnclosedQuotes t
                :WrongQuotes :json-false
                :LongSentences t
                :RepeatedWords t
                :Spaces t
                :Matcher t
                :CorrectNumberSuffix t)
     :codeActions (:ForceStable :json-false)
     :markdown (:IgnoreLinkTitle :json-false)
     :diagnosticSeverity "hint"
     :isolateEnglish :json-false
     :dialect "American"
     :maxFileLength 120000)))

;; Register some missing LSP servers
(+eglot-register
  '(awk-mode awk-ts-mode)
  "awk-language-server")

(+eglot-register
  '(nxml-mode xml-mode)
  "lemminx")

(+eglot-register
  '(vhdl-mode vhdl-ts-mode)
  "vhdl_ls") ; vhdl_ls from rust_hdl (AUR: rust_hdl-git)

(+eglot-register
  '(verilog-mode verilog-ts-mode)
  "svls"
  "verible-verilog-ls"
  "svlangserver")

(+eglot-register ; add `pylyzer' and `ty'
  '(python-mode python-ts-mode)
  "pylsp"
  "pyls"
  '("pylyzer" "--server")
  '("ty" "server")
  '("basedpyright-langserver" "--stdio")
  '("pyright-langserver" "--stdio")
  "jedi-language-server"
  "ruff-lsp")

(+eglot-register ; better (!) parameters for Clangd
  '(c++-mode c++-ts-mode c-mode c-ts-mode)
  '("clangd" "--background-index" "-j=12" "--clang-tidy" ;; "--clang-tidy-checks=*"
    "--query-driver=/usr/bin/**/clang-*,/bin/clang,/bin/clang++,/usr/bin/gcc,/usr/bin/g++"
    "--all-scopes-completion" "--cross-file-rename" "--completion-style=detailed"
    "--header-insertion-decorators" "--header-insertion=iwyu" "--pch-storage=memory")
  "ccls")

(+eglot-register
  '(text-mode org-mode markdown-mode markdown-ts-mode rst-mode git-commit-mode)
  `(,eglot-ltex-ls-program "--server-type=TcpSocket" "--port" :autoport) ; Add LTeX+ LS
  `("harper-ls" "--stdio")) ; Add Harper LS

(+eglot-register
  '(tex-mode context-mode texinfo-mode bibtex-mode)
  "digestif"
  "texlab"
  `(,eglot-ltex-ls-program "--server-type=TcpSocket" "--port" :autoport))


(provide 'me-eglot-x)
;;; me-eglot-x.el ends here
