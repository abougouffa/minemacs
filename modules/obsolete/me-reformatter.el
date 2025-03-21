;;; me-reformatter.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; Define commands which run reformatters on the current Emacs buffer
(use-package reformatter
  :straight t
  :config
  (reformatter-define ref-black
    :program "black"
    :args (append
           (when (equal "pyi" (and buffer-file-name (file-name-extension buffer-file-name)))
             '("--pyi"))
           '("-"))
    :lighter "Black ")

  (reformatter-define ref-lua-format
    :program "lua-format"
    :args (append
           (when-let* ((indent (cond ((derived-mode-p 'lua-ts-mode) lua-ts-indent-offset)
                                     ((derived-mode-p 'lua-mode) lua-indent-level))))
             (list (format "--indent-width=%d")))
           (unless indent-tabs-mode
             '("--no-use-tab")))
    :lighter "LuaFmt ")

  (reformatter-define ref-clang-format
    :program "clang-format"
    :args (list
           "-style" (+clang-format-get-style)
           "-assume-filename"
           (or (buffer-file-name)
               (alist-get major-mode
                          '(((c-mode c-ts-mode)               . ".c")
                            ((csharp-mode csharp-ts-mode)     . ".cs")
                            ((c++-mode c++-ts-mode)           . ".cpp")
                            ((cuda-mode cuda-ts-mode)         . ".cu")
                            ((glsl-mode glsl-ts-mode)         . ".glsl")
                            ((java-mode java-ts-mode)         . ".java")
                            ((objc-mode objc-ts-mode)         . ".m")
                            ((protobuf-mode protobuf-ts-mode) . ".proto"))
                          ".c" nil
                          (lambda (lst elt) (memq elt lst)))))
    :lighter "ClangFmt "))


(provide 'obsolete/me-reformatter)
;;; me-reformatter.el ends here
