;;; me-dotnet.el --- Microsoft .NET development -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-dotnet
  :auto-mode '(("\\.\\(frm\\|bas\\|cls\\|vb\\)\\'" . vbnet-mode)
               ("\\.\\(vb\\|cs\\|fs\\|vcx\\|vd\\)proj\\'" . csproj-mode)
               ("\\.sln\\'" . sln-mode))
  :companion-packages '(((csharp-mode csharp-ts-mode) . (dotnet csproj-mode sharper))))


(defun +dotnet-get-templates (&optional full-desc)
  "Get the supported templates from \"dotnet new list\".

When FULL-DESC, return an alist with the short and long names alongside
the lanugage and the type."
  (let ((lines (string-lines (shell-command-to-string "dotnet new list --columns=language --columns=type")))
        (regexp (rx (group (+ ?-))    ; 1. Template Name
                    (group (+ space))
                    (group (+ ?-))    ; 3. Short Name
                    (group (+ space))
                    (group (+ ?-))    ; 5. Language
                    (group (+ space))
                    (group (+ ?-))))  ; 7. Type
        skel templates)
    (dolist (line lines)
      (cond ((string-match regexp line)
             (setq skel (mapcar (lambda (n) (cons (match-beginning n) (match-end n)))
                                (number-sequence 1 7 2))))
            ((and skel (length> line 1))
             (let ((l-name (cl-destructuring-bind (beg . end) (nth 0 skel) (string-trim (substring line beg end))))
                   (l-shrt (cl-destructuring-bind (beg . end) (nth 1 skel) (string-trim (substring line beg end))))
                   (l-lang (cl-destructuring-bind (beg . end) (nth 2 skel) (string-trim (substring line beg end))))
                   (l-type (cl-destructuring-bind (beg . end) (nth 3 skel) (string-trim (substring line beg end)))))
               (push (if full-desc
                         (list l-shrt l-name (string-replace "]" "" (string-replace "[" "" l-lang)) l-type)
                       l-shrt)
                     templates)))))
    templates))


(use-package font-lock-ext ; Dependency of `sln-mode'
  :straight t)


;; A major mode to edit Visual Studio's solution files `*.sln'
(use-package sln-mode
  :straight t
  :mode "\\.sln\\'")


;; Work with .NET project files (csproj, vbproj, fsproj, vdproj, vcxproj)
(use-package csproj-mode
  :straight t
  :mode "\\.\\(vb\\|cs\\|fs\\|vcx\\|vd\\)proj\\'"
  :config
  ;; BUG+FIX: Switch to a working function
  (advice-add 'csproj-mode--get-dotnet-new-templates :override #'+dotnet-get-templates))


;; Interact with dotnet CLI tool
(use-package dotnet
  :straight t
  :config
  (defun +dotnet-goto-vbproj ()
    "Search for a VB.Net project file in any enclosing folders relative to current directory."
    (interactive)
    (dotnet-goto ".vbproj"))

  (defvar +dotnet--templates-cache nil)
  (defun +dotnet--templates-cache ()
    (or +dotnet--templates-cache
        (and (executable-find "dotnet")
             (setq +dotnet--templates-cache (+dotnet-get-templates t)))))

  ;; Register the annonater for Marginalia
  (with-eval-after-load 'marginalia
    (defun +marginalia-annotate-dotnet-template (cand)
      (when-let* ((desc (cl-first (alist-get cand (+dotnet--templates-cache) nil nil 'equal)))
                  (lang (cl-second (alist-get cand (+dotnet--templates-cache) nil nil 'equal))))
        (marginalia--fields
         (lang :face 'marginalia-size :width -10)
         (desc :face 'marginalia-file-name))))
    (add-to-list 'marginalia-annotator-registry '(dotnet-template +marginalia-annotate-dotnet-template builtin none)))

  ;; TWEAK+FIX: Fix the path issue and provide a more smart prompting for templates and languages
  (advice-add
   'dotnet-new :override
   (lambda (path template language)
     "Initialize a new console .NET project.
PATH is the path to the new project, TEMPLATE is a template, and
LANGUAGE is a supported language."
     (interactive
      (let* ((dir (expand-file-name (read-directory-name "Project path: ")))
             (templ (completing-read "Choose a template: " (+completion-mark-category (+dotnet--templates-cache) 'dotnet-template)))
             (langs (cl-third (assoc-string templ (+dotnet--templates-cache))))
             (lang (unless (string-empty-p langs)
                     (completing-read "Choose a language: " (string-split langs ",")))))
        (list dir (car (string-split templ ",")) lang)))
     (dotnet-command
      (mapconcat 'shell-quote-argument
                 `("dotnet" "new" ,template "-o" ,path ,@(when language (list "-lang" language)))
                 " ")))))


;; A dotnet CLI wrapper, using Transient
(use-package sharper
  :straight t
  :commands (sharper-main-transient))


;; A mode for editing Visual Basic .NET code
(use-package vbnet-mode
  :straight t
  :mode "\\.\\(frm\\|bas\\|cls\\|vbs?\\)\\'"
  :commands (vbnet-mode))


(provide 'on-demand/me-dotnet)
;;; me-dotnet.el ends here
