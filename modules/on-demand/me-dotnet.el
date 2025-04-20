;;; me-dotnet.el --- Microsoft .NET development -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-dotnet
  :auto-mode '(("\\.\\(frm\\|bas\\|cls\\|vb\\)\\'" . vbnet-mode)
               ("\\.\\(vb\\|cs\\|fs\\|vcx\\|vd\\)proj" . csproj-mode))
  :companion-packages '(((csharp-mode csharp-ts-mode) . (dotnet csproj-mode sharper))))


(defun +dotnet-get-templates (&optional type)
  "Get the supported templates from \"dotnet new list\".

When TYPE is specified, it will return only templates of that type."
  (let ((out-lines (string-lines (shell-command-to-string "dotnet new list --columns=type")))
        skel templates)
    (dolist (line out-lines)
      (cond ((string-match "\\([-]+\\)\\([[:space:]]+\\)\\([-]+\\)\\([[:space:]]+\\)\\([-]+\\)" line)
             (setq skel (list (length (match-string 1 line)) (length (match-string 2 line))
                              (length (match-string 3 line)) (length (match-string 4 line)))))
            ((and skel (length> line 1))
             (when (or (not type)
                       (equal type (string-trim (substring line (+ (nth 0 skel) (nth 1 skel) (nth 2 skel))))))
               (push (car (string-split
                           (string-trim (substring line (+ (nth 0 skel) (nth 1 skel)) (+ (nth 0 skel) (nth 1 skel) (nth 2 skel) 1)))
                           ","))
                     templates)))))
    templates))


;; Work with .NET project files (csproj, vbproj)
(use-package csproj-mode
  :straight t
  :config
  ;; BUG+FIX: Switch to a working function
  (advice-add 'csproj-mode--get-dotnet-new-templates :override #'+dotnet-get-templates))


;; Interact with dotnet CLI tool
(use-package dotnet
  :straight t
  :config
  (add-to-list 'dotnet-langs "vb")
  (when (executable-find "dotnet")
    (setq dotnet-templates (+dotnet-get-templates "project")))
  (defun +dotnet-goto-vbproj ()
    "Search for a VB.Net project file in any enclosing folders relative to current directory."
    (interactive)
    (dotnet-goto ".vbproj")))


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
