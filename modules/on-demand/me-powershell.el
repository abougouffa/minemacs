;;; me-powershell.el --- PowerShell -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-powershell
  :auto-mode '(("\\.ps[dm]?1\\'" . powershell-mode)))


;; Mode for editing PowerShell scripts
(use-package powershell
  :straight t
  :custom
  (powershell-default-langserver-path (expand-file-name "powershell/" minemacs-local-dir))
  :config
  ;; TEMP+BUG: Temporary fix until jschaf/powershell.el#44 gets merged
  (advice-add
   'powershell--download-langserver
   :override
   (satch-defun +powershell--download-langserver:override-a ()
     (let* ((download-dir  (expand-file-name "dl" powershell-default-langserver-path))
            (download-file (expand-file-name "powershell-langserver.zip" download-dir)))
       (make-directory download-dir :parents)
       (let* ((version     (powershell--get-latest-release-version))
              (url         (format "https://github.com/PowerShell/PowerShellEditorServices/releases/download/%s/PowerShellEditorServices.zip" version)))
         (url-copy-file url download-file 't)
         (powershell--unzip-file download-file powershell-default-langserver-path)
         (delete-directory download-dir t)
         ;; make our function respond with something more interesting than nil :)
         (message (format "Powershell LangServer version %s downloaded and unpacked to \'%s\'" version powershell-default-langserver-path)))))))


(provide 'on-demand/me-powershell)
;;; me-powershell.el ends here
