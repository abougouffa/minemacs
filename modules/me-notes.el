;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;; Useful resources:
;; https://takeonrules.com/2022/10/01/exploring-the-denote-emacs-package/
;; https://www.thesing-online.de/blog/20230302T215700--denote-as-a-zettelkasten.html

;; Simple notes for Emacs with an efficient file-naming scheme
(use-package denote
  :ensure t
  :commands (denote-create-note denote-insert-link denote-show-backlinks-buffer)
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-date-prompt-use-org-read-date t) ; And `org-read-date' is an amazing bit of tech
  :config
  (denote-rename-buffer-mode 1)
  :init
  (with-eval-after-load 'org-capture
    (add-to-list
     'org-capture-templates
     '("j" "New journal (with Denote)" plain
       (file denote-last-path)
       (function
        (lambda ()
          ;; The "journal" subdirectory of the `denote-directory'---this must exist!
          (let* ((denote-use-directory (expand-file-name "journal" (denote-directory)))
                 ;; Use the existing `denote-prompts' as well as the one for a date.
                 (denote-prompts (denote-add-prompts '(date))))
            (mkdir denote-use-directory t)
            (denote-org-capture))))
       :no-save t
       :immediate-finish nil
       :kill-buffer t
       :jump-to-captured t))
    (add-to-list
     'org-capture-templates
     '("r" "New reference (with Denote)" plain
       (file denote-last-path)
       (function
        (lambda ()
          (let ((denote-use-directory (expand-file-name "reference" (denote-directory))))
            (mkdir denote-use-directory t)
            (denote-org-capture))))
       :no-save t
       :immediate-finish nil
       :kill-buffer t
       :jump-to-captured t))))


;; Convenience functions for working with multiple silos
(use-package denote-silo
  :ensure t)


;; Convenience functions for daily journaling with Denote
(use-package denote-journal
  :ensure t
  :hook (calendar-mode . denote-journal-calendar-mode))


;; Sequence notes or Folgezettel with Denote
(use-package denote-sequence
  :ensure t)


;; Extensions to better integrate Org with Denote
(use-package denote-org
  :ensure t)


;; Extensions to better integrate Markdown with Denote
(use-package denote-markdown
  :ensure t)


;; Use Consult in tandem with Denote
(use-package consult-denote
  :ensure t
  :unless (+package-disabled-p 'consult 'me-completion)
  :after consult
  :init
  (consult-denote-mode 1)
  :config
  ;; Prefer `ripgrep' and `fd' variants when available
  (when (executable-find "fd")
    (setopt consult-denote-find-command #'consult-fd))
  (when (executable-find "rg")
    (setopt consult-denote-grep-command #'consult-ripgrep)))


(provide 'me-notes)

;;; me-notes.el ends here
