;;; me-biblio.el --- Bibliography -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package zotxt
  :straight t
  :preface
  (defconst +zotero-available-p (executable-find "zotero"))
  :when +zotero-available-p
  :init
  (+map-local! :keymaps 'org-mode-map
    "z" #'org-zotxt-mode)
  (+map-local! :keymaps 'markdown-mode-map
    "z" #'zotxt-citekey-mode))

(use-package citar
  :straight t
  :after oc
  :demand t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-symbol-separator "  ")
  :config
  (with-eval-after-load 'nerd-icons
    (defun +citar--set-symbols ()
      (setq citar-symbols
            `((file ,(nerd-icons-codicon "nf-cod-file_pdf" :face 'error) . " ")
              (note ,(nerd-icons-faicon "nf-fa-file_text" :face 'warning) . " ")
              (link ,(nerd-icons-mdicon "nf-md-link" :face 'org-link) . " "))))

    ;; Properly setup citar-symbols
    (if (display-graphic-p)
        (+citar--set-symbols)
      (add-hook
       'server-after-make-frame-hook
       (defun +citar--set-symbols-once-h ()
         (when (display-graphic-p)
           (+citar--set-symbols)
           (remove-hook 'server-after-make-frame-hook
                        #'+citar--set-symbols-once-h)))))))

(use-package citar-org-roam
  :straight t
  :after citar org-roam
  :demand t
  :commands +org-roam-node-from-cite
  :config
  ;; Modified form: jethrokuan.github.io/org-roam-guide/
  (defun +org-roam-node-from-cite (entry-key)
    "Create an Org-Roam node from a bibliography reference."
    (interactive (list (citar-select-ref)))
    (let ((title (citar-format--entry
                  "${author editor} (${date urldate}) :: ${title}"
                  (citar-get-entry entry-key))))
      (org-roam-capture- :templates
                         `(("r" "reference" plain
                            "%?"
                            :if-new (file+head "references/${citekey}.org"
                                     ,(concat
                                       ":properties:\n"
                                       ":roam_refs: [cite:@${citekey}]\n"
                                       ":end:\n"
                                       "#+title: ${title}\n"))
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey entry-key)
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))
  (citar-org-roam-mode 1))

(use-package citar-embark
  :straight t
  :after citar embark
  :demand t
  :config
  (citar-embark-mode 1))


(provide 'me-biblio)

;;; me-biblio.el ends here
