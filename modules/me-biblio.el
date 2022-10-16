;; -*- lexical-binding: t; -*-

(use-package oc
  :after org
  :custom
  (org-cite-export-processors '((latex biblatex) (t csl)))
  (org-support-shift-select t)
  :config
  (+map-local :keymaps 'org-mode-map
    "C" #'org-cite-insert))


(use-package oc-csl
  :after oc)


(use-package oc-natbib
  :after oc)


(use-package oc-biblatex
  :after oc)


(use-package citar
  :straight t
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-symbol-separator "  ")
  :config
  (defun +citar--set-symbols ()
    (setq citar-symbols
          `((file ,(all-the-icons-octicon "file-pdf"      :face 'error) . " ")
            (note ,(all-the-icons-octicon "file-text"     :face 'warning) . " ")
            (link ,(all-the-icons-octicon "link-external" :face 'org-link) . " "))))

  ;; Properly setup citar-symbols
  (if (display-graphic-p)
      (+citar--set-symbols)
    (add-hook
     'server-after-make-frame-hook
     (defun +citar--set-symbols-h ()
       (when (display-graphic-p)
         (+citar--set-symbols)
         (remove-hook 'server-after-make-frame-hook
                      #'+citar--set-symbols-h))))))


(use-package citar-org-roam
  :straight t
  :after citar org-roam
  :init
  ;; Modified form: https://jethrokuan.github.io/org-roam-guide/
  (defun +org-roam-node-from-cite (entry-key)
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
                                       "  :roam_refs: [cite:@${citekey}]\n"
                                       "  :end:\n"
                                       "  #+title: ${title}\n"))
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey entry-key)
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))
  :config
  (citar-org-roam-mode))


(use-package citar-embark
  :straight t
  :after citar embark
  :config
  (citar-embark-mode))


(provide 'me-biblio)
