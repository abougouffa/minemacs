;;; me-plantuml.el --- PlantUML -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-plantuml
  :auto-mode '(("\\.\\(plantuml\\|pum\\|plu\\)\\'" . plantuml-mode)))

(use-package plantuml-mode
  :straight t
  :hook (plantuml-mode . +plantuml-mode-setup)
  :custom
  (plantuml-jar-path (concat minemacs-local-dir "plantuml/plantuml.jar"))
  (plantuml-indent-level 2)
  :config
  ;; Define `capf' function, based on `plantuml-complete-symbol'
  (defun +plantuml-completion-at-point ()
    "Perform symbol-at-pt completion on word before cursor."
    (when (derived-mode-p 'plantuml-mode) ; do not fire up on other modes
      (let* ((end-pos (point))
             (sym-at-pt (or (thing-at-point 'symbol) ""))
             (max-match (try-completion sym-at-pt plantuml-kwdList)))
        (unless (null max-match)
          (list (- end-pos (length sym-at-pt))
                end-pos
                (if (eq max-match t)
                    (list keyword)
                  (all-completions sym-at-pt plantuml-kwdList)))))))

  ;; Add support for `capf'
  (defun +plantuml-mode-setup ()
    (add-to-list 'completion-at-point-functions #'+plantuml-completion-at-point)))

(use-package flymake-plantuml
  :straight (:host github :repo "shaohme/flymake-plantuml")
  :hook (plantuml-mode . flymake-plantuml-setup))


(provide 'on-demand/me-plantuml)
;;; me-plantuml.el ends here
