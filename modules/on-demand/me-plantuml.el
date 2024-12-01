;;; me-plantuml.el --- PlantUML -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-plantuml
  :auto-mode '(("\\.\\(plantuml\\|pum\\|plu\\)\\'" . plantuml-mode)))


;; Major mode for PlantUML
(use-package plantuml-mode
  :ensure t
  :hook (plantuml-mode . +plantuml-mode-setup)
  :custom
  (plantuml-jar-path (concat minemacs-local-dir "plantuml.jar"))
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

  (if (executable-find "plantuml")
      ;; Use the executable
      (setopt plantuml-default-exec-mode 'executable)
    ;; Automatically download the latest version of PlantUML
    (setopt plantuml-default-exec-mode 'jar
            plantuml-jar-path (+github-download-release "plantuml/plantuml" "plantuml-{{ver}}.jar" nil :ver "1.2024.7")))

  ;; Add support for `capf'
  (defun +plantuml-mode-setup ()
    (add-to-list 'completion-at-point-functions #'+plantuml-completion-at-point)))


;; Add `flymake' support for editing PlantUML files
(use-package flymake-plantuml
  :vc (:url "https://github.com/shaohme/flymake-plantuml")
  :hook (plantuml-mode . flymake-plantuml-setup))


(provide 'on-demand/me-plantuml)
;;; me-plantuml.el ends here
