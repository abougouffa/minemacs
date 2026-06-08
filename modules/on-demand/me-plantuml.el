;;; me-plantuml.el --- PlantUML -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-06-08

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-plantuml
  :auto-mode '(("\\.\\(plantuml\\|pum\\|plu\\|puml\\)\\'" . plantuml-mode)))


;; Major mode for PlantUML
(use-package plantuml-mode
  :straight t
  :hook (plantuml-mode . +plantuml--fix-completion-at-point)
  :custom
  (plantuml-indent-level 2)
  :preface
  ;; FIX: `plantuml-mode' includes a Capf function, but by default, it hooks the
  ;; legacy `plantuml-complete-symbol' in `completion-at-point-functions', which
  ;; is very annoying
  (defun +plantuml--fix-completion-at-point ()
    (remove-hook 'completion-at-point-functions #'plantuml-complete-symbol t)
    (add-hook 'completion-at-point-functions #'plantuml-completion-at-point-function nil t))
  :config
  ;; Use the executable if available or automatically download the latest version of PlantUML
  (if (executable-find "plantuml")
      (setopt plantuml-default-exec-mode 'executable)
    (setopt plantuml-default-exec-mode 'jar
            plantuml-jar-path (+github-download-release "plantuml/plantuml" "plantuml-\\([[:digit:]]*\\.\\)\\{3\\}jar" nil :ver "v1.2026.5"))))


;; Add `flymake' support for editing PlantUML files
(use-package flymake-plantuml
  :straight (:host github :repo "shaohme/flymake-plantuml")
  :hook (plantuml-mode . flymake-plantuml-setup))


(provide 'on-demand/me-plantuml)
;;; me-plantuml.el ends here
