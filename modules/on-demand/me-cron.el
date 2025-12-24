;;; me-cron.el --- Crontab files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-23
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cron
  :auto-mode '(("\\.?cron\\(tab\\)?\\(\\.X*[[:alnum:]]+\\)?\\'" . crontab-mode)))


;; Major mode for crontab
(use-package crontab-mode
  :ensure t)


(provide 'on-demand/me-cron)
;;; me-cron.el ends here
