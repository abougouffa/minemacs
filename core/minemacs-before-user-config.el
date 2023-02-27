;; minemacs-before-user-config.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Virtual module loaded before loading the user `custom-file' and
;; configuration file (defaults to "~/.minemacs.d/config.el").

;; This module's hook can be used to execute some pre-config stuff.

;; Run hooks
(when minemacs-before-user-config-hook
  (run-hooks 'minemacs-before-user-config-hook))

(+log! "Providing `minemacs-before-user-config'.")

(provide 'minemacs-before-user-config)
