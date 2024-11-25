;;; init-tweaks.el --- Initialization tweaks, loaded early in the Emacs' "init.el" -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; NOTE: At this point, `me-vars' and `me-lib' are already available and
;; "core/me-loaddefs.el" is loaded.

;; Define special "first file" hooks and features for Rust and Octave/Matlab
;; files, this will creates the hooks `minemacs-first-rust-file-hook' and and
;; `minemacs-first-octave-file-hook' that will provide the features
;; `minemacs-first-rust-file' and `minemacs-first-octave-file'.
(+make-first-file-hook! 'rust "\\.rs$")
(+make-first-file-hook! 'octave "\\.m$")

;;; init-tweaks.el ends here
