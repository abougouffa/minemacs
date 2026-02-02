;;; me-opengl.el --- OpenGL support packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-02-02
;; Last modified: 2026-02-02

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-opengl
  :auto-mode `((,(rx "." (or "vert" "frag" "geom" "tesc" "tese" "mesh" "task" "comp" "rgen" "rint" "rchit" "rahit" "rcall" "rmiss" "glsl") eol) . glsl-mode)))


;; OpenGL Shading Language (GLSL)
(use-package glsl-mode
  :straight t)


;; OpenGL Shading Language (GLSL), Tree-sitter based
(use-package glsl-ts-mode
  :straight glsl-mode
  :mode (rx "." (or "vert" "frag" "geom" "tesc" "tese" "mesh" "task" "comp" "rgen" "rint" "rchit" "rahit" "rcall" "rmiss" "glsl") eol)
  :when (featurep 'feat/tree-sitter))


(provide 'on-demand/me-opengl)
;;; me-opengl.el ends here
