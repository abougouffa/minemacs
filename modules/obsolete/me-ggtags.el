;;; me-ggtags.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-09-10
;; Last modified: 2025-09-10

;;; Commentary:

;;; Code:


;; Emacs frontend to GNU Global source code tagging system
(use-package ggtags
  :straight t
  :custom
  (ggtags-extra-args (when (or (getenv "GTAGSOBJDIRPREFIX") (getenv "MAKEOBJDIRPREFIX")) '("--objdir")))
  (ggtags-use-sqlite3 t))


(provide 'obsolete/me-ggtags)
;;; me-ggtags.el ends here
