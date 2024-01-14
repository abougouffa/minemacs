;;; me-calendar.el --- Calendar integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; This is a WIP

;;; Code:

(use-package calfw
  :straight t
  :custom
  (cfw:face-item-separator-color nil)
  (cfw:render-line-breaker #'cfw:render-line-breaker-none)
  (cfw:fchar-junction ?╋)
  (cfw:fchar-vertical-line ?┃)
  (cfw:fchar-horizontal-line ?━)
  (cfw:fchar-left-junction ?┣)
  (cfw:fchar-right-junction ?┫)
  (cfw:fchar-top-junction ?┯)
  (cfw:fchar-top-left-corner ?┏)
  (cfw:fchar-top-right-corner ?┓)
  :commands cfw:open-calendar-buffer
  :init
  (+map! "oC" (+def-dedicated-tab! cfw:open-calendar-buffer))
  :config
  (advice-add
   'cfw:render-button :override
   (defun +cfw:render-button-flat:override-a (title command &optional state)
     "Render a flat button with TITLE, COMMAND and STATE."
     (let ((text (concat " " title " "))
           (keymap (make-sparse-keymap)))
       (cfw:rt text (if state 'cfw:face-toolbar-button-on
                      'cfw:face-toolbar-button-off))
       (define-key keymap [mouse-1] command)
       (cfw:tp text 'keymap keymap)
       (cfw:tp text 'mouse-face 'highlight)
       text))))

(use-package calfw-ical
  :straight t
  :demand t
  :after calfw)

(use-package calfw-cal
  :straight t
  :demand t
  :after calfw)

(use-package calfw-org
  :straight t
  :demand t
  :after calfw
  :commands +cfw-open-calendar
  :config
  (defun +cfw-open-calendar ()
    "Open the calendar."
    (interactive)
    (cfw:open-calendar-buffer
     ;; :custom-map cfw:my-cal-map
     :contents-sources
     (list (cfw:org-create-source (face-foreground 'default))))))


(provide 'me-calendar)

;;; me-calendar.el ends here
