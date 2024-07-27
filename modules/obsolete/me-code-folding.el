;;; me-code-folding.el --- The glue that make code-folding works as expected -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;; Commentary:

;; This implementation has bee adapted from Doom Emacs' `editor/fold' module.

;; `hideshow' is a decent code folding implementation, but it won't let you
;; create custom folds. `vimish-fold' offers custom folds, but essentially
;; ignores any other type of folding (indent or custom markers, which hideshow
;; and `outline-mode' give you). This is my effort to combine them.

;;
;;; Helpers

(use-package vimish-fold
  :straight t
  :hook (minemacs-first-file . vimish-fold-global-mode))

(use-package evil-vimish-fold
  :straight t
  :unless (+package-disabled-p 'evil 'me-evil)
  :hook (vimish-fold-global-mode . global-evil-vimish-fold-mode)
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold vimish-fold/delete evil-vimish-fold/delete-all evil-vimish-fold/create evil-vimish-fold/create-line)
  :custom
  (vimish-fold-indication-mode 'right-fringe)
  :init
  (with-eval-after-load 'evil
    (evil-define-key* 'motion 'global
      "zf" #'evil-vimish-fold/create
      "zF" #'evil-vimish-fold/create-line
      "zd" #'vimish-fold-delete
      "zE" #'vimish-fold-delete-all)))

(with-eval-after-load 'evil
  ;; Add vimish-fold, outline-mode & hideshow support to folding commands
  (keymap-global-set "<remap> <evil-toggle-fold>"   '+fold/toggle)
  (keymap-global-set "<remap> <evil-close-fold>"    '+fold/close)
  (keymap-global-set "<remap> <evil-open-fold>"     '+fold/open)
  (keymap-global-set "<remap> <evil-open-fold-rec>" '+fold/open)
  (keymap-global-set "<remap> <evil-close-folds>"   '+fold/close-all)
  (keymap-global-set "<remap> <evil-open-folds>"    '+fold/open-all)
  (evil-define-key* 'motion 'global
    "zj" #'+fold/next
    "zk" #'+fold/previous))

(defun +fold--ensure-hideshow-mode ()
  (unless (bound-and-true-p hs-minor-mode) (hs-minor-mode 1)))

(defun +fold--vimish-fold-p ()
  (and (featurep 'vimish-fold) (cl-some #'vimish-fold--vimish-overlay-p (overlays-at (point)))))

(defun +fold--outline-fold-p ()
  (and (or (bound-and-true-p outline-minor-mode) (derived-mode-p 'outline-mode)) (outline-on-heading-p)))

(defun +fold--hideshow-fold-p ()
  (+fold--ensure-hideshow-mode)
  (save-excursion
    (ignore-errors
      (or (hs-looking-at-block-start-p)
          (hs-find-block-beginning)
          (unless (eolp) (end-of-line) (+fold--hideshow-fold-p))))))

(defun +fold--invisible-points (count)
  (let (points)
    (save-excursion
      (catch 'abort
        (if (< count 0) (beginning-of-line))
        (while (re-search-forward hs-block-start-regexp nil t
                                  (if (> count 0) 1 -1))
          (unless (invisible-p (point))
            (end-of-line)
            (when (hs-already-hidden-p)
              (push (point) points)
              (when (>= (length points) count)
                (throw 'abort nil))))
          (forward-line (if (> count 0) 1 -1)))))
    points))

(defmacro +fold-from-eol (&rest body)
  "Execute BODY after moving to the end of the line."
  `(save-excursion
     (end-of-line)
     ,@body))


;;; Commands

;;;###autoload
(defun +fold/toggle ()
  "Toggle the fold at point.

Targets `vimmish-fold', `hideshow' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-toggle))
          ((+fold--outline-fold-p)
           (cl-letf (((symbol-function #'outline-hide-subtree) #'outline-hide-entry))
             (outline-toggle-children)))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-toggle-hiding))))))

;;;###autoload
(defun +fold/open ()
  "Open the folded region at point.

Targets `vimmish-fold', `hideshow' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-unfold))
          ((+fold--outline-fold-p) (outline-show-children) (outline-show-entry))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-show-block))))))

;;;###autoload
(defun +fold/close ()
  "Close the folded region at point.

Targets `vimmish-fold', `hideshow' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-refold))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-hide-block)))
          ((+fold--outline-fold-p) (outline-hide-subtree)))))

;;;###autoload
(defun +fold/open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (when (featurep 'vimish-fold)
    (vimish-fold-unfold-all))
  (save-excursion
    (+fold--ensure-hideshow-mode)
    (if (integerp level)
        (progn
          (outline-hide-sublevels (max 1 (1- level)))
          (hs-life-goes-on
           (hs-hide-level-recursive (1- level) (point-min) (point-max))))
      (hs-show-all)
      (when (fboundp 'outline-show-all)
        (outline-show-all)))))

;;;###autoload
(defun +fold/close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (save-excursion
    (when (featurep 'vimish-fold) (vimish-fold-refold-all))
    (+fold--ensure-hideshow-mode)
    (hs-life-goes-on
     (if (integerp level)
         (hs-hide-level-recursive (1- level) (point-min) (point-max))
       (hs-hide-all)))))

;;;###autoload
(defun +fold/next (count)
  "Jump to the next vimish fold, outline heading or folded region."
  (interactive "p")
  (cl-loop with orig-pt = (point)
           for fn
           in (list (lambda ()
                      (when (bound-and-true-p hs-block-start-regexp)
                        (car (+fold--invisible-points count))))
                    (lambda ()
                      (when (featurep 'vimish-fold)
                        (if (> count 0)
                            (evil-vimish-fold/next-fold count)
                          (evil-vimish-fold/previous-fold (- count))))
                      (if (/= (point) orig-pt) (point))))
           if (save-excursion (funcall fn))
           collect it into points
           finally do
           (if-let* ((pt (car (sort points (if (> count 0) #'< #'>)))))
               (goto-char pt)
             (message "No more folds %s point" (if (> count 0) "after" "before"))
             (goto-char orig-pt))))

;;;###autoload
(defun +fold/previous (count)
  "Jump to the previous vimish fold, outline heading or folded region."
  (interactive "p")
  (+fold/next (- count)))


;;; hideshow.el

;;;###autoload
(defun +fold-hideshow-haml-forward-sexp-fn (arg)
  (haml-forward-sexp arg)
  (move-beginning-of-line 1))

;;;###autoload
(defun +fold-hideshow-forward-block-by-indent-fn (_arg)
  (let ((start (current-indentation)))
    (forward-line)
    (unless (= start (current-indentation))
      (let ((range (+fold-hideshow-indent-range)))
        (goto-char (cadr range))
        (end-of-line)))))


;;; Indentation detection

(defun +fold--hideshow-empty-line-p (_)
  (string= "" (string-trim (thing-at-point 'line 'no-props))))

(defun +fold--hideshow-geq-or-empty-p (base-indent)
  (or (+fold--hideshow-empty-line-p base-indent)
      (>= (current-indentation) base-indent)))

(defun +fold--hideshow-g-or-empty-p (base-indent)
  (or (+fold--hideshow-empty-line-p base-indent)
      (> (current-indentation) base-indent)))

(defun +fold--hideshow-seek (start direction before skip predicate base-indent)
  "Seek function forward or backward.

Seeks forward (if DIRECTION is 1) or backward (if DIRECTION is -1) from
START, until PREDICATE fails. If BEFORE is nil, it will return the first line
where predicate fails, otherwise it returns the last line where predicate
holds."
  (save-excursion
    (goto-char start)
    (goto-char (point-at-bol))
    (let ((bnd (if (> 0 direction)
                   (point-min)
                 (point-max)))
          (pt (point)))
      (when skip (forward-line direction))
      (cl-loop while (and (/= (point) bnd) (funcall predicate base-indent))
               do (progn
                    (when before (setq pt (point-at-bol)))
                    (forward-line direction)
                    (unless before (setq pt (point-at-bol)))))
      pt)))

(defun +fold-hideshow-indent-range (&optional point)
  "Return the point at the begin and end of the text block with the same (or greater) indentation.

If POINT is supplied and non-nil it will return the
begin and end of the block surrounding point."
  (save-excursion
    (when point
      (goto-char point))
    (let ((base-indent (current-indentation))
          (begin (point))
          (end (point)))
      (setq begin (+fold--hideshow-seek begin -1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
            begin (+fold--hideshow-seek begin 1 nil nil #'+fold--hideshow-g-or-empty-p base-indent)
            end   (+fold--hideshow-seek end 1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
            end   (+fold--hideshow-seek end -1 nil nil #'+fold--hideshow-empty-line-p base-indent))
      (list begin end base-indent))))


(provide 'obsolete/me-code-folding)
;;; me-code-folding.el ends here
