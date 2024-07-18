;;; goto-last-change.el --- Goto last change in buffer -*- lexical-binding: t; -*-

;; Copyright 2003 Kevin Rodgers
;; Copyright 2018 fmdkdd
;; Copyright 2024 Abdelhak BOUGOUFFA

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Taken from:
;; Original: https://www.emacswiki.org/emacs/download/goto-last-change.el
;; Modified: https://github.com/fmdkdd/dotfiles/blob/master/emacs/.emacs.d/elisp/inotify-revert.el

;;; Code:

(defvar-local goto-last-change-undo nil
  "The `buffer-undo-list' entry of the previous `goto-last-change' command.")

;;;###autoload
(defun goto-last-change (&optional mark-point)
  "Set point to the position of the last change.
Consecutive calls set point to the position of the previous
change.  With a prefix arg (optional arg MARK-POINT non-nil), set
mark so `exchange-point-and-mark' will return point to the
current position."
  (interactive "P")
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  (when mark-point
    (push-mark))
  (let ((position nil)
        (minimal-line-distance 20)
	(undo-list (if (and (eq this-command last-command)
			    goto-last-change-undo)
		       (cdr (memq goto-last-change-undo buffer-undo-list))
		     buffer-undo-list))
	undo)
    (while (and undo-list
                (or (not position)
                    (eql position (point))
                    (and minimal-line-distance
                         ;; The first invocation always goes to the last change, subsequent ones skip
                         ;; changes closer to (point) then minimal-line-distance.
                         (memq last-command '(goto-last-change
                                              goto-last-change-with-auto-marks))
                         (< (count-lines (min position (point-max)) (point))
                            minimal-line-distance))))
      (setq undo (car undo-list))
      (cond ((and (consp undo) (integerp (car undo)) (integerp (cdr undo)))
	     ;; (BEG . END)
	     (setq position (cdr undo)))
	    ((and (consp undo) (stringp (car undo))) ; (TEXT . POSITION)
	     (setq position (abs (cdr undo))))
	    ((and (consp undo) (eq (car undo) t))) ; (t HIGH . LOW)
	    ((and (consp undo) (null (car undo)))
	     ;; (nil PROPERTY VALUE BEG . END)
	     (setq position (cdr (last undo))))
	    ((and (consp undo) (markerp (car undo)))) ; (MARKER . DISTANCE)
	    ((integerp undo))		; POSITION
	    ((null undo))		; nil
	    (t (user-error "Invalid undo entry: %s" undo)))
      (setq undo-list (cdr undo-list)))
    (cond (position
	   (setq goto-last-change-undo undo)
	   (goto-char (min position (point-max))))
	  ((and (eq this-command last-command)
		goto-last-change-undo)
	   (setq goto-last-change-undo nil)
	   (user-error "No further undo information"))
	  (t
	   (setq goto-last-change-undo nil)
	   (user-error "Buffer not modified")))))


(provide 'goto-last-change)
;;; goto-last-change.el ends here
