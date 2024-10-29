;;; goto-last-change.el --- Move point through buffer-undo-list positions  -*- lexical-binding: t; -*-

;; Copyright © 2003 Kevin Rodgers

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Created: 17 Jun 2003
;; Modified: 21 Jul 2024
;; Version: 1.2.2
;; Keywords: convenience

;; Contributors:
;;   Attila Lendvai <attila.lendvai@gmail.com> (line distance and auto marks)
;;   Abdelhak Bougouffa (modernization)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; You may want to bind a key to `M-x goto-last-change', e.g.
;;   (global-set-key "\C-x\C-\\" 'goto-last-change)

;; goto-last-change.el was written in response to the following:
;;
;; From: Dan Jacobson <jidanni@jidanni.org>
;; Newsgroups: gnu.emacs.bug
;; Subject: function to go to spot of last change
;; Date: Sun, 15 Jun 2003 00:15:08 +0000 (UTC)
;; Sender: news <news@main.gmane.org>
;; Message-ID: <mailman.7910.1055637181.21513.bug-gnu-emacs@gnu.org>
;; NNTP-Posting-Host: monty-python.gnu.org
;;
;; Why of course, a function to get the user to the spot of last changes in the
;; current buffer(s?), that's what Emacs must lack.
;;
;; How many times have you found yourself mosying [<-not in spell checker!?]
;; thru a file when you wonder, where the heck was I just editing? Well, the
;; best you can do is hit undo, ^F, and undo again, to get back. Hence the
;; "burning need" for the additional function, which you might name
;; the-jacobson-memorial-function, due to its brilliance.
;;
;; http://jidanni.org/ Taiwan(04)25854780

;;; Code:

(defgroup goto-last-change nil
  "Move point through `buffer-undo-list' positions."
  :group 'convenience)

(defvar-local goto-last-change-undo nil
  "The `buffer-undo-list' entry of the previous \\[goto-last-change] command.")

(defcustom goto-last-change-minimal-line-distance 20
  "The default minimal line distance under which no subsequent jumps are done.

The first invocation of `goto-last-change' always goes to the last
change, subsequent ones skip changes closer to (point) than
MINIMAL-LINE-DISTANCE."
  :group 'goto-last-change
  :type '(choice natnum (symbol nil)))

;;;###autoload
(defun goto-last-change (&optional mark-point)
  "Set point to the position of the last change.

Consecutive calls set point to the position of the previous changes.
With a prefix arg (optional arg MARK-POINT non-nil), this will set mark
so \\[exchange-point-and-mark] will return point to the current
position."
  (interactive "P")
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  (when mark-point
    (push-mark))
  (let ((undo-list (if (and (eq this-command last-command)
                            goto-last-change-undo)
                       (cdr (memq goto-last-change-undo buffer-undo-list))
                     buffer-undo-list))
        position
        undo)
    (while (and undo-list
                (or (not position)
                    (eql position (point))
                    (and goto-last-change-minimal-line-distance
                         ;; The first invocation always goes to the last change, subsequent ones skip
                         ;; changes closer to (point) then minimal-line-distance.
                         (memq last-command '(goto-last-change goto-last-change-with-auto-marks))
                         (< (count-lines (min position (point-max)) (point))
                            goto-last-change-minimal-line-distance))))
      (setq undo (car undo-list))
      (cond ((and (consp undo) (integerp (car undo)) (integerp (cdr undo)))
             (setq position (cdr undo)))                      ; (BEG . END)
            ((and (consp undo) (stringp (car undo)))          ; (TEXT . POSITION)
             (setq position (abs (cdr undo))))
            ((and (consp undo) (eq (car undo) t)))            ; (t HIGH . LOW)
            ((and (consp undo) (null (car undo)))             ; (nil PROPERTY VALUE BEG . END)
             (setq position (cdr (last undo))))
            ((and (listp undo) (eq (car-safe undo) #'apply))) ; (apply ...)
            ((and (consp undo) (markerp (car undo))))         ; (MARKER . DISTANCE)
            ((integerp undo))                                 ; POSITION
            ((null undo))                                     ; nil
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

(defun goto-last-change-with-auto-marks ()
  "Call `goto-last-change' and set the mark at the first invocation."
  (interactive "P")
  (goto-last-change (not (or (eq last-command 'goto-last-change-with-auto-marks)
                             (eq last-command t)))))


(provide 'goto-last-change)
;;; goto-last-change.el ends here
