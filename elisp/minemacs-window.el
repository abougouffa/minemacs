;;; minemacs-window.el --- Display-buffer and window-related extensions for my dotemacs -*- lexical-binding: t -*-

;; Stolen from Protesilaos' config at: https://protesilaos.com/emacs/dotemacs

;;; Code:

;;;###autoload
(defun +window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold',
respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (> (window-total-height) split-height-threshold))))

;;;###autoload
(defun +window-three-or-more-p (&optional frame)
  "Return non-nil if three or more windows occupy FRAME.
If FRAME is non-nil, inspect the current frame."
  (>= (length (window-list frame :no-minibuffer)) 3))

(defvar +window-sizes
  '( :max-height (lambda () (floor (frame-height) 3))
     :min-height 12
     :max-width (lambda () (floor (frame-width) 4))
     :min-width 24)
  "Property list of maximum and minimum window sizes.
The property keys are `:max-height', `:min-height', `:max-width',
and `:min-width'.  They all accept a value of either a
number (integer or floating point) or a function.")

(defun +get-window-size (key)
  "Extract the value of KEY from `+window-sizes'."
  (when-let* ((value (plist-get +window-sizes key)))
    (cond
     ((functionp value)
      (funcall value))
     ((numberp value)
      value)
     (t
      (error "The value of `%s' is neither a number nor a function" key)))))

(defun +window-select-fit-size (window)
  "Select WINDOW and resize it.
The resize pertains to the maximum and minimum values for height
and width, per `+window-sizes'.

Use this as the `body-function' in a `display-buffer-alist' entry."
  (select-window window)
  (fit-window-to-buffer
   window
   (+get-window-size :max-height)
   (+get-window-size :min-height)
   (+get-window-size :max-width)
   (+get-window-size :min-width))
  ;; If we did not use `display-buffer-below-selected', then we must
  ;; be in a lateral window, which has more space.  Then we do not
  ;; want to dedicate the window to this buffer, because we will be
  ;; running out of space.
  (when (or (window-in-direction 'above) (window-in-direction 'below))
    (set-window-dedicated-p window t)))

(defun +window--get-display-buffer-below-or-pop ()
  "Return list of functions for `+window-display-buffer-below-or-pop'."
  (list
   #'display-buffer-reuse-mode-window
   (if (or (+window-small-p)
           (+window-three-or-more-p))
       #'display-buffer-below-selected
     #'display-buffer-pop-up-window)))

(defun +window-display-buffer-below-or-pop (&rest args)
  "Display buffer below current window or pop a new window.
The criterion for choosing to display the buffer below the
current one is a non-nil return value for
`+window-small-p'.

Apply ARGS expected by the underlying `display-buffer' functions.

This as the action function in a `display-buffer-alist' entry."
  (let ((functions (+window--get-display-buffer-below-or-pop)))
    (catch 'success
      (dolist (fn functions)
        (when (apply fn args)
          (throw 'success fn))))))

(defun +window-shell-or-term-p (buffer &rest _)
  "Check if BUFFER is a shell or terminal.
This is a predicate function for `buffer-match-p', intended for
use in `display-buffer-alist'."
  (when (string-match-p "\\*.*\\(e?shell\\|v?term\\|eat\\).*" (buffer-name (get-buffer buffer)))
    (with-current-buffer buffer
      ;; REVIEW 2022-07-14: Is this robust?
      (and (not (derived-mode-p '(message-mode text-mode)))
           (derived-mode-p '(eshell-mode shell-mode comint-mode fundamental-mode eat-mode))))))

(defun +window-remove-dedicated (&rest _)
  "Remove dedicated window parameter.
Use this as :after advice to `delete-other-windows' and
`delete-window'."
  (when (one-window-p :no-mini)
    (set-window-dedicated-p nil nil)))

(mapc
 (lambda (fn)
   (advice-add fn :after #'+window-remove-dedicated))
 '(delete-other-windows delete-window))


(provide 'minemacs-window)
;;; minemacs-window.el ends here
