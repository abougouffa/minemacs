(defun visual-format--cleanup (start end)
  (while (and (< start end)
              (setq start (text-property-any start end 'display visual-format--space)))
    (remove-text-properties
     start
     (setq start (or (text-property-not-all start end 'display visual-format--space)
                     end))
     '(display nil))))

(require 'treesit)

(defvar-local visual-format--buffer nil)
(defvar-local visual-format--buffer-formatted nil)

(defmacro visual-format--with-buff (&rest body)
  `(with-current-buffer visual-format--buffer
    ,@body))

(defmacro visual-format--with-buff-fmt (&rest body)
  `(with-current-buffer visual-format--buffer-formatted
    ,@body))

(defun visual-format--depth-first-walk (&optional node node-fmt prev-node prev-node-fmt depth)
  (let ((node (or node (visual-format--with-buff (treesit-buffer-root-node))))
        (node-fmt (or node-fmt (visual-format--with-buff-fmt (treesit-buffer-root-node))))
        (prev-leaf prev-node)
        (prev-leaf-fmt prev-node-fmt)
        (depth (or depth 0)))
    (cl-assert (= (treesit-node-child-count node) (treesit-node-child-count node-fmt)))
    (dotimes (i (treesit-node-child-count node))
      (let* ((n (treesit-node-child node i))
             (n-fmt (treesit-node-child node-fmt i)))
        (if (zerop (treesit-node-child-count n)) ; leaf
            (let* ((pos-beg
                    (visual-format--with-buff
                     (or (and prev-leaf (max 1 (1- (treesit-node-end prev-leaf))))
                         (point-min))))
                   (pos-end (treesit-node-start n))
                   (pos-beg-fmt
                    (visual-format--with-buff-fmt
                     (or (and prev-leaf-fmt (max 1 (1- (treesit-node-end prev-leaf-fmt))))
                         (point-min))))
                   (pos-end-fmt (treesit-node-start n-fmt))
                   (fmt-spaces (visual-format--with-buff-fmt (buffer-substring pos-beg-fmt pos-end-fmt))))
              (visual-format--with-buff
               (message "Token at (%d):\t %s \t\t\t\t Prev: %s"
                        depth
                        (treesit-node-text n t)
                        (treesit-node-text prev-leaf t))
               (unless (string= (buffer-substring pos-beg pos-end) fmt-spaces)
                 (put-text-property pos-beg pos-end 'display fmt-spaces)))
              (setq prev-leaf n
                    prev-leaf-fmt n-fmt))
          (let ((last-nodes (visual-format--depth-first-walk n n-fmt prev-leaf prev-leaf-fmt (1+ depth))))
            (setq prev-leaf (car last-nodes)
                  prev-leaf-fmt (cdr last-nodes))))))
    (cons prev-leaf prev-leaf-fmt)))

(defvar visual-format-function #'apheleia-format-buffer)

(defun visual-format-buffer ()
  "Visually format the buffer without modifing it."
  (interactive)
  (visual-format--cleanup (point-min) (point-max))
  (let ((visual-format--buffer (current-buffer))
        (visual-format--buffer-formatted (get-buffer-create (format " *visual-format: %s*" (buffer-name))))
        (buf-str (buffer-string))
        (buf-mode major-mode))
    (visual-format--with-buff-fmt
     (delay-mode-hooks (funcall buf-mode))
     (delete-region (point-min) (point-max))
     (insert buf-str)
     (call-interactively visual-format-function))
    (with-silent-modifications
      (visual-format--depth-first-walk))))

;;;###autoload
(define-minor-mode visual-format-mode
  "Format code without modifying the buffer."
  :lighter " VFmt"
  :global nil
  (if visual-format-mode
      (visual-format-buffer)
    (visual-format--cleanup (point-min) (point-max))))
