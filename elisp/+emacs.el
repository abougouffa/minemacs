;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;;;###autoload
(defun +dir-locals-reload-for-this-buffer ()
  "Reload directory-local for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)
    (+info! "Reloaded directory-local variables for buffer %s"
            (buffer-name (current-buffer)))))

;;;###autoload
(defun +dir-locals-reload-for-all-buffers-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (+dir-locals-reload-for-this-buffer))))))

(defun +dir-locals--autoreload-h ()
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (+dir-locals-reload-for-all-buffers-in-this-directory)
    (message "Reloaded directory-local variables defined in %s." dir-locals-file)))

(defvar +dir-locals--autoreload-p nil)

;;;###autoload
(defun +dir-locals-toggle-autoreload (&optional enable)
  "Toggle autoloading directory-local variables after editing the \".dir-locals\" file.
If ENABLE is non-nil, force enabling autoreloading."
  (interactive)
  (if (or enable +dir-locals--autoreload-p)
      (progn
        (remove-hook 'after-save-hook #'+dir-locals--autoreload-h)
        (setq +dir-locals--autoreload-p nil)
        (message "Disabled auto-reloading directory-locals."))
    (add-hook 'after-save-hook #'+dir-locals--autoreload-h)
    (setq +dir-locals--autoreload-p t)
    (message "Enabled auto-reloading directory-locals.")))

;;;###autoload
(defun +dir-locals-open-or-create ()
  "Open or create the dir-locals.el for the current project."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (base-dir (car (ensure-list (dir-locals-find-file file-name)))))
    (find-file
     (cond (base-dir (expand-file-name dir-locals-file base-dir))
           ((project-current) (expand-file-name dir-locals-file (project-root (project-current))))
           ((vc-root-dir) (expand-file-name dir-locals-file (vc-root-dir)))
           (t (expand-file-name dir-locals-file (file-name-directory file-name)))))))

;; The hook is defined and enabled by default in `me-defaults'
;;;###autoload
(defun +toggle-auto-whitespace-cleanup ()
  "Toggle auto-deleting trailing whitespaces."
  (interactive)
  (if (member #'+save--whitespace-cleanup-h before-save-hook)
      (progn
        (message "+toggle-auto-whitespace-cleanup: Disabled.")
        (remove-hook 'before-save-hook #'+save--whitespace-cleanup-h))
    (message "+toggle-auto-whitespace-cleanup: Enabled.")
    (add-hook 'before-save-hook #'+save--whitespace-cleanup-h)))

;; Adapted from: rougier/nano-emacs
;;;###autoload
(defun +what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(defcustom +screenshot-delay 5
  "A delay to wait before taking the screenshot.
Applicable only when calling `+screenshot-svg' with a prefix."
  :group 'minemacs-utils
  :type 'number)

;; Inspired by: reddit.com/r/emacs/comments/idz35e/comment/g2c2c6y
;;;###autoload
(defun +screenshot-svg (outfile)
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring. If launched with a
prefix or universal argument, it waits for a moment (defined by
`+screenshot-delay') before taking the screenshot."
  (interactive "FSave to file: ")
  (let ((outfile (file-name-with-extension outfile "svg")))
    (if current-prefix-arg
        (run-with-timer +screenshot-delay nil (apply-partially #'+screenshot-svg--take-screenshot outfile))
      (+screenshot-svg--take-screenshot outfile))))

(defun +screenshot-svg--take-screenshot (&optional outfile)
  (let* ((tmp-file (make-temp-file "emacs-" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file tmp-file (insert data))
    (when (stringp outfile) (copy-file tmp-file outfile))
    (message "Screenshot saved to %s" (or outfile tmp-file))))

;;;###autoload
(defun +region-or-thing-at-point ()
  "Return the region or the thing at point."
  (when-let ((thing (or (prog1 (thing-at-point 'region t)
                          (deactivate-mark))
                        (cl-some (+apply-partially-right #'thing-at-point t)
                                 '(symbol email number string word)))))
    ;; If the matching thing has multi-lines, join them
    (string-join (string-lines thing))))

(defvar +webjump-read-string-initial-query nil)

(defun +webjump-read-string-with-initial-query (prompt)
  (let ((input (read-string (concat prompt ": ") +webjump-read-string-initial-query)))
    (unless (webjump-null-or-blank-string-p input) input)))

;;;###autoload
(defun +webjump ()
  "Like `webjump', with initial query filled from `+region-org-thing-at-point'."
  (interactive)
  (require 'webjump)
  (let ((+webjump-read-string-initial-query (+region-or-thing-at-point)))
    (cl-letf (((symbol-function 'webjump-read-string) #'+webjump-read-string-with-initial-query))
      (webjump))))

;;;###autoload
(defmacro +def-dedicated-tab! (cmd &rest body)
  "Define +CMD command to run BODY in a dedicated tab.
If not specified, BODY defaults to `(CMD)'.

You can pass an exit hook or exit function on which, the created workspace will
be deleted.

\(fn NAME [[:exit-hook HOOK] [:exit-func FUNC]] FORMS...)"
  (let* ((cmd (+unquote cmd))
         (fn-name (intern (format "+%s" cmd)))
         (fn-doc (format "Launch %s in a dedicated workspace." cmd))
         (tab-name (intern (format "+%s-tab-name" cmd)))
         (exit-fn-name (intern (format "+%s--close-workspace" cmd)))
         exit-func exit-hook sexp fn-body)
    (while (keywordp (car body))
      (pcase (pop body)
        (:exit-func (setq exit-func (+unquote (pop body))))
        (:exit-hook (setq exit-hook (+unquote (pop body))))))
    (setq sexp (if (null body) `((,cmd)) body))
    (when (or exit-func exit-hook)
      (setq
       fn-body
       `((defun ,exit-fn-name (&rest _)
          (if (fboundp 'tabspaces-mode)
              ;; When `tabspaces' is available, use it.
              (when-let ((tab-num (seq-position (tabspaces--list-tabspaces) ,tab-name #'string=)))
               (tabspaces-close-workspace (1+ tab-num)))
            ;; Or default to the built-in `tab-bar'.
            (when-let ((tab-num (seq-position (tab-bar-tabs) ,tab-name (lambda (tab name) (string= name (alist-get 'name tab))))))
             (tab-close (1+ tab-num)))))))
      (when exit-func
        (setq fn-body (append fn-body `((advice-add ',exit-func :after #',exit-fn-name)))))
      (when exit-hook
        (setq fn-body (append fn-body `((add-hook ',exit-hook #',exit-fn-name))))))
    `(progn
       (defvar ,tab-name ,(format "*%s*" cmd))
       (defun ,fn-name ()
        ,fn-doc
        (interactive)
        (when ,tab-name
         (if (fboundp 'tabspaces-mode)
             (tabspaces-switch-or-create-workspace ,tab-name)
           (tab-new)
           (tab-rename ,tab-name)))
        ,@sexp)
       ,(macroexp-progn fn-body)
       #',fn-name)))

;;; +emacs.el ends here
