;;; me-project-x.el --- Extensions for the builtin `project' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-26
;; Last modified: 2025-07-14

;;; Commentary:

;;; Code:

;;;###autoload(with-eval-after-load 'project (require 'me-project-x))

;;; Thin `projectile' compatibility layer

;; Define some `projectile' wrapper functions on top of `project' (required by
;; some packages like `fzf', `neotree', `platformio-mode', etc.)

;;;###autoload
(progn
  (provide 'projectile)

  (defun projectile-project-p (&optional dir)
    (let ((default-directory (or dir default-directory)))
      (and (project-current) t)))

  (defun projectile-project-root (&optional dir)
    (when-let* ((default-directory (or dir default-directory))
                (proj (project-current)))
      (expand-file-name (project-root proj))))

  (defun projectile-project-name (&optional proj)
    (when-let* ((proj (or proj (project-current))))
      (project-name proj)))

  (defun projectile-project-files (&optional proj-root)
    (when-let* ((default-directory (or proj-root default-directory))
                (proj (project-current)))
      (mapcar #'file-relative-name (project-files proj))))

  (defun projectile-project-buffers (&optional proj)
    (when-let* ((proj (or proj (project-current))))
      (project-buffers proj)))

  (defun projectile-expand-root (name &optional dir)
    (when (projectile-project-p dir)
      (expand-file-name name (projectile-project-root dir))))

  (defun projectile-verify-file (file &optional dir)
    (when-let* ((file (projectile-expand-root file dir)))
      (file-exists-p file)))

  (defun projectile-project-buffer-p (buffer proj-root)
    (and (let ((default-directory proj-root))
           (member buffer (projectile-project-buffers)))
         t)))



;;; Find files x3.5 times faster using "fd", with optional caching for x1000
;;; faster listing

(defcustom +fd-program (if (executable-find "fdfind") "fdfind" "fd")
  "The fd program to use."
  :group 'minemacs-project
  :type 'string)

(defcustom +fd-ignores '("GPATH" "GRTAGS" "GTAGS" ".tags" ".ctags.d" "gtags.files" "cscope.files" "compile_commands.json")
  "List of files to ignore in \"fd\"."
  :group 'minemacs-project
  :type '(repeat string))

(defcustom +project-use-fd-on-remote t
  "Whether to use fd (if found) on remote machines' projects or not."
  :group 'minemacs-project
  :type 'boolean)

(defcustom +project-cache-project-files t
  "Cache project files when using the generic fd/find backend."
  :group 'minemacs-project
  :type 'boolean)

(defvar +project--caches (make-hash-table :test #'equal))

(defun +fd-ignores-arguments (ignores dir)
  "Like `xref--find-ignores-arguments', but for \"fd\"."
  (cl-assert (not (string-match-p "\\`~" dir)))
  (if ignores
      (concat " -E " (mapconcat #'shell-quote-argument ignores " -E "))
    ""))

(defun +fd-files-in-directory (dir ignores &optional files)
  "Find FILES (or all) using \"fd\" in directory DIR, excluding IGNORES.
Can override `project--files-in-directory' for x3.5 faster listing."
  (let* ((dir (file-name-as-directory dir))
         (default-directory dir)
         ;; Make sure ~/ etc. in local directory name is expanded and not left
         ;; for the shell command to interpret.
         (localdir (file-name-unquote (file-local-name (expand-file-name dir))))
         (command (format "%s -c never -H %s -t f -0 --strip-cwd-prefix %s"
                          +fd-program
                          (+fd-ignores-arguments (append ignores +fd-ignores) "./")
                          (if files
                              (concat (shell-quote-argument "(")
                                      (shell-quote-argument
                                       (mapconcat (lambda (wildcard)
                                                    (thread-last
                                                      (wildcard-to-regexp wildcard)
                                                      (string-remove-suffix "\\'")
                                                      (string-remove-prefix "\\`")))
                                                  (string-split files)
                                                  (concat "|")))
                                      (shell-quote-argument ")"))
                            "")))
         res)
    (with-temp-buffer
      (let ((status (process-file-shell-command command nil t)))
        (unless (zerop status)
          (goto-char (point-min))
          (if (and (not (eql status 127)) (search-forward "Permission denied\n" nil t))
              (let ((end (1- (point))))
                (re-search-backward "\\`\\|\0")
                (error "File listing failed: %s" (buffer-substring (1+ (point)) end)))
            (error "File listing failed: %s" (buffer-string))))
        (setq res (string-split (buffer-substring-no-properties (point-min) (point-max)) "\0" t))))
    (if project-files-relative-names
        (sort res #'string<)
      (project--remote-file-names
       (mapcar (apply-partially #'concat localdir)
               (sort res #'string<))))))

;; x3.5 faster than the default
(defun +project--files-in-directory-faster (orig-fn dir ignores &optional files)
  "Like `project--files-in-directory', uses \"fd\" with optional caching."
  (let ((+fd-program (let ((default-directory dir)
                           (remote (and (file-remote-p dir) t)))
                       (and (or (not remote) +project-use-fd-on-remote)
                            (seq-some (+apply-partially-right #'executable-find remote)
                                      (seq-uniq `(,+fd-program "fd" "fdfind"))))))
        (find-fn (if +fd-program #'+fd-files-in-directory orig-fn)))
    (if (and +project-cache-project-files (not files))
        (if-let* ((cached-value (gethash dir +project--caches)))
            (progn
              (+log! "Reusing cached files list for %S" dir)
              cached-value)
          (+log! "Caching files list for %S" dir)
          (let ((value (funcall find-fn dir ignores files)))
            (puthash dir value +project--caches)
            value))
      (funcall find-fn dir ignores files))))

(advice-add 'project--files-in-directory :around '+project--files-in-directory-faster)

(defun +project-clear-cache (all)
  "Clear project's files cache.
When ALL is non-nil, clear the cache of all projects."
  (interactive "P")
  (if all
      (setq +project--caches (make-hash-table :test #'equal))
    (let* ((proj (project-current t))
           (root (project-root proj)))
      (if (gethash root +project--caches)
          (progn
            (remhash root +project--caches)
            (message "Cleared cache for %s" root))
        (when (interactive-p)
          (user-error "The current project doesn't have any cache"))))))


(provide 'me-project-x)
;;; me-project-x.el ends here
