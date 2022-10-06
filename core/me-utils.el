;;; utils.el --- Useful utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>
;; Keywords: convenience

;;; === Primitives ===

;;;###autoload
(defun me-log! (msg &rest vars)
  "Log MSG and VARS using `message' when `init-file-debug' is non-nil."
  (when init-file-debug
    (apply #'message (cons (concat "[MinEmacs] " msg) vars))))

;;;###autoload
(defun me-info! (msg &rest vars)
  "Log info MSG and VARS using `message'."
  (let ((inhibit-message))
    (apply #'message (cons (concat "[MinEmacs] " msg) vars))))

;; (me-bool "someval") ;; ==> t
;;;###autoload
(defun me-bool (val) (not (null val)))

;;; === Higher order functions ===

;; (me-foldr (lambda (a b) (message "(%d + %d)" a b) (+ a b)) 0 '(1 2 3 4 5)) ;; ==> 15
;; (5 + 0) -> (4 + 5) -> (3 + 9) -> (2 + 12) --> (1 + 14)
;;;###autoload
(defun me-foldr (fun acc seq)
  (if (null seq) acc
    (funcall fun (car seq) (me-foldr fun acc (cdr seq)))))

;; (me-foldl (lambda (a b) (message "(%d + %d)" a b) (+ a b)) 0 '(1 2 3 4 5)) ;; ==> 15
;; (0 + 1) -> (1 + 2) -> (3 + 3) -> (6 + 4) -> (10 + 5)
;;;###autoload
(defun me-foldl (fun acc seq)
  (if (null seq) acc
    (me-foldl fun (funcall fun acc (car seq)) (cdr seq))))

;; (me-all '(83 88 t "txt")) ;; ==> t
;;;###autoload
(defun me-all (seq)
  (me-foldr (lambda (r l) (and r l)) t seq))

;; (me-some '(nil nil "text" nil 2)) ;; ==> t
;;;###autoload
(defun me-some (seq)
  (me-bool (me-foldr (lambda (r l) (or r l)) nil seq)))

;; (me-filter 'stringp '("A" 2 "C" nil 3)) ;; ==> ("A" "C")
;;;###autoload
(defun me-filter (fun seq)
  (when seq
    (let ((head (car seq))
          (tail (cdr seq)))
      (if (funcall fun head)
          (cons head (me-filter fun tail))
        (me-filter fun tail)))))

;; (me-zip '(1 2 3 4) '(a b c d) '("A" "B" "C" "D")) ;; ==> ((1 a "A") (2 b "B") (3 c "C") (4 d "D"))
;;;###autoload
(defun me-zip (&rest seqs)
  (if (null (car seqs)) nil
    (cons (mapcar #'car seqs)
          (apply #'me-zip (mapcar #'cdr seqs)))))

;;; === Strings ===

;; (me-str-join ", " '("foo" "10" "bar")) ;; ==> "foo, 10, bar"
;;;###autoload
(defun me-str-join (sep seq)
  (me-foldl (lambda (l r) (concat l sep r))
            (car seq) (cdr seq)))

;; (me-str-split "foo, 10, bar" ", ") ;; ==> ("foo" "10" "bar")
;;;###autoload
(defun me-str-split (str sep)
  (let ((s (string-search sep str)))
    (if s (cons (substring str 0 s)
                (me-str-split (substring str (me- s (length sep))) sep))
      (list str))))

;;;###autoload
(defun me-str-replace (old new s)
  "Replaces OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

;;;###autoload
(defun me-str-replace-all (replacements s)
  "REPLACEMENTS is a list of cons-cells. Each `car` is replaced with `cdr` in S."
  (replace-regexp-in-string (regexp-opt (mapcar 'car replacements))
                            (lambda (it) (cdr (assoc-string it replacements)))
                            s t t))

;;; === Files, IO ===

;;;###autoload
(defun me-file-mime-type (file)
  "Get MIME type for FILE based on magic codes provided by the 'file' command.
Return a symbol of the MIME type, ex: `text/x-lisp', `text/plain',
`application/x-object', `application/octet-stream', etc."
  (let ((mime-type (shell-command-to-string (format "file --brief --mime-type %s" file))))
    (intern (string-trim-right mime-type))))

;;;###autoload
(defun me-file-name-incremental (filename)
  "Return an unique file name for FILENAME.
If \"file.ext\" exists, returns \"file-0.ext\"."
  (let* ((ext (file-name-extension filename))
         (dir (file-name-directory filename))
         (file (file-name-base filename))
         (filename-regex (concat "^" file "\\(?:-\\(?1:[[:digit:]]me-\\)\\)?" (if ext (concat "\\." ext) "")))
         (last-file (car (last (directory-files dir nil filename-regex))))
         (last-file-num (when (and last-file (string-match filename-regex last-file) (match-string 1 last-file))))
         (num (1me- (string-to-number (or last-file-num "-1"))))
         (filename (file-name-concat dir (format "%s%s%s" file (if last-file (format "-%d" num) "") (if ext (concat "." ext) "")))))
    filename))

;;;###autoload
(defun me-file-read-to-string (filename)
  "Return a string with the contents of FILENAME."
  (when (and (file-exists-p filename) (not (file-directory-p filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

;;; === Systemd ===

;;;###autoload
(defun me-systemd-running-p (service)
  "Check if the systemd SERVICE is running."
  (zerop (call-process "systemctl" nil nil nil "--user" "is-active" "--quiet" service ".service")))

;;;###autoload
(defun me-systemd-command (service command &optional pre-fn post-fn)
  "Call systemd with COMMAND and SERVICE."
  (interactive)
  (when pre-fn (funcall pre-fn))
  (let ((success (zerop (call-process "systemctl" nil nil nil "--user" command service ".service"))))
    (unless success
      (user-error "[systemd]: Failed on calling '%s' on service %s.service." command service))
    (when post-fn (funcall post-fn success))
    success))

;;;###autoload
(defun me-systemd-start (service &optional pre-fn post-fn)
  "Start systemd SERVICE."
  (interactive)
  (me-systemd-command service "start" pre-fn post-fn))

;;;###autoload
(defun me-systemd-stop (service &optional pre-fn post-fn)
  "Stops the systemd SERVICE."
  (interactive)
  (me-systemd-command service "stop" pre-fn post-fn))

;;;###autoload
(defun me-delete-this-file (&optional path force-p)
  "Delete PATH.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (when (file-exists-p path)
          (error "Failed to delete %S" short-path))))))

;;;###autoload
(defun me-move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (expand-file-name (file-name-nondirectory old-path) new-path)))
    (make-directory (file-name-directory new-path) t)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    ;; (doom-files--update-refs old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

;; See https://emacs.stackexchange.com/questions/3022/reset-custom-variable-to-default-value-programmatically0
;;;###autoload
(defun me-reset-sym (sym)
  "Reset SYM to its standard value."
  (set sym (eval (car (get sym 'standard-value)))))


;;;###autoload
(defmacro me-reset-var! (var)
  "Reset VAR to its standard value."
  `(setq ,var (eval (car (get ',var 'standard-value)))))

;;;###autoload
(defmacro me-cmdfy! (body)
  "Convert BODY to an interactive command."
  `(lambda ()
     (interactive)
     (,@body)))

;;;###autoload
(defun me-dir-locals-reload-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

;;;###autoload
(defun me-dir-locals-reload-for-all-buffers-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (me-dir-locals-reload-for-current-buffer))))))

;;;###autoload
(defun me-dir-locals-enable-autoreload ()
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (message "Dir-locals will be reloaded after saving.")
    (add-hook 'after-save-hook 'me-dir-locals-reload-for-all-buffers-in-this-directory nil t)))

(defun me--sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

;;;###autoload
(defun me-sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (me--sudo-file-path file)))

;;;###autoload
(defun me-sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (find-file
   (me--sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))

;;;###autoload
(defun me-sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (let ((file (me--sudo-file-path buffer-file-name)))
    (if-let (buffer (find-file-noselect file))
        (let ((origin (current-buffer)))
          (copy-to-buffer buffer (point-min) (point-max))
          (unwind-protect
              (with-current-buffer buffer
                (save-buffer))
            (unless (eq origin buffer)
              (kill-buffer buffer))
            (with-current-buffer origin
              (revert-buffer t t))))
      (user-error "Unable to open %S" file))))

;;;###autoload
(defun me-clean-file-name (filename &optional conv-downcase)
  "Clean file name."
  (replace-regexp-in-string "[:;\t \\]+" "-" (if conv-downcase (downcase filename) filename)))

;;;###autoload
(defun me-set-fonts ()
  (custom-set-faces
   `(default
      ((t (:font ,(format "%s %d"
                          (or (plist-get me-fonts :font-family)
                              (plist-get me-default-fonts :font-family))
                          (or (plist-get me-fonts :font-size)
                              (plist-get me-default-fonts :font-size)))))))
   `(fixed-pitch
     ((t (:inherit (default)))))
   `(fixed-pitch-serif
     ((t (:inherit (default)))))
   `(variable-pitch
     ((t (:font ,(format "%s %d"
                         (or (plist-get me-fonts :variable-pitch-font-family)
                             (plist-get me-default-fonts :variable-pitch-font-family))
                         (or (plist-get me-fonts :variable-pitch-font-size)
                             (plist-get me-default-fonts :variable-pitch-font-size)))))))))

;;;###autoload
(defun me-dir-locals-open-or-create ()
  "Open or create the dir-locals.el for the current project."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (base-dir (car (ensure-list (dir-locals-find-file file-name)))))
    (find-file
     (cond (base-dir (expand-file-name dir-locals-file base-dir))
           ((project-current) (expand-file-name dir-locals-file (project-root (project-current))))
           ((vc-root-dir) (expand-file-name dir-locals-file (vc-root-dir)))
           (t (expand-file-name dir-locals-file (file-name-directory file-name)))))))


(provide 'me-utils)
