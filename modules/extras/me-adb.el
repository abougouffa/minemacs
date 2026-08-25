;;; me-adb.el --- Interface for ADB commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-22
;; Last modified: 2026-08-25

;;; Commentary:

;;; Code:

(defvar +adb-buffer-name "*adb*")
(defvar +adb-process-name "adb-command")
(defvar +adb-push-src-dest-cache nil)
(defvar +adb-push-dest-history nil)
(defvar +adb-after-command-functions nil
  "Functions to call after the command finishes.")

(defvar +adb--buffer nil)
(defun +adb--after-command (buff desc)
  (when (and buff (eq buff +adb--buffer))
    (run-hook-with-args '+adb-after-command-functions (equal desc "finished\n") buff)))

(with-eval-after-load 'compile
  (add-hook 'compilation-finish-functions '+adb--after-command))

;;;###autoload
(progn
  (defvar +adb-program "adb")
  (defvar +adb-available-p nil)
  (defun +adb-available-p (&rest _args)
    (with-memoization +adb-available-p
      (and (executable-find +adb-program) t))))

;;;###autoload
(dolist (cmd '(+adb-push +adb-remount +adb-reboot +adb-root))
  (put cmd 'completion-predicate #'+adb-available-p))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables '+adb-push-src-dest-cache)
  (add-to-list 'savehist-additional-variables '+adb-push-dest-history))

(defun +adb-run-command (&rest args)
  "Run adb with command ARGS."
  (let ((display-buffer-overriding-action
         '((display-buffer-in-side-window) (window-height . 0.2) (reusable-frames . visible) (dedicated . t) (side . bottom) (slot . -1)))
        (compilation-buffer-name-function (lambda (&rest _args) +adb-buffer-name))
        (cmd (string-join `(,+adb-program ,@(seq-filter #'identity args)) " ")))
    (setq +adb--buffer (compile cmd))))

;;;###autoload
(defun +adb-push (src dest)
  "Run adb push SRC DEST."
  (interactive
   (let* ((src-path (or (when (not current-prefix-arg)
                          (if (derived-mode-p 'dired-mode)
                              (dired-file-name-at-point)
                            (buffer-file-name)))
                        (read-file-name "Source path: ")))
          (dest-path (alist-get (expand-file-name src-path) +adb-push-src-dest-cache nil nil #'equal)))
     (list src-path
           (read-string "Destination path: " dest-path '+adb-push-dest-history))))
  (let ((src (expand-file-name src)))
    (+alist-set! src dest +adb-push-src-dest-cache)
    (+adb-run-command "push" src dest)))

(defconst +adb-devices--header "List of devices attached" "Header line that precedes the device list in `adb devices' output.")
(defconst +adb-devices--kv-regexp "\\`\\([A-Za-z_][A-Za-z0-9_]*\\):\\(.*\\)\\'" "Regexp matching a complete KEY:VALUE token, e.g. \"transport_id:1\".")

(defun +adb-devices--keyword (name)
  "Return NAME as a keyword, turning underscores into dashes.
For example \"transport_id\" becomes `:transport-id'."
  (intern (concat ":" (subst-char-in-string ?_ ?- (downcase name)))))

(defun +adb-devices--parse-line (line)
  "Parse LINE, a single device line of `adb devices -l' output.
Return a cons cell (SERIAL . PLIST), or nil if LINE holds no device."
  (let ((tokens (split-string line "[[:space:]]+" t)))
    (when (cdr tokens) ; need a serial plus at least a state
      (let ((serial (car tokens))
            state
            plist)
        (dolist (token (cdr tokens))
          (if (string-match +adb-devices--kv-regexp token)
              (let ((key (+adb-devices--keyword (match-string 1 token)))
                    (value (match-string 2 token)))
                (push key plist)
                (push value plist))
            ;; Tokens before the first KEY:VALUE make up the connection state:
            ;; "device", "offline", "unauthorized", or the multi-word "no
            ;; permissions".
            (unless plist (push token state))))
        (cons serial
              (nconc (list :state (string-join (nreverse state) " "))
                     (nreverse plist)))))))

(defun +adb-devices-parse (output)
  "Parse OUTPUT, the text printed by `adb devices -l', into an alist.
Each element is (SERIAL . PLIST), where PLIST holds `:state' plus one
keyword per KEY:VALUE field on the line.  Lines before the
\"List of devices attached\" header are ignored, so nil is returned if
that header never appears."
  (let ((seen-header nil)
        (devices '()))
    (dolist (line (split-string output "\n"))
      (setq line (string-trim line))
      (cond
       ((string-empty-p line))          ; blank line: skip
       ((not seen-header)
        (setq seen-header (string-prefix-p +adb-devices--header line)))
       (t
        (let ((device (+adb-devices--parse-line line)))
          (when device (push device devices))))))
    (nreverse devices)))

;;;###autoload
(defun +adb-devices ()
  "Run `adb devices -l' and return the result of `+adb-devices-parse'."
  (with-temp-buffer
    (let ((status (call-process +adb-program nil t nil "devices" "-l")))
      (unless (eq status 0)
        (error "adb exited with %S: %s" status (string-trim (buffer-string))))
      (+adb-devices-parse (buffer-string)))))

;;;###autoload
(defun +adb-devices-get (devices serial &optional property)
  "Look up SERIAL in DEVICES, as returned by `+adb-devices-parse'.
Return its plist, or with PROPERTY only that property's value."
  (let ((plist (cdr (assoc serial devices))))
    (if property (plist-get plist property) plist)))

(defvar +adb-devices-alist nil)
(with-eval-after-load 'marginalia
  (defun +marginalia-annotate-adb-device (cand)
    (when-let* ((plist (alist-get cand +adb-devices-alist nil nil #'equal)))
      (marginalia--fields
       ((format "%s (%s)" (plist-get plist :product) (plist-get plist :model)) :face 'marginalia-file-name))))
  (add-to-list 'marginalia-annotators '(+adb-device +marginalia-annotate-adb-device builtin none)))

;;;###autoload
(defun +adb-get-device ()
  "Get the ADB connected device, ask with `completing-read' if found many."
  (if-let* ((devs (+adb-devices)))
      (if (length= devs 1)
          (car devs)
        (let* ((+adb-devices-alist devs))
          (completing-read "Select the device: " (+completion-mark-category devs '+adb-device))))
    (error "No connected device")))

;;;###autoload
(defun +adb-remount (auto-reboot-device)
  "Run adb remount, with -R when AUTO-REBOOT-DEVICE is non-nil."
  (interactive "P")
  (+adb-run-command "remount" (when auto-reboot-device "-R")))

;;;###autoload
(defun +adb-reboot (&optional mode no-confirm)
  "Run adb reboot MODE, when NO-CONFIRM is non-nil, don't ask."
  (interactive (list (and current-prefix-arg (completing-read "Reboot in mode: " '("bootloader" "recovery" "sideload" "sideload-auto-reboot" "edl")))))
  (when (or no-confirm (y-or-n-p "Do you really want to reboot the device? "))
    (+adb-run-command "reboot" mode)))

;;;###autoload
(defun +adb-root (&optional arg)
  "Run adb root (or unroot with \\[universal-argument])."
  (interactive "P")
  (+adb-run-command (if arg "unroot" "root")))


(provide 'me-adb)
;;; me-adb.el ends here
