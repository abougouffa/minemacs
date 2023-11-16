;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(defgroup minemacs-binary nil
  "MinEmacs binary files."
  :group 'minemacs)

(defcustom +binary-objdump-executable "objdump"
  "The \"objdump\" command."
  :group 'minemacs-binary
  :type '(choice file string))

(defcustom +binary-objdump-enable t
  "Enable or disable disassembling suitable files with objdump."
  :group 'minemacs-binary
  :type 'boolean)

(defcustom +binary-hexl-enable t
  "Enable or disable openning suitable files in `hexl-mode'."
  :group 'minemacs-binary
  :type 'boolean)

(defun +binary-objdump-p (filename)
  "Can FILENAME be recognized by \"objdump\"."
  (when-let* ((file (and filename (file-truename filename))))
    (and +binary-objdump-executable
         (executable-find +binary-objdump-executable)
         (not (file-remote-p file))
         (file-exists-p file)
         (not (file-directory-p file))
         (not (zerop (file-attribute-size (file-attributes file))))
         (not (string-match-p "file format not recognized"
                              (shell-command-to-string
                               (format "%s --file-headers %s"
                                       +binary-objdump-executable
                                       (shell-quote-argument file))))))))

;;;###autoload
(defun +binary-objdump-buffer-p (&optional buffer)
  "Can the BUFFER be viewed as a disassembled code with objdump."
  (and +binary-objdump-enable (+binary-objdump-p (buffer-file-name buffer))))

;; A predicate for detecting binary files. Inspired by:
;; emacs.stackexchange.com/q/10277/37002)
(defun +binary-buffer-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least one null byte.

Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (search-forward (string ?\x00) nil t 1))))

;;;###autoload
(defun +binary-hexl-buffer-p (&optional buffer)
  "Is the current buffer should be viewed using `hexl-mode'."
  (and +binary-hexl-enable
       (+binary-buffer-p buffer)
       ;; Executables are viewed with objdump mode
       (not (+binary-objdump-buffer-p buffer))))

;;;###autoload
(define-derived-mode objdump-disassemble-mode
  asm-mode "Objdump Mode"
  "Major mode for viewing executable files disassembled using objdump."
  (if-let* ((file (buffer-file-name))
            (objdump-file (+binary-objdump-p file)))
      (let ((buffer-read-only nil))
        (message "Disassembling file \"%s\" using objdump." (file-name-nondirectory file))
        (erase-buffer)
        (call-process "objdump" nil (current-buffer) nil "-d" file)
        (view-mode)
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (set-visited-file-name nil t)
        (buffer-disable-undo)
        (set-buffer-modified-p nil)
        (asm-mode)
        (setq-local buffer-read-only t))
    (message "Objdump can not be used with this buffer.")))

;;;###autoload
(defun +binary-hexl-mode-maybe ()
  "If `hexl-mode' is not already active, and the current buffer
is binary, activate `hexl-mode'."
  (interactive)
  (when (and (not (eq major-mode 'hexl-mode)) (+binary-hexl-buffer-p))
    (hexl-mode 1)))

;;;###autoload
(defun +binary-setup-modes ()
  (add-to-list 'magic-fallback-mode-alist '(+binary-objdump-buffer-p . objdump-disassemble-mode) t)
  (add-to-list 'magic-fallback-mode-alist '(+binary-hexl-buffer-p . +binary-hexl-mode-maybe) t))

;;; +binary.el ends here
