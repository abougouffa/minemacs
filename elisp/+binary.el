;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(defcustom +binary-objdump-executable (executable-find "objdump")
  "Path to the executable \"objdump\" utility.")

(defcustom +binary-objdump-enable t
  "Enable or disable disassembling suitable files with objdump.")

(defcustom +binary-hexl-enable t
  "Enable or disable openning suitable files in `hexl-mode'.")

;;;###autoload
(defun +binary-objdump-buffer-p (&optional buffer)
  "Can the BUFFER be viewed as a disassembled code with objdump."
  (and +binary-objdump-enable
       (when-let* ((file (buffer-file-name (or buffer (current-buffer))))
                   (file (file-truename file)))
         (and +binary-objdump-executable
              (file-exists-p file)
              (not (file-directory-p file))
              (not (zerop (file-attribute-size (file-attributes file))))
              (not (string-match-p
                    "file format not recognized"
                    (with-temp-buffer
                      (shell-command (format "%s --file-headers %s"
                                             +binary-objdump-executable
                                             (shell-quote-argument file))
                                     (current-buffer))
                      (buffer-string))))))))

;; A predicate for detecting binary files. Inspired by:
;; https://emacs.stackexchange.com/questions/10277/make-emacs-automatically-open-binary-files-in-hexl-mode
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
  (if (+binary-objdump-buffer-p)
      (when-let ((file (buffer-file-name))
                 (buffer-read-only nil))
        (message "Disassembling file \"%s\" using objdump." (file-name-nondirectory file))
        (erase-buffer)
        (call-process "objdump" nil (current-buffer) nil "-d" file)
        (view-mode)
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (set-visited-file-name nil t)
        (buffer-disable-undo)
        (set-buffer-modified-p nil)
        (setq-local buffer-read-only t))
    (message "Objdump can not be used with this buffer.")))

;;;###autoload
(defun +binary-hexl-mode-maybe ()
  "If `hexl-mode' is not already active, and the current buffer
is binary, activate `hexl-mode'."
  (interactive)
  (unless (eq major-mode 'hexl-mode)
    (when (+binary-hexl-buffer-p)
      (hexl-mode))))

;;;###autoload
(defun +binary-setup-modes ()
  (add-to-list 'magic-fallback-mode-alist '(+binary-objdump-buffer-p . objdump-disassemble-mode) t)
  (add-to-list 'magic-fallback-mode-alist '(+binary-hexl-buffer-p . +binary-hexl-mode-maybe) t))
