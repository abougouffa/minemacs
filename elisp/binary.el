;;; -*- lexical-binding: t; -*-

(defvar +binary-objdump-executable (executable-find "objdump"))

(defcustom +binary-objdump-enable t
  "Enable or disable disassembling suitable files with objdump.")

;;;###autoload
(defun +binary-objdump-buffer-p (&optional buffer)
  "Can the BUFFER be viewed as a disassembled code with objdump."
  (when +binary-objdump-enable
    (when-let ((file (buffer-file-name (or buffer (current-buffer)))))
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

;; Predicate for detecting binary files, inspired by:
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
(defun +binary-hexl-buffer-p ()
  (and (+binary-buffer-p)
       ;; Executables are viewed with objdump mode
       (not (+binary-objdump-buffer-p))))

;;;###autoload
(define-derived-mode objdump-disassemble-mode
  asm-mode "Objdump Mode"
  "Major mode for viewing executable files disassembled using objdump."
  (if (not (+binary-objdump-buffer-p))
      (message "Objdump can not be used with this buffer.")
    (let ((file (buffer-file-name))
          (buffer-read-only nil))
      (erase-buffer)
      (message "Disassembling file \"%s\" using objdump." (file-name-nondirectory file))
      (call-process "objdump" nil (current-buffer) nil "-d" file)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (view-mode)
      (set-visited-file-name nil t))))

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
