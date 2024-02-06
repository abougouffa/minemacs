;;; me-embedded.el --- Embedded systems stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package embed
  :straight (:host github :repo "xal-0/embed-el")
  :init
  (+map! :infix "o"
    "b" '(nil :wk "embed")
    "bo" #'embed-openocd-start
    "bO" #'embed-openocd-stop
    "bg" #'embed-openocd-gdb
    "bf" #'embed-openocd-flash))

(use-package arduino-mode
  :straight (:host github :repo "bookest/arduino-mode")
  :hook (arduino-mode . display-line-numbers-mode)
  :hook (arduino-mode . hs-minor-mode))

(use-package dts-mode
  :straight t)

(use-package virtual-dts-mode
  :straight (:host github :repo "connorfeeley/virtual-dts-mode"))

(use-package bitbake
  :straight (bitbake-modes :host bitbucket :repo "olanilsson/bitbake-modes")
  :config
  (+map-local! :keymaps 'bitbake-mode-map
    "b"  #'bitbake-recipe-build-dir-dired)
  (+map-local! :keymaps 'bitbake-mode-map
    :infix "i"
    "i" #'bitbake-inc-pr))

(use-package bitbake-electric
  :straight (bitbake-modes :host bitbucket :repo "olanilsson/bitbake-modes")
  :hook (bitbake-mode . bitbake-electric-mode))

(use-package bitbake-insert
  :straight (bitbake-modes :host bitbucket :repo "olanilsson/bitbake-modes")
  :after bitbake
  :demand t
  :config
  (+map-local! :keymaps 'bitbake-mode-map
    :infix "i"
    "v" #'bitbake-insert-var
    "a" #'bitbake-append-var
    "o" #'bitbake-insert-override))

(use-package mips-mode
  :straight t)

(use-package riscv-mode
  :straight t)

(use-package x86-lookup
  :straight t
  :custom
  (x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-pdf-tools)
  ;; Get manual from intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
  (x86-lookup-pdf (concat minemacs-local-dir "intel-64-and-ia32-volumes-1234.pdf"))
  :config
  (unless (file-exists-p x86-lookup-pdf)
    (url-copy-file "https://cdrdv2.intel.com/v1/dl/getContent/671200" x86-lookup-pdf t)))

(autoload 'term-send-string "term")
(defcustom +serial-port "/dev/ttyUSB0"
  "The default port (device) to use."
  :group 'minemacs-embedded
  :type 'file)

(defcustom +serial-baudrate 115200
  "The default baudrate to use."
  :group 'minemacs-embedded
  :type 'natnum)

(defcustom +serial-first-commands nil
  "A list of commands to run in the serial terminal after creation."
  :group 'minemacs-embedded
  :type '(repeat string))

(defvar +serial-buffer nil)
(defvar +serial-process nil)

(defun +serial-running-p () (buffer-live-p +serial-buffer) (process-live-p +serial-process))

(defun +serial--run-command-via-serial (port baud &rest commands)
  "Run COMMANDS on a device via serial communication.

Connect at PORT with baudrate BAUD."
  (let ((commands (ensure-list commands)))
    (unless (+serial-running-p)
      (setq +serial-buffer (serial-term port baud)
            +serial-process (get-buffer-process +serial-buffer)
            commands (append +serial-first-commands commands)))
    (if (+serial-running-p)
        (term-send-string +serial-process (string-join (append commands '("\n")) "\n"))
      (user-error "Unable to communicate with the serial terminal process"))))

(defun +serial-run-command-via-serial (&optional port baud &rest commands)
  "Run COMMANDS on a device via serial communication.

If PORT or BAUD are nil, use values from `+serial-port' and `+serial-baudrate'."
  (interactive)
  (let* ((port (or port +serial-port))
         (baud (or baud +serial-baudrate))
         (commands (or commands (list (read-shell-command (format "Run on %s@%d: " port baud))))))
    (apply #'+serial--run-command-via-serial (append (list port baud) commands))))


(provide 'me-embedded)

;;; me-embedded.el ends here
