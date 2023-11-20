;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;; Modified from Crafted Emacs, pass `eglot-server-programs' to this function
;; to fill `+eglot-auto-enable-modes' with all supported modes.
(defcustom +eglot-auto-enable-modes
  '(c++-mode c++-ts-mode c-mode c-ts-mode python-mode python-ts-mode rust-mode
    rust-ts-mode cmake-mode js-mode js-ts-mode typescript-mode
    typescript-ts-mode json-mode json-ts-mode js-json-mode)
  "Modes for which Eglot can be automatically enabled by `+eglot-auto-enable'."
  :group 'minemacs-prog
  :type '(repeat symbol))

;;;###autoload
(defun +eglot-auto-enable ()
  "Auto-enable Eglot in configured modes in `+eglot-auto-enable-modes'."
  (interactive)
  (dolist (mode +eglot-auto-enable-modes)
    (let ((hook (intern (format "%s-hook" mode))))
      (add-hook hook #'eglot-ensure)
      (remove-hook hook #'lsp-deferred))))

(defun +eglot-use-on-all-supported-modes (mode-list)
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (cond
       ((listp mode) (+eglot-use-on-all-supported-modes mode))
       (t
        (when (and (not (eq 'clojure-mode mode)) ; prefer cider
                   (not (eq 'lisp-mode mode))    ; prefer sly
                   (not (eq 'scheme-mode mode))) ; prefer geiser
          (add-to-list '+eglot-auto-enable-modes mode)))))))

;;;###autoload
(defun +eglot-register (modes &rest servers)
  "Register MODES with LSP SERVERS.
Examples:
  (+eglot-register 'vhdl-mode \"vhdl_ls\")
  (+eglot-register 'lua-mode \"lua-language-server\" \"lua-lsp\")
  (+eglot-register '(c-mode c++-mode) '(\"clangd\" \"--clang-tidy\" \"-j=12\") \"ccls\")"
  (declare (indent 0))
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     (cons modes (if (length> servers 1)
                     (eglot-alternatives (ensure-list servers))
                   (ensure-list (car servers)))))))

;; From: github.com/MaskRay/ccls/wiki/eglot#misc
(defun +eglot-ccls-inheritance-hierarchy (&optional derived)
  "Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
  (interactive "P")
  (if-let* ((res (jsonrpc-request
                  (eglot--current-server-or-lose)
                  :$ccls/inheritance
                  (append (eglot--TextDocumentPositionParams)
                          `(:derived ,(if derived t :json-false))
                          '(:levels 100) '(:hierarchy t))))
            (tree (list (cons 0 res))))
      (with-help-window "*ccls inheritance*"
        (with-current-buffer standard-output
          (while tree
            (pcase-let ((`(,depth . ,node) (pop tree)))
              (cl-destructuring-bind (&key uri range) (plist-get node :location)
                (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                (make-text-button
                 (+ (pos-bol 0) depth) (pos-eol 0)
                 'action (lambda (_arg)
                           (interactive)
                           (find-file (eglot--uri-to-path uri))
                           (goto-char (car (eglot--range-region range)))))
                (cl-loop for child across (plist-get node :children)
                         do (push (cons (1+ depth) child) tree)))))))
    (eglot--error "Hierarchy unavailable")))


;;; +eglot.el ends here
