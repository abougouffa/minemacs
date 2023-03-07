;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;; PERF+HACK At some point, MinEmacs startup become too slow, specially when
;; initializing `general' and `evil'. After trying several configurations, I
;; figured out that deferring `general' solves the issue. However, deferring
;; `general' means that we cannot define the keybindings when loading other
;; packages, i.e. before `general' gets loaded and the MinEmacs definers (i.e.
;; `+minemacs--internal-map!', `+minemacs--internal-map-local!', ...) are made
;; available. We overcome this by defining these macros to define the
;; keybindings by wrapping the actual definition in a `with-eval-after-load'
;; block to be evaluated only after `general' gets loaded and configured and the
;; definers are ready (See `me-keybindings').
;;;###autoload
(defmacro +map! (&rest args)
  "A wrapper around `+minemacs--internal-map!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (+minemacs--internal-map! ,@args)))

;;;###autoload
(defmacro +map-local! (&rest args)
  "A wrapper around `+minemacs--internal-map-local!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (+minemacs--internal-map-local! ,@args)))

;; Wrappers around `general's VIM like definers, needs `general-evil-setup' to
;; be executed (See `me-keybindings')
;;;###autoload
(defmacro +nmap! (&rest args)
  "A wrapper around `general-nmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-nmap ,@args)))

;;;###autoload
(defmacro +vmap! (&rest args)
  "A wrapper around `general-vmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-vmap ,@args)))

;;;###autoload
(defmacro +mmap! (&rest args)
  "A wrapper around `general-mmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-mmap ,@args)))

;;;###autoload
(defmacro +imap! (&rest args)
  "A wrapper around `general-imap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-imap ,@args)))

;;;###autoload
(defmacro +emap! (&rest args)
  "A wrapper around `general-emap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-emap ,@args)))

;;;###autoload
(defmacro +omap! (&rest args)
  "A wrapper around `general-omap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-omap ,@args)))

;;;###autoload
(defmacro +rmap! (&rest args)
  "A wrapper around `general-rmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-rmap ,@args)))

;;;###autoload
(defmacro +iemap! (&rest args)
  "A wrapper around `general-iemap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-iemap ,@args)))

;;;###autoload
(defmacro +nvmap! (&rest args)
  "A wrapper around `general-nvmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-nvmap ,@args)))

;; Make old definitions obsolete, and make aliases for them to avoid breaking
;; users configurations.
(dolist (fn '(map map-local nmap vmap mmap imap emap omap rmap iemap nvmap))
  (let ((new-fn (intern (format "+%s!" fn)))
        (old-fn (intern (format "+%s" fn))))
    (define-obsolete-function-alias old-fn new-fn "2023-03-07")))
