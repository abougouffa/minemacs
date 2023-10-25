;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;;; === Some plist and alist missing functions ===

;;;###autoload
(defun +varplist-get (vplist keyword &optional car-p)
  "Get KEYWORD's value from variable value length VPLIST.
Ex: (+varplist-get '(:a 'a :b 'val 'other-val) :b) -> '(val other-val)."
  (funcall
   (if car-p #'cadr #'cdr)
   (cl-loop for element in (memq keyword vplist)
            until (and (not (eq element keyword)) (keywordp element))
            collect element)))

;;;###autoload
(defun +plist-keys (plist)
  "Return the keys of PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

;;;###autoload
(defmacro +plist-push! (plist &rest key-vals)
  "Push KEY-VALS to PLIST."
  (declare (indent 1))
  (let ((out (list 'progn)))
    (while (length> key-vals 0)
      (let ((key (pop key-vals))
            (val (pop key-vals)))
        (setq out (append out `((setq ,plist (plist-put ,plist ,key ,val)))))))
    out))

;;;###autoload
(defun +plist-combine (&rest plists)
  "Create a single property list from all plists in PLISTS.
Modified from `org-combine-plists'. This supposes the values to be vectors,
and concatenate them."
  (let ((res (copy-sequence (pop plists)))
        prop val plist)
    (while plists
      (setq plist (pop plists))
      (while plist
        (setq prop (pop plist) val (pop plist))
        (setq res (plist-put res prop (vconcat val (plist-get res prop))))))
    res))

;;;###autoload
(defun +plist-delete (plist prop)
  "Delete property PROP from PLIST.
Adapted from `org-plist-delete'."
  (let (p)
    (while plist
      (if (not (eq prop (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

;;;###autoload
(defun +plist-to-alist (plist &optional trim-col)
  (let ((res '()))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (key (if (and trim-col (string-prefix-p ":" (symbol-name key)))
                      (intern (substring (symbol-name key) 1))
                    key)))
        (push (cons key val) res)))
    (nreverse res)))

;;;###autoload
(defun +alist-to-plist (alist &optional add-col)
  (let ((res '()))
    (dolist (x alist)
      (push (if add-col (intern (format ":%s" (car x))) (car x)) res)
      (push (cdr x) res))
    (nreverse res)))

;;;###autoload
(defun +alist-set (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST. Return new alist.
This creates the association if it is missing, and otherwise sets
the cdr of the first matching association in the list. It does
not create duplicate associations. By default, key comparison is
done with `equal'. However, if SYMBOL is non-nil, then `eq' is
used instead.

This method may mutate the original alist, but you still need to
use the return value of this method instead of the original
alist, to ensure correct results."
  ;; Implementation taken from `straight--alist-set'
  ;; See [1] for the genesis of this method, which should really be
  ;; built in.
  ;;
  ;; [1]: emacs.stackexchange.com/q/33892/12534
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

;;; === Serialization ===

(defcustom +serialized-symbols-directory (concat minemacs-local-dir "+serialized-symbols/")
  "Default directory to store serialized symbols."
  :group 'minemacs-core
  :type 'directory)

;;;###autoload
(defun +serialize-sym (sym &optional dir filename-format)
  "Serialize SYM to DIR.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex. \"file-%s.el\").
Return the written file name, or nil if SYM is not bound."
  (when (boundp sym)
    (let ((out-file (expand-file-name
                     (format (or filename-format "%s.el") (symbol-name sym))
                     (or dir +serialized-symbols-directory))))
      (+log! "Saving `%s' to file \"%s\"" (symbol-name sym) (abbreviate-file-name out-file))
      (with-temp-buffer
        (prin1 (eval sym) (current-buffer))
        (+shutup! (write-file out-file)))
      out-file)))

;;;###autoload
(defun +deserialize-sym (sym &optional dir mutate filename-format)
  "Deserialize SYM from DIR, if MUTATE is non-nil, assign the object to SYM.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex. \"file-%s.el\").
Return the deserialized object, or nil if the SYM.el file dont exist."
  (let ((in-file (expand-file-name
                  (format (or filename-format "%s.el") (symbol-name sym))
                  (or dir +serialized-symbols-directory)))
        res)
    (when (file-exists-p in-file)
      (+log! "Loading `%s' from file \"%s\"" sym (abbreviate-file-name in-file))
      (with-temp-buffer
        (insert-file-contents in-file)
        (goto-char (point-min))
        (ignore-errors (setq res (read (current-buffer)))))
      (when mutate (set sym res)))
    res))

;;; === Misc ===

;; See: emacs.stackexchange.com/q/3022/37002
;;;###autoload
(defun +reset-sym (sym)
  "Reset SYM to its standard value."
  (set sym (eval (car (get sym 'standard-value)))))

;;;###autoload
(defmacro +reset-var! (var)
  "Reset VAR to its standard value."
  `(setq ,var (eval (car (get ',var 'standard-value)))))

;; Adapted from `evil-unquote', takes functions into account
;;;###autoload
(defun +unquote (expr)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe expr) '(quote function))
    (setq expr (cadr expr)))
  expr)

;;;###autoload
(defun +quoted-p (expr)
  "Retrun t when EXP is quoted."
  (memq (car-safe expr) '(quote function)))

;;;###autoload
(defun +apply-partially-right (fun &rest args)
  "Like `apply-partially', but applies the ARGS to the right of FUN."
  (lambda (&rest args2)
    (apply fun (append args2 args))))

;;; +primitives.el ends here
