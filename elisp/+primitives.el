;; -*- lexical-binding: t; -*-

;; (+bool "someval") ;; ==> t
;;;###autoload
(defun +bool (val) (not (null val)))

;;; === Higher order functions ===

;; (+foldr (lambda (a b) (message "(%d + %d)" a b) (+ a b)) 0 '(1 2 3 4 5)) ;; ==> 15
;; (5 + 0) -> (4 + 5) -> (3 + 9) -> (2 + 12) --> (1 + 14)
;;;###autoload
(defun +foldr (fun acc seq)
  (if (null seq) acc
    (funcall fun (car seq) (+foldr fun acc (cdr seq)))))

;; (+foldl (lambda (a b) (message "(%d + %d)" a b) (+ a b)) 0 '(1 2 3 4 5)) ;; ==> 15
;; (0 + 1) -> (1 + 2) -> (3 + 3) -> (6 + 4) -> (10 + 5)
;;;###autoload
(defun +foldl (fun acc seq)
  (if (null seq) acc
    (+foldl fun (funcall fun acc (car seq)) (cdr seq))))

;; (+all '(83 88 t "txt")) ;; ==> t
;;;###autoload
(defun +all (seq)
  (+foldr (lambda (r l) (and r l)) t seq))

;; (+some '(nil nil "text" nil 2)) ;; ==> t
;;;###autoload
(defun +some (seq)
  (+bool (+foldr (lambda (r l) (or r l)) nil seq)))

;; (+zip '(1 2 3 4) '(a b c d) '("A" "B" "C" "D")) ;; ==> ((1 a "A") (2 b "B") (3 c "C") (4 d "D"))
;;;###autoload
(defun +zip (&rest seqs)
  (if (null (car seqs)) nil
    (cons (mapcar #'car seqs)
          (apply #'+zip (mapcar #'cdr seqs)))))

;;; === Strings ===

;;;###autoload
(defun +str-replace (old new s)
  "Replaces OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

;;;###autoload
(defun +str-replace-all (replacements s)
  "REPLACEMENTS is a list of cons-cells. Each `car` is replaced with `cdr` in S."
  (replace-regexp-in-string (regexp-opt (mapcar 'car replacements))
                            (lambda (it) (cdr (assoc-string it replacements)))
                            s t t))

;;;###autoload
(defun +symbol-or-car (sym-or-cons)
  (if (symbolp sym-or-cons) sym-or-cons (car sym-or-cons)))

;;;###autoload
(defun +symbol-or-cdr (sym-or-cons)
  (if (symbolp sym-or-cons) sym-or-cons (cdr sym-or-cons)))

;;;###autoload
(defun +symbol-or-cadr (sym-or-cons)
  (if (symbolp sym-or-cons) sym-or-cons (cadr sym-or-cons)))

;;;###autoload
(defun +symbol-or-cddr (sym-or-cons)
  (if (symbolp sym-or-cons) sym-or-cons (cddr sym-or-cons)))

;;; === Property lists ===

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
        (add-to-list
         'out
         `(setq ,plist (plist-put ,plist ,key ,val)) t)))
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
  ;; [1]: https://emacs.stackexchange.com/q/33892/12534
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

;;; === Symbols ===

(defvar +serialized-symbols-directory (concat minemacs-local-dir "+serialized-symbols/"))

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

;; Adapted from `evil-unquote', takes functions into account
;;;###autoload
(defun +unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)
