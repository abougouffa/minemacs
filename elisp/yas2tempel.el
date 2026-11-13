;;; yas2tempel.el --- Convert YASnippet snippets to Tempel templates -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: abbrev, convenience, tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Convert YASnippet snippet files/directories into a Tempel template
;; file (the Lisp data format read by `tempel-path').
;;
;; Neither YASnippet nor Tempel need to be loaded to run the converter,
;; and the generated templates do not depend on this package or on
;; YASnippet (unless the original snippets call YASnippet helper
;; functions, which is reported).
;;
;; Entry points:
;;
;;   M-x yas2tempel-convert-directory   snippet tree -> templates file
;;   M-x yas2tempel-convert-file        one snippet file -> preview buffer
;;   M-x yas2tempel-convert-buffer      current snippet buffer -> preview
;;
;;   (yas2tempel-convert-string BODY)   snippet body -> Tempel elements
;;
;; Translation summary
;;
;;   YASnippet                    Tempel
;;   ---------------------------  -------------------------------------
;;   plain text, \$ \` escapes    "string"
;;   $1  ${1:default}  ${1}       p  (p "default")  (s f1) when mirrored
;;   repeated $1 (mirror)         f1  (updated as the field changes)
;;   ${2:$(upcase yas-text)}      (upcase f2)
;;   ${1:$$(yas-choose-value ..)} (p (completing-read "Choose: " ..))
;;   ${1:`(user-full-name)`}      `(p ,(user-full-name))  replaceable
;;   `elisp`                      (list 'l ELISP) evaluated once
;;   `yas-selected-text`          r
;;   $0  /  ${0:text}             q  /  q "text"
;;   newline, auto indentation    n ... >  (indent the completed line)
;;   expand-env 'fixed            indent to the expansion column
;;   $>                           >
;;   # condition: FORM            mode section with :when FORM
;;   # name: NAME                 :ann "NAME"
;;   .yas-parents                 child modes on the parent's section
;;
;; The conversion was checked against YASnippet itself on the 2387
;; snippets of the yasnippet-snippets collection (see the test
;; directory): 91% expand and navigate identically.  Constructs Tempel
;; cannot express (nested fields, numeric field order differing from
;; buffer order, $0 before other fields, field transformations that
;; run on every keystroke, adjacent fields, ...) are approximated and
;; reported as warnings in the *yas2tempel* buffer and as comments in
;; the generated file.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Customization

(defgroup yas2tempel nil
  "Convert YASnippet snippets to Tempel templates."
  :group 'convenience
  :prefix "yas2tempel-")

(defcustom yas2tempel-field-name-format "f%d"
  "Format string producing the Tempel field name for field number N."
  :type 'string)

(defcustom yas2tempel-indent-line 'auto
  "Indentation policy assumed when a snippet does not override it.
Mirrors the default value of `yas-indent-line'.  `auto' re-indents
every non-empty line after the first, `fixed' indents them to the
column where the snippet was expanded, and nil keeps the text as is."
  :type '(choice (const auto) (const fixed) (const :tag "Nothing" nil)))

(defcustom yas2tempel-mode-indent-alist nil
  "Alist of (MODE . POLICY) overriding `yas2tempel-indent-line' per mode.
Use this for modes where your configuration sets `yas-indent-line'
buffer-locally.  A `.yas-setup.el' that turns indentation off (as
yasnippet-snippets does for `bibtex-mode') is detected automatically."
  :type '(alist :key-type symbol
                :value-type (choice (const auto) (const fixed)
                                    (const :tag "Nothing" nil))))

(defcustom yas2tempel-also-auto-indent-first-line nil
  "Default for `yas-also-auto-indent-first-line'."
  :type 'boolean)

(defcustom yas2tempel-selected-text-element 'r
  "Tempel element used for `yas-selected-text' backquote expressions."
  :type '(choice (const r) (const r>)))

(defcustom yas2tempel-add-exit t
  "Non-nil means add a final `q' when a snippet has fields but no $0.
YASnippet leaves point at the end of the snippet in that case, which
is what the extra `q' reproduces."
  :type 'boolean)

(defcustom yas2tempel-expand-parents t
  "Non-nil means honor `.yas-parents' when converting a snippet tree.
The snippets of a parent mode are made available in its child modes
by listing the child modes on the section line of the parent."
  :type 'boolean)

(defcustom yas2tempel-annotate t
  "Non-nil means store the snippet name as the template :ann."
  :type 'boolean)

;;;; Internal state

(defvar yas2tempel--warnings nil
  "Warnings collected while converting the current snippet.")

(defconst yas2tempel--escapable '(?\\ ?` ?\" ?' ?$ ?} ?{ ?\( ?\))
  "Characters a backslash escapes in a snippet body.")

(defconst yas2tempel--string-functions
  '(format format-message concat format-time-string upcase downcase
           capitalize upcase-initials make-string string-trim string-trim-left
           string-trim-right replace-regexp-in-string number-to-string substring
           substring-no-properties completing-read read-string buffer-name
           user-full-name user-login-name system-name file-name-nondirectory
           file-name-base file-name-sans-extension string-join mapconcat
           symbol-name prin1-to-string make-temp-name string-replace)
  "Functions known to always return a string.")

(defun yas2tempel--warn (fmt &rest args)
  "Record a warning made from FMT and ARGS."
  (let ((msg (apply #'format-message fmt args)))
    (unless (member msg yas2tempel--warnings)
      (push msg yas2tempel--warnings))))

;;;; Snippet file parsing

(defun yas2tempel-parse-snippet (text &optional file)
  "Parse snippet TEXT (header and body) read from FILE.
Return a plist with :key :name :condition :expand-env :type :binding
:body and :file, following `yas--parse-template'."
  (let ((name nil)
        key condition expand-env binding (type 'snippet) body)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (if (re-search-forward "^# --\\s-*\n" nil t)
          (let ((bound (point)))
            (setq body (buffer-substring-no-properties (point) (point-max)))
            (goto-char (point-min))
            (while (re-search-forward
                    "^# *\\([^ ]+?\\) *: *\\(.*?\\)[[:space:]]*$" bound t)
              (let ((val (match-string-no-properties 2)))
                (pcase (match-string-no-properties 1)
                  ("key" (setq key val))
                  ("name" (setq name val))
                  ("type" (setq type (intern val)))
                  ("binding" (setq binding val))
                  ("condition"
                   (setq condition (yas2tempel--read val "condition")))
                  ("expand-env"
                   (setq expand-env (yas2tempel--read val "expand-env")))))))
        (setq body (buffer-string))))
    (unless (or key binding)
      (setq key (and file (file-name-nondirectory file))))
    (list :key (or key (and file (file-name-nondirectory file)))
          :name (or name (and file (file-name-nondirectory file)))
          :explicit-name name :condition condition :expand-env expand-env
          :type type :binding binding :body body :file file)))

(defun yas2tempel--read (string what)
  "Read STRING as one Lisp form, warning about WHAT on failure."
  (condition-case nil
      (car (read-from-string string))
    (error (yas2tempel--warn "Could not read %s: %s" what string)
           nil)))

;;;; Body parsing
;;
;; The parser produces a list of nodes:
;;   (text STRING)
;;   (ref N)                     $N
;;   (field N CHILDREN XFORM)    ${N:children}, ${children}, ${N:..$$(XFORM)}
;;   (mirror N FORM)             ${N:$(FORM)}
;;   (elisp FORM)                `FORM`
;;   (indent)                    $>

(defun yas2tempel--parse-body (str)
  "Parse the snippet body STR into a list of nodes."
  (let ((pos 0) (len (length str)))
    (cl-labels
        ((peek (&optional k)
           (let ((p (+ pos (or k 0))))
             (and (< p len) (aref str p))))
         (digitp (c) (and c (<= ?0 c ?9)))
         (paren-after (k)
           ;; Position of "(" after optional whitespace at POS+K, or nil.
           (let ((p (+ pos k)))
             (while (and (< p len) (memq (aref str p) '(?\s ?\t ?\n)))
               (cl-incf p))
             (and (< p len) (eq (aref str p) ?\() p)))
         (read-code (start)
           (let ((res (read-from-string str start)))
             (setq pos (cdr res))
             (car res)))
         (skip-ws ()
           (while (memq (peek) '(?\s ?\t ?\n)) (cl-incf pos)))
         (brace ()
           ;; POS is at "${".
           (cl-incf pos 2)
           (let ((start pos) num)
             (while (digitp (peek)) (cl-incf pos))
             (cond
              ((and (> pos start) (eq (peek) ?:))
               (setq num (string-to-number (substring str start pos)))
               (cl-incf pos))
              ;; ${N} is the empty field N.
              ((and (> pos start) (eq (peek) ?}))
               (setq num (string-to-number (substring str start pos))))
              (t (setq pos start)))
             (let ((p (and (eq (peek) ?$) (paren-after 1))))
               (if p
                   (let ((form (read-code p)))
                     (skip-ws)
                     (unless (eq (peek) ?}) (error "Unterminated mirror"))
                     (cl-incf pos)
                     (list 'mirror num form))
                 (pcase-let ((`(,children ,xform) (seq t)))
                   (unless (eq (peek) ?}) (error "Unterminated field"))
                   (cl-incf pos)
                   (list 'field num children xform))))))
         (seq (in-field)
           (let (nodes chars xform)
             (cl-flet ((flush ()
                         (when chars
                           (push (list 'text (apply #'string (nreverse chars)))
                                 nodes)
                           (setq chars nil))))
               (catch 'done
                 (while (< pos len)
                   (let ((c (aref str pos)))
                     (cond
                      ((and (eq c ?\\) (memq (peek 1) yas2tempel--escapable))
                       (push (peek 1) chars)
                       (cl-incf pos 2))
                      ((eq c ?`)
                       (let ((end (cl-position ?` str :start (1+ pos))))
                         (if (not end)
                             (progn (push c chars) (cl-incf pos))
                           (let ((code (substring str (1+ pos) end)))
                             (flush)
                             (push (condition-case nil
                                       (list 'elisp
                                             (yas2tempel--read-code code))
                                     (error
                                      (yas2tempel--warn
                                       "Unreadable elisp kept as text: `%s`"
                                       code)
                                      (list 'text (concat "`" code "`"))))
                                   nodes)
                             (setq pos (1+ end))))))
                      ((eq c ?$)
                       (let ((n (peek 1)))
                         (cond
                          ((digitp n)
                           (flush)
                           (let ((start (1+ pos)))
                             (setq pos start)
                             (while (digitp (peek)) (cl-incf pos))
                             (push (list 'ref (string-to-number
                                               (substring str start pos)))
                                   nodes)))
                          ((eq n ?>)
                           (flush)
                           (push (list 'indent) nodes)
                           (cl-incf pos 2))
                          ((eq n ?{)
                           (let* ((save pos)
                                  (node (condition-case nil (brace)
                                          (error nil))))
                             (if node
                                 (progn (flush) (push node nodes))
                               (setq pos save)
                               (push c chars)
                               (cl-incf pos))))
                          ((and in-field (or (and (eq n ?$) (paren-after 2))
                                             (paren-after 1)))
                           (let ((save pos))
                             (condition-case nil
                                 (setq xform (read-code (or (and (eq n ?$)
                                                                 (paren-after 2))
                                                            (paren-after 1))))
                               (error (setq pos save)
                                      (push c chars)
                                      (cl-incf pos)))))
                          (t (push c chars) (cl-incf pos)))))
                      ;; Like YASnippet, the first unescaped "}" ends
                      ;; the field (braces are not balanced).
                      ((and in-field (eq c ?})) (throw 'done nil))
                      (t (push c chars) (cl-incf pos))))))
               (flush))
             (list (nreverse nodes) xform))))
      (car (seq nil)))))

(defun yas2tempel--read-code (code)
  "Read the backquoted elisp CODE into a single form."
  (let ((forms (car (read-from-string (concat "(" code "\n)")))))
    (if (cdr forms) (cons 'progn forms) (car forms))))

;;;; Conversion

(cl-defstruct (yas2tempel--ctx (:constructor yas2tempel--make-ctx))
  masters    ; hash N -> master node (field or ref)
  nested     ; hash N -> t when the master is nested in another field
  named      ; hash N -> t when the field needs a Tempel name
  defaults)  ; hash N -> static default string

(defun yas2tempel--field-symbol (n)
  "Return the Tempel field name for field number N."
  (intern (format yas2tempel-field-name-format n)))

(defun yas2tempel--walk (nodes fn &optional nested)
  "Call FN with each node of NODES and its NESTED flag, depth first."
  (dolist (node nodes)
    (funcall fn node nested)
    (when (eq (car node) 'field)
      (yas2tempel--walk (nth 2 node) fn t))))

(defun yas2tempel--code-field-refs (form)
  "Return field numbers used via `yas-field-value' in FORM."
  (let (refs)
    (cl-labels ((walk (f)
                  (when (consp f)
                    (if (and (memq (car f) '(yas-field-value yas/field-value))
                             (integerp (cadr f)))
                        (push (cadr f) refs)
                      (unless (memq (car f) '(quote function))
                        (while (consp f)
                          (walk (car f))
                          (setq f (cdr f))))))))
      (walk form))
    refs))

(defun yas2tempel--analyze (nodes)
  "Compute the conversion context of NODES."
  (let ((masters (make-hash-table)) (nested (make-hash-table))
        (named (make-hash-table)) (defaults (make-hash-table))
        (refs nil))
    ;; Masters: first ${N:..} field, else the first $N.
    (yas2tempel--walk
     nodes (lambda (node in-field)
             (pcase node
               (`(field ,(and n (pred integerp)) . ,_)
                (let ((cur (gethash n masters)))
                  (when (or (null cur) (eq (car cur) 'ref))
                    (puthash n node masters)
                    (puthash n in-field nested)))))))
    (yas2tempel--walk
     nodes (lambda (node in-field)
             (pcase node
               (`(ref 0)
                ;; YASnippet exits at the last $0.
                (unless (eq (car (gethash 0 masters)) 'field)
                  (puthash 0 node masters)
                  (puthash 0 in-field nested)))
               (`(ref ,n)
                (if (gethash n masters)
                    (push n refs)
                  (puthash n node masters)
                  (puthash n in-field nested)))
               (`(field ,(and n (pred integerp)) . ,_)
                (unless (eq (gethash n masters) node) (push n refs)))
               (`(mirror ,n ,form)
                (when n (push n refs))
                (setq refs (append (yas2tempel--code-field-refs form) refs))))))
    (dolist (n refs)
      (when (and (gethash n masters) (not (gethash n nested)))
        (puthash n t named)))
    (let ((ctx (yas2tempel--make-ctx :masters masters :nested nested
                                     :named named :defaults defaults)))
      ;; Static default texts (used where Tempel cannot be dynamic).
      (maphash (lambda (n node)
                 (puthash n (if (eq (car node) 'field)
                                (yas2tempel--static-text (nth 2 node) ctx)
                              "")
                          defaults))
               masters)
      ctx)))

(defun yas2tempel--static-text (nodes ctx)
  "Flatten NODES to the text YASnippet shows before any editing.
CTX is the conversion context."
  (mapconcat
   (lambda (node)
     (pcase node
       (`(text ,s) s)
       (`(field ,_ ,children ,_) (yas2tempel--static-text children ctx))
       (`(ref ,n) (let ((m (gethash n (yas2tempel--ctx-masters ctx))))
                    (if (and m (eq (car m) 'field) (not (eq m node)))
                        (yas2tempel--static-text (nth 2 m) ctx)
                      "")))
       (_ "")))
   nodes ""))

(defun yas2tempel--default (ctx n)
  "Static default text of field N in CTX."
  (or (gethash n (yas2tempel--ctx-defaults ctx)) ""))

(defun yas2tempel--dynamic-ref (ctx n)
  "Expression giving the live value of field N in CTX."
  (cond
   ((gethash n (yas2tempel--ctx-named ctx)) (yas2tempel--field-symbol n))
   ((gethash n (yas2tempel--ctx-masters ctx)) (yas2tempel--default ctx n))
   (t (yas2tempel--warn "Reference to missing field $%d" n) "")))

(defun yas2tempel--subst (form ctx dynamic &optional text-field)
  "Translate the YASnippet idioms in FORM to plain Emacs Lisp.
CTX is the conversion context.  DYNAMIC non-nil means field values
are live Tempel variables, otherwise their static defaults are used.
TEXT-FIELD is the field number `yas-text' refers to, if any; it may
also be a string used as the value of `yas-text'."
  (cl-labels
      ((ref (n) (if dynamic (yas2tempel--dynamic-ref ctx n)
                  (yas2tempel--default ctx n)))
       (text ()
         (cond ((integerp text-field) (ref text-field))
               ((stringp text-field) text-field)
               (t (yas2tempel--warn "`yas-text' used outside a transform")
                  "")))
       (walk (f)
         (pcase f
           ((or 'yas-text 'yas/text) (text))
           ((or `(yas-text) `(yas/text))
            (let ((v (text))) `(and (not (equal ,v "")) ,v)))
           (`(,(or 'yas-field-value 'yas/field-value) ,(and n (pred integerp)))
            (ref n))
           ((or 'yas-selected-text 'yas/selected-text
                `(yas-selected-text) `(yas/selected-text))
            (yas2tempel--warn
             "`yas-selected-text' inside an expression was replaced by nil")
            nil)
           ((or 'yas-modified-p 'yas/modified-p)
            (if (and dynamic (integerp text-field))
                `(not (equal ,(ref text-field)
                             ,(yas2tempel--default ctx text-field)))
              nil))
           ((or 'yas-moving-away-p 'yas/moving-away-p) nil)
           (`(,(or 'yas-auto-next 'yas/auto-next) ,x) (walk x))
           (`(,(or 'yas-choose-value 'yas/choose-value) . ,args)
            `(completing-read "Choose: "
                              (flatten-tree (list ,@(mapcar #'walk args)))))
           (`(,(or 'yas-substr 'yas/substr) ,s ,pat . ,sub)
            `(let ((str ,(walk s)))
               (save-match-data
                 (if (string-match ,(walk pat) str)
                     (match-string-no-properties ,(or (car sub) 0) str)
                   str))))
           (`(,(or 'quote 'function) . ,_) f)
           ((pred consp)
            (when (and (symbolp (car f))
                       (string-match-p "\\`yas[-/]" (symbol-name (car f))))
              (yas2tempel--warn "Requires YASnippet function `%s'" (car f)))
            (let (out)
              (while (consp f)
                (push (walk (pop f)) out))
              (nconc (nreverse out) f)))
           (_ f))))
    (walk form)))

(defconst yas2tempel--lisp-dir
  (when-let ((subr (locate-library "subr")))
    (file-name-directory (expand-file-name subr)))
  "Directory of the Emacs core Lisp files.")

(defun yas2tempel--core-function-p (f)
  "Non-nil if F is a function of Emacs itself (not of some package)."
  (or (special-form-p f) (macrop f)
      (and (fboundp f)
           (let ((def (indirect-function f)))
             (or (subrp def)
                 (let ((file (if (autoloadp def)
                                 (locate-library (cadr def))
                               (symbol-file f 'defun))))
                   (and file yas2tempel--lisp-dir
                        (string-prefix-p yas2tempel--lisp-dir
                                         (expand-file-name file)))))))))

(defun yas2tempel--uses-symbol-p (form sym)
  "Non-nil if FORM mentions SYM outside quoted data."
  (cond ((eq form sym) t)
        ((and (consp form) (not (memq (car form) '(quote function))))
         (let (found)
           (while (and (consp form) (not found))
             (setq found (yas2tempel--uses-symbol-p (pop form) sym)))
           (or found (eq form sym))))))

(defun yas2tempel--calls-package-function-p (form)
  "Non-nil if FORM calls a function that is not part of Emacs core.
Such functions (typically defined in .yas-setup.el files) may read
`yas-text' dynamically."
  (and (consp form)
       (not (memq (car form) '(quote function)))
       (or (and (symbolp (car form))
                (not (memq (car form)
                           '(yas-text yas/text yas-field-value yas/field-value
                                      yas-selected-text yas/selected-text
                                      yas-choose-value yas/choose-value
                                      yas-auto-next yas/auto-next
                                      yas-substr yas/substr lambda)))
                (not (yas2tempel--core-function-p (car form))))
           (let (found)
             (while (and (consp form) (not found))
               (setq found (yas2tempel--calls-package-function-p (pop form))))
             found))))

(defun yas2tempel--transform (form ctx dynamic text-field)
  "Translate the transformation FORM, see `yas2tempel--subst'.
CTX, DYNAMIC and TEXT-FIELD are passed on.  When FORM may read
`yas-text' implicitly through a helper function, bind it dynamically."
  (let ((code (yas2tempel--subst form ctx dynamic text-field)))
    (if (and (not (yas2tempel--uses-symbol-p form 'yas-text))
             (yas2tempel--calls-package-function-p form))
        `(dlet ((yas-text ,(if (integerp text-field)
                               (if dynamic
                                   (yas2tempel--dynamic-ref ctx text-field)
                                 (yas2tempel--default ctx text-field))
                             text-field)))
           ,code)
      code)))

(defun yas2tempel--moves-point-p (form)
  "Non-nil if FORM seems to call a function that moves point."
  (and (consp form)
       (not (memq (car form) '(quote function)))
       (or (and (symbolp (car form))
                (string-match-p
                 "\\`\\(?:goto-\\|forward-\\|backward-\\|re-search-\\|search-\\|beginning-of-\\|end-of-\\|skip-\\|move-\\|back-to-\\|up-list\\|down-list\\)"
                 (symbol-name (car form))))
           (let (found)
             (while (and (consp form) (not found))
               (setq found (yas2tempel--moves-point-p (pop form))))
             found))))

(defun yas2tempel--protect-point (form)
  "Wrap FORM in `save-excursion' if it moves point, like YASnippet."
  (if (yas2tempel--moves-point-p form) `(save-excursion ,form) form))

(defun yas2tempel--stringify (form)
  "Wrap FORM so it evaluates to a string like YASnippet's insertion."
  (if (or (stringp form)
          (and (consp form) (memq (car form) yas2tempel--string-functions)))
      form
    `(format "%s" (or ,form ""))))

(defun yas2tempel--selected-text-p (form)
  "Non-nil if FORM just returns the selected text."
  (member form '(yas-selected-text yas/selected-text
                                   (yas-selected-text) (yas/selected-text))))

(defun yas2tempel--field-element (node ctx)
  "Convert the master field NODE to a Tempel element using CTX."
  (pcase-let* ((`(field ,n ,children ,xform) node)
               (name (and n (gethash n (yas2tempel--ctx-named ctx))
                          (yas2tempel--field-symbol n)))
               (static (yas2tempel--static-text children ctx)))
    (when (and (string-search "\n" static) (eq yas2tempel-indent-line 'auto))
      (yas2tempel--warn "Multi-line default of field %s is not re-indented" n))
    (when (cl-some (lambda (c) (memq (car c) '(field ref))) children)
      (yas2tempel--warn "Nested fields flattened into field %s" (or n "")))
    (cond
     ((eql n 0) :exit)
     (xform
      (unless (memq (car-safe xform) '(yas-choose-value yas/choose-value
                                                        yas-auto-next yas/auto-next))
        (yas2tempel--warn
         "Field transformation of field %s is only evaluated on expansion" n))
      (let ((code (yas2tempel--transform xform ctx nil static)))
        (if (eq (car-safe code) 'completing-read)
            ;; A choice: further typing extends it, like in YASnippet.
            `(p ,code ,@(and name (list name)))
          (yas2tempel--computed-field (yas2tempel--stringify code) name))))
     ((and (= (length children) 1) (eq (caar children) 'elisp)
           (yas2tempel--selected-text-p (cadar children)))
      `(,yas2tempel-selected-text-element "" ,@(and name (list name))))
     ((cl-every (lambda (c) (memq (car c) '(text field ref))) children)
      (cond ((not (string-empty-p static))
             `(p ,static ,@(and name (list name))))
            (name `(s ,name))
            (t 'p)))
     (t
      (yas2tempel--computed-field
       (yas2tempel--concat
        (mapcar
         (lambda (c)
           (pcase c
             (`(elisp ,f) (yas2tempel--stringify
                           (yas2tempel--protect-point
                            (yas2tempel--subst f ctx nil))))
             (_ (yas2tempel--static-text (list c) ctx))))
         children))
       name)))))

(defun yas2tempel--concat (parts)
  "Return a form concatenating PARTS, dropping empty strings."
  (setq parts (delete "" parts))
  (if (cdr parts) `(concat ,@parts) (or (car parts) "")))

(defun yas2tempel--node-elements (node ctx)
  "Convert NODE to a list of Tempel elements using CTX."
  (let ((masters (yas2tempel--ctx-masters ctx)))
    (pcase node
      (`(text ,s)
       (let ((lines (split-string s "\n")) out)
         (push (car lines) out)
         (dolist (l (cdr lines)) (push 'n out) (push l out))
         (delete "" (nreverse out))))
      ('(indent) (list '>))
      (`(ref ,n)
       (cond
        ((eql n 0) (and (eq (gethash 0 masters) node) (list :exit)))
        ((not (eq (gethash n masters) node))
         (list (yas2tempel--dynamic-ref ctx n)))
        ((eql n 0) (list :exit))
        ((gethash n (yas2tempel--ctx-named ctx))
         (list `(s ,(yas2tempel--field-symbol n))))
        (t (list 'p))))
      (`(field 0 ,children . ,_)
       ;; YASnippet leaves the default of $0 in place and exits before it.
       (and (eq (gethash 0 masters) node)
            (delete "" (list :exit (yas2tempel--static-text children ctx)))))
      (`(field ,n . ,_)
       (if (or (null n) (eq (gethash n masters) node))
           (list (yas2tempel--field-element node ctx))
         ;; A later ${N:..} of an existing field acts as a mirror.
         (list (yas2tempel--dynamic-ref ctx n))))
      (`(mirror ,n ,form)
       (cond
        ((not (and n (gethash n masters)))
         (yas2tempel--warn "Dropped transformation of missing field %s" n)
         nil)
        (t
         (let ((code (yas2tempel--transform form ctx t n)))
           (list (if (and (consp code)
                          (memq (car code) yas2tempel--string-functions))
                     code
                   `(or ,code ,(yas2tempel--dynamic-ref ctx n))))))))
      (`(elisp ,form)
       (if (yas2tempel--selected-text-p form)
           (list yas2tempel-selected-text-element)
         (list `(list 'l ,(yas2tempel--stringify
                           (yas2tempel--protect-point
                            (yas2tempel--subst form ctx nil))))))))))

(defun yas2tempel--field-element-p (elt)
  "Non-nil if ELT is a Tempel element the user can jump to."
  (or (memq elt '(p q r r> :exit))
      (memq (car-safe elt) '(p s r r>))
      (and (eq (car-safe elt) '\`)
           (memq (car-safe (cadr elt)) '(p s r r>)))))

(defun yas2tempel--computed-field (code name)
  "Field whose default is computed by CODE, optionally named NAME.
The form evaluates to a (p DEFAULT NAME) element, so that the default
is replaced when typing, like a static YASnippet default."
  (list '\` (append (list 'p (list '\, code)) (and name (list name)))))

(defun yas2tempel--line-blank-p (line)
  "Non-nil if the elements of LINE produce no text on expansion."
  (cl-every (lambda (e)
              (or (memq e '(p q r r> > :exit))
                  (and (stringp e) (string-empty-p e))
                  (eq (car-safe e) 's)
                  (and (eq (car-safe e) 'p) (equal (cadr e) ""))))
            line))

(defun yas2tempel--empty-field-p (elt)
  "Non-nil if ELT is a field that is empty when the template expands."
  (or (memq elt '(p q))
      (eq (car-safe elt) 's)
      (and (memq (car-safe elt) '(p r r>)) (equal (cadr elt) ""))))

(defun yas2tempel--eat-indentation (line)
  "Drop whitespace that follows the empty fields starting LINE.
Such whitespace is part of the line's indentation when YASnippet
re-indents the line, so it disappears."
  (let (out)
    (while (and line
                (or (yas2tempel--empty-field-p (car line))
                    (and (stringp (car line))
                         (string-empty-p
                          (string-trim-left (car line) "[ \t]+")))))
      (unless (stringp (car line)) (push (car line) out))
      (pop line))
    (when (stringp (car line))
      (setq line (cons (string-trim-left (car line) "[ \t]+") (cdr line))))
    (append (nreverse out) line)))

(defun yas2tempel--indent (elts indent first-line)
  "Insert indentation elements into ELTS.
INDENT is the indentation policy (auto, fixed or nil) and FIRST-LINE
non-nil means the first line is re-indented too (auto policy only)."
  (let ((lines (list nil)) (idx 0) out fixed-used)
    (dolist (e elts)
      (if (eq e 'n) (push nil lines) (push e (car lines))))
    (setq lines (mapcar #'nreverse (nreverse lines)))
    (dolist (line lines)
      (let ((blank (yas2tempel--line-blank-p line)))
        (pcase indent
          ('auto
           (setq line (delq '> line))
           (when (and (not blank) (or (> idx 0) first-line))
             ;; YASnippet keeps the literal indentation and calls
             ;; `indent-according-to-mode' at the indentation once the
             ;; line is complete.  Indent before a field that starts the
             ;; line, so the field keeps its position.
             (let* ((ws (and (stringp (car line))
                             (string-match-p "\\`[ \t]*\\'" (car line))
                             (list (car line))))
                    (rest (if ws (cdr line) line)))
               (setq line
                     (cond
                      ((null rest) (append ws (list '>)))
                      ((stringp (car rest)) (append line (list '>)))
                      (t (append ws (list '>)
                                 (yas2tempel--eat-indentation rest))))))))
          ('fixed
           (when (and (> idx 0) (not blank))
             (setq fixed-used t)
             (push '(ignore (indent-to col)) line)))))
      (when (> idx 0)
        (if (eq (car line) '>)
            (progn (push 'n> out) (pop line))
          (push 'n out)))
      (dolist (e line) (push e out))
      (cl-incf idx))
    (setq out (nreverse out))
    (if fixed-used
        (cons '(p (current-column) col noinsert) out)
      out)))

(defun yas2tempel--merge-strings (elts)
  "Merge adjacent strings in ELTS."
  (let (out)
    (dolist (e elts)
      (if (and (stringp e) (stringp (car out)))
          (setcar out (concat (car out) e))
        (push e out)))
    (nreverse out)))

(defun yas2tempel--file-indent (file)
  "Indentation policy for the snippet FILE, given its directory.
Looks at `yas2tempel-mode-indent-alist' and `.yas-setup.el' files of
the enclosing directories."
  (let ((dir (file-name-directory (expand-file-name file)))
        (policy yas2tempel-indent-line) (found nil))
    (dotimes (_ 3)
      (unless found
        (let ((mode (intern (file-name-nondirectory (directory-file-name dir))))
              (setup (expand-file-name ".yas-setup.el" dir)))
          (cond
           ((assq mode yas2tempel-mode-indent-alist)
            (setq policy (cdr (assq mode yas2tempel-mode-indent-alist))
                  found t))
           ((and (file-readable-p setup)
                 (with-temp-buffer
                   (insert-file-contents setup)
                   (re-search-forward
                    "yasnippet-snippets--no-indent\\|yas-indent-line[ ')]*nil"
                    nil t)))
            (setq policy nil found t))))
        (setq dir (file-name-directory (directory-file-name dir)))))
    policy))

(defun yas2tempel--env-settings (expand-env)
  "Return (INDENT . FIRST-LINE) from the snippet EXPAND-ENV."
  (let ((indent yas2tempel-indent-line)
        (first yas2tempel-also-auto-indent-first-line))
    (dolist (binding (and (listp expand-env) expand-env))
      (let ((var (car-safe binding))
            (val (ignore-errors (eval (car-safe (cdr-safe binding)) t))))
        (pcase var
          ('yas-indent-line (setq indent (if (eq val 'nothing) nil val)))
          ('yas-also-auto-indent-first-line (setq first val))
          ('yas-wrap-around-region nil)
          (_ (yas2tempel--warn "Ignored expand-env binding of `%s'" var)))))
    (cons indent first)))

(defun yas2tempel--place-exit (elts)
  "Turn the :exit marker of ELTS into Tempel elements.
YASnippet visits $0 last and leaves point there; without $0 point ends
at the end of the snippet."
  (let* ((tail (memq :exit elts))
         (prev (and tail (cadr (memq :exit (reverse elts))))))
    (cond
     ((and tail (cl-some #'yas2tempel--field-element-p (cdr tail)))
      (yas2tempel--warn "$0 is not the last field; kept as a plain field")
      (setcar tail 'p))
     ;; Right after the last field.  A `q' there would be merged with
     ;; the field, so rely on Tempel's own stop at the template end.
     ((and tail prev (yas2tempel--field-element-p prev))
      (when (cl-some (lambda (e) (not (equal e ""))) (cdr tail))
        (yas2tempel--warn
         "$0 directly follows a field; Tempel exits at the template end"))
      (setq elts (delq :exit elts)))
     (tail (setcar tail 'q))
     ((and yas2tempel-add-exit
           ;; Some text follows the last field.
           (let ((after (cl-loop with last = nil
                                 for rest on elts
                                 when (yas2tempel--field-element-p (car rest))
                                 do (setq last rest)
                                 finally return (and last (cdr last)))))
             (and (cl-some (lambda (e) (not (equal e ""))) after)
                  ;; After a mirror, `q' would be pushed in front of
                  ;; the mirror text whenever it updates.
                  (memq (type-of (car (last after))) '(string symbol))
                  (not (and (symbolp (car (last after)))
                            (not (eq (car (last after)) 'n)))))))
      (setq elts (append elts '(q)))))
    ;; A bare `r' finishes the template when no region is active, so it
    ;; may only stand where no other field follows.
    (let ((rest elts))
      (while rest
        (when (and (memq (car rest) '(r r>))
                   (cl-some #'yas2tempel--field-element-p
                            (remq 'q (cdr rest))))
          (setcar rest (list (car rest) "")))
        (setq rest (cdr rest))))
    elts))

;;;###autoload
(defun yas2tempel-convert-string (body &optional expand-env)
  "Convert the YASnippet snippet BODY into a list of Tempel elements.
EXPAND-ENV is the snippet's expand-env directive value, if any."
  (let* ((nodes (yas2tempel--parse-body body))
         (ctx (yas2tempel--analyze nodes))
         (env (yas2tempel--env-settings expand-env))
         (elts (mapcan (lambda (n) (copy-sequence
                                    (yas2tempel--node-elements n ctx)))
                       nodes))
         (order nil))
    ;; Field order: Tempel visits fields in buffer order.
    (dolist (n nodes)
      (pcase n
        ((or `(field ,(and k (pred integerp)) . ,_) `(ref ,k))
         (when (and (> k 0)
                    (eq n (gethash k (yas2tempel--ctx-masters ctx))))
           (push k order)))))
    (setq order (nreverse order))
    (let ((seen-unnumbered nil))
      (dolist (n nodes)
        (pcase n
          (`(field nil . ,_) (setq seen-unnumbered t))
          ((or `(field ,(and k (pred integerp)) . ,_) `(ref ,k))
           (when (and seen-unnumbered (> k 0))
             (yas2tempel--warn
              "Unnumbered field visited before numbered fields"))))))
    (unless (equal order (sort (copy-sequence order) #'<))
      (yas2tempel--warn
       "Fields visited in buffer order %S instead of numeric order" order))
    (setq elts (yas2tempel--place-exit elts))
    ;; Adjacent fields share a boundary in Tempel.
    (cl-loop for (a b) on elts
             when (and b (yas2tempel--field-element-p a)
                       (yas2tempel--field-element-p b)
                       (not (memq a '(r r>))) (not (memq b '(r r>))))
             do (yas2tempel--warn
                 "Adjacent fields: text typed at their boundary may go to both")
             and return nil)
    (cl-loop for (a b) on elts
             when (and b (yas2tempel--field-element-p b)
                       (or (and (symbolp a) (not (keywordp a))
                                (not (memq a '(n n> > p q r r>))))
                           (and (consp a)
                                (not (memq (car a) '(p s r r> list \`))))))
             do (yas2tempel--warn
                 "A field directly follows a mirror; it may move when the mirror changes")
             and return nil)
    (yas2tempel--merge-strings
     (yas2tempel--indent elts (car env) (cdr env)))))

;;;###autoload
(defun yas2tempel-convert-snippet (text &optional file)
  "Convert snippet file TEXT (read from FILE) into a Tempel template.
Return a plist (:template TEMPLATE :condition COND :warnings LIST
:snippet SNIPPET) or nil plus warnings for unsupported snippets."
  (let* ((yas2tempel--warnings nil)
         (yas2tempel-indent-line (if file (yas2tempel--file-indent file)
                                   yas2tempel-indent-line))
         (snippet (yas2tempel-parse-snippet text file))
         template)
    (if (eq (plist-get snippet :type) 'command)
        (yas2tempel--warn "Command snippets are not supported, skipped")
      (let* ((elts (yas2tempel-convert-string
                    (plist-get snippet :body)
                    (plist-get snippet :expand-env)))
             (key (plist-get snippet :key))
             (name (plist-get snippet :explicit-name)))
        (when (equal key "nil")
          (yas2tempel--warn "Key `%s' is reserved in Lisp, renamed to `%s-'"
                            key key)
          (setq key (concat key "-")))
        (setq template
              (append (list (intern key))
                      elts
                      (and yas2tempel-annotate name (not (equal name key))
                           (list :ann name))))))
    (list :template template
          :condition (plist-get snippet :condition)
          :warnings (reverse yas2tempel--warnings)
          :snippet snippet)))

;;;; Printing

(defun yas2tempel--prin1 (obj)
  "Print OBJ readably as a string."
  (let ((print-escape-newlines t) (print-length nil) (print-level nil)
        (print-quoted t) (print-escape-nonascii nil)
        (print-escape-multibyte nil))
    (prin1-to-string obj)))

(defun yas2tempel-format-template (template)
  "Return TEMPLATE as text, with a line break after each newline element."
  (let ((parts (list (yas2tempel--prin1 (car template))))
        (rest (cdr template)))
    (while rest
      (let ((e (pop rest)))
        (if (keywordp e)
            (progn
              (push (concat "\n " (yas2tempel--prin1 e) " "
                            (yas2tempel--prin1 (pop rest)))
                    parts))
          (push (concat (if (memq (car-safe parts) '(nil)) "" " ")
                        (yas2tempel--prin1 e))
                parts)
          (when (memq e '(n n>)) (push "\n" parts)))))
    (let ((s (concat "(" (apply #'concat (nreverse parts)) ")")))
      ;; Tidy: no trailing spaces before line breaks, indent continuation.
      (setq s (replace-regexp-in-string " *\n *" "\n " s))
      (string-trim-right s))))

;;;; Directories

(defun yas2tempel--snippet-file-p (file)
  "Non-nil if FILE is loaded as a snippet by YASnippet."
  (let ((base (file-name-nondirectory file)))
    (not (or (string-prefix-p "." base)
             (string-match-p "\\`#.*#\\'" base)
             (string-suffix-p "~" base)))))

(defun yas2tempel--snippet-files (dir)
  "Return all snippet files below DIR, recursively, sorted."
  (let (files)
    (dolist (f (directory-files dir t))
      (when (yas2tempel--snippet-file-p f)
        (if (file-directory-p f)
            (setq files (append files (yas2tempel--snippet-files f)))
          (setq files (append files (list f))))))
    files))

(defun yas2tempel--mode-dirs (root)
  "Return the mode directories of the snippet tree ROOT.
ROOT itself is a mode directory if it contains snippet files directly."
  (if (cl-some (lambda (f) (and (yas2tempel--snippet-file-p f)
                                (file-regular-p f)))
               (directory-files root t))
      (list root)
    (cl-remove-if-not (lambda (f) (and (yas2tempel--snippet-file-p f)
                                       (file-directory-p f)))
                      (directory-files root t))))

(defun yas2tempel--read-parents (dir)
  "Read the parent modes listed in DIR/.yas-parents."
  (let ((file (expand-file-name ".yas-parents" dir)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (mapcar #'intern (split-string (buffer-string)))))))

(defun yas2tempel--descendants (mode parents-alist)
  "Modes of PARENTS-ALIST inheriting MODE through `.yas-parents'."
  (let (result)
    (cl-labels ((ancestors (m seen)
                  (cl-loop for p in (alist-get m parents-alist)
                           unless (memq p seen)
                           append (cons p (ancestors p (cons p seen))))))
      (dolist (entry parents-alist)
        (let ((child (car entry)))
          (when (and (not (eq child mode))
                     (memq mode (ancestors child (list child)))
                     ;; Tempel already handles real derived modes.
                     (not (and (fboundp child)
                               (provided-mode-derived-p child mode))))
            (push child result)))))
    (sort result (lambda (a b) (string< a b)))))

;;;###autoload
(defun yas2tempel-convert-tree (root)
  "Convert the snippet tree ROOT.
Return a plist (:sections SECTIONS :warnings ALIST :count N) where
SECTIONS is a list of (MODES CONDITION TEMPLATE-ENTRIES...) and each
template entry is (TEMPLATE FILE WARNINGS)."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (mode-dirs (yas2tempel--mode-dirs (directory-file-name root)))
         (parents (mapcar (lambda (d)
                            (cons (intern (file-name-nondirectory
                                           (directory-file-name d)))
                                  (yas2tempel--read-parents d)))
                          mode-dirs))
         (sections nil) (all-warnings nil) (count 0))
    (dolist (dir mode-dirs)
      (let* ((mode (intern (file-name-nondirectory (directory-file-name dir))))
             (modes (cons mode (and yas2tempel-expand-parents
                                    (yas2tempel--descendants mode parents))))
             (used (make-hash-table :test #'equal))
             (groups nil))
        (dolist (file (yas2tempel--snippet-files dir))
          (let* ((text (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))
                 (res (yas2tempel-convert-snippet text file))
                 (template (plist-get res :template))
                 (warnings (plist-get res :warnings)))
            (when template
              ;; Disambiguate duplicate keys within a mode.
              (let* ((base (symbol-name (car template)))
                     (name base) (i 1))
                (while (gethash name used)
                  (setq name (format "%s-%d" base (cl-incf i))))
                (unless (equal name base)
                  (push (format-message "Duplicate key `%s' renamed to `%s'"
                                        base name)
                        warnings)
                  (setq template (cons (intern name) (cdr template))))
                (puthash name t used))
              (cl-incf count)
              (let* ((condition (plist-get res :condition))
                     (group (assoc condition groups)))
                (unless group
                  (setq group (list condition))
                  (setq groups (append groups (list group))))
                (nconc group (list (list template file warnings)))))
            (when warnings
              (push (cons file warnings) all-warnings))))
        (dolist (g groups)
          (push (cons modes g) sections))))
    (list :sections (nreverse sections)
          :warnings (nreverse all-warnings)
          :count count)))

(defun yas2tempel-format-sections (sections &optional root)
  "Format SECTIONS from `yas2tempel-convert-tree' as a templates file.
ROOT is the converted directory, used for comments."
  (with-temp-buffer
    (insert ";; -*- mode: lisp-data; coding: utf-8 -*-\n"
            ";; Tempel templates converted from YASnippet snippets"
            (if root (format " in %s" (abbreviate-file-name root)) "")
            "\n;; by yas2tempel on " (format-time-string "%Y-%m-%d") ".\n")
    (dolist (section sections)
      (pcase-let ((`(,modes ,cond . ,entries) section))
        (insert "\n" (mapconcat #'symbol-name modes " "))
        (when cond (insert " :when " (yas2tempel--prin1 cond)))
        (insert "\n\n")
        (dolist (entry entries)
          (pcase-let ((`(,template ,file ,warnings) entry))
            (when file
              (insert ";; " (if root (file-relative-name file root)
                              (file-name-nondirectory file))
                      "\n"))
            (dolist (w warnings) (insert ";; WARNING: " w "\n"))
            (insert (yas2tempel-format-template template) "\n\n")))))
    (buffer-string)))

(defun yas2tempel--report (title warnings count)
  "Display a report buffer named TITLE with WARNINGS for COUNT snippets."
  (with-current-buffer (get-buffer-create "*yas2tempel*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert title "\n"
              (format "%d template(s) converted, %d file(s) with warnings.\n\n"
                      count (length warnings)))
      (dolist (w warnings)
        (insert (abbreviate-file-name (car w)) "\n")
        (dolist (msg (cdr w)) (insert "  - " msg "\n"))))
    (goto-char (point-min))
    (special-mode)
    (current-buffer)))

;;;###autoload
(defun yas2tempel-convert-directory (dir output)
  "Convert the YASnippet tree DIR into the Tempel templates file OUTPUT.
DIR is either a snippet collection (with one directory per major
mode, like `yas-snippet-dirs' entries) or a single mode directory.
OUTPUT is overwritten.  Return the number of converted templates."
  (interactive
   (list (read-directory-name "YASnippet directory: " nil nil t)
         (read-file-name "Write Tempel templates to: " nil nil nil
                         "templates.eld")))
  (let* ((res (yas2tempel-convert-tree dir))
         (text (yas2tempel-format-sections (plist-get res :sections)
                                           (expand-file-name dir))))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region text nil output nil (if (called-interactively-p 'any)
                                            nil 'silent)))
    (when (called-interactively-p 'any)
      (display-buffer
       (yas2tempel--report (format "Converted %s -> %s" dir output)
                           (plist-get res :warnings)
                           (plist-get res :count))))
    (plist-get res :count)))

(defun yas2tempel--preview (text file)
  "Convert snippet TEXT from FILE and show the template in a buffer."
  (let* ((res (yas2tempel-convert-snippet text file))
         (mode (and file (file-name-nondirectory
                          (directory-file-name (file-name-directory file)))))
         (out (yas2tempel-format-sections
               (list (list (list (intern (or mode "fundamental-mode")))
                           (plist-get res :condition)
                           (list (plist-get res :template) file
                                 (plist-get res :warnings))))))
         (buf (get-buffer-create "*yas2tempel preview*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert out)
      (if (fboundp 'lisp-data-mode) (lisp-data-mode) (emacs-lisp-mode))
      (goto-char (point-min)))
    (pop-to-buffer buf)
    res))

;;;###autoload
(defun yas2tempel-convert-file (file)
  "Show the Tempel template converted from the snippet FILE."
  (interactive "fSnippet file: ")
  (yas2tempel--preview (with-temp-buffer (insert-file-contents file)
                                         (buffer-string))
                       file))

;;;###autoload
(defun yas2tempel-convert-buffer ()
  "Show the Tempel template converted from the current snippet buffer."
  (interactive)
  (yas2tempel--preview (buffer-substring-no-properties (point-min) (point-max))
                       buffer-file-name))

(provide 'yas2tempel)
;;; yas2tempel.el ends here
