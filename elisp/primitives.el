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
