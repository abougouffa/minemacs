;; -*- lexical-binding: t; -*-

;; (me-bool "someval") ;; ==> t
;;;###autoload
(defun me-bool (val) (not (null val)))

;;; === Higher order functions ===

;; (me-foldr (lambda (a b) (message "(%d + %d)" a b) (+ a b)) 0 '(1 2 3 4 5)) ;; ==> 15
;; (5 + 0) -> (4 + 5) -> (3 + 9) -> (2 + 12) --> (1 + 14)
;;;###autoload
(defun me-foldr (fun acc seq)
  (if (null seq) acc
    (funcall fun (car seq) (me-foldr fun acc (cdr seq)))))

;; (me-foldl (lambda (a b) (message "(%d + %d)" a b) (+ a b)) 0 '(1 2 3 4 5)) ;; ==> 15
;; (0 + 1) -> (1 + 2) -> (3 + 3) -> (6 + 4) -> (10 + 5)
;;;###autoload
(defun me-foldl (fun acc seq)
  (if (null seq) acc
    (me-foldl fun (funcall fun acc (car seq)) (cdr seq))))

;; (me-all '(83 88 t "txt")) ;; ==> t
;;;###autoload
(defun me-all (seq)
  (me-foldr (lambda (r l) (and r l)) t seq))

;; (me-some '(nil nil "text" nil 2)) ;; ==> t
;;;###autoload
(defun me-some (seq)
  (me-bool (me-foldr (lambda (r l) (or r l)) nil seq)))

;; (me-filter 'stringp '("A" 2 "C" nil 3)) ;; ==> ("A" "C")
;;;###autoload
(defun me-filter (fun seq)
  (when seq
    (let ((head (car seq))
          (tail (cdr seq)))
      (if (funcall fun head)
          (cons head (me-filter fun tail))
        (me-filter fun tail)))))

;; (me-zip '(1 2 3 4) '(a b c d) '("A" "B" "C" "D")) ;; ==> ((1 a "A") (2 b "B") (3 c "C") (4 d "D"))
;;;###autoload
(defun me-zip (&rest seqs)
  (if (null (car seqs)) nil
    (cons (mapcar #'car seqs)
          (apply #'me-zip (mapcar #'cdr seqs)))))

;;; === Strings ===

;; (me-str-join ", " '("foo" "10" "bar")) ;; ==> "foo, 10, bar"
;;;###autoload
(defun me-str-join (sep seq)
  (me-foldl (lambda (l r) (concat l sep r))
            (car seq) (cdr seq)))

;; (me-str-split "foo, 10, bar" ", ") ;; ==> ("foo" "10" "bar")
;; Use built-in split-string instead!
;;;###autoload
(defun me-str-split (str sep)
  (let ((s (string-search sep str)))
    (if s (cons (substring str 0 s)
                (me-str-split (substring str (+ s (length sep))) sep))
      (list str))))

;;;###autoload
(defun me-str-replace (old new s)
  "Replaces OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

;;;###autoload
(defun me-str-replace-all (replacements s)
  "REPLACEMENTS is a list of cons-cells. Each `car` is replaced with `cdr` in S."
  (replace-regexp-in-string (regexp-opt (mapcar 'car replacements))
                            (lambda (it) (cdr (assoc-string it replacements)))
                            s t t))
