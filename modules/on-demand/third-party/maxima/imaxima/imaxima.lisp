;; Copyright (C) 2001, 2002, 2003, 2004 Jesper Harder
;; Copyright (C) 2007, 2008 Yasuaki Honda
;; Copyright (C) 2020, 2021, 2022 Leo Butler
;;
;; Plotting support section of this file is the copy of the same
;; section of wxmathml.lisp with very small modification. The file
;; wxmathml.lisp is a part of the distribution of wxMaxima.

;; Created: 14 Nov 2001
;; Version: See version.texi
;; Keywords: maxima
;; Time-stamp: <16-05-2022 10:50:32 Leo Butler>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
;;
;;; History:
;;; Putting prefix package name to the variable does cause
;;; error when the file is loaded into older clisp, e.g. 2.29.
;;; I changed it so that it checks the existence of the symbol
;;; in the custom package prior to accessing it.
;;; by yasuaki honda 2006/02/26
;;;
;;; When imaxima-setup bug in imaxima.el was fixed,
;;; it became clear that the following code does not work
;;; when imaxima.lisp is loaded by:
;;;     %i1 load("/xxx/imaxima.lisp");
;;; Putting prefix package name to the variable solves the
;;; issue.
;;; by yasuaki honda

(in-package :maxima)

(setq $maxima_frontend "imaxima")
(setq $maxima_frontend_version *autoconf-version*)
(setq $maxima_frontend_bugreportinfo "imaxima is part of maxima.")

(defvar *windows-OS* (string= *autoconf-windows* "true"))
(defmvar $wxplot_size '((mlist simp) 400 250))
(defmvar $wxplot_old_gnuplot nil)
(defvar *image-counter* 0)

(defun wx-gnuplot-installation-check ()
  ;; The function check-gnuplot-process is defined in
  ;; maxima/src/plot.lisp since at least 5.12.0.
  (flet ((wx-gnuplot-installed-p ()
	   (ignore-errors (check-gnuplot-process) t)))
    (unless (wx-gnuplot-installed-p)
      (merror (format t "Gnuplot error: Gnuplot is not installed,
nor Gnuplot is not recognized by maxima")))))

(declare-top (special lop rop $gcprint $inchar *autoconf-version*))

;;;
;;; Very unfortunately, the following code does not work in
;;; SBCL.
;;; by yasuaki honda
#-sbcl
(unless (fboundp 'maxima::print-invert-case)
  (defun print-invert-case (obj)
    (princ-to-string obj)))

(defun print-case-sensitive (obj)
  (if obj
      (print-invert-case obj)
    nil))

(defun diff-symbol () '$d)

(defun main-prompt ()
  (format () (concatenate 'string (string (code-char 3)) "(~A~D) " (string (code-char 4)))
    (stripdollar (print-case-sensitive $inchar)) $linenum))

(defun break-dbm-loop (at)
  (let* (
	 (*quit-tags* (cons (cons *break-level* *quit-tag*) *quit-tags*))
	 (*break-level* (if (not at) *break-level* (cons t *break-level*)))
	 (*quit-tag* (cons nil nil))
	 (*break-env* *break-env*)
	 (*mread-prompt* "")
	 (*diff-bindlist* nil)
	 (*diff-mspeclist* nil)
	 val
	 )
    (declare (special *mread-prompt* ))
    (and (consp at) (set-env at))
    (cond ((null at)
	   (break-frame 0 nil)))
    (catch 'step-continue
      (catch *quit-tag*
	(unwind-protect
	    (do () (())
		(format *debug-io*
			(concatenate 'string
				     (string (code-char 3))
				     "~&~@[(~a:~a) ~]"
				     (string (code-char 4)))
		    (unless (stringp at) "dbm")
		    (length *quit-tags*))
		(setq val
		      (catch 'macsyma-quit
			(let ((res (dbm-read *debug-io*  nil *top-eof* t)))
			  (declare (special *mread-prompt*))
			  (cond ((and (consp res) (keywordp (car res)))
				 (let ((value (break-call (car res)
							  (cdr res) 'break-command)))
				   (cond ((eq value :resume) (return)))
				   ))
				(t
				 (setq $__ (nth 2 res))
				 (setq $% (meval* $__))
				 (setq $_ $__)
				 (displa $%)
				 ))
			  nil
			  )))
		(and (eql val 'top)
		     (throw-macsyma-top))
		      )
	 (restore-bindings)
	)))))

(setq $display2d '$imaxima)

;; TeX-printing
;; (c) copyright 1987, Richard J. Fateman
;; Small changes for interfacing with TeXmacs: Andrey Grozin, 2001
;; Yet more small changes for interfacing with imaxima: Jesper Harder 2001

;; (defun tex (... is removed

(defun unquote-%-internal (str c)
  (let* ((qstr (format nil "~A~A" #\\ c))
	 (pos (search qstr str)))
    (if pos
	(concatenate 'string (subseq str 0 pos) (format nil "~A" c)
		     (unquote-%-internal (subseq str (+ pos 2)) c))
      str)))

(defun unquote-% (str)
  (setq str (unquote-%-internal str #\$))
  (setq str (unquote-%-internal str #\%))
  (setq str (unquote-%-internal str #\&))
  (setq str (unquote-%-internal str #\_))
  (setq str (unquote-%-internal str #\#))
  str)

(defun verb-quote (str)
  (let ((var "") (charlist
		  '((#\Newline . "| \\\\ \\verb| "))))
    (dotimes (i (length str))
      (let ((chari (elt str i)))
	(setq var (concatenate 'string var
			       (or (cdr (assoc chari charlist :test #'eql))
				   (string chari))))))
  var))

(defun tex-string (x)
  (let ((sym-name
	 (if (symbolp x)
	     (print-case-sensitive x)
	   x)))
    (cond ((equal sym-name "") "")
	  ((eql (elt sym-name 0) #\\) sym-name)
	  ((member (elt sym-name 0) '(#\$ #\&))
	   (setq sym-name (unquote-% (subseq sym-name 1)))
	   (concatenate 'string "\\verb|   " (verb-quote sym-name) "|"))
	  (t (setq sym-name (unquote-% sym-name))
	     (concatenate 'string "\\verb|" (verb-quote sym-name) "|")))))

(defun tex-char (x)
  (if (eql x #\|) "\\verb/|/"
    (concatenate 'string "\\verb|" (string x) "|")))

(defun myquote (str)
  (let ((var "") (charlist
		  '((#\{ . "\\left\\{\\right.")
		    (#\} . "\\left\\}\\right.")
		    (#\space . "\\ ")
		    (#\Newline . "} \\\\ \\mathrm{ ")
		    (#\# . "\\#")
		    (#\$ . "\\$")
		    (#\% . "\\%")
		    (#\& . "\\&")
		    (#\_ . "\\_"))))
    (dotimes (i (length str))
      (let ((chari (elt str i)))
	(setq var (concatenate 'string var 
			       (or (cdr (assoc chari charlist :test #'eql))
				   (string chari))))))
  var))

(defun ascii-char-p (c)
  "Helper function for IMAXIMA-ACCUMULATE."
  (if (listp c)
      (every #'identity (mapcar #'ascii-char-p c))
    (and (characterp c) (< (char-code c) 127.))))
(defun imaxima-accumulate (l &optional (result '()) (state 'other))
  "L is a list of characters of length >1. Wrap each sublist of ascii
characters in \\mathrm{}, leave others alone."
  (if (null l)
      (if (eq state 'ascii)
	  (append result '(#\}))
	result)
    (let ((c (list (car l))))
      (cond ((eq state 'other)
	     (if (ascii-char-p c)
		 (setq c (append (coerce "\\mathrm{" 'list) c) state 'ascii)))
	    ((eq state 'ascii)
	     (unless (ascii-char-p c)
	       (setq c (append (list #\}) c) state 'other)))
	    (t
	     (merror "imaxima-accumulate")))
      (setq result (append result c))
      (imaxima-accumulate (cdr l) result state))))

(defun tex-stripdollar (sym)
  "TEX-STRIPDOLLAR strips a leading `$' or `&' from symbol SYM. If the
length of the NAME of SYM is 1, return that string; otherwise, wrap
NAME in \\mathrm{} (but see IMAXIMA-ACCUMULATE for handling of
non-ascii characters."
  (or (symbolp sym) (return-from tex-stripdollar sym))
  (let* ((name (print-case-sensitive sym))
	 (pname (if (member (elt name 0) '(#\$ #\&)) (subseq name 1) name))
	 (mname (myquote pname))
	 (lname (coerce mname 'list))
	 (len (length pname)))
      (cond
       ((eql len 1) mname)
       (t (coerce (imaxima-accumulate lname) 'string)))))

;; (defun strcat (... is removed

;; 10/14/87 RJF  convert 1.2e20 to 1.2 \cdot 10^{20}
;; 03/30/01 RLT  make that 1.2 \times 10^{20}
;; (defun texnumformat(atom) is removed

;; (defun tex-paren (x l r)  is removed

;;;
;;; The definition of tex-array is modified to fix bug #30, reported by Thomas Weidner.
;;; The following definition is provided by Thomas. 
;;; Dec.6, 2006
;;;

(defun tex-array (x l r)
 (let ((f))
      (if (eq 'mqapply (caar x))
          (setq f (cadr x)
                x (cdr x))
          (setq f (caar x)))
      (if (and (atom (cadr x)) (atom f))
          ;; subscript is an atom -- don't use \isubscript
          (progn
            (setq l (tex f l nil lop 'mfunction)
                  r (nconc (tex-list (cdr x) nil (list "}") ",") r))
            (nconc l (list "_{") r))
        (progn
          (setq l (tex f (append l (list "\\isubscript{"))  nil lop 'mfunction)
                r (nconc (tex-list (cdr x) nil (list "}") ",") r))
          (nconc  l (list "}{") r )))))

;; set up a list , separated by symbols (, * ...)  and then tack on the
;; ending item (e.g. "]" or perhaps ")"

(defun tex-list (x l r sym)
  (if (null x) r
      (do ((nl))
	  ((null (cdr x))
	   (setq nl (nconc nl (tex (car x)  l r 'mparen 'mparen)))
	   nl)
;;	  (setq nl (nconc nl (tex (car x)  l (list sym) 'mparen 'mparen))
	  (setq nl (nconc nl (tex (car x)  l (list (concatenate 'string sym "\\linebreak[0]")) 'mparen 'mparen))
		  x (cdr x)
		  l nil))))

;; (defun tex-prefix (x l r) is removed

;; (defun tex-infix (x l r) is removed
  
;; (defun tex-postfix (x l r) is removed

;; (defun tex-nary (x l r) is removed
#|
(defun tex-nary (x l r)
  (let* ((op (caar x)) (sym (texsym op)) (y (cdr x)) (ext-lop lop) (ext-rop rop))
    (cond ((null y)       (tex-function x l r t)) ; this should not happen
          ((null (cdr y)) (tex-function x l r t)) ; this should not happen, too
          (t (do ((nl) (lop ext-lop op) (rop op (if (null (cdr y)) ext-rop op)))
                 ((null (cdr y)) (setq nl (nconc nl (tex (car y)  l r lop rop))) nl)
	         (setq nl (nconc nl (tex (car y)  l (list sym)   lop rop))
		       y (cdr y) 
		       l nil))))))
|#
;; (defun tex-nofix (x l r) is removed

;; (defun tex-matchfix (x l r) is removed

;; (defun texsym (x) is removed

;; (defun texword (x) is removed

;; (defprop bigfloat tex-bigfloat tex) is removed

;;;
;;; Fixed to treat big float correctly.
;;;
(defun tex-bigfloat (x l r) (tex-list (fpformat x) l r nil))

;;absolute value
(defprop $%phi "\\phi" texword) ;; yhonda

;; reported conjugate treatment in imaxima be fixed.
(defprop $conjugate ("^{\\star}") texsym)

(defprop mquote 201. tex-rbp)

(defprop msetq 180. tex-rbp)
(defprop msetq 20. tex-rbp)

(defprop mset 180. tex-lbp)
(defprop mset 20. tex-rbp)

(defprop mdefine 180. tex-lbp)
(defprop mdefine 20. tex-rbp)

(defprop mdefmacro 180. tex-lbp)
(defprop mdefmacro 20. tex-rbp)

(defprop marrow 25 tex-lbp)
(defprop marrow 25 tex-rbp)

(defprop mfactorial 160. tex-lbp)

(defprop mexpt 140. tex-lbp)
(defprop mexpt 139. tex-rbp)


(defprop mncexpt 135. tex-lbp)
(defprop mncexpt 134. tex-rbp)

(defprop mnctimes 110. tex-lbp)
(defprop mnctimes 109. tex-rbp)

;;(defprop mtimes tex-nary tex)
;;(defprop mtimes "\\*" texsym)
(defprop mtimes 120. tex-lbp)
(defprop mtimes 120. tex-rbp)

(defprop %sqrt tex-sqrt tex)

(defun tex-sqrt(x l r)
;; format as \\sqrt { } assuming implicit parens for sqr grouping
   (tex (cadr x) (append l  '("\\isqrt{")) (append '("}") r) 'mparen 'mparen))

(defprop mquotient 122. tex-lbp) ;;dunno about this
(defprop mquotient 123. tex-rbp) 

(defun tex-mquotient (x l r)
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (cond ((and (atom (cadr x)) (atom (caddr x)))
	 ;; both denom and numerator are atoms
	 (setq l (tex (cadr x) (append l '("\\frac{")) nil nil nil) ;;fixme
	       r (tex (caddr x) (list "}{") (append '("}")r) 'mparen 'mparen)))
	((atom (cadr x))
	 ;; numerator is an atom
	 (setq l (tex (cadr x) (append l '("\\ifracd{")) nil 'mparen 'mparen)
	       r (tex (caddr x) (list "}{") (append '("}")r) 'mparen 'mparen)))
	((atom (caddr x))
	 ;; denom is an atom
	 (setq l (tex (cadr x) (append l '("\\ifracn{")) nil 'mparen 'mparen)
	       r (tex (caddr x) (list "}{") (append '("}")r) 'mparen 'mparen)))
	(t
	 ;; neither are atoms
	 (setq l (tex (cadr x) (append l '("\\ifrac{")) nil 'mparen 'mparen)
	       r (tex (caddr x) (list "}{") (append '("}")r) 'mparen 'mparen))))
  (append l r))

;; easily extended to union, intersect, otherops

;; (defun tex-limit(x l r) is removed. mactex.lisp version considers direction.

;;binomial coefficients

(defprop %binomial tex-choose tex)

;; (defun tex-choose (x l r) is removed

(defprop rat 120. tex-lbp)
(defprop rat 121. tex-rbp)

(defprop mplus 100. tex-lbp)
(defprop mplus 100. tex-rbp)

;; (defun tex-mplus (x l r) is removed

(defprop mminus 100. tex-rbp)
(defprop mminus 100. tex-lbp)

(defprop mequal 80. tex-lbp)
(defprop mequal 80. tex-rbp)

(defprop mnotequal 80. tex-lbp)
(defprop mnotequal 80. tex-rbp)

(defprop mgreaterp 80. tex-lbp)
(defprop mgreaterp 80. tex-rbp)

(defprop mgeqp 80. tex-lbp)
(defprop mgeqp 80. tex-rbp)

(defprop mlessp 80. tex-lbp)
(defprop mlessp 80. tex-rbp)

(defprop mleqp 80. tex-lbp)
(defprop mleqp 80. tex-rbp)

(defprop mnot 70. tex-rbp)

(defprop mand 80. tex-lbp)
(defprop mand 80. tex-rbp)

(defprop mor 50. tex-lbp)
(defprop mor 50. tex-rbp)

;; make sin(x) display as sin x , but sin(x+y) as sin(x+y)
;; etc

(mapc #'tex-setup 
  '( 
     (%acot "\\operatorname{arccot}")
     (%asec "\\operatorname{arcsec}")
     (%acsc "\\operatorname{arccsc}")
     (%sech "\\operatorname{sech}")          
     (%csch "\\operatorname{csch}")
     (%asinh "\\operatorname{arcsinh}")
     (%acosh "\\operatorname{arccosh}")
     (%atanh "\\operatorname{arctanh}")
     (%acoth "\\operatorname{arccoth}")
     (%asech "\\operatorname{arcsech}")
     (%acsch "\\operatorname{arccsch}")
     )) ;; etc

(defprop mcond 25. tex-lbp)
(defprop mcond 25. tex-rbp)
(defprop %derivative tex-derivative tex)

(defprop mdo 30. tex-lbp)
(defprop mdo 30. tex-rbp)
(defprop mdoin 30. tex-rbp)

;; these aren't quite right


;; Undone and trickier:
;; handle reserved symbols stuff, just in case someone
;; has a macsyma variable named (yuck!!) \over  or has a name with 
;; {} in it.
;; Maybe do some special hacking for standard notations for 
;; hypergeometric fns, alternative summation notations  0<=n<=inf, etc.

;;Undone and really pretty hard: line breaking


(defun tex-mtext (x l r) (tex-list (cdr x) l r ""))

(defun tex-mlabel (x l r)
  (tex (caddr x)
    (append l
      (if (cadr x)
	  (list (format nil (concatenate 'string (string (code-char 23))
					 "~A"
					 (string (code-char 23)))
			(myquote (print-case-sensitive (stripdollar (cadr x))))))
        nil))
    r 'mparen 'mparen))

(defun tex-spaceout (x l r)
  (append l (list "\\verb|" (make-string (cadr x) :initial-element #\space) "|") r))

; jh: verb & mbox

(defun input-label-p (label)
  (if (symbolp label)
      (let ((name (symbol-name label)))
	(and (> (length name) 3)
	     (string= "$%I" (subseq name 0 3))))))

(defun latex (x)
;  (princ x)  ;; uncomment to debug.
  (if (and (listp x) (car x) (listp (car x)) (caar x)
	   (equal (caar x) 'mlabel)
	   (cdr x)
	   (cadr x)
	   (input-label-p (cadr x)))
      (let (($display2d nil))
	(declare (special $display2d))
	(displa x)
	(return-from latex)))
  (mapc #'princ
	(cond ((and (listp x) (cdr x) (stringp (cadr x))
		    (equal (string-right-trim '(#\Space) (cadr x)) "Is"))
	       (tex x (list (string (code-char 21)))
		    (list (string (code-char 22))) 'mparen 'mparen))
	      ((and (listp x) (cdr x) (stringp (caddr x))
		    (equal (string-right-trim '(#\Space) (caddr x)) ""))
	       (tex (reverse (cons "\\ " (rest (reverse x)))) (list (string (code-char 2)))
		    (list (string (code-char 5))) 'mparen 'mparen))
	      (t
	       (tex x (list (string (code-char 2)))
	         (list (string (code-char 5))) 'mparen 'mparen)))))

(let ((old-displa (symbol-function 'displa)))
  (defun displa (form)
    (if (eq $display2d '$imaxima)
        (latex form)
      (funcall old-displa form))))

(defun ask-prop (object property fun-or-number)
  (if fun-or-number (setq fun-or-number (list '| | fun-or-number)))
;;; Asks the user a question about the property of an object.
;;; Returns only $yes, $no or $unknown.
  (if (symbolp property)
      (setq property (print-case-sensitive property)))
  (do ((end-flag) (answer))
      (end-flag (cond ((memq answer '($yes |$Y| |$y|)) '$yes)
		      ((memq answer '($no |$N| |$n|)) '$no)
		      ((memq answer '($unknown $uk)) '$unknown)))
    (setq answer (retrieve
		  `((mtext) "Is  " ,object 
		    ,(if (member (get-first-char property) '(#\a #\e #\i #\o #\u)
				 :test #'char-equal)
			 '"  an "
			 '"  a ")
		    ,property ,@fun-or-number "?")
		  nil))
    (cond 
      ((memq answer '($yes |$Y| |$y| |$N| |$n| $no $unknown $uk))
       (setq end-flag t))
      (t (mtell
	  "~%Acceptable answers are Yes, Y, No, N, Unknown, Uk~%")))))

;;
;; Plotting support
;;

(defvar $imaxima_tmp_subdir nil "Bound to the value of `imaxima-tmp-subdir' in `imaxima-mode'.")
(defun imaxima-apply (fun args)
  "Binds `*maxima-tempdir*' to `$imaxima_tmp_subdir' before `apply'."
  (declare (special *maxima-tempdir* $imaxima_tmp_subdir))
  (let ((*maxima-tempdir* $imaxima_tmp_subdir))
    (apply fun args)))
		       
(defun wxxml-tag (x l r)
  (let ((name (cadr x))
	(tag (caddr x)))
    (append l (list (format nil "<~a>~a</~a>" tag name tag)) r)))

(defun wxplot-filename (&optional (suff t))
  (declare (special *image-counter*))
  (incf *image-counter*)
  (let* ((name (format nil "maxout_~d~a" *image-counter* (if suff ".eps" "")))
	 (filename (imaxima-apply #'plot-temp-file (list name))))
    (if (probe-file filename)
	(delete-file filename))
    filename))

(defvar $wx_data_file "data_~a.gnuplot" "A FORMAT string that takes exactly one argument, *image-counter*; or a MAXIMA function.")
(defvar $wx_gnuplot_file "maxout_~a.gnuplot" "A FORMAT string that takes exactly one argument, *image-counter*; or a MAXIMA function.")

(defun wxplot-data+maxout ()
  (declare (special *image-counter* $wx_data_file $wx_gnuplot_file))
  (let ((datafile (if (stringp $wx_data_file) (format nil $wx_data_file *image-counter*) (mfuncall $wx_data_file *image-counter*)))
	(gnpltfile (if (stringp $wx_data_file) (format nil $wx_gnuplot_file *image-counter*) (mfuncall $wx_gnuplot_file *image-counter*))))
    (cons datafile gnpltfile)))

(defun $range (i j)
  (let ((x (gensym)))
    (mfuncall '$makelist x x i j)))

(defun maybe-load-package-for (wx-fun)
  (let ((fun ($get wx-fun '$function))
	(pkg ($get wx-fun '$load_package)))
    (if (and pkg (not (fboundp fun))) ($load pkg))))

(defun wxplot (wx-fun &rest args)
  ;; if gnuplot is not installed, this will terminate the
  ;; further execution.
  (wx-gnuplot-installation-check)
  (let ((filename (wxplot-filename))
	(data+maxout (wxplot-data+maxout))
	(fun ($get wx-fun '$function)))
    (maybe-load-package-for wx-fun)
    (imaxima-apply fun
		   `(,@args
		     ((mlist simp) $plot_format $gnuplot)
		     ((mlist simp) $gnuplot_term $ps)
		     ((mlist simp) $ps_file ,filename)
		     ((mlist simp) $gnuplot_script_file ,(cdr data+maxout))))
    ($ldisp `((wxxmltag simp) ,filename "img"))
    fun))

(defun wxdraw (wx-fun &rest args)
  (declare (special *image-counter*))
  ;; if gnuplot is not installed, this will terminate the
  ;; further execution.
  (wx-gnuplot-installation-check)
  (let* ((filename (wxplot-filename nil))
	 (datafile (format nil "data_~a.gnuplot" *image-counter*))
	 (gnpltfile (format nil "maxout_~a.gnuplot" *image-counter*))
	 (fun ($get wx-fun '$function))
	 (*windows-OS* t))
    (maybe-load-package-for wx-fun)
    (prog1
	(imaxima-apply fun
		       (append
			`(((mequal simp) $terminal $eps_color)
			  ((mequal simp) $dimensions
			   ((mlist simp)
			    ;; convert points to 1/100 of cm
			    ,(* 3.53 ($first $wxplot_size))
			    ,(* 3.53 ($second $wxplot_size))))
			  ((mequal simp) $file_name ,filename)
			  ((mequal simp) $data_file_name ,datafile)
			  ((mequal simp) $gnuplot_file_name ,gnpltfile))
			args))
      ($ldisp `((wxxmltag simp) ,(format nil "~a.eps" filename) "img")))))

(defmacro wx-def-plot/draw (fun pkg parent &optional init  &rest body)
  (let ((wx-fun (intern (format nil "$WX~a" (stripdollar fun)))))
    `(progn
       ,init
       ($put ',wx-fun ',pkg '$load_package)
       ($put ',wx-fun ',fun '$function)
       (defun ,wx-fun (&rest args)
	 ,body
	 (imaxima-apply ',parent (cons ',wx-fun args))))))

(wx-def-plot/draw $draw 	 $draw 		wxdraw)
(wx-def-plot/draw $draw2d 	 $draw 		wxdraw)
(wx-def-plot/draw $draw3d 	 $draw 		wxdraw)
(wx-def-plot/draw $plot2d 	 nil   		wxplot)
(wx-def-plot/draw $plot3d 	 nil   		wxplot)
(wx-def-plot/draw $implicit_plot $implicit_plot wxplot)
(wx-def-plot/draw $contour_plot  nil 		wxplot)
(wx-def-plot/draw $julia 	 $dynamics 	wxplot)
(wx-def-plot/draw $mandelbrot    $dynamics 	wxplot)

;; We could load drawdf package, because we want to overwrite wxdrawdf.
;; However, we do not need to, because wxdrawdf is a wrapper for wxdraw.
;; The following will load drawdf, then overwrite wxdrawdf with our own:
;; (wx-def-plot/draw $drawdf 	 $drawdf	wxdraw ($load '$drawdf))


;; end of imaxima.lisp
