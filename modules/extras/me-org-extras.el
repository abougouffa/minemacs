;;; me-org-extras.el --- Extra tweaks Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(defvar +org-responsive-image-percentage 0.4)
(defvar +org-responsive-image-width-limits '(400 . 700)) ;; '(min . max)
(defvar-local +org-export-to-pdf-main-file nil
  "The main (entry point) Org file for a multi-files document.")

(put '+org-export-to-pdf-main-file 'safe-local-variable 'stringp)

(defun +org-extras--responsive-image-h ()
  (when (derived-mode-p 'org-mode)
    (setq-local
     org-image-actual-width
     (list (max (car +org-responsive-image-width-limits)
                (min (cdr +org-responsive-image-width-limits)
                     (truncate (* (window-pixel-width)
                                  +org-responsive-image-percentage))))))))

(defun +org-extras--parse-latex-env (str)
  "Parse the LaTeX environment STR.
Return an AST with newlines counts in each level."
  (let (ast)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward
              (rx "\\"
                  (group (or "\\" "begin" "end" "nonumber"))
                  (zero-or-one "{" (group (zero-or-more not-newline)) "}"))
              nil t)
        (let ((cmd (match-string 1))
              (env (match-string 2)))
          (cond ((string= cmd "begin")
                 (push (list :env (intern env)) ast))
                ((string= cmd "\\")
                 (let ((curr (pop ast)))
                   (push (plist-put curr :newline (1+ (or (plist-get curr :newline) 0))) ast)))
                ((string= cmd "nonumber")
                 (let ((curr (pop ast)))
                   (push (plist-put curr :nonumber (1+ (or (plist-get curr :nonumber) 0))) ast)))
                ((string= cmd "end")
                 (let ((child (pop ast))
                       (parent (pop ast)))
                   (push (plist-put parent :childs (cons child (plist-get parent :childs))) ast)))))))
    (plist-get (car ast) :childs)))

;; Adapted from Scimax
(defun +org-extras-renumber-env (orig-func &rest args)
  "A function to inject numbers in LaTeX fragment previews."
  (let ((results '())
        (counter -1))
    (setq results
          (cl-loop for (begin . env) in
                   (org-element-map (org-element-parse-buffer) 'latex-environment
                     (lambda (env)
                       (cons
                        (org-element-property :begin env)
                        (org-element-property :value env))))
                   collect
                   (cond
                    ((and (string-match "\\\\begin{equation}" env)
                          (not (string-match "\\\\tag{" env)))
                     (cl-incf counter)
                     (cons begin counter))
                    ((string-match "\\\\begin{align}" env)
                     (cl-incf counter)
                     (let ((p (car (+org-extras--parse-latex-env env))))
                       ;; Parse the `env', count new lines in the align env as equations, unless
                       (cl-incf counter (- (or (plist-get p :newline) 0)
                                           (or (plist-get p :nonumber) 0))))
                     (cons begin counter))
                    (t
                     (cons begin nil)))))
    (when-let ((number (cdr (assoc (point) results))))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" number)
             (car args)))))
  (apply orig-func args))

(defun +org-extras-toggle-latex-equation-numbering (&optional enable)
  "Toggle whether LaTeX fragments are numbered."
  (interactive)
  (if (or enable (not (get '+org-extras-renumber-env 'enabled)))
      (progn
        (advice-add 'org-create-formula-image :around #'+org-extras-renumber-env)
        (put '+org-extras-renumber-env 'enabled t)
        (message "LaTeX numbering enabled."))
    (advice-remove 'org-create-formula-image #'+org-extras-renumber-env)
    (put '+org-extras-renumber-env 'enabled nil)
    (message "LaTeX numbering disabled.")))

(defun +org-extras-inject-latex-fragment (orig-func &rest args)
  "Advice function to inject latex code before and/or after the equation in a latex fragment.
  You can use this to set \\mathversion{bold} for example to make
  it bolder. The way it works is by defining
  :latex-fragment-pre-body and/or :latex-fragment-post-body in the
  variable `org-format-latex-options'. These strings will then be
  injected before and after the code for the fragment before it is
  made into an image."
  (setf (car args)
        (concat
         (or (plist-get org-format-latex-options :latex-fragment-pre-body) "")
         (car args)
         (or (plist-get org-format-latex-options :latex-fragment-post-body) "")))
  (apply orig-func args))

(defun +org-extras-inject-latex-fragments ()
  "Toggle whether you can insert latex in fragments."
  (interactive)
  (if (not (get '+org-extras-inject-latex-fragment 'enabled))
      (progn
        (advice-add 'org-create-formula-image :around #'+org-extras-inject-latex-fragment)
        (put '+org-extras-inject-latex-fragment 'enabled t)
        (message "Inject latex enabled"))
    (advice-remove 'org-create-formula-image #'+org-extras-inject-latex-fragment)
    (put '+org-extras-inject-latex-fragment 'enabled nil)
    (message "Inject latex disabled")))

;; Adapted from: https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-org.el
(defun +org-lower-case-keywords-and-properties ()
  "Lower case Org keywords and properties and block identifiers.
Example: \"#+TITLE\" -> \"#+title\"
         \"#+BEGIN_EXAMPLE\" -> \"#+begin_example\"
         \":PROPERTIES:\" -> \":properties:\"."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      (while (re-search-forward
              (rx (group-n 1
                    bol
                    (zero-or-more " ")
                    (or "#+" ":")
                    (one-or-more (any "A-Z"))
                    (zero-or-more (seq "_" (one-or-more alpha)))
                    (or (any " " ":" "=") eol)))
              nil :noerror)
        (setq count (1+ count))
        (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil 1))
      (message "Lower-cased %d matches" count))))

(defun +org-extras-responsive-images-setup ()
  (add-hook 'window-configuration-change-hook
            #'+org-extras--responsive-image-h))

(defun +org-extras-equation-numbering-setup ()
  ;; Enable LaTeX equations renumbering
  (+shutup!
   (+org-extras-toggle-latex-equation-numbering :enable)))

(defun +org-extras-multifiles-document-setup ()
  (advice-add
   'org-latex-export-to-pdf :around
   (defun +org--latex-export-to-pdf-main-file-a (orig-fn &rest orig-args)
     (let* ((main-file (or (bound-and-true-p +org-export-to-pdf-main-file) "main.org"))
            (out-file
             (if (file-exists-p (expand-file-name main-file))
                 (with-current-buffer (find-file-noselect main-file)
                   (apply orig-fn orig-args))
               (apply orig-fn orig-args))))
       (if org-export-in-background
           (progn
             (message "Started exporting \"%s\" asynchronously."
                      (abbreviate-file-name
                       (file-name-nondirectory main-file)))
             (when-let ((org-export-process (get-process "org-export-process")))
               (set-process-sentinel
                org-export-process
                (lambda (process event)
                  (unless (process-live-p process)
                    (message "Org async export finised, see *Org Export Process* for more details."))))))
         (message "PDF exported to: %s."
                  (abbreviate-file-name
                   (file-name-nondirectory out-file))))))))

(defun +org-extras-latex-classes-setup ()
  (with-eval-after-load 'ox-latex
    (dolist
        (class
         '(("lettre"
            "\\documentclass{lettre}"
            ("\\section{%s}"       . "\\section*{%s}")
            ("\\subsection{%s}"    . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}"     . "\\paragraph*{%s}")
            ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
           ("blank"
            "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
            ("\\section{%s}"       . "\\section*{%s}")
            ("\\subsection{%s}"    . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}"     . "\\paragraph*{%s}")
            ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
           ("IEEEtran"
            "\\documentclass{IEEEtran}"
            ("\\section{%s}"       . "\\section*{%s}")
            ("\\subsection{%s}"    . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}"     . "\\paragraph*{%s}")
            ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
           ("ieeeconf"
            "\\documentclass{ieeeconf}"
            ("\\section{%s}"       . "\\section*{%s}")
            ("\\subsection{%s}"    . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}"     . "\\paragraph*{%s}")
            ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
           ("sagej"
            "\\documentclass{sagej}"
            ("\\section{%s}"       . "\\section*{%s}")
            ("\\subsection{%s}"    . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}"     . "\\paragraph*{%s}")
            ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
           ("thesis"
            "\\documentclass[11pt]{book}"
            ("\\chapter{%s}"       . "\\chapter*{%s}")
            ("\\section{%s}"       . "\\section*{%s}")
            ("\\subsection{%s}"    . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}"     . "\\paragraph*{%s}"))
           ("thesis-fr"
            "\\documentclass[french,12pt,a4paper]{book}"
            ("\\chapter{%s}"       . "\\chapter*{%s}")
            ("\\section{%s}"       . "\\section*{%s}")
            ("\\subsection{%s}"    . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}"     . "\\paragraph*{%s}"))))
      (add-to-list 'org-latex-classes class))))

(defun +org-extras-outline-path-setup ()
  (advice-add
   #'org-format-outline-path :around
   (defun +org--strip-properties-from-outline-a (fn &rest args)
     (let ((org-level-faces
            (cl-loop for face in org-level-faces
                     collect `(:foreground ,(face-foreground face nil t)
                               :weight regular))))
       (apply fn args)))))

(defun +org-extras-pretty-latex-fragments-setup ()
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

  ;; Can be dvipng, dvisvgm, imagemagick
  (setq org-preview-latex-default-process 'dvisvgm)

  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))

  (unless (+emacs-features-p 'pgtk) ;; PGTK not need extra up-scaling
    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale 1.5))))

(defun +org-extras-lower-case-keywords-and-properties-setup ()
  (add-hook
   'before-save-hook
   (defun +org--lower-case-keywords-and-properties-h ()
     (when (derived-mode-p 'org-mode)
       (+org-lower-case-keywords-and-properties)))))

(defun +org-extras-setup ()
  (+org-extras-outline-path-setup)
  (+org-extras-latex-classes-setup)
  (+org-extras-pretty-latex-fragments-setup)
  (+org-extras-responsive-images-setup)
  (+org-extras-equation-numbering-setup)
  (+org-extras-multifiles-document-setup)
  (+org-extras-lower-case-keywords-and-properties-setup))


(provide 'me-org-extras)
