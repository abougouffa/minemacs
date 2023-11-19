;;; me-spell-fu.el --- Better integraion of spell-fu -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;; Adapted from Doom Emacs
(defun +spell-fu--correct (replace poss word orig-pt start end)
  "Correct word with spell-fu."
  (cond ((eq replace 'ignore)
         (goto-char orig-pt)
         nil)
        ((eq replace 'save)
         (goto-char orig-pt)
         (ispell-send-string (concat "*" word "\n"))
         (ispell-send-string "#\n")
         (setq ispell-pdict-modified-p '(t)))
        ((or (eq replace 'buffer) (eq replace 'session))
         (ispell-send-string (concat "@" word "\n"))
         (add-to-list 'ispell-buffer-session-localwords word)
         (or ispell-buffer-local-name ; session localwords might conflict
             (setq ispell-buffer-local-name (buffer-name)))
         (when (null ispell-pdict-modified-p) (setq ispell-pdict-modified-p (list nil)))
         (goto-char orig-pt)
         (when (eq replace 'buffer) (ispell-add-per-file-word-list word)))
        (replace
         (let ((new-word (if (atom replace) replace (car replace)))
               (orig-pt (+ (- (length word) (- end start)) orig-pt)))
           (unless (equal new-word (car poss))
             (delete-region start end)
             (goto-char start)
             (insert new-word))))
        ((goto-char orig-pt)
         nil)))

;; Adapted from Doom Emacs
;;;###autoload
(defun +spell-fu-correct ()
  "Correct spelling of word at point."
  (interactive)
  ;; spell-fu fails to initialize correctly if it can't find aspell or a similar
  ;; program. We want to signal the error, not tell the user that every word is
  ;; spelled correctly.
  (unless (or (and ispell-really-aspell ispell-program-name) (executable-find "aspell"))
    (user-error "Aspell is required for spell checking"))

  (ispell-set-spellchecker-params)
  (save-current-buffer (ispell-accept-buffer-local-defs))
  (if (not (featurep 'vertico))
      (call-interactively #'ispell-word)
    (cl-destructuring-bind (start . end)
        (or (bounds-of-thing-at-point 'word) (user-error "No word at point"))
      (let ((word (thing-at-point 'word t))
            (orig-pt (point))
            poss ispell-filter)
        (ispell-send-string "%\n")
        (ispell-send-string (concat "^" word "\n"))
        (while (progn (accept-process-output ispell-process)
                      (not (string= "" (car ispell-filter)))))
        (setq ispell-filter (cdr ispell-filter)) ; Remove leading empty element
        ;; ispell process should return something after word is sent. Tag word as valid (i.e., skip) otherwise
        (unless ispell-filter (setq ispell-filter '(*)))
        (when (consp ispell-filter) (setq poss (ispell-parse-output (car ispell-filter))))
        (cond
         ((or (eq poss t) (stringp poss)) ; don't correct word
          (message "%s is correct" (funcall ispell-format-word-function word))
          t)
         ((null poss) ; ispell error
          (error "Ispell: error in Ispell process"))
         (t ; The word is incorrect, we have to propose a replacement
          (setq res (completing-read (format "Corrections for %S: " word) (nth 2 poss)))
          (unless res (setq res (cons 'break word)))
          (cond
           ((stringp res)
            (+spell-fu--correct res poss word orig-pt start end))
           ((let ((cmd (car res))
                  (wrd (cdr res)))
              (unless (memq cmd '(skip break stop))
                (+spell-fu--correct cmd poss wrd orig-pt start end)
                (unless (string-equal wrd word)
                  (+spell-fu--correct wrd poss word orig-pt start end))))))
          (ispell-pdict-save t)))))))

(defun +spell-fu--add-dictionary (lang)
  "Add `LANG` to spell-fu multi-dict, with a personal dictionary."
  ;; Add the dictionary
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary lang))
  (let ((personal-dict-file (expand-file-name (format "personal-aspell.%s.pws" lang) spell-fu-directory)))
    ;; Create an empty personal dictionary if it doesn't exists
    (unless (file-exists-p personal-dict-file) (write-region "" nil personal-dict-file))
    ;; Add the personal dictionary
    (spell-fu-dictionary-add (spell-fu-get-personal-dictionary (format "%s-personal" lang) personal-dict-file))))

;;;###autoload
(defmacro +spell-fu-register-dictionaries! (&rest langs)
  "Register dictionaries for `LANGS` to spell-fu's multi-dict."
  (let* ((fn-name (intern (format "+spell-fu--multi-langs-%s-h" (string-join langs "-"))))
         (closure `(defun ,fn-name ())))
    (dolist (lang langs)
      (setq closure (append closure `((+spell-fu--add-dictionary ,lang)))))
    (append '(add-hook (quote spell-fu-mode-hook)) (list closure))))


(provide 'me-spell-fu)

;;; me-spell-fu.el ends here
