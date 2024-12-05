;; valgrind.el -- Valgrind -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;; Original inspiration: https://github.com/filcab/elisp/blob/master/valgrind.el

;;; Commentary:

;;; Code:

(require 'compile)
(autoload 'project-root "project")
(autoload 'project-current "project")
(eval-when-compile (require 'savehist))

(defgroup valgrind nil
  "Run Valgrind as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'processes)

(defcustom valgrind-command "valgrind"
  "Valgrind command."
  :type '(choice string function sexp)
  :group 'valgrind)

(defcustom valgrind-command-args "--leak-check=full"
  "Default arguments to pass to `valgrind-command'."
  :type '(choice string function sexp)
  :group 'valgrind)

(defcustom valgrind-full-command nil
  "The `valgrind' command will use this instead of asking."
  :type '(choice string function sexp)
  :group 'valgrind)

(defcustom valgrind-buffer-name-function (lambda (_mode-name) "Takes the MODE-NAME, returns a string." "*valgrind*")
  "A function that takes one argument (mode name) and returns name of the buffer."
  :type 'function
  :group 'valgrind)

(defcustom valgrind-default-tool "memcheck"
  "The default Valgrind tool."
  :type 'string
  :group 'valgrind)

;; History of compiple commands.
(defvar valgrind-history nil)

;; Integration with `savehist'
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'valgrind-history))

(defconst valgrind-tools
  '(("memcheck"    . "Memory error detector")
    ("cachegrind"  . "High-precision tracing profiler")
    ("callgrind"   . "Call-graph generating cache and branch prediction profiler")
    ("helgrind"    . "Thread error detector")
    ("drd"         . "Thread error detector")
    ("massif"      . "Heap profiler")
    ("dhat"        . "Dynamic heap analysis tool")
    ("lackey"      . "Example tool")
    ("none"        . "The minimal Valgrind tool")
    ("verrou"      . "Floating-point rounding errors checker")
    ("exp-bbv"     . "Experimental basic block vector generation tool")
    ("exp-sgcheck" . "Experimental stack and global array overrun detector"))
  "The list of Valgrind tools.")


;;;###autoload
(defun valgrind-tool (tool &optional extra-args)
  "Run Valgrind with TOOL and EXTRA-ARGS.
Prompt for the tool when called with \\[universal-argument]."
  (interactive (list (if current-prefix-arg
                         (let ((completion-extra-properties
                                `(:annotation-function ,(lambda (sel) (propertize (format " \t %s" (alist-get sel valgrind-tools nil nil #'equal)) 'face font-lock-comment-face)))))
                           (completing-read "Select a Valgrind tool to use: " valgrind-tools nil nil nil nil valgrind-default-tool))
                       valgrind-default-tool)))
  (valgrind (read-from-minibuffer "Valgrind command: "
                                  (concat (format "%s --tool=%s " valgrind-command tool)
                                          (string-join extra-args " "))
                                  nil nil '(valgrind-history . 1))))

;;;###autoload
(defun valgrind (command)
  "Run Valgrind.
Runs a shell COMMAND in a separate process asynchronously with output going to
the buffer `*valgrind*'.
You can then use the command \\[next-error] to find the next error message and
move to the source code that caused it."
  (interactive
   (let* ((ask? (or compilation-read-command current-prefix-arg))
          (cmd (or (and (not ask?) valgrind-full-command)
                   (cond ((functionp valgrind-command) (funcall valgrind-command))
                         (t (eval valgrind-command))))))
     (list (if ask?
               (read-from-minibuffer "Valgrind command: " cmd nil nil '(valgrind-history . 1))
             cmd))))
  (let ((default-directory (or (let ((proj (project-current))) (project-root proj)) default-directory)))
    (setq valgrind-full-command command)
    (compilation-start command nil valgrind-buffer-name-function)))


(provide 'valgrind)
;;; valgrind.el ends here
