;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'compile)
(require 'project)
(eval-when-compile (require 'savehist))

(defgroup valgrind nil
  "Run valgrind as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'processes)

(defcustom valgrind-command "valgrind --leak-check=full "
  "Last shell command used to run valgrind; default for next valgrind run.
Sometimes it is useful for files to supply local values for this variable.
You might also use mode hooks to specify it in certain modes, like this:
  (add-hook \\='c-mode-hook
    (lambda ()
      (unless (or (file-exists-p \"makefile\")
                  (file-exists-p \"Makefile\"))
        (set (make-local-variable \\='valgrind-command)
             (concat \"make -k \"
                     (file-name-sans-extension buffer-file-name))))))"
  :type 'string
  :group 'valgrind)

(defcustom valgrind-buffer-name-function (lambda (_mode-name) "Takes the MODE-NAME, returns a string." "*valgrind*")
  "A function that takes one argument (mode name) and returns name of the buffer."
  :type 'function
  :group 'valgrind)

;; History of compile commands.
(defvar valgrind-history nil)

;; Integration with `savehist'
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'valgrind-history))

;;;###autoload
(defun valgrind (command)
  "Run valgrind.
Runs a shell COMMAND in a separate process asynchronously with output going to
the buffer `*valgrind*'.
You can then use the command \\[next-error] to find the next error message and
move to the source code that caused it."
  (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "Valgrind command: "
                                   (eval valgrind-command) nil nil
                                   '(valgrind-history . 1)))
     (list (eval valgrind-command))))
  (let ((default-directory (or (let ((proj (project-current))) (project-root proj)) default-directory)))
    (unless (equal command (eval valgrind-command))
      (setq valgrind-command command))
    (compilation-start command nil valgrind-buffer-name-function)))


(provide 'valgrind)

;;; me-valgrind.el ends here
