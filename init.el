;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(defvar minemacs-core-modules
  '(defaults splash bootstrap core-ui keybindings evil completion))

(defvar minemacs-modules
  '(ui editor vc prog lisp data org notes window email docs
    natural-langs files tools biblio daemon rss ros eaf math media binary))

(defun minemacs-generate-autoloads ()
  (interactive)
  (when (file-exists-p minemacs-autoloads-file)
    (delete-file minemacs-autoloads-file))

  (let ((autoload-dirs nil)) 
    (dolist (dir (list minemacs-core-dir
                       minemacs-modules-dir
                       (expand-file-name "elisp" minemacs-root-dir)))
      (when (file-directory-p dir)
        (setq autoload-dirs
              (append
               autoload-dirs
               (list dir)
               (seq-filter
                #'file-directory-p
                (directory-files-recursively dir ".*" t))))))
    (if (<= emacs-major-version 28)
        (make-directory-autoloads autoload-dirs
                                  minemacs-autoloads-file)
      (loaddefs-generate autoload-dirs
                         minemacs-autoloads-file))))

;; Auto-loads
(unless (file-exists-p minemacs-autoloads-file)
  (minemacs-generate-autoloads))

;; Load autoloads file
(load minemacs-autoloads-file nil (not minemacs-verbose))

;; The modules.el file can override minemacs-modules and minemacs-core-modules
(let ((mods (expand-file-name "modules.el" minemacs-config-dir)))
  (when (file-exists-p mods)
    (+log! "Loading modules file from \"%s\"" mods)
    (load mods nil (not minemacs-verbose))))

(defun minemacs-load (&optional load-core-modules)
  "Reload all configuration, including user's config.el."

  ;; Load fonts early (they are read from the default `minemacs-default-fonts').
  (+set-fonts)

  ;; Core modules
  (when load-core-modules
    (dolist (module minemacs-core-modules)
      (+log! "Loading core module \"%s\"" module)
      (let ((filename (expand-file-name (format "me-%s.el" module) minemacs-core-dir)))
        (if (file-exists-p filename)
            (load filename nil (not minemacs-verbose))
          (+info! "Core module \"%s\" not found!" module)))))

  ;; Modules
  (dolist (module minemacs-modules)
    (+log! "Loading module \"%s\"" module)
    (let ((filename (expand-file-name (format "me-%s.el" module) minemacs-modules-dir)))
      (if (file-exists-p filename)
          (load filename nil (not minemacs-verbose))
        (+info! "Module \"%s\" not found!" module))))

  (when (and custom-file (file-exists-p custom-file))
    (+log! "Loafing user customs from custom.el")
    (load custom-file nil (not minemacs-verbose)))

  ;; Load user config when available
  (let ((user-config (expand-file-name "config.el" minemacs-config-dir)))
    (when (file-exists-p user-config)
      (+log! "Loading user config file from \"%s\"" user-config)
      (load user-config nil (not minemacs-verbose))))

  ;; Load GC module lastly
  (run-at-time
   5 nil
   (lambda ()
     (load (expand-file-name "me-gc.el" minemacs-core-dir)
           nil (not minemacs-verbose)))))

;; Load for the first time
(minemacs-load t)

(+log! "Loaded early-config.el")
