;; me-lib-extra.el -- MinEmacs Library (extra features and commands) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;; Minemacs' core functions and macros

(require 'me-lib)


;;;###autoload
(defun minemacs-run-build-functions (&optional dont-ask-p)
  "Run all build functions in `minemacs-build-functions'.

Call functions without asking when DONT-ASK-P is non-nil."
  (interactive "P")
  (dolist (fn minemacs-build-functions)
    (message "[MinEmacs]: Running `%s'" fn)
    (if dont-ask-p
        ;; Do not ask before installing
        (cl-letf (((symbol-function 'yes-or-no-p) #'always)
                  ((symbol-function 'y-or-n-p) #'always))
          (funcall-interactively fn))
      (funcall-interactively fn))))

;;;###autoload
(defun minemacs-bump-packages ()
  "Update MinEmacs packages to the last revisions (can cause breakages)."
  (interactive)
  ;; Load all modules
  (message "[MinEmacs]: Loading all modules and on-demand modules")
  (apply #'minemacs-load-module (minemacs-modules t))

  ;; Update straight recipe repositories
  (straight-pull-recipe-repositories)

  ;; Run `straight's update cycle, taking into account the explicitly pinned versions
  (message "[MinEmacs]: Pulling packages")
  (straight-pull-all)
  (message "[MinEmacs]: Freezing packages")
  (straight-freeze-versions)
  (message "[MinEmacs]: Rebuilding packages")
  (straight-rebuild-all)

  ;; Run package-specific build functions (ex: `pdf-tools-install')
  (message "[MinEmacs]: Running additional package-specific build functions")
  (minemacs-run-build-functions 'dont-ask))

;;;###autoload
(defun minemacs-upgrade (pull-minemacs)
  "Upgrade the packages list to the locked revisions.
This takes into account the explicitly pinned packages. When called with
\\[universal-argument] or with PULL-MINEMACS, it will run \"git pull\"
in MinEmacs directory before upgrading."
  (interactive "P")
  (when pull-minemacs
    (let ((default-directory minemacs-root-dir))
      (vc-pull)))
  ;; Update straight recipe repositories
  (straight-pull-recipe-repositories)
  (message "[MinEmacs] Restoring packages from the global lockfile versions")
  (straight-thaw-versions)
  ;; Rebuild the packages
  (message "[MinEmacs] Rebuilding packages")
  (straight-rebuild-all)
  ;; Run package-specific build functions (ex: `pdf-tools-install')
  (message "[MinEmacs] Running additional package-specific build functions")
  (minemacs-run-build-functions 'dont-ask))

;;;###autoload
(defun minemacs-root-dir-cleanup ()
  "Cleanup MinEmacs' root directory."
  (let ((default-directory minemacs-root-dir))
    (mapc (+apply-partially-right #'+delete-file-or-directory 'trash 'recursive)
          (directory-files minemacs-root-dir nil (rx (seq bol (or "eln-cache" "auto-save-list" "elpa") eol))))))

;;;###autoload
(defun +straight-prune-build-cache ()
  "Prune straight.el build directories for old Emacs versions."
  (let* ((default-directory (file-name-concat straight-base-dir "straight/")))
    ;; Prune the build cache and build directory.
    (straight-prune-build)
    ;; Prune old build directories
    (mapc (+apply-partially-right #'+delete-file-or-directory 'trash 'recursive)
          (seq-filter
           (lambda (name)
             (not (member name (list straight-build-dir (concat straight-build-dir "-cache.el") "versions" "repos"))))
           (directory-files default-directory nil directory-files-no-dot-files-regexp)))))

;;;###autoload
(defun minemacs-cleanup-emacs-directory ()
  "Cleanup unwanted files/directories from MinEmacs' directory."
  (interactive)
  (when (featurep 'native-compile)
    (+info! "Trying to clean outdated native compile cache")
    ;; Delete outdated natively compiled files when Emacs become idle
    (+shutup! (native-compile-prune-cache)))
  (+info! "Trying to clean outdated straight build cache")
  (+shutup! (+straight-prune-build-cache))
  (+info! "Trying to clean MinEmacs' root directory")
  (+shutup! (minemacs-root-dir-cleanup)))

;;;###autoload
(defun minemacs-apply-performance-tweaks ()
  "Set some Emacs variables for better (!) performance."
  (interactive)
  (setq inhibit-compacting-font-caches t ; Don’t compact font caches during GC
        read-process-output-max (* 1024 1024) ; Increase single chunk bytes to read from subprocess
        fast-but-imprecise-scrolling t)) ; Fast scrolling

;;;###autoload
(defun minemacs-load-module (&rest modules)
  "Interactively install and load MODULES that aren't enabled in \"modules.el\".

When called with \\[universal-argument], it prompts also for on-demand modules.
When called with \\[universal-argument] \\[universal-argument], it prompts also for obsolete modules."
  (interactive (completing-read-multiple
                "Select modules: "
                (seq-filter (lambda (module) (not (featurep module)))
                            (let ((prefix (prefix-numeric-value current-prefix-arg)))
                              (minemacs-modules (>= prefix 4) (>= prefix 16))))))
  (let ((old-hooks ; save the old MinEmacs hooks to detect when the loaded module requires a hook to be run
         (append minemacs-after-startup-hook minemacs-lazy-hook
                 minemacs-after-load-theme-hook minemacs-after-setup-fonts-hook
                 (cl-loop for hook in +first-file-hooks append (eval hook))))
        (old-fns minemacs-build-functions-hook))
    (mapc #'+load (mapcar (apply-partially #'format "%s%s.el" minemacs-modules-dir) modules))
    (let ((new-hooks
           (cl-set-difference
            (append minemacs-after-startup-hook minemacs-lazy-hook
                    minemacs-after-load-theme-hook minemacs-after-setup-fonts-hook
                    (cl-loop for hook in +first-file-hooks append (eval hook)))
            old-hooks))
          (minemacs-build-functions (cl-set-difference minemacs-build-functions old-fns)))
      (mapc #'funcall new-hooks)
      (minemacs-run-build-functions (not (called-interactively-p 'interactive))))))



;;; Files, directories and IO helper functions

;;;###autoload
(defun +file-name-incremental (filename)
  "Return a unique file name for FILENAME.
If \"file.ext\" exists, returns \"file-0.ext\"."
  (let* ((ext (file-name-extension filename))
         (dir (file-name-directory filename))
         (file (file-name-base filename))
         (filename-regex (concat "^" file "\\(?:-\\(?1:[[:digit:]]+\\)\\)?" (if ext (concat "\\." ext) "")))
         (last-file (car (last (directory-files dir nil filename-regex))))
         (last-file-num (and last-file (string-match filename-regex last-file) (match-string 1 last-file)))
         (num (1+ (string-to-number (or last-file-num "-1")))))
    (file-name-concat dir (format "%s%s%s" file (if last-file (format "-%d" num) "") (if ext (concat "." ext) "")))))

;;;###autoload
(defun +delete-this-file (&optional path force-p)
  "Delete PATH.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (unwind-protect
        (progn (delete-file path delete-by-moving-to-trash) t)
      (when (file-exists-p path)
        (error "Failed to delete %S" short-path)))))

;;;###autoload
(defun +delete-file-or-directory (file-or-directory &optional trash recursive)
  "Delete FILE-OR-DIRECTORY with `delete-file' or `delete-directory'.

Move to trash when TRASH is non-nil, delete directories recursively when
RECURSIVE is non-nil."
  (if (file-directory-p file-or-directory)
      (delete-directory file-or-directory recursive trash)
    (delete-file file-or-directory trash)))

;; Rewrite of: `crux-delete-file-and-buffer', proposes also to delete VC
;; controlled files even when `vc-delete-file' fails (edited, conflict, ...).
;;;###autoload
(defun +delete-this-file-and-buffer (&optional filename)
  "Delete FILENAME and its associated visiting buffer."
  (interactive)
  (when-let* ((filename (or filename (buffer-file-name)))
              (short-path (abbreviate-file-name filename)))
    (if (vc-backend filename)
        (or (ignore-errors (vc-delete-file (buffer-file-name)))
            (+delete-this-file filename)
            (kill-buffer))
      (when (y-or-n-p (format "Are you sure you want to delete %s? " short-path))
        (delete-file filename delete-by-moving-to-trash)
        (message "Deleted file %s" short-path)
        (kill-buffer)))))

;;;###autoload
(defun +copy-this-file-name ()
  "Save (copy) the file name of this buffer to the kill ring."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (with-temp-buffer
        (insert file)
        (kill-ring-save (point-min) (point-max)))
    (user-error "This buffer isn't bound to a file")))

(defvar +apply-patch-dwim-proj-dir nil)
(defvar +apply-patch-dwim-extra-options '("--ignore-whitespace"))
(autoload 'project-files "project")
(autoload 'diff-hunk-next "diff-mode")
(autoload 'diff-hunk-file-names "diff-mode")

(defun +patch-get-patched-files (patch-buff)
  "Get the list of the patches A/B files mentioned in PATCH-BUFF."
  (with-current-buffer (get-buffer patch-buff)
    (save-excursion
      (goto-char (point-min))
      (let ((last-pos (point))
            patched-files)
        (while (prog2 (ignore-errors (diff-hunk-next))
                   ;; To detect the last hunk
                   (not (= last-pos (point)))
                 (setq last-pos (point)))
          (setq patched-files (append patched-files (diff-hunk-file-names))))
        (mapcar #'substring-no-properties (delete-dups patched-files))))))

(defvar +apply-patch-dwim-pre-patch-functions nil)
(defvar +apply-patch-dwim-post-patch-functions nil)

;;;###autoload
(defun +apply-patch-dwim (patch-buf &optional proj-dir)
  "Apply PATCH-BUF to the relevant file in PROJ-DIR.
When a region is active, propose to use it as the patch buffer."
  (interactive (list (current-buffer)))
  (when-let* ((default-directory (or proj-dir (if (or current-prefix-arg (not +apply-patch-dwim-proj-dir))
                                                  (read-directory-name "Apply patch in directory: ")
                                                +apply-patch-dwim-proj-dir)))
              (proj (project-current))
              (patch-buf
               (or (cond ((use-region-p) ; When a region is selected, use it as a patch
                          (let ((patch-region (buffer-substring-no-properties (region-beginning) (region-end))))
                            (with-current-buffer (get-buffer-create (format " *%s:region-%d-%d-patch*" (buffer-file-name patch-buf) (region-beginning) (region-end)))
                              (delete-region (point-min) (point-max))
                              (insert patch-region)
                              (current-buffer))))
                         ((member (file-name-extension (buffer-file-name patch-buf)) '("diff" "DIFF" "patch" "PATCH"))
                          (get-buffer patch-buf))
                         ((y-or-n-p (format "The buffer %s doesn't seem to be a patch, select another buffer? " patch-buf))
                          (get-buffer (read-buffer "Select the buffer containing the patch: ")))
                         (t (get-buffer patch-buf))))))
    (setq +apply-patch-dwim-proj-dir default-directory) ; To remember it!
    (let* (candidates
           (patch-files (+patch-get-patched-files patch-buf))
           (existing-patched-files
            (delete-dups
             (mapcar (lambda (str) (substring str 2)) ; Remove the "a/" prefix
                     (seq-filter ; Keep only patch-files that already exist ("a/*") in the file tree (new files are tricky)
                      (apply-partially #'string-prefix-p "a/") patch-files))))
           (proj-files (project-files proj)))
      (dolist (existing-patched-file existing-patched-files)
        (when-let* ((cand-files (seq-filter (apply-partially #'string-suffix-p (concat "/" existing-patched-file)) proj-files)))
          (push (cons existing-patched-file (mapcar (lambda (str) (substring str 0 (- (length str) (length existing-patched-file)))) cand-files)) candidates)))
      ;; Accurate strategy, the directory that applies to all files
      (let ((results (cdr (car candidates))))
        (dolist (candidate (cdr candidates))
          (setq results (seq-intersection candidate results #'equal)))
        ;; TODO: Frequency strategy, merge all directories and sort them by frequencies (maybe use dash's `-frequencies')
        (let ((target-dir (cond ((length= results 1) (car results))
                                ((length> results 1) (completing-read "Select a target directory: " results))
                                (t (read-directory-name "Cannot deduce the target directory, select one: ")))))
          (when (y-or-n-p (format "Apply patch %S in directory %S?" (file-name-nondirectory (buffer-file-name patch-buf)) target-dir))
            (let* ((default-directory target-dir)
                   (patch-file
                    (or (buffer-file-name patch-buf)
                        (with-current-buffer patch-buf
                          (let ((temp-filename (make-temp-file "apply-patch-dwim-" nil ".patch")))
                            (write-region (point-min) (point-max) temp-filename)
                            temp-filename))))
                   (out-buf (get-buffer-create (format " *apply-patch-dwim:%s*" (file-name-nondirectory patch-file)))))
              (run-hook-with-args '+apply-patch-dwim-pre-patch-functions patch-buf patch-files target-dir)
              (with-current-buffer out-buf
                (setq default-directory target-dir)
                (let ((inhibit-read-only t))
                  (make-process
                   :name (format "patch-%s" (file-name-nondirectory patch-file))
                   :buffer out-buf
                   :command `("patch" "-p1" "--force" ,@+apply-patch-dwim-extra-options "-i" ,patch-file) ; TODO: --silent instead of --force may be interesting!
                   :sentinel (lambda (proc _event)
                               (unless (process-live-p proc)
                                 (if (zerop (process-exit-status proc))
                                     (when (y-or-n-p (format "Patch applied successfylly, open target directory %S?" target-dir))
                                       (dired target-dir))
                                   (pop-to-buffer out-buf))))))
                (view-mode 1))
              (run-hook-with-args '+apply-patch-dwim-post-patch-functions patch-buf patch-files target-dir))))))))

;;;###autoload
(defun +clean-file-name (filename &optional downcase-p)
  "Clean FILENAME, optionally convert to DOWNCASE-P."
  ;; Clean slashes, backslashes, ":", ";", spaces, and tabs
  (replace-regexp-in-string
   "[:;\t\n\r /\\_]+" "-"
   (replace-regexp-in-string
    "[‘’‚“”„\"`'()&]+" ""
    (if downcase-p (downcase filename) filename))))



;;; Exporter and converters

(defcustom +html2pdf-default-backend
  (cond ((executable-find "wkhtmltopdf") 'wkhtmltopdf)
        ((executable-find "htmldoc") 'htmldoc)
        ((executable-find "weasyprint") 'weasyprint)
        ((and (executable-find "pandoc") (executable-find "context")) 'pandoc+context)
        ((executable-find "pandoc") 'pandoc))
  "The default backend to convert HTML files to PDFs in `+html2pdf'."
  :group 'minemacs-utils
  :type '(choice
          (const wkhtmltopdf)
          (const htmldoc)
          (const weasyprint)
          (const pandoc+context)
          (const pandoc)))

(defcustom +html2pdf-backend-config-file nil
  "A config file to use with the backend tool (pandoc, weasyprint, ...)."
  :group 'minemacs-utils
  :type 'file)

;;;###autoload
(defun +html2pdf (infile outfile &optional backend)
  "Convert HTML file INFILE to PDF and save it to OUTFILE.
When BACKEND is provided, the corresponding program is used, otherwise, the
value of `+html2pdf-default-backend' is used."
  (if-let* ((default-directory (file-name-directory infile))
            (backend (or backend +html2pdf-default-backend))
            (backend-command
             (pcase backend
               ('wkhtmltopdf
                (list "wkhtmltopdf"
                      "--images" "--disable-javascript" "--enable-local-file-access"
                      "--encoding" "utf-8"
                      infile outfile))
               ('htmldoc
                (list "htmldoc"
                      "--charset" "utf-8"
                      "--bodyfont" "sans" "--textfont" "sans" "--headfootfont" "sans"
                      "--top" "50#mm" "--bottom" "50#mm" "--right" "50#mm" "--left" "50#mm"
                      "--fontsize" "10"
                      "--size" "a4"
                      "--continuous"
                      "--outfile" outfile infile))
               ('weasyprint
                (list "weasyprint"
                      "--encoding" "utf-8"
                      "--stylesheet" (or +html2pdf-backend-config-file
                                         (expand-file-name "templates/+html2pdf/weasyprint-pdf.css" minemacs-assets-dir))
                      infile outfile))
               ('pandoc+context
                (list "pandoc"
                      "--pdf-engine=context"
                      "--variable" "fontsize=10pt"
                      "--variable" "linkstyle=slanted"
                      "-o" outfile infile))
               ('pandoc
                (list "pandoc"
                      "--defaults" (or +html2pdf-backend-config-file
                                       (expand-file-name "templates/+html2pdf/pandoc.yaml" minemacs-assets-dir))
                      "-o" outfile infile)))))
      (apply #'call-process (append (list (car backend-command) nil nil nil) (cdr backend-command)))
    (user-error "Backend \"%s\" not available" backend)))

;;;###autoload
(defun +txt2html (infile outfile &optional mail-mode-p)
  "Convert plain-text file INFILE to HTML and save it to OUTFILE.
When MAIL-MODE-P is non-nil, --mailmode is passed to \"txt2html\"."
  (apply #'call-process (append '("txt2html" nil nil nil "-8")
                                (when mail-mode-p '("--mailmode"))
                                (list "--outfile" outfile infile))))

(defvar +save-as-pdf-filename nil
  "File name to use, if non-nil, for the output file.")

;;;###autoload
(defun +save-as-pdf (infile &optional mail-mode-p)
  "Save URL as PDF.
This function's signature is compatible with `browse-url-browser-function'
so it can be used to save HTML pages or emails to PDF.
When MAIL-MODE-P is non-nil, treat INFILE as a mail."
  (let* ((infile (string-trim-left infile "file://"))
         (outfile (+file-name-incremental
                   (or +save-as-pdf-filename
                       (expand-file-name
                        (file-name-with-extension (file-name-base infile) ".pdf")
                        (file-name-directory infile))))))
    (if (zerop
         ;; For HTML files (with extension ".html" or ".htm"), just call `+html2pdf'
         (if (string-match-p "^html?$" (file-name-extension infile))
             (+html2pdf infile outfile)
           ;; For non-HTML (plain-text) files, convert them to HTML then call `+html2pdf'
           (let ((tmp-html (make-temp-file "txt2html-" nil ".html")))
             (+txt2html infile tmp-html mail-mode-p)
             (+html2pdf tmp-html outfile))))
        (message "Exported PDF to %S"
                 (truncate-string-to-width (abbreviate-file-name outfile) (/ (window-width (minibuffer-window)) 2) nil nil t))
      (user-error (if (file-exists-p outfile) "PDF created but with some errors!" "An error occurred, cannot create the PDF!")))))

(defcustom +single-file-executable "single-file"
  "The executable for \"single-file\" which is used archive HTML pages."
  :type 'string
  :group 'minemacs-utils)

;;;###autoload
(defun +single-file (url out-file)
  "Save URL into OUT-FILE as a standalone HTML file."
  (interactive
   (let ((url (or (thing-at-point 'url) (read-string "URL to save: "))))
     (list url (read-file-name "Save to: " nil nil nil (url-filename (url-generic-parse-url url))))))
  (if (executable-find +single-file-executable)
      (make-process
       :name "single-file-cli"
       :buffer "*single-file*"
       :command (list +single-file-executable
                      "--browser-executable-path" browse-url-chromium-program
                      url (expand-file-name out-file)))
    (user-error "Please set `+single-file-executable' accordingly")))

(defvar +browse-html-file-browser-priority '(xwidget-webkit-browse-url eww-browse-url)
  "A list of `browse-url' functions for `+browse-html-file'.
The list is in priority order.")

;;;###autoload
(defun +browse-html-file (file)
  "Browser HTML FILE following `+browse-html-file-browser-priority'.

If no function from `+browse-html-file-browser-priority' is available,
use `browse-url'.

When called with universal argument, open the current buffer's file."
  (interactive (list (if current-prefix-arg (buffer-file-name) (read-file-name "HTML file to open: "))))
  (let* ((file (or file
                   ;; Non file-visiging buffer, make a temporary file
                   (let ((buf-content (buffer-substring-no-properties (point-min) (point-max))))
                     (with-temp-buffer
                       (set-visited-file-name (make-temp-file "browse-html-" nil ".html"))
                       (insert buf-content)
                       (save-buffer)
                       (buffer-file-name)))))
         (url (format "file://%s" (expand-file-name file)))
         (browse-backend (or (cl-find-if #'fboundp +browse-html-file-browser-priority)
                             #'browse-url)))
    (funcall browse-backend url)))



;;; Serial port

(autoload 'term-send-string "term")
(defcustom +serial-port "/dev/ttyUSB0"
  "The default port (device) to use."
  :group 'minemacs-utils
  :type 'file)

(defcustom +serial-baudrate 115200
  "The default baudrate to use."
  :group 'minemacs-utils
  :type 'natnum)

(defcustom +serial-first-commands nil
  "A list of commands to run in the serial terminal after creation."
  :group 'minemacs-utils
  :type '(repeat string))

(defvar +serial-buffer nil)
(defvar +serial-process nil)

;;;###autoload
(defun +serial-running-p ()
  "Is there a serial port terminal running?"
  (buffer-live-p +serial-buffer) (process-live-p +serial-process))

(defun +serial--run-commands (port baud &rest commands)
  "Run COMMANDS on a device via serial communication.

Connect at PORT with baudrate BAUD."
  (let ((commands (ensure-list commands)))
    (unless (+serial-running-p)
      (setq +serial-buffer (serial-term port baud)
            +serial-process (get-buffer-process +serial-buffer))
      (cl-callf append commands +serial-first-commands))
    (if (+serial-running-p)
        (term-send-string +serial-process (string-join (append commands '("\n")) "\n"))
      (user-error "Unable to communicate with the serial terminal process"))))

;;;###autoload
(defun +serial-run-commands (commands &optional port baud)
  "Run COMMANDS on a device via serial communication.

If PORT or BAUD are nil, use values from `+serial-port' and `+serial-baudrate'."
  (interactive (list (read-shell-command (format "Run command via serial port: "))))
  (let ((port (or port +serial-port))
        (baud (or baud +serial-baudrate)))
    (+log! "Dev %s@%d: running commands %S" port baud commands)
    (apply #'+serial--run-commands (append (list port baud) (ensure-list commands)))))



;;; Networking

(defvar +net-default-device "wlan0")

;;;###autoload
(defun +net-get-ip-address (&optional dev)
  "Get the IP-address for device DEV (default: eth0) of the current machine."
  (let ((dev (or dev +net-default-device)))
    (format-network-address (car (network-interface-info dev)) t)))



;;; Github

;;;###autoload
(defun +github-latest-release (repo &optional fallback-release trim-v-prefix)
  "Get the latest release of REPO. Strips the \"v\" at left.
Fallback to FALLBACK-RELEASE when it can't get the last one.
When TRIM-V-PREFIX is non-nil, trim the \"v\" prefix from the version."
  (if-let* ((latest
             (ignore-errors
               (with-temp-buffer
                 (+shutup!
                  (url-insert-file-contents
                   (format "https://api.github.com/repos/%s/releases/latest" repo)))
                 (json-parse-buffer :object-type 'plist)))))
      (let ((ver (car (last (split-string (plist-get latest :html_url) "/")))))
        (if trim-v-prefix
            (string-trim-left ver "v")
          ver))
    fallback-release))

(defvar +github-download-dir minemacs-local-dir)

;;;###autoload
(cl-defun +github-download-release (repo filename-fmt &optional ok-if-already-exists &key ver out-file)
  "Download release from REPO.

FILENAME-FMT is a string representing the file name, it can include the
special format {{ver}} which gets replaced with the release version.
When OK-IF-ALREADY-EXISTS is non-nil, the file gets overwritten if it
already exists.

Keyword argument :VER can be used to pass the version to download, when
no version is passed, the latest release is downloaded. The :OUT-FILE
can be used to choose the output file name, otherwise, the file will be
downloaded with it's original file name to `+github-download-dir'"
  (when-let* ((ver (or ver (+github-latest-release repo ver t))))
    (let* ((url (format "https://github.com/%s/releases/download/v%s/%s" repo ver (string-replace "{{ver}}" ver filename-fmt)))
           (out-file (or out-file (expand-file-name (url-file-nondirectory url) +github-download-dir))))
      (if (and (not ok-if-already-exists) (file-exists-p out-file))
          (+log! "File %S already exist" out-file)
        (mkdir (file-name-directory out-file) t)
        (+shutup! (url-copy-file url out-file ok-if-already-exists)))
      out-file)))



;;; Directory local tweaks & hacks

;;;###autoload
(defun +dir-locals-reload-for-this-buffer ()
  "Reload directory-local for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)
    (+info! "Reloaded directory-local variables for buffer %s"
            (buffer-name (current-buffer)))))

;;;###autoload
(defun +dir-locals-reload-for-all-buffers-in-this-directory ()
  "Reload dir-locals for all buffers in the current `default-directory'."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (+dir-locals-reload-for-this-buffer))))))

(defun +dir-locals--autoreload-h ()
  "Is it relevant to auto reload dir-locals for his buffer."
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (+dir-locals-reload-for-all-buffers-in-this-directory)
    (message "Reloaded directory-local variables defined in %s." dir-locals-file)))

(defvar +dir-locals--autoreload-p nil)

;;;###autoload
(defun +dir-locals-toggle-autoreload (&optional enable)
  "Toggle autoloading dir-local variables after editing the \".dir-locals\" file.

If ENABLE is non-nil, force enabling autoreloading."
  (interactive)
  (if (or enable (not +dir-locals--autoreload-p))
      (progn
        (remove-hook 'after-save-hook #'+dir-locals--autoreload-h)
        (setq +dir-locals--autoreload-p nil)
        (message "Disabled auto-reloading directory-locals."))
    (add-hook 'after-save-hook #'+dir-locals--autoreload-h)
    (setq +dir-locals--autoreload-p t)
    (message "Enabled auto-reloading directory-locals.")))

;;;###autoload
(defun +dir-locals-open-or-create ()
  "Open or create the dir-locals.el for the current project."
  (interactive)
  (let* ((file-name (or buffer-file-name default-directory))
         (base-dir (car (ensure-list (dir-locals-find-file file-name)))))
    (find-file (expand-file-name
                dir-locals-file
                (or (expand-file-name dir-locals-file base-dir)
                    (+project-safe-root)
                    (file-name-directory file-name))))))



;;; Misc Emacs tweaks

;; Adapted from: https://github.com/rougier/nano-emacs
;;;###autoload
(defun +what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(defcustom +screenshot-delay 5
  "A delay to wait before taking the screenshot.
Applicable only when calling `+screenshot-svg' with a prefix."
  :group 'minemacs-utils
  :type 'number)

;; Inspired by: https://reddit.com/r/emacs/comments/idz35e/comment/g2c2c6y
;;;###autoload
(defun +screenshot-svg (outfile)
  "Save a screenshot of the current frame as an SVG image to OUTFILE.

If launched with a prefix or universal argument, it waits for a moment (defined
by `+screenshot-delay') before taking the screenshot."
  (interactive "FSave to file: ")
  (let ((outfile (file-name-with-extension outfile "svg")))
    (if current-prefix-arg
        (run-with-timer +screenshot-delay nil (apply-partially #'+screenshot-svg--take-screenshot outfile))
      (+screenshot-svg--take-screenshot outfile))))

(defun +screenshot-svg--take-screenshot (&optional outfile)
  "Save a SVG screenshot of the current frame to OUTFILE."
  (let* ((tmp-file (make-temp-file "emacs-" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file tmp-file (insert data))
    (when (stringp outfile) (copy-file tmp-file outfile))
    (message "Screenshot saved to %s" (or outfile tmp-file))))

;; Kill the minibuffer even when in another windown.
;; Adapted from: https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
;;;###autoload
(defun +minibuffer-kill-minibuffer ()
  "Kill the minibuffer when switching to window with mouse."
  (interactive)
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

;;;###autoload
(defun +region-or-thing-at-point (&optional leave-region-marked)
  "Return the region or the thing at point.

If LEAVE-REGION-MARKED is no-nil, don't call `desactivate-mark'
when a region is selected."
  (when-let* ((thing (ignore-errors
                       (or (prog1 (thing-at-point 'region t)
                             (unless leave-region-marked (deactivate-mark)))
                           (cl-some (+apply-partially-right #'thing-at-point t)
                                    '(symbol email number string word))))))
    ;; If the matching thing has multi-lines, join them
    (string-join (string-lines thing))))

;;;###autoload
(defun +insert-thing-at-point ()
  "Insert region or symbol in the minibuffer."
  (interactive)
  (insert (with-current-buffer (window-buffer (minibuffer-selected-window))
            (or (+region-or-thing-at-point t) ""))))

;;;###autoload
(defun +kill-region-or-backward-word ()
  "Kill selected region if region is active. Otherwise kill a backward word."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

;;;###autoload
(defun +kill-whitespace-or-word (arg)
  "Kill forward whitespace or word.
With argument ARG, do this that many times.
Restricts the effect of `kill-word' to the current line."
  (interactive "p")
  (if (looking-at-p "[ \t\n]")
      (let ((pt (point)))
        (re-search-forward "[^ \t\n]" nil :no-error)
        (backward-char)
        (kill-region pt (point)))
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (kill-word arg)
      (widen))))

;;;###autoload
(defun +backward-kill-whitespace-or-word (arg)
  "Kill backward whitespace or word.
With argument ARG, do this that many times.
Restricts the effect of `backward-kill-word' to the current line."
  (interactive "p")
  (if (save-excursion (backward-char) (looking-at-p "[ \t\n]"))
      (let ((pt (point)))
        (re-search-backward "[^ \t\n]" nil :no-error)
        (forward-char)
        (kill-region pt (point)))
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (backward-kill-word arg)
      (widen))))

;; Inspired by Doom Emacs
;;;###autoload
(defun +set-indent-width (width)
  "Change the indentation size to WIDTH of the current buffer.

The effectiveness of this command is significantly improved if
you have `editorconfig' or `dtrt-indent' installed."
  (interactive
   (list (if (integerp current-prefix-arg)
             current-prefix-arg
           (read-number "New indent size: "))))
  (setq tab-width width)
  (setq-local standard-indent width)
  (when (boundp 'evil-shift-width)
    (setq evil-shift-width width))
  (cond ((and (require 'editorconfig nil t)
              (editorconfig-find-current-editorconfig))
         (let (editorconfig-lisp-use-default-indent)
           (editorconfig-set-indentation nil width)))
        ((require 'dtrt-indent nil t)
         (with-demoted-errors "dtrt-indent: %s"
           (dtrt-indent-set width))))
  (message "Changed indentation to %d" width))

(defun +goto-line (n)
  "Go to line N, like `goto-line' but for Lisp code."
  (goto-char (point-min))
  (forward-line (1- n)))

;;;###autoload
(defun +autoload-region (beg end)
  "Add the ;;;###autoload to region (BEG . END)."
  (interactive "r")
  (cl-loop
   for line from (line-number-at-pos beg) to (line-number-at-pos end)
   do (progn (+goto-line line)
             (beginning-of-line)
             (unless (looking-at "^$")
               (insert ";;;###autoload")))))

(defvar +webjump-read-string-initial-query nil)

(defun +webjump-read-string-with-initial-query (prompt)
  "To be used as a replacement for `webjump-read-string', PROMPT."
  (let ((input (read-string (concat prompt ": ") +webjump-read-string-initial-query)))
    (unless (webjump-null-or-blank-string-p input) input)))

;;;###autoload
(defun +webjump ()
  "Like `webjump', with initial query filled from `+region-or-thing-at-point'."
  (interactive)
  (require 'webjump)
  (let ((+webjump-read-string-initial-query (+region-or-thing-at-point)))
    (cl-letf (((symbol-function 'webjump-read-string) #'+webjump-read-string-with-initial-query))
      (webjump))))



;;; Eglot extras

;; From: https://github.com/MaskRay/ccls/wiki/eglot#misc
;;;###autoload
(defun +eglot-ccls-inheritance-hierarchy (&optional derived)
  "Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
  (interactive "P")
  (if-let* ((res (jsonrpc-request
                  (eglot--current-server-or-lose)
                  :$ccls/inheritance
                  (append (eglot--TextDocumentPositionParams)
                          `(:derived ,(if derived t :json-false))
                          '(:levels 100) '(:hierarchy t))))
            (tree (list (cons 0 res))))
      (with-help-window "*ccls inheritance*"
        (with-current-buffer standard-output
          (while tree
            (pcase-let ((`(,depth . ,node) (pop tree)))
              (cl-destructuring-bind (&key uri range) (plist-get node :location)
                (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                (make-text-button
                 (+ (pos-bol 0) depth) (pos-eol 0)
                 'action (lambda (_arg)
                           (interactive)
                           (find-file (eglot-uri-to-path uri))
                           (goto-char (car (eglot-range-region range)))))
                (cl-loop for child across (plist-get node :children)
                         do (push (cons (1+ depth) child) tree)))))))
    (eglot--error "Hierarchy unavailable")))

;; Adapted from: Doom Emacs
;; https://github.com/doomemacs/doomemacs/blob/master/modules/tools/lsp/autoload/eglot.el
;;
;; HACK: Eglot removed `eglot-help-at-point' in joaotavora/eglot@a044dec for a
;; more problematic approach of deferred to eldoc. Here, I've restored it.
;; This handler try to open documentation in a separate window (so it can be
;; copied or kept open), but doing so with an eldoc buffer is difficult because
;; a) its contents are generated asynchronously, making them tough to scrape,
;; and b) their contents change frequently (every time you move your cursor).
(defvar +eglot--help-buffer nil)

;;;###autoload
(defun +eglot-help-at-point ()
  "Request documentation for the thing at point."
  (interactive)
  (eglot--dbind ((Hover) contents range)
      (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover (eglot--TextDocumentPositionParams))
    (let ((blurb (and (not (seq-empty-p contents)) (eglot--hover-info contents range)))
          (hint (thing-at-point 'symbol)))
      (if blurb
          (with-current-buffer (or (and (buffer-live-p +eglot--help-buffer) +eglot--help-buffer)
                                   (setq +eglot--help-buffer (generate-new-buffer "*eglot-help*")))
            (with-help-window (current-buffer)
              (rename-buffer (format "*eglot-help for %s*" hint))
              (with-current-buffer standard-output (insert blurb))
              (setq-local nobreak-char-display nil)))
        (display-local-help)))))

(defvar +shellcheck--current-error nil)

;;;###autoload
(defun +shellcheck-describe-error (&rest codes)
  "Describe a ShellCheck message CODES."
  (interactive)
  (let* ((wiki-dir (expand-file-name "shellcheck.wiki" minemacs-local-dir))
         (codes (seq-uniq
                 (sort
                  (or codes
                      (let* ((eglot-stay-out-of '(eldoc))
                             (eldoc-display-functions ; error code from eldoc
                              (list (lambda (docs _interactive)
                                      (setq +shellcheck--current-error
                                            (cl-loop for (docs . _rest) on docs
                                                     for (this-doc . _plist) = docs
                                                     collect (and (string-match "\\(SC[[:digit:]]\\{4\\}\\)" this-doc)
                                                                  (match-string 1 this-doc))))))))
                        (eldoc-print-current-symbol-info t)
                        +shellcheck--current-error)
                      (completing-read-multiple "Enter shellcheck error code: " ; prompt for error code
                                                (mapcar #'file-name-base
                                                        (and (file-directory-p wiki-dir)
                                                             (directory-files wiki-dir nil "SC[[:digit:]]\\{4\\}\\.md")))
                                                nil t "SC")))))
         (wiki-files (mapcar (lambda (code) (expand-file-name (file-name-with-extension code "md") wiki-dir)) codes))
         (desc-buf (format "*shellcheck:%s*" (string-join codes ","))))
    (unless (file-directory-p wiki-dir)
      (vc-git-clone "https://github.com/koalaman/shellcheck.wiki.git" wiki-dir nil))
    (if (not (cl-some #'file-exists-p wiki-files))
        (user-error "No description found for %s" codes)
      (unless (buffer-live-p (get-buffer desc-buf))
        (with-current-buffer (get-buffer-create desc-buf)
          (dolist (file wiki-files)
            (when (file-exists-p file)
              (insert-file-contents file)
              (goto-char (point-max))
              (insert "\n---\n")))
          (cond ((fboundp 'markdown-view-mode)
                 (markdown-view-mode))
                ((fboundp 'markdown-ts-mode)
                 (markdown-ts-mode)
                 (view-mode 1))
                (t (view-mode 1)))))
      (pop-to-buffer desc-buf))))

;;;###autoload
(defun +explainshell (command)
  "Get an explanation of the shell COMMAND from explainshell.com."
  (interactive (list (read-shell-command "Enter a command: " (when (region-active-p) (buffer-substring (region-beginning) (region-end))))))
  (browse-url (url-encode-url (format "https://explainshell.com/explain?cmd=%s" (string-join (string-split command nil t) "+")))))

(defun +fetch-json-from-url (url)
  "Get an Emacs JSON object from a specified URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move) ; Move to the end of the headers
    (prog1 (json-read) (kill-buffer (current-buffer)))))

(declare-function string-remove-prefix "subr-x")

(defun +json-schemas-for-file (filename)
  "Get a list of JSON Schemas that apply to FILENAME."
  (and filename
       (seq-filter
        (lambda (spec)
          (seq-find (lambda (pattern)
                      (string-match-p
                       (string-remove-prefix "\\`" (wildcard-to-regexp pattern))
                       (expand-file-name filename)))
                    (alist-get 'fileMatch spec)))
        (alist-get 'schemas (+json-schemas-catalog)))))

(defvar +json-schemas-catalog-cache (+deserialize-sym '+json-schemas-catalog-cache minemacs-cache-dir))

(defun +json-schemas-catalog (&optional refresh)
  "Get the catalog of schemas from JSON Schemas Store.
When REFRESH is non-nil, don't use the cached version and force
reloading the JSON file."
  (when-let* ((catalog (or (and (not refresh) +json-schemas-catalog-cache)
                           (+fetch-json-from-url "https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/api/json/catalog.json"))))
    (setq +json-schemas-catalog-cache catalog)
    (unless (equal (+deserialize-sym '+json-schemas-catalog-cache minemacs-cache-dir) +json-schemas-catalog-cache)
      (+serialize-sym '+json-schemas-catalog-cache minemacs-cache-dir))
    catalog))

;;;###autoload
(defun +yaml-insert-schema (&optional ask)
  "Insert a schema for the current buffer file.
When ASK is non-nil, ask which schema to insert without trying to guess
the schema from the file name."
  (interactive "P")
  (when-let* ((catalog (+json-schemas-catalog))
              (schemas (alist-get 'schemas catalog))
              (name (or (and (not ask) (alist-get 'name (car (+json-schemas-for-file (buffer-file-name)))))
                        (completing-read "Choose a JSON schema: " (mapcar (apply-partially #'alist-get 'name) schemas))))
              (url (alist-get 'url (seq-find (lambda (a) (equal (alist-get 'name a) name)) schemas))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (insert "# yaml-language-server: $schema=" url "\n\n")))))



;;; Emacs server

;;;###autoload
(defun +server-restart ()
  "Restart the Emacs server."
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))



;;; Binary files tweaks

;; A predicate for detecting binary files. Inspired by:
;; https://emacs.stackexchange.com/q/10277/37002
;;;###autoload
(defun +binary-buffer-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least one null byte.

Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (search-forward (string ?\x00) nil t 1))))

;;;###autoload
(defun +binary-file-p (file &optional chunk)
  "Is FILE a binary?

This checks the first CHUNK of bytes, defaults to 1024."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 (or chunk 1024))
    (+binary-buffer-p)))

;;;###autoload
(defun +binary-hexl-buffer-p (&optional buffer)
  "Does BUFFER (defaults to the current buffer) should be viewed using `hexl-mode'."
  (and +binary-hexl-enable
       (+binary-buffer-p buffer)
       (not (and (fboundp 'objdump-recognizable-buffer-p)
                 ;; Executables are viewed with objdump mode
                 (objdump-recognizable-buffer-p buffer)))))

;;;###autoload
(defun +binary-hexl-mode-maybe ()
  "Activate `hexl-mode' if relevant for the current buffer."
  (interactive)
  (when (and (not (eq major-mode 'hexl-mode)) (+binary-hexl-buffer-p))
    (hexl-mode 1)))



;;; Buffer related tweaks

;; From: https://emacswiki.org/emacs/download/misc-cmds.el
;; Candidate as a replacement for `kill-buffer', at least when used interactively.
;; For example: (define-key global-map [remap kill-buffer] 'kill-buffer-and-its-windows)
;; We cannot just redefine `kill-buffer', because some programs count on a
;; specific other buffer taking the place of the killed buffer (in the window).
;;;###autoload
(defun +kill-buffer-and-its-windows (buffer &optional msgp)
  "Kill BUFFER and delete its windows.
Default is `current-buffer'. When MSGP is non-nil, signal an error when
the buffer isn't alive. BUFFER may be either a buffer or its name (a
string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) t) t))
  (setq buffer (get-buffer buffer))
  (if (buffer-live-p buffer) ; Kill live buffer only.
      (let ((wins (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (kill-buffer buffer) ; Only delete windows if buffer killed.
          (dolist (win wins) ; (User might keep buffer if modified.)
            (when (window-live-p win)
              ;; Ignore error, in particular,
              ;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-window win) (error nil))))))
    (when msgp (user-error "Cannot kill buffer. Not a live buffer: `%s'" buffer))))

;; From: https://emacswiki.org/emacs/download/misc-cmds.el
;;;###autoload
(defun +region-to-buffer (start end buffer arg)
  "Copy region to BUFFER: At beginning (prefix >= 0), end (< 0), or replace.
START and END are the region boundaries.
BUFFER is a buffer or its name (a string).
With prefix ARG >= 0: `append-to-buffer':
  Append contents of region to end of BUFFER.
  (Point is moved to end of BUFFER first.)
With prefix ARG < 0:  `prepend-to-buffer':
  Prepend contents of region to beginning of BUFFER.
  (Point is moved to beginning of BUFFER first.)
With no prefix ARG (nil): `copy-to-buffer'.
  Write region to BUFFER, replacing any previous contents."
  (interactive
   (let ((arg (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
     (list (region-beginning)
           (region-end)
           (read-buffer
            (concat (if arg
                        (if (natnump arg) "Append" "Prepend")
                      "Write")
                    " region to buffer: ")
            (if (fboundp 'another-buffer) ; Defined in `misc-fns.el'.
                (another-buffer nil t)
              (other-buffer (current-buffer))))
           arg)))
  (setq buffer (get-buffer-create buffer)) ; Convert to buffer.
  (when (eq buffer (current-buffer)) (error "Cannot copy region to its own buffer"))
  (cond ((natnump arg)
         (with-current-buffer buffer (goto-char (point-max)))
         (append-to-buffer buffer start end))
        (arg
         (with-current-buffer buffer (goto-char (point-min)))
         (prepend-to-buffer buffer start end))
        (t (copy-to-buffer buffer start end))))

;; From: https://emacswiki.org/emacs/download/misc-cmds.el
;;;###autoload
(defun +region-to-file (start end filename arg)
  "With prefix arg, this is `append-to-file'.  Without, it is `write-region'.
START and END are the region boundaries.
Prefix ARG non-nil means append region to end of file FILENAME.
Prefix ARG nil means write region to FILENAME, replacing contents."
  (interactive
   (list (region-beginning)
         (region-end)
         (read-file-name (concat (if current-prefix-arg "Append" "Write")
                                 " region to file: "))
         current-prefix-arg))
  (let* ((curr-file (buffer-file-name))
         (same-file-p (and curr-file (string= curr-file filename))))
    (cond ((or (not same-file-p)
               (progn (when (fboundp 'flash-ding) (flash-ding))
                      (yes-or-no-p
                       (format
                        "Do you really want to REPLACE the contents of `%s' by just the REGION? "
                        (file-name-nondirectory curr-file)))))
           (write-region start end filename arg)
           (when same-file-p (revert-buffer t t)))
          (t (message "OK.  Not written.")))))

;;;###autoload
(defun +kill-some-buffers (&optional list)
  "Kill some buffers.  Asks the user whether to kill the modified ones.
Non-interactively, if optional argument LIST is non-nil, it
specifies the list of buffers to kill, asking for approval for each one.
See `kill-some-buffers'."
  (interactive)
  ;; Replace the `kill-buffer-ask' locally (used by `kill-some-buffers')
  ;; with our function which don't ask about unmodified buffers.
  (cl-letf (((symbol-function 'kill-buffer-ask) #'+kill-buffer-ask-if-modified))
    (kill-some-buffers list)))

(defcustom +kill-buffer-no-ask-list
  (list (or (bound-and-true-p messages-buffer-name) "*Messages*") "*Warnings*")
  "A list of buffer names to be killed without confirmation."
  :group 'minemacs-buffer
  :type '(repeat string))

(with-eval-after-load 'comp
  (when (featurep 'native-compile)
    (cl-callf append +kill-buffer-no-ask-list
      (ensure-list (bound-and-true-p comp-async-buffer-name))
      (ensure-list (bound-and-true-p comp-log-buffer-name)))))

;;;###autoload
(defun +kill-buffer-ask-if-modified (buffer)
  "Like `kill-buffer-ask', but don't ask if BUFFER isn't modified.
Kill without asking for buffer names in `+kill-buffer-no-ask-list'."
  (when (or (not (buffer-modified-p buffer))
            (member (buffer-name buffer) +kill-buffer-no-ask-list)
            (yes-or-no-p (format "Buffer %s HAS BEEN MODIFIED.  Kill? "
                                 (buffer-name buffer))))
    (kill-buffer buffer)))

;; From: https://emacswiki.org/emacs/download/misc-cmds.el
;;;###autoload
(defun +delete-extra-windows-for-buffer ()
  "Delete all other windows showing the selected window's buffer."
  (interactive)
  (let* ((selwin (selected-window))
         (buf (window-buffer selwin)))
    (walk-windows
     (lambda (ww)
       (unless (eq ww selwin)
         (when (eq (window-buffer ww) buf)
           (delete-window ww))))
     'NO-MINI 'THIS-FRAME)))

;; From: https://emacswiki.org/emacs/download/misc-cmds.el
;;;###autoload
(defun +delete-window-maybe-kill-buffer ()
  "Delete selected window.
If no other window shows its buffer, kill the buffer too."
  (interactive)
  (let* ((selwin (selected-window))
         (buf (window-buffer selwin)))
    (delete-window selwin)
    (unless (get-buffer-window buf 'visible) (kill-buffer buf))))

;;;###autoload
(defun +replace-in-buffer (old new)
  "Replace OLD with NEW in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (matches 0))
      (while (re-search-forward old nil t)
        (replace-match new)
        (cl-incf matches))
      matches)))

;;;###autoload
(defun +clear-frenchy-ponctuations ()
  "Replace french ponctuations (like unsectable space) by regular ones."
  (interactive)
  (let ((chars
         '(("[\u00a0\u200b]" . "") ;; Non-breaking and zero-width spaces
           ;; Special spaces and quads
           ("[\u2000-\u200A\u202F\u205F\u3000]" . " ")
           ("[‘’‚’]" . "'")
           ("[“”„”«»]" . "\"")))
        (matches 0))
    (dolist (pair chars)
      (cl-incf matches (+replace-in-buffer (car pair) (cdr pair))))
    (message "Replaced %d match%s." matches (if (> matches 1) "es" ""))))

;; https://emacs.stackexchange.com/a/13549
;;;###autoload
(defun +save-buffer-preserving-modtime ()
  "Call `save-buffer', but keep the visited file's modtime the same."
  (interactive)
  (let ((original-time (visited-file-modtime)))
    (save-buffer)
    (set-file-times buffer-file-name original-time)
    (set-visited-file-modtime original-time)))

;;;###autoload
(defun +kill-region-as-paragraph ()
  "Kill (copy) region as one paragraph.
This command removes new line characters between lines."
  (interactive)
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (re-search-forward "\n[^\n]" nil t)
            (replace-region-contents
             (- (point) 2) (- (point) 1)
             (lambda (&optional _a _b) " ")))
          (kill-new (buffer-string)))))
    (deactivate-mark)))

;;;###autoload
(defun +first-line-empty-p ()
  "Return t when the first line of the buffer is empty."
  (save-excursion (goto-char (point-min))
                  (and (bolp) (eolp))))



;;; Project tweaks

;;;###autoload
(defun +project-forget-zombie-projects ()
  "Forget all known projects that don't exist any more.

Like `project-forget-zombie-projects', but handles remote projects differently,
it forget them only when we are sure they don't exist."
  (interactive)
  (dolist (proj (project-known-project-roots))
    (unless (or (and (file-remote-p proj nil t) (file-readable-p proj)) ; Connected remote + existent project
                (file-remote-p proj) ; Non connected remote project
                (file-exists-p proj)) ; Existent local project
      (project-forget-project proj))))

;;;###autoload
(defun +project-gdb ()
  "Invoke `gdb' in the project's root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'gdb)))

;;;###autoload
(defun +project-list-cleanup ()
  "Forget all duplicate known projects (like /home/user/proj and ~/proj)."
  (interactive)
  (let* ((projs (mapcar #'expand-file-name (project-known-project-roots)))
         (projs-dups (cl-set-difference projs (cl-remove-duplicates projs :test #'string=))))
    (mapc #'project-forget-project projs-dups)
    (+project-forget-zombie-projects)
    (dolist (proj (reverse projs))
      (let ((proj-abbrev (abbreviate-file-name proj)))
        (unless (string= proj proj-abbrev)
          (+shutup!
           (project-forget-project proj)
           (project-remember-project (project-current nil proj-abbrev))))))))

;;;###autoload
(defvar +project-root-wildcards '("~/Projects/*/*"))

;; Inspired by: https://mastodon.catgirl.cloud/@mekeor/113251381004866734
;;;###autoload
(defun +project-root-initialize ()
  "Initialize project list from `+project-root-wildcards'."
  (interactive)
  ;; Forget non-existent projects
  (mapc
   (lambda (pr)
     (or (file-remote-p pr) ; Don't forget remote projects
         (file-directory-p pr)
         (project-forget-project pr)))
   (project-known-project-roots))
  ;; Remember some Git repositories as projects by path
  (mapc
   (lambda (dir)
     (when-let* ((project (project--find-in-directory dir)))
       (project-remember-project project)))
   (seq-filter #'file-directory-p (mapcan #'file-expand-wildcards +project-root-wildcards))))



;;; Xref

;; TWEAK: When working on big C/C++ codebases (using `citre-mode'), the
;; `xref--read-identifier' takes forever to load the list of identifiers to
;; choose from! Consequently, calling `xref-find-references' freezes Emacs and
;; don't return the references. This command disable prompting for identifiers
;; locally and call `xref-find-references' with the indentifier at point.
;;;###autoload
(defun +xref-find-references-at-point ()
  "Find references to the identifier at or around point."
  (interactive)
  (if-let* ((identifier (save-excursion (xref-backend-identifier-at-point (xref-find-backend))))
            (xref-prompt-for-identifier '(not xref-find-references)))
      (xref-find-references identifier)
    (user-error "No identifier here")))



;;;###autoload
(defun minemacs-extract-packages-descriptions ()
  "Extract the descriptions of MinEmacs packages."
  (interactive)
  (when-let* ((buff (get-buffer "*minemacs-modules-pkg-desc*")))
    (kill-buffer buff))
  (let ((doc-buff (get-buffer-create "*minemacs-modules-pkg-desc*"))
        (on-demand-heading t))
    (with-current-buffer doc-buff (insert "# MinEmacs modules and packages\n"))
    (dolist (module (seq-sort #'string< (mapcar #'symbol-name (minemacs-modules t))))
      (when (and on-demand-heading (string-prefix-p "on-demand/" module))
        (setq on-demand-heading nil)
        (with-current-buffer doc-buff (insert "\n# MinEmacs on-demand modules and packages\n")))
      (with-current-buffer doc-buff (insert (format "## `%s`\n" module)))
      (with-temp-buffer
        (delete-region (point-min) (point-max))
        (insert-file-contents (format "%s%s.el" minemacs-modules-dir module))
        (goto-char (point-min))
        (while (search-forward-regexp "^;; \\(.*\\)\n(use-package[[:space:]]*\\([^[:space:]]*\\)$" nil :no-error)
          (let ((desc (match-string 1))
                (pkg (match-string 2)))
            (setq desc (replace-regexp-in-string "`\\([^']*\\)'" "`\\1`" desc))
            (with-current-buffer doc-buff (insert (format "* `%s`: %s\n" pkg desc))))))
      (with-current-buffer doc-buff (insert "\n")))
    (with-current-buffer doc-buff
      (markdown-mode)
      (view-mode)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun +list-external-dependencies ()
  "Show the list of declared external dependencies."
  (interactive)
  (require 'me-external-tools)
  (with-current-buffer (get-buffer-create "*external-dependencies*")
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert "# External Tools
To get the maximum out of this configuration, you would need to install some
external tools, either in your development machine, docker, remote host, etc.
The presence of these programs isn't mandatory, however, for better experience,
you might need install some of these tools.\n\n")
    (let ((counter 0))
      (dolist (dep minemacs-external-dependencies)
        (insert (format "%d. [%s](%s) - %s\n"
                        (cl-incf counter)
                        (string-join (mapcar (apply-partially #'format "`%s`")
                                             (ensure-list (plist-get dep :tool)))
                                     ", ")
                        (plist-get dep :link)
                        (plist-get dep :desc)))))
    (markdown-mode)
    (read-only-mode 1)
    (pop-to-buffer (current-buffer))))




(provide 'me-lib-extra)
;;; me-lib-extra.el ends here
