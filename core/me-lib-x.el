;; me-lib-x.el -- MinEmacs Library (extra features and commands) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-05-20
;; Last modified: 2025-08-26

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
    (message "Running `%s'" fn)
    (condition-case err
        (if dont-ask-p ; Do not ask before installing
            (cl-letf (((symbol-function 'yes-or-no-p) #'always)
                      ((symbol-function 'y-or-n-p) #'always))
              (funcall-interactively fn))
          (funcall-interactively fn))
      (error (message "While running `%S' got: \"%s\", skipping" fn (error-message-string err))))))

;;;###autoload
(defun minemacs-root-dir-cleanup ()
  "Cleanup MinEmacs' root directory."
  (let ((default-directory minemacs-root-dir))
    (mapc (+apply-partially-right #'+delete-file-or-directory 'trash 'recursive)
          (directory-files minemacs-root-dir nil (rx (seq bol (or "eln-cache" "auto-save-list" "elpa") eol))))))

;;;###autoload
(defun minemacs-cleanup-emacs-directory ()
  "Cleanup unwanted files/directories from MinEmacs' directory."
  (interactive)
  (+shutup!
   (when (featurep 'native-compile) (native-compile-prune-cache))
   (when (fboundp '+straight-prune-build-cache) (+straight-prune-build-cache))
   (minemacs-root-dir-cleanup))
  (message "Finished cleanup!"))

;;;###autoload
(defun minemacs-user-config (ask)
  "Open MinEmacs user configuration.

When ASK is non-nil (\\[universal-argument]), ask about which file to open."
  (interactive "P")
  (if ask
      (find-file (read-file-name "Select which file to open: " minemacs-config-dir))
    (dired minemacs-config-dir)))



;;; Files, directories and IO helper functions

;;;###autoload
(defun +clean-file-name (filename &optional downcase-p)
  "Clean FILENAME, optionally convert to DOWNCASE-P."
  (replace-regexp-in-string ; Clean slashes, backslashes, ":", ";", spaces, and tabs
   "[:;\t\n\r /\\_]+" "-"
   (replace-regexp-in-string
    "[‘’‚“”„\"`'()&]+" ""
    (if downcase-p (downcase filename) filename))))

;;;###autoload
(defun +file-name-incremental (filename)
  "Return a unique file name for FILENAME.
If \"file.ext\" exists, returns \"file-0.ext\"."
  (let* ((directory (file-name-directory filename))
         (basename (file-name-base filename))
         (extension (file-name-extension filename t))
         (candidate filename)
         (counter 0))
    (while (file-exists-p candidate)
      (setq counter (1+ counter))
      (setq candidate (concat directory basename "-" (number-to-string counter) extension)))
    candidate))

;;;###autoload
(defun +delete-this-file (&optional path force-p)
  "Delete PATH.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer)) current-prefix-arg))
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
(defun +delete-current-file-and-buffer (&optional filename)
  "Delete FILENAME and its associated visiting buffer."
  (interactive)
  (when-let* ((filename (or filename (buffer-file-name)))
              (short-path (abbreviate-file-name filename)))
    (if (vc-backend filename)
        (or (condition-case nil (progn (vc-delete-file (buffer-file-name)) t) (error nil))
            (and (condition-case nil (progn (+delete-this-file filename) t) (error nil))
                 (kill-buffer)))
      (when (y-or-n-p (format "Are you sure you want to delete %s? " short-path))
        (delete-file filename delete-by-moving-to-trash)
        (message "Deleted file %s" short-path)
        (kill-buffer)))))

;;;###autoload
(defun +copy-current-file-name ()
  "Save (copy) the file name of this buffer to the kill ring."
  (interactive)
  (if-let* ((filename (buffer-file-name)))
      (with-temp-buffer
        (insert filename)
        (kill-ring-save (point-min) (point-max)))
    (user-error "This buffer is not visiting a file")))

(defvar +apply-patch-dwim-proj-dir nil)
(defvar +apply-patch-dwim-extra-options '("--ignore-whitespace"))
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
                   (not (= last-pos (point))) ; To detect the last hunk
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
                   ;; TODO: "--silent" instead of "--force" might be interesting!
                   :command `("patch" "-p1" "--force" ,@+apply-patch-dwim-extra-options "-i" ,patch-file)
                   :sentinel (lambda (proc _event)
                               (unless (process-live-p proc)
                                 (if (zerop (process-exit-status proc))
                                     (when (y-or-n-p (format "Patch applied successfylly, open target directory %S?" target-dir))
                                       (dired target-dir))
                                   (pop-to-buffer out-buf))))))
                (view-mode 1))
              (run-hook-with-args '+apply-patch-dwim-post-patch-functions patch-buf patch-files target-dir))))))))



;;; Exporter and converters

(defcustom +html2pdf-default-backend
  (cond ((executable-find "wkhtmltopdf") 'wkhtmltopdf)
        ((executable-find "htmldoc") 'htmldoc)
        ((executable-find "weasyprint") 'weasyprint)
        ((and (require 'browse-url) (executable-find browse-url-chromium-program)) 'chromium)
        ((and (executable-find "pandoc") (executable-find "context")) 'pandoc+context)
        ((executable-find "pandoc") 'pandoc))
  "The default backend to convert HTML files to PDFs in `+html2pdf'."
  :group 'minemacs-utils
  :type '(choice
          (const wkhtmltopdf)
          (const chromium)
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
               ('chromium
                (list browse-url-chromium-program
                      "--headless" "--disable-gpu" "--no-pdf-header-footer"
                      (format "--print-to-pdf=%s" outfile) infile))
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

(defcustom +monolith-program "monolith"
  "The executable for \"monolith\" which is used archive HTML pages."
  :type 'string
  :group 'minemacs-utils)

;;;###autoload
(defun +save-url-to-html-file (url out-file)
  "Save URL into OUT-FILE as a standalone HTML file."
  (interactive
   (let ((url (or (thing-at-point 'url) (read-string "URL to save: "))))
     (list url (read-file-name "Save to: " nil nil nil (url-filename (url-generic-parse-url url))))))
  (if (executable-find +monolith-program)
      (make-process
       :name "monolith" :buffer "*monolith*"
       :command (list +monolith-program url "-o" (expand-file-name out-file)))
    (user-error "Please set `+monolith-program' accordingly")))

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
  (and (buffer-live-p +serial-buffer) (process-live-p +serial-process)))

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
    (apply #'+serial--run-commands `(,port ,baud ,@(ensure-list commands)))))



;;; Networking

(defvar +net-default-device nil
  "Default network interface, like \"wlan0\" or \"wlp0s20f3\".
Set to nil to use the first interface reported by
`network-interface-list'.")

;;;###autoload
(defun +net-get-ip-address (&optional dev)
  "Get the IP-address for device DEV of the current machine."
  (when-let* ((dev (or dev +net-default-device (caar (network-interface-list)))))
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
(cl-defun +github-download-release (repo filename-regexp &optional ok-if-already-exists &key ver out-file prerelease)
  "Download release from REPO.

If FILENAME-REGEXP is a string, use it as a regexp to match against the
file name.

When OK-IF-ALREADY-EXISTS is non-nil, the file gets overwritten if it
already exists.

Keyword argument :VER can be used to pass the version to download, when
no version is passed, the latest release is downloaded. The :OUT-FILE
can be used to choose the output file path, otherwise, the file will be
downloaded with the original file name to `+github-download-dir'. If a
non-nil value is provided for :PRERELEASE, we download the latest
prerelease if no :VER is provided."
  (when-let* ((releases (ignore-errors
                          (with-temp-buffer
                            (+shutup!
                             (url-insert-file-contents
                              (format "https://api.github.com/repos/%s/releases" repo)))
                            (json-parse-buffer :object-type 'plist))))
              (release (if ver
                           (seq-find (lambda (rel) (equal ver (plist-get rel :tag_name))) releases)
                         (seq-first (if prerelease releases (seq-filter (lambda (rel) (eq :false (plist-get rel :prerelease))) releases)))))
              (url (seq-find (apply-partially #'string-match-p filename-regexp)
                             (mapcar (+apply-partially-right #'plist-get :browser_download_url) (plist-get release :assets))))
              (out-file (or out-file (expand-file-name (url-file-nondirectory url) +github-download-dir))))
    (if (and (not ok-if-already-exists) (file-exists-p out-file))
        (+log! "File %S already exist" out-file)
      (mkdir (file-name-directory out-file) t)
      (+shutup! (url-copy-file url out-file ok-if-already-exists)))
    out-file))



;;; Directory local tweaks & hacks

(defvar +dir-locals-enable-local-vars :all
  "A value to use for `enable-local-variables'.")

;;;###autoload
(defun +dir-locals-reload-for-this-buffer ()
  "Reload directory-local for the current buffer."
  (interactive)
  (let ((enable-local-variables +dir-locals-enable-local-vars))
    (hack-dir-local-variables-non-file-buffer)
    (+info! "Reloaded directory-local variables for buffer %S" (current-buffer))))

;;;###autoload
(defun +dir-locals-reload-for-all-buffers-in-this-directory ()
  "Reload dir-locals for all buffers under `default-directory'."
  (interactive)
  (let ((dir-locals-dir (car (ensure-list (dir-locals-find-file default-directory)))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let* ((file (or buffer-file-name default-directory))
                    ((equal (car (ensure-list (dir-locals-find-file file))) dir-locals-dir)))
          (+dir-locals-reload-for-this-buffer))))))

(defun +dir-locals-autoreload ()
  "Is it relevant to auto reload dir-locals for his buffer."
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (+dir-locals-reload-for-all-buffers-in-this-directory)
    (+log! "Reloaded directory-local variables defined in %s to buffer %S" dir-locals-file (current-buffer))))

;;;###autoload
(define-minor-mode +dir-locals-autoreload-mode
  "Autoload buffers affected by editing the current `dir-locals-file'."
  :global t
  (if +dir-locals-autoreload-mode
      (add-hook 'after-save-hook #'+dir-locals-autoreload)
    (remove-hook 'after-save-hook #'+dir-locals-autoreload)))



;;; Misc Emacs tweaks

(autoload 'ansi-color-apply-on-region "ansi-color")

;;;###autoload
(defun +ansi-color-apply-on-buffer ()
  "Decode and apply ANSI color chars in the curernt buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;;;###autoload
(defun +completion-mark-category (seq category)
  "Mark SEQ as being in CATEGORY for use with `completing-read'."
  (lambda (str pred flag)
    (pcase flag
      ('metadata
       `(metadata (category . ,category)))
      (_
       (all-completions str seq pred)))))

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

;;;###autoload
(defun +bookmark-set-at-mouse (event)
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (bookmark-set
     (truncate-string-to-width
      (format "%s: %s" (buffer-name) (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      60 nil nil t))))



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
                  `(,@(eglot--TextDocumentPositionParams) :derived ,(if derived t :json-false) :levels 100 :hierarchy t)))
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
(defvar-local +eglot--help-buffer nil)

;;;###autoload
(defun +eglot-help-at-point ()
  "Request documentation for the thing at point."
  (interactive)
  (eglot--dbind ((Hover) contents range)
      (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover (eglot--TextDocumentPositionParams))
    (let ((blurb (and (not (seq-empty-p contents)) (eglot--hover-info contents range)))
          (hint (thing-at-point 'symbol)))
      (if blurb
          (with-current-buffer (or (buffer-live-p +eglot--help-buffer)
                                   (setq +eglot--help-buffer (generate-new-buffer "*eglot-help*")))
            (with-help-window (current-buffer)
              (rename-buffer (format "*eglot-help for %s*" hint))
              (with-current-buffer standard-output (insert blurb))
              (setq-local nobreak-char-display nil)))
        (display-local-help)))))

(defvar +shellcheck--current-error nil)
(defvar +shellcheck--buffer nil)

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
         (wiki-files (mapcar (lambda (code) (expand-file-name (file-name-with-extension code "md") wiki-dir)) codes)))
    (unless (file-directory-p wiki-dir)
      (vc-git-clone "https://github.com/koalaman/shellcheck.wiki.git" wiki-dir nil))
    (if (not (cl-some #'file-exists-p wiki-files))
        (user-error "No description found for %s" codes)
      (unless (buffer-live-p +shellcheck--buffer)
        (setq +shellcheck--buffer (get-buffer-create "*shellcheck:describe*")))
      (with-current-buffer +shellcheck--buffer
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
              (t (view-mode 1))))
      (pop-to-buffer +shellcheck--buffer))))

;;;###autoload
(defun +explainshell (command)
  "Get an explanation of the shell COMMAND from explainshell.com."
  (interactive (list (read-shell-command "Enter a command: " (when (region-active-p) (buffer-substring (region-beginning) (region-end))))))
  (browse-url (url-encode-url (format "https://explainshell.com/explain?cmd=%s" (string-join (string-split command nil t) "+")))))

(defun +fetch-json-from-url (url)
  "Get an Emacs JSON object from a specified URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char url-http-end-of-headers)
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
(defun +insert-schema (&optional ask)
  "Insert a schema for the current buffer's file (YAML or TOML).
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
        (cond ((derived-mode-p '(yaml-mode yaml-ts-mode))
               (insert "# yaml-language-server: $schema=" url "\n\n"))
              ((derived-mode-p '(toml-mode toml-ts-mode conf-toml-mode))
               (insert "\"$schema\" = '" url "'\n\n")))))))

;;;###autoload
(defun +clang-format-config-file (&optional dir)
  (when-let* ((config-dir (locate-dominating-file (or dir default-directory) ".clang-format")))
    (expand-file-name ".clang-format" config-dir)))

(defvar +clang-format-mode-alist
  '(((c-ts-mode) "c" c-ts-mode-indent-offset)
    ((c++-ts-mode) "cpp" c-ts-mode-indent-offset)
    ((js-mode js-ts-mode) "js" js-indent-level)
    ((typescript-ts-base-mode) "ts" typescript-ts-mode-indent-offset)
    ((csharp-mode csharp-ts-mode) "cs" csharp-ts-mode-indent-offset)
    ((protobuf-ts-mode) "proto" protobuf-ts-mode-indent-offset)
    ((java-ts-mode) "java" java-ts-mode-indent-offset)
    ((java-mode) "java" c-basic-offset)
    ((c-mode cuda-mode opencl-c-mode) "c" c-basic-offset)
    ((c++-mode) "cpp" c-basic-offset)
    ((json-mode) "json" c-basic-offset)
    ((json-ts-mode) "json" json-ts-mode-indent-offset)
    ((verilog-mode) "v" verilog-indent-level)
    ((verilog-ts-mode) "v" verilog-ts-indent-level)))

(defun +clang-format-dump-config (&optional extension)
  "Dump config for the current buffer assuming a file with EXTENSION."
  (when-let* ((extension (or extension (car (+clang-format-get-lang)))))
    (with-temp-buffer
      (when (zerop (call-process +clang-format-command nil (current-buffer) nil (concat "--assume-filename=dummy." extension) "--dump-config"))
        (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun +clang-format-get-lang ()
  (alist-get major-mode +clang-format-mode-alist nil nil (+reverse-args #'provided-mode-derived-p)))

;; Helper function to get the style for "clang-format"
;;;###autoload
(defun +clang-format-get-style (&optional no-opt)
  "Get the \"-style=XXX\" argument for clang-format.

When NO-OPT isn non-nil, don't return the \"-style=\" part."
  (let ((lang (+clang-format-get-lang)))
    (concat (if no-opt "" "-style=")
            (if (and (+clang-format-config-file)
                     ;; In case of a missing config for the language or a malformed ".clang-format" file
                     (+clang-format-dump-config (car lang)))
                "file"
              (let ((indent-sym (cadr lang)))
                (format "{IndentWidth: %d, TabWidth: %d}"
                        (or (and indent-sym (symbol-value indent-sym)) standard-indent)
                        (or (and indent-sym (symbol-value indent-sym)) tab-width)))))))

;;;###autoload
(defun +editorconfig-guess-style-from-clang-format ()
  "Set some editor settings from \".clang-format\" when available."
  (interactive)
  (if-let* (((and (require 'yaml nil t)
                  (executable-find +clang-format-command)
                  (derived-mode-p (flatten-list (mapcar #'car +clang-format-mode-alist)))))
            (out (+clang-format-dump-config (car (+clang-format-get-lang))))
            (yaml-hash (yaml-parse-string out))
            (fc (gethash 'ColumnLimit yaml-hash))
            (iw (gethash 'IndentWidth yaml-hash))
            (tw (gethash 'TabWidth yaml-hash))
            (is (if (equal "never" (downcase (gethash 'UseTab yaml-hash))) "space" "tab")))
      (progn
        (setq fill-column (if (zerop fc) nil fc))
        (let ((hash (make-hash-table)))
          (puthash 'indent_style is hash)
          (puthash 'indent_size (number-to-string iw) hash)
          (puthash 'tab_width (number-to-string tw) hash)
          (editorconfig-set-local-variables hash))
        (when (called-interactively-p 'interactive)
          (message "Set fill-column=%s, tab-width=%s, indent-offset=%s and indent-style=%s" fc tw iw is)))
    (when (called-interactively-p 'interactive)
      (user-error "No applicable \".clang-format\" for buffer %S" (buffer-name)))))

(put '+editorconfig-guess-style-from-clang-format 'completion-predicate
     (lambda (_cmd buf)
       (with-current-buffer buf
         (and (+clang-format-get-lang) (+clang-format-config-file) (+clang-format-dump-config) t))))

(defvar +compile-commands-json-directories
  '(""
    "build"
    "build/release"
    "build/debug"
    "build/linux"
    "bld"
    "cmake.bld/Linux"
    "cmake-build"
    "cmake-build/release"
    "cmake-build/debug"
    "cmake-build/linux_64_static_ninja_Release"
    "cmake-build/linux_64_static_make_Release"
    "cmake-build/linux_64_static_ninja_Debug"
    "cmake-build/linux_64_static_make_Debug"))

;;;###autoload
(defun +compilation-db-find-file (&optional proj-root)
  (let ((proj-root (or proj-root (+project-safe-root))))
    (cl-find-if
     #'file-exists-p
     (mapcar (lambda (dir)
               (expand-file-name "compile_commands.json" (expand-file-name dir proj-root)))
             +compile-commands-json-directories))))

(define-obsolete-function-alias '+project-have-compile-commands-p '+compilation-db-find-file "13.5.0")
(define-obsolete-function-alias '+compile-commands-find-file '+compilation-db-find-file "13.6.0")

(defvar +compilation-db-cache (make-hash-table :test #'equal))

;;;###autoload
(defun +get-compilation-db (&optional proj-root)
  "Get the  \"compile_commands.json\" for project at PROJ-ROOT as a plist."
  (when-let* ((compile-commands-file (+compilation-db-find-file proj-root)))
    (or (gethash proj-root +compilation-db-cache)
        (when-let* ((json-object-type 'plist)
                    (json-array-type 'list)
                    (compile-commands (json-read-file compile-commands-file)))
          (puthash proj-root compile-commands +compilation-db-cache)
          compile-commands))))

;;;###autoload
(defun +compilation-db-get-entry (file-name &optional proj-root)
  (when-let* ((proj-root (or proj-root (+project-safe-root) (file-name-directory file-name)))
              (file-name (file-relative-name file-name proj-root))
              (compile-commands (+get-compilation-db proj-root)))
    (catch 'found
      (dolist (entry compile-commands)
        (when (string-match-p (rx-to-string `(seq ,(file-name-sans-extension file-name) "." (or "cpp" "cc" "c" "cxx" "c++")))
                              (plist-get entry :file))
          (throw 'found entry))))))

;;;###autoload
(defun +cmd-split-rm-single (cmd flag &optional test)
  "Remove a single FLAG from CMD.  Test according to TEST."
  (mapconcat #'identity (cl-remove flag (split-string cmd) :test (or test #'string=)) " "))

;;;###autoload
(defun +cmd-split-rm-double (cmd flag)
  "Remove a FLAG and subsequent arg from CMD."
  (cl-loop while split with split = (split-string cmd)
           for i from 0
           for probe = (car split)
           if (string= probe flag) do (setq split (cddr split))
           else
           concat (and (cl-plusp i) " ")
           and concat probe and do (setq split (cdr split))))

;;;###autoload
(defun +guess-args-from-compilation-db (file-name)
  (when-let* ((ccj (+compilation-db-get-entry file-name))
              (cmd (or (ensure-list (plist-get ccj :command))
                       (butlast (plist-get ccj :arguments))))
              (cmd (string-join cmd " "))
              (cmd (+cmd-split-rm-double cmd "-o"))
              (cmd (+cmd-split-rm-double cmd "-c"))
              (cmd (+cmd-split-rm-single cmd "-flto" #'string-prefix-p)))
    (cons (plist-get ccj :directory) cmd)))

;;;###autoload
(defun +hide-ifdef-get-env-from-compilation-db ()
  "Integrate `hideif' with \"compile_commands.json\"."
  (when-let* ((entry (+compilation-db-get-entry (buffer-file-name)))
              (command (plist-get entry :command)))
    (dolist (str (string-split command))
      (when-let* (((string-prefix-p "-D" str))
                  (def-val (string-split (substring str 2) "=")))
        (hif-set-var
         (intern (car def-val))
         (or (when-let* ((val (cadr def-val)))
               (cond ((string-match-p "^\"\\(.*\\)\"$" val)
                      (match-string 1 val))
                     ((equal val (number-to-string (string-to-number val)))
                      (string-to-number val))
                     (t (intern val))))
             1))))))

;; To use as an advice for sentinel functions, for example for `term-sentinel' or `eat--sentinel'
;;;###autoload
(defun +kill-buffer-after-sentinel-exit (orig-fn proc msg)
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        (apply orig-fn (list proc msg))
        (kill-buffer buffer))
    (apply orig-fn (list proc msg))))



;;; Emacs server

;;;###autoload
(defun +server-restart ()
  "Restart the Emacs server."
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))



;;; Buffer related tweaks

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
      (cl-incf matches (save-excursion
                         (goto-char (point-min))
                         (let ((case-fold-search nil)
                               (matches 0))
                           (while (re-search-forward (car pair) nil t)
                             (replace-match (cdr pair))
                             (cl-incf matches))
                           matches))))
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
(defun +copy-region-as-paragraph ()
  "Copy region as one paragraph.
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
                (file-directory-p proj)) ; Existent local project
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
  (+project-forget-zombie-projects) ; Forget non-existent projects
  (mapc ; Remember some Git repositories as projects by path
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
      (markdown-ts-mode)
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
    (markdown-ts-mode)
    (read-only-mode 1)
    (pop-to-buffer (current-buffer))))



;;;###autoload
(defun +describe-at-point ()
  "Show help for the symbol at point."
  (interactive)
  (if-let* ((sym (symbol-at-point))
            (fn (cond ((and (fboundp sym) (boundp sym))
                       (if (= ?v (read-char-choice (format "Ambiguous `%s', describe [v]ariable or [c]allable? " sym) '(?v ?c)))
                           'describe-variable
                         'describe-function))
                      ((fboundp sym) 'describe-function)
                      ((boundp sym) 'describe-variable)
                      ((symbolp sym) 'describe-symbol))))
      (funcall fn sym)
    (user-error "There is no symbol at point")))



(provide 'me-lib-x)
;;; me-lib-x.el ends here
