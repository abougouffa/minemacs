;; -*- lexical-binding: t; -*-

(defvar +mu4e-main-bullet "⦿"
  "Prefix to use instead of \"  *\" in the mu4e main view.
This is enacted by `+mu4e--main-action-str-prettier-a' and
`+mu4e--main-keyval-str-prettier-a'.")

(defvar +mu4e-header-colorized-faces
  '(all-the-icons-green
    all-the-icons-lblue
    all-the-icons-purple-alt
    all-the-icons-blue-alt
    all-the-icons-purple
    all-the-icons-yellow)
  "Faces to use when coloring folders and account stripes.")

(defun +mu4e-colorize-str (str &optional unique herring)
  "Apply a face from `+mu4e-header-colorized-faces' to STR.
If HERRING is set, it will be used to determine the face instead of STR.
Will try to make unique when non-nil UNIQUE,
a quoted symbol for a alist of current strings and faces provided."
  (unless herring
    (setq herring str))
  (put-text-property
   0 (length str)
   'face
   (list
    (if (not unique)
        (+mu4e--str-color-face herring str)
      (let ((unique-alist (eval unique)))
        (unless (assoc herring unique-alist)
          (if (> (length unique-alist) (length +mu4e-header-colorized-faces))
              (push (cons herring (+mu4e--str-color-face herring)) unique-alist)
            (let ((offset 0) color color?)
              (while (not color)
                (setq color? (+mu4e--str-color-face herring offset))
                (if (not (rassoc color? unique-alist))
                    (setq color color?)
                  (setq offset (1+ offset))
                  (when (> offset (length +mu4e-header-colorized-faces))
                    (message "Warning: +mu4e-colorize-str was called with non-unique-alist UNIQUE-alist alist.")
                    (setq color (+mu4e--str-color-face herring)))))
              (push (cons herring color) unique-alist)))
          (set unique unique-alist))
        (cdr (assoc herring unique-alist))))
    'default)
   str)
  str)

(defun +mu4e--str-color-face (str &optional offset)
  "Select a face from `+mu4e-header-colorized-faces' based on
STR and any integer OFFSET."
  (let* ((str-sum (apply #'+ (mapcar (lambda (c) (% c 3)) str)))
         (color (nth (% (+ str-sum (if offset offset 0))
                        (length +mu4e-header-colorized-faces))
                     +mu4e-header-colorized-faces)))
    color))

(defun +mu4e--main-action-str-prettier-a (str &optional func-or-shortcut)
  "Highlight the first occurrence of [.] in STR.
If FUNC-OR-SHORTCUT is non-nil and if it is a function, call it
when STR is clicked (using RET or mouse-2); if FUNC-OR-SHORTCUT is
a string, execute the corresponding keyboard action when it is
clicked."
  (let ((newstr
         (replace-regexp-in-string
          "\\[\\(..?\\)\\]"
          (lambda(m)
            (format "%s"
                    (propertize (match-string 1 m) 'face '(mode-line-emphasis bold))))
          (replace-regexp-in-string "\t\\*" (format "\t%s" +mu4e-main-bullet) str)))
        (map (make-sparse-keymap))
        (func (if (functionp func-or-shortcut)
                  func-or-shortcut
                (if (stringp func-or-shortcut)
                    (lambda()
                      (interactive)
                      (execute-kbd-macro func-or-shortcut))))))
    (define-key map [mouse-2] func)
    (define-key map (kbd "RET") func)
    (put-text-property 0 (length newstr) 'keymap map newstr)
    (put-text-property (string-match "[A-Za-z].+$" newstr)
                       (- (length newstr) 1) 'mouse-face 'highlight newstr)
    newstr))

(defun +mu4e--main-keyval-str-prettier-a (str)
  "Replace '*' with `+mu4e-main-bullet' in STR."
  (replace-regexp-in-string "\t\\*" (format "\t%s" +mu4e-main-bullet) str))

(defun +mu4e--get-string-width (str)
  "Return the width in pixels of a string in the current
window's default font. If the font is mono-spaced, this
will also be the width of all other printable characters."
  (let ((window (selected-window))
        (remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert str)
      (car (window-text-pixel-size)))))

(cl-defun +normalized-icon (name &key set color height v-adjust)
  "Convert :icon declaration to icon"
  (let* ((icon-set (intern (concat "all-the-icons-" (or set "material"))))
         (v-adjust (or v-adjust 0.02))
         (height (or height 0.8))
         (icon (if color
                   (apply icon-set `(,name :face ,(intern (concat "all-the-icons-" color)) :height ,height :v-adjust ,v-adjust))
                 (apply icon-set `(,name  :height ,height :v-adjust ,v-adjust))))
         (icon-width (+mu4e--get-string-width icon))
         (space-width (+mu4e--get-string-width " "))
         (space-factor (- 2 (/ (float icon-width) space-width))))
    (concat (propertize " " 'display `(space . (:width ,space-factor))) icon)))

(defun +mu4e--ui-setup ()
  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark      (cons "D" (+normalized-icon "edit"))
        mu4e-headers-flagged-mark    (cons "F" (+normalized-icon "flag"))
        mu4e-headers-new-mark        (cons "N" (+normalized-icon "file_download" :color "dred"))
        mu4e-headers-passed-mark     (cons "P" (+normalized-icon "forward"))
        mu4e-headers-replied-mark    (cons "R" (+normalized-icon "reply"))
        mu4e-headers-seen-mark       (cons "S" "")
        mu4e-headers-trashed-mark    (cons "T" (+normalized-icon "delete"))
        mu4e-headers-attach-mark     (cons "a" (+normalized-icon "attach_file"))
        mu4e-headers-encrypted-mark  (cons "x" (+normalized-icon "lock"))
        mu4e-headers-signed-mark     (cons "s" (+normalized-icon "verified_user" :color "dpurple"))
        mu4e-headers-unread-mark     (cons "u" (+normalized-icon "remove_red_eye" :color "dred"))
        mu4e-headers-list-mark       (cons "l" (+normalized-icon "list"))
        mu4e-headers-personal-mark   (cons "p" (+normalized-icon "person"))
        mu4e-headers-calendar-mark   (cons "c" (+normalized-icon "date_range"))
        mu4e-headers-date-format "%d/%m/%y"
        mu4e-headers-time-format "%H:%M"
        mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
        mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ ")
        mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
        mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
        mu4e-headers-thread-child-prefix         '("├>" . "├▶")
        mu4e-headers-thread-last-child-prefix    '("└>" . "╰▶")
        mu4e-headers-fields '((:account-stripe . 2)
                              (:flags . 6) ;; 3 flags
                              (:human-date . 8)
                              (:from-or-to . 25)
                              (:subject-truncated)))

  ;; Add a column to display what email account the email belongs to,
  ;; and an account color stripe column
  (defvar +mu4e-header--maildir-colors nil)
  (setq mu4e-header-info-custom
        '((:account
           . (:name "Account"
              :shortname "Account"
              :help "which account/maildir this email belongs to"
              :function
              (lambda (msg)
                (let ((maildir (replace-regexp-in-string
                                "\\`/?\\([^/]+\\)/.*\\'" "\\1"
                                (mu4e-message-field msg :maildir))))
                 (+mu4e-colorize-str
                  (replace-regexp-in-string
                   "^gmail"
                   (propertize "g" 'face 'bold-italic)
                   maildir)
                  '+mu4e-header--maildir-colors
                  maildir)))))
          (:subject-truncated
           . (:name "Subject"
              :shortname "Subject"
              :help "Subject of the message"
              :sortable t
              :function
              (lambda (msg)
                (let ((prefix (mu4e~headers-thread-prefix (mu4e-message-field msg :meta))))
                 (concat
                  prefix
                  (truncate-string-to-width
                   (mu4e-message-field msg :subject)
                   (- 100 (length prefix)) nil nil "…"))))))
          (:account-stripe
           . (:name "Account"
              :shortname "▐"
              :help "Which account/maildir this email belongs to"
              :function
              (lambda (msg)
                (let ((account
                       (replace-regexp-in-string
                        "\\`/?\\([^/]+\\)/.*\\'" "\\1"
                        (mu4e-message-field msg :maildir))))
                 (propertize
                  (+mu4e-colorize-str "▌" '+mu4e-header--maildir-colors account)
                  'help-echo account)))))
          (:recipnum
           . (:name "Number of recipients"
              :shortname " ⭷"
              :help "Number of recipients for this message"
              :function
              (lambda (msg)
                (propertize (format "%2d"
                             (+ (length (mu4e-message-field msg :to))
                              (length (mu4e-message-field msg :cc))))
                 'face 'mu4e-footer-face))))))

  ;; Evil collection overwrite the jump, search, compose and quit commands
  (with-eval-after-load 'evil-collection
    (setq evil-collection-mu4e-new-region-basic
          (concat (+mu4e--main-action-str-prettier-a
                   "\t* [J]ump to some maildir\n" 'mu4e-jump-to-maildir)
                  (+mu4e--main-action-str-prettier-a
                   "\t* Enter a [s]earch query\n" 'mu4e-search)
                  (+mu4e--main-action-str-prettier-a
                   "\t* [C]ompose a new message\n" 'mu4e-compose-new))
          evil-collection-mu4e-end-region-misc "quit"))

  (advice-add #'mu4e--key-val :filter-return #'+mu4e--main-keyval-str-prettier-a)
  (advice-add #'mu4e--main-action-str :override #'+mu4e--main-action-str-prettier-a))

(defun +mu4e-ui-setup ()
  (if (display-graphic-p)
      (+mu4e--ui-setup)
    (add-hook
     'server-after-make-frame-hook
     (defun +mu4e-ui--setup-once-h ()
       (when (display-graphic-p)
         (+mu4e--ui-setup)
         (remove-hook 'server-after-make-frame-hook
                      #'+mu4e--ui-setup-once-h))))))


(provide 'me-mu4e-ui)
