;;; me-mu4e-ui.el --- Better UI for mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;; Most of this has been inspired by Doom Emacs

;;; Code:

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

(defun +mu4e--main-action-prettier-a (title cmd &optional bindstr alt)
  (let* ((bindstr (or bindstr (mu4e-key-description cmd) (and alt (string alt))
                      (mu4e-error "No binding for %s" cmd)))
         (bindstr
          (if (and alt (> (length bindstr) 1)) alt bindstr))
         (title ;; remove first letter afrer [] if it equal last of binding
          (mu4e-string-replace
           (concat "[@]" (substring bindstr -1)) "[@]" title))
         (title ;; Special cases: replace "jump" with "Jump", "enter" -> "Enter"
          (cond ((string= "j" bindstr)
                 (setq bindstr "J")
                 (replace-regexp-in-string "jump" "Jump" title))
                ((string= "s" bindstr)
                 (replace-regexp-in-string "enter" "Enter" title))
                (t title)))
         (title ;; insert binding in [@]
          (mu4e-string-replace
           "[@]" (format "[%s]" (propertize bindstr 'face 'mu4e-highlight-face))
           title))
         (title ;; Prettify the title
          (replace-regexp-in-string
           "\\[\\(..?\\)\\]"
           (lambda(m)
             (format "%s"
                     (propertize (match-string 1 m) 'face '(mode-line-emphasis bold))))
           (replace-regexp-in-string "\t\\*" (format "\t%s" +mu4e-main-bullet) title)))
         (map (make-sparse-keymap)))
    (define-key map [mouse-2] cmd)
    (define-key map (kbd "RET") cmd)
    ;; Add highlighting on mouse hover
    (put-text-property 0 (length title) 'keymap map title)
    (put-text-property (string-match "[A-Za-z].+$" title)
                       (- (length title) 1) 'mouse-face 'highlight title)
    (propertize title 'keymap map)))

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
         (space-factor (- 2.0 (/ (float icon-width) space-width))))
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
                   ;; Some times, a newline/carriage return char slips in the
                   ;; subject and drives mu4e crazy! Let's fix it and truncate
                   ;; the string at 100 characters.
                   (replace-regexp-in-string
                    "[\n\r]" ""
                    (mu4e-message-field msg :subject))
                   (- 100 (length prefix)) nil nil t))))))
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
  ;; TODO: Useless now, to be updated/deleted when `evil-collection-mu4e' gets
  ;; fixed.
  ;; (with-eval-after-load 'evil-collection
  ;;   (setq evil-collection-mu4e-new-region-basic
  ;;         (concat (+mu4e--main-action-prettier-a
  ;;                  "\t* [J]ump to some maildir\n" 'mu4e-search-maildir)
  ;;                 (+mu4e--main-action-prettier-a
  ;;                  "\t* Enter a [s]earch query\n" 'mu4e-search)
  ;;                 (+mu4e--main-action-prettier-a
  ;;                  "\t* [C]ompose a new message\n" 'mu4e-compose-new))
  ;;         evil-collection-mu4e-end-region-misc "quit"))

  (advice-add #'mu4e--key-val :filter-return #'+mu4e--main-keyval-str-prettier-a)
  (advice-add #'mu4e--main-action :override #'+mu4e--main-action-prettier-a))

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

;;; me-mu4e-ui.el ends here
