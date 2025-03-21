;;; me-mu4e-ui.el --- Better UI for mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; Most of this has been inspired by Doom Emacs

;;; Code:

(defvar +mu4e-main-bullet "⦿")
(defvar +mu4e-main-bullet-icon "nf-fa-dot_circle_o")

(defvar +mu4e-header-colorized-faces
  '(nerd-icons-green
    nerd-icons-lblue
    nerd-icons-purple-alt
    nerd-icons-blue-alt
    nerd-icons-purple
    nerd-icons-yellow
    nerd-icons-maroon
    nerd-icons-dorange)
  "Faces to use when coloring folders and account stripes.")

(defun +mu4e-colorize-str (str &optional unique herring)
  "Apply a face from `+mu4e-header-colorized-faces' to STR.
If HERRING is set, it will be used to determine the face instead of STR.
Will try to make unique when non-nil UNIQUE,
a quoted symbol for a alist of current strings and faces provided."
  (let ((herring (or herring str)))
    (put-text-property
     0 (length str) 'face
     (list
      (if (null unique)
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
    str))

(defun +mu4e--str-color-face (str &optional offset)
  "Select a face from `+mu4e-header-colorized-faces' based on STR and any
integer OFFSET."
  (let* ((str-sum (apply #'+ (mapcar (lambda (c) (% c 3)) str)))
         (color (nth (% (+ str-sum (or offset 0))
                        (length +mu4e-header-colorized-faces))
                     +mu4e-header-colorized-faces)))
    color))

(defun +mu4e--main-action-prettier:override-a (title cmd &optional bindstr alt)
  (let* ((bindstr (or bindstr (mu4e-key-description cmd) (and alt (string alt))
                      (mu4e-error "No binding for %s" cmd)))
         (bindstr (if (and alt (> (length bindstr) 1)) alt bindstr))
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
           (replace-regexp-in-string "\t\\*" (format "\t%s" (+mu4e--get-icon-for-section title)) title)))
         (map (make-sparse-keymap)))
    (keymap-set map "<mouse-2>" cmd)
    (keymap-set map "RET" cmd)
    ;; Add highlighting on mouse hover
    (put-text-property 0 (length title) 'keymap map title)
    (put-text-property (string-match "[A-Za-z].+$" title)
                       (- (length title) 1) 'mouse-face 'highlight title)
    (propertize title 'keymap map)))

(defvar +icon-colors
  '(("jump to some maildir" "nf-md-mailbox" nerd-icons-red)
    ("enter a search query" "nf-md-email_search" nerd-icons-blue)
    ("compose a new message" "nf-md-email_edit" nerd-icons-green)
    ("unread messages" "nf-md-email" nerd-icons-green)
    ("today's messages" "nf-md-calendar_today")
    ("yesterday's messages" "nf-md-calendar_today")
    ("last 7 days" "nf-md-calendar_multiple")
    ("messages with images" "nf-fa-image" nerd-icons-red)
    ("unified inbox" "nf-md-inbox_multiple" nerd-icons-blue)
    ("spams" "nf-md-fire" nerd-icons-orange)
    ("choose query" "nf-md-email_search_outline" nerd-icons-blue)
    ("switch context" "nf-fa-exchange")
    ("update email & database" "nf-md-email_sync_outline")
    ("news" "nf-md-newspaper_variant_outline")
    ("help" "nf-md-help_circle" nerd-icons-dgreen)
    ("about mu4e" "nf-fa-info_circle" nerd-icons-green)
    ("quit" "nf-md-exit_to_app" nerd-icons-dred)
    (".*" "nf-fa-dot_circle_o")))

(defun +mu4e--get-icon-for-section (title)
  (if (fboundp '+nerd-icons-icon)
      (let ((case-fold-search t)
            (icon-spec (cl-find-if (lambda (e) (string-match-p (car e) (replace-regexp-in-string "[][]" "" title))) +icon-colors)))
        (+nerd-icons-icon (cadr icon-spec) :face (caddr icon-spec) :height 0.95))
    +mu4e-main-bullet))

(defun +mu4e--main-keyval-str-prettier:filter-return-a (str)
  "Replace the start * with a prettier bullet in STR."
  (replace-regexp-in-string
   "\t\\*"
   (format "\t%s"
           (if (fboundp '+nerd-icons-icon)
               (+nerd-icons-icon +mu4e-main-bullet-icon)
             +mu4e-main-bullet))
   str))

(defun +mu4e--get-string-width (str)
  "Return the width in pixels of STR in the current window's default font.
If the font is mono-spaced, this will also be the width of all other printable
characters."
  (let ((window (selected-window))
        (remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert str)
      (car (window-text-pixel-size)))))

(cl-defun +normalized-icon (name &key set color height v-adjust)
  "Convert icon declaration to nerd icon with width normalized to space-width."
  (let* ((set (or set "fa"))
         (icon-set (intern (format "nerd-icons-%sicon" set)))
         (v-adjust (or v-adjust 0.02))
         (height (or height 0.8))
         (name (format "nf-%s-%s" set name))
         (icon (if color
                   (apply icon-set `(,name :face ,(intern (format "nerd-icons-%s" color)) :height ,height :v-adjust ,v-adjust))
                 (apply icon-set `(,name :height ,height :v-adjust ,v-adjust))))
         (icon-width (+mu4e--get-string-width icon))
         (space-width (+mu4e--get-string-width " "))
         (space-factor (- 2.0 (/ (float icon-width) space-width))))
    (concat (propertize " " 'display `(space . (:width ,space-factor))) icon)))

(defun +mu4e--ui-setup ()
  "Tweak UI elements."
  ;; Add a column to display what email account the email belongs to,
  ;; and an account color stripe column
  (defvar +mu4e-header--maildir-colors nil)
  (setq
   mu4e-header-info-custom
   '((:account
      . (:name "Account"
               :shortname "Account"
               :help "Which account/maildir this email belongs to"
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
               :help "Which account/maildir this email belongs to, as a colorized stripe"
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
                             'face 'mu4e-footer-face)))))
   mu4e-headers-date-format "%d/%m/%y"
   mu4e-headers-time-format "%H:%M"
   mu4e-use-fancy-chars t
   mu4e-headers-attach-mark    (cons "a" (+normalized-icon 'attachment :set 'md))
   mu4e-headers-calendar-mark  (cons "c" (+normalized-icon 'calendar :set 'md))
   mu4e-headers-draft-mark     (cons "D" (+normalized-icon 'edit))
   mu4e-headers-encrypted-mark (cons "x" (+normalized-icon 'lock))
   mu4e-headers-flagged-mark   (cons "F" (+normalized-icon 'flag :set 'md))
   mu4e-headers-list-mark      (cons "l" (+normalized-icon 'list_ul))
   mu4e-headers-new-mark       (cons "N" (+normalized-icon 'download :set 'oct :color 'dred))
   mu4e-headers-passed-mark    (cons "P" (+normalized-icon 'mail_forward))
   mu4e-headers-personal-mark  (cons "p" (+normalized-icon 'person :set 'oct))
   mu4e-headers-replied-mark   (cons "R" (+normalized-icon 'mail_reply))
   mu4e-headers-seen-mark      (cons "S" "")
   mu4e-headers-signed-mark    (cons "s" (+normalized-icon 'verified :set 'oct :color 'dpurple))
   mu4e-headers-trashed-mark   (cons "T" (+normalized-icon 'trash_can_outline :set 'md))
   mu4e-headers-unread-mark    (cons "u" (+normalized-icon 'unread :set 'oct :color 'dred))
   mu4e-headers-thread-child-prefix         '("├>" . "├▶")
   mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
   mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
   mu4e-headers-thread-last-child-prefix    '("└>" . "╰▶")
   mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶")
   mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
   mu4e-headers-fields '((:account-stripe . 2)
                         (:flags . 6) ;; 3 flags
                         (:human-date . 8)
                         (:from-or-to . 25)
                         (:subject-truncated)))

  (advice-add 'mu4e--key-val :filter-return #'+mu4e--main-keyval-str-prettier:filter-return-a)
  (advice-add 'mu4e--main-action :override #'+mu4e--main-action-prettier:override-a))

(defun +mu4e-ui-setup ()
  "Apply UI setup."
  (once-x-call '(:check display-graphic-p :hook server-after-make-frame-hook :packages (mu4e)) #'+mu4e--ui-setup))

(defun +mu4e-ui-modeline-tweaks ()
  "Apply UI tweaks based on `nerd-icons'."
  (with-eval-after-load 'nerd-icons
    (setq mu4e-modeline-all-clear `("C:" . ,(format "%s  " (nerd-icons-octicon "nf-oct-read")))
          mu4e-modeline-all-read `("R:" . ,(format "%s  " (nerd-icons-octicon "nf-oct-check")))
          mu4e-modeline-unread-items `("U:" . ,(format "%s  " (nerd-icons-octicon "nf-oct-unread")))
          mu4e-modeline-new-items `("N:" . ,(format "%s  " (nerd-icons-octicon "nf-oct-bell"))))))


(provide 'me-mu4e-ui)

;;; me-mu4e-ui.el ends here
