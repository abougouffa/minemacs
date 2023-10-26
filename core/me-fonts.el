;; me-fonts.el --- MinEmacs fonts -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:


(defcustom minemacs-fonts-plist
  '(:default
    ((:family "Iosevka" :height 130)
     (:family "Cascadia Code" :height 130)
     (:family "Fira Code" :height 130)
     (:family "Jetbrains Mono" :height 110)
     (:family "Hack" :height 130)
     (:family "Roboto Mono" :height 120)
     (:family "SF Mono" :height 130)
     (:family "Source Code Pro" :height 130)
     (:family "Menlo" :height 130)
     (:family "Monaco" :height 130)
     (:family "DejaVu Sans Mono" :height 130)
     (:family "Consolas" :height 130))
    :symbol
    ((:family "Segoe UI Symbol" :prepend t)
     (:family "Symbola" :prepend t)
     (:family "Symbol" :prepend t))
    :emoji
    ((:family "Noto Color Emoji" :prepend t)
     (:family "Apple Color Emoji" :prepent t)
     (:family "Segoe UI Emoji" :prepend t)
     (:family "Quivira" :prepend t))
    ;; Arabic script
    :arabic
    ("KacstOne"
     "Amiri Typewriter"
     "Scheherazade"
     "Koodak"
     (:family "Amiri" :scale 0.9))
    ;; Chinese script
    :han
    ((:family "LXGW Neo Xihei" :scale 1.3)
     (:family "WenQuanYi Micro Hei Mono" :scale 1.3)
     (:family "LXGW WenKai Screen" :scale 1.3)
     (:family "LXGW WenKai Mono" :scale 1.3)
     (:family "PingFang SC" :scale 1.3)
     (:family "Microsoft Yahei UI" :scale 1.3)
     (:family "Simhei" :scale 1.3)))
  "Default fonts of MinEmacs."
  :group 'minemacs-ui
  :type '(choice
          (list string)
          (plist
           (:family string)
           (:scale number)
           (:height number))))

;; Setup default fonts (depending on the OS)
(let ((mono-font (cond (os/linux "monospace")
                       (os/win "Lucida Console")
                       (os/mac "monospace")))
      (varp-font (cond (os/linux "monospace")
                       (os/win "Tahoma")
                       (os/mac "monospace"))))
  (defconst minemacs-default-fonts
    `(:font-family ,mono-font
      :font-size 13
      :unicode-font-family nil
      :variable-pitch-font-family ,varp-font
      :variable-pitch-font-size 13)
    "Default fonts of MinEmacs."))

(make-obsolete-variable 'minemacs-default-fonts nil "v3.0.0")

(defcustom minemacs-fonts nil
  "Fonts to use within MinEmacs."
  :group 'minemacs-ui
  :type '(plist
          (:font-family string)
          (:font-size natnum)
          (:unicode-font-family string)
          (:variable-pitch-font-family string)
          (:variable-pitch-font-size natnum)))

(make-obsolete-variable 'minemacs-fonts 'minemacs-fonts-plist "v3.0.0")

(defun +font--scale (spec)
  (when (plistp spec) (plist-get spec :scale)))

(defun +font--family (spec)
  (if (stringp spec) spec (plist-get spec :family)))

(defun +font--height (spec)
  (when (plistp spec) (plist-get spec :height)))

(defun +font--prepend (spec)
  (when (plistp spec) (plist-get spec :prepend)))

(defun +font-installed-p (font-family)
  "Check if FONT-FAMILY is installed on the system."
  (and (member font-family (font-family-list)) t))

(defun +apply-font-script (script-or-face)
  "Set font for SCRIPT-OR-FACE from `minemacs-fonts-plist'."
  (catch 'done
    (dolist (font (plist-get minemacs-fonts-plist (intern (format ":%s" script-or-face))))
      (let* ((family (+font--family font))
             (scale (+font--scale font))
             (height (+font--height font))
             (prepend (+font--prepend font))
             (spec (append `(:family ,family) (when height `(:height ,height)))))
        (when (+font-installed-p family)
          (if (not (memq script-or-face (mapcar #'car script-representative-chars)))
              (apply #'set-face-attribute (append `(,script-or-face nil) spec))
            (set-fontset-font t script-or-face (apply #'font-spec spec) nil prepend))
          (when scale (add-to-list 'face-font-rescale-alist (cons family scale)))
          (+log! "Font for `%s' set to \"%s\"%s" script-or-face family
                 (format "%s%s%s"
                         (if height (format " :height %s" height) "")
                         (if scale (format " :scale %s" scale) "")
                         (if prepend (format " :prepend %s" prepend) "")))
          (throw 'done family))))))

;; Inspired by: github.com/seagle0128/.emacs.d/blob/master/custom-example.el
;;;###autoload
(defun +setup-fonts ()
  "Setup fonts."
  (interactive)
  (mapc #'+apply-font-script
        (reverse
         (mapcar (lambda (k) (intern (substring (symbol-name k) 1)))
                 (+plist-keys minemacs-fonts-plist))))
  ;; Run hooks
  (run-hooks 'minemacs-after-setup-fonts-hook))

(defun +set-fonts ()
  "Set Emacs' fonts from `minemacs-fonts'."
  (interactive)
  (custom-set-faces
   `(default
     ((t (:font ,(format "%s %d"
                  (or (plist-get minemacs-fonts :font-family)
                   (plist-get minemacs-default-fonts :font-family))
                  (or (plist-get minemacs-fonts :font-size)
                   (plist-get minemacs-default-fonts :font-size)))))))
   `(fixed-pitch
     ((t (:inherit (default)))))
   `(fixed-pitch-serif
     ((t (:inherit (default)))))
   `(variable-pitch
     ((t (:font ,(format "%s %d"
                  (or (plist-get minemacs-fonts :variable-pitch-font-family)
                   (plist-get minemacs-default-fonts :variable-pitch-font-family))
                  (or (plist-get minemacs-fonts :variable-pitch-font-size)
                   (plist-get minemacs-default-fonts :variable-pitch-font-size))))))))
  ;; Run hooks
  (run-hooks 'minemacs-after-set-fonts-hook))

(make-obsolete #'+set-fonts #'+setup-fonts "v3.0.0")

(add-hook 'window-setup-hook #'+setup-fonts)
(add-hook 'server-after-make-frame-hook #'+setup-fonts)


(provide 'me-fonts)
