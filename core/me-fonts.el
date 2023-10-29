;; me-fonts.el --- MinEmacs fonts -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:


(defcustom minemacs-fonts-plist
  '(:default
    ((:family "Iosevka Fixed Curly Slab" :height 130)
     (:family "Iosevka Comfy Fixed" :height 130)
     (:family "Iosevka Fixed Curly" :height 130)
     (:family "Iosevka Comfy Motion Fixed" :height 130)
     (:family "Iosevka" :height 130)
     (:family "Iosevka Comfy" :height 130)
     (:family "Cascadia Code" :height 130)
     (:family "Fira Code" :height 130)
     (:family "Jetbrains Mono" :height 110)
     (:family "Hack" :height 130)
     (:family "Roboto Mono" :height 120)
     (:family "SF Mono" :height 130)
     (:family "Source Code Pro" :height 130)
     (:family "Menlo" :height 130)
     (:family "Monaco" :height 130)
     (:family "Ubuntu Mono" :height 130)
     (:family "DejaVu Sans Mono" :height 130)
     (:family "Consolas" :height 130))
    :fixed-pitch
    ((:inherit default))
    :fixed-pitch-serif
    ((:inherit default))
    :variable-pitch
    ("Lato"
     "Roboto"
     "Inter"
     "San Francisco"
     "Helvetica Neue"
     "Helvetica"
     "Ubuntu"
     "Liberation Sans"
     "Segoe UI")
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
    ("Amiri Typewriter"
     "KacstOne"
     "Greta Arabic"
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
  "MinEmacs fonts used by `+setup-fonts'.

The function checks and enables the first available font from these defined in
this plist. This variable can be customized to set font specs for specific Emacs
faces or to enable some language-specific fonts. The plist keywords are either
face names or script names expressed as keywords (with the \":\" prefix).

For example to set `default' face, use `:default', to set the `mode-line' face,
use `:mode-line', and so on. The parameters for each font in these cases (ie.
for face names) are used in the `set-face-attribute' function, so you can pass
any key value pairs supported by `set-face-attribute' (like `:weight',
`:slanted', ...). A list of supported keywords are available in the variable
`+set-face-attribute-keywords'.

You can also setup some language-specific fonts. All scripts supported by Emacs
can be found in `+known-scripts'. The keyword in this plist will be the script
name expressed as a keyword, for example, for `latin' use `:latin', for `arabic'
use `:arabic', for `emoji' use `:emoji', and so on. In this case, the parameters
are used with `set-fontset-font', so you can send any key value pair supported
by `set-fontset-font'. A list of supported keywords in this case is available in
`+font-spec-keywords'.

The value of the extra `:prepend' keyword is passed the last argument to
`set-fontset-font'. The value of the extra `:scale' keyword can be used to set a
scaling factor for the font in Emacs' `face-font-rescale-alist'. See the
`+setup-fonts' implementation for more details."
  :group 'minemacs-ui
  :type 'plist)

(defvar minemacs-fonts nil)

(make-obsolete-variable 'minemacs-fonts 'minemacs-fonts-plist "v3.0.0")

(defconst +known-scripts (mapcar #'car script-representative-chars)
  "Supported scripts, like `latin', `arabic', `han', and so on.")

(defconst +font-family-list (font-family-list)
  "List of available fonts on the system, initialized at startup from
  `font-family-list'.")

(defconst +set-face-attribute-keywords
  '(:family :foundry :width :height :weight :slant :foreground
    :background :underline :overline :strike-through :box
    :inverse-video :stipple :extend :font :inherit)
  "Arguments accepted by the `set-face-attribute' function.")

(defconst +font-spec-keywords
  '(:family :foundry :width :weight :slant :adstyle :registry :dpi :size :spacing
    :avgwidth :name :script :lang :otf)
  "Arguments accepted by the `font-spec' function.")

(defun +font--get-valid-args (script-or-face font)
  (if (stringp font)
      `(:family ,font)
    (apply
     #'append
     (mapcar (lambda (a) (list a (plist-get font a)))
             (cl-intersection (+plist-keys font)
                              (if (memq script-or-face +known-scripts)
                                  +font-spec-keywords
                                +set-face-attribute-keywords))))))

(defun +font-installed-p (font-family)
  "Check if FONT-FAMILY is installed on the system."
  (and font-family (member font-family +font-family-list) t))

(defun +apply-font-script (script-or-face)
  "Set font for SCRIPT-OR-FACE from `minemacs-fonts-plist'."
  (catch 'done
    (dolist (font (plist-get minemacs-fonts-plist (intern (format ":%s" script-or-face))))
      (let* ((spec (+font--get-valid-args script-or-face font))
             (prepend (and (plistp font) (plist-get font :family)))
             (scale (and (plistp font) (plist-get font :scale)))
             (family (plist-get spec :family)))
        (when (+font-installed-p family)
          (if (memq script-or-face +known-scripts)
              (set-fontset-font t script-or-face (apply #'font-spec spec) nil prepend)
            (apply #'set-face-attribute (append `(,script-or-face nil) spec)))
          (when scale (add-to-list 'face-font-rescale-alist (cons (plist-get spec :family) scale)))
          (throw 'done spec))))))

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

(make-obsolete #'+set-fonts #'+setup-fonts "v3.0.0")

(+add-hook! (window-setup server-after-make-frame) #'+setup-fonts)


(provide 'me-fonts)
