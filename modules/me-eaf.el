;; -*- lexical-binding: t; -*-


(use-package eaf
  :straight (:host github :repo "emacs-eaf/emacs-application-framework" :files (:defaults "*"))
  :unless feat/lucid
  :commands (eaf-install-and-update
             eaf-open
             eaf-open-jupyter
             eaf-open-mindmap
             eaf-file-sender-qrcode
             eaf-file-sender-qrcode-in-dired
             eaf-open-browser
             +eaf-open-mail-as-html)
  :general
  (me-map
    "oo" '(eaf-open :which-key "Open with EAF"))
  :custom
  (eaf-apps-to-install (message "CUSTOM"))
  ;; Generic
  (eaf-apps-to-install '(browser pdf-viewer mindmap jupyter org-previewer
                         markdown-previewer file-sender video-player))
  (eaf-start-python-process-when-require t)
  (eaf-kill-process-after-last-buffer-closed t)
  (eaf-fullscreen-p nil)
  (eaf-config-location (expand-file-name "eaf" minemacs-var-dir))
  ;; Debug
  (eaf-enable-debug nil)
  ;; Web engine
  (eaf-webengine-font-family (plist-get me-fonts :font-family))
  (eaf-webengine-fixed-font-family (plist-get me-fonts :font-family))
  (eaf-webengine-serif-font-family (plist-get me-fonts :variable-pitch-font-family))
  (eaf-webengine-font-size 16)
  (eaf-webengine-fixed-font-size 16)
  (eaf-webengine-enable-scrollbar t)
  (eaf-webengine-scroll-step 200)
  (eaf-webengine-default-zoom 1.25)
  (eaf-webengine-show-hover-link t)
  (eaf-webengine-download-path "~/Downloads")
  (eaf-webengine-enable-plugin t)
  (eaf-webengine-enable-javascript t)
  (eaf-webengine-enable-javascript-access-clipboard t)
  ;; Jupyter
  (eaf-jupyter-font-family (plist-get me-fonts :font-family))
  (eaf-jupyter-font-size 14)
  ;; Video player
  (eaf-video-player-keybinding
   '(("p" . "toggle_play")
     ("q" . "close_buffer")
     ("h" . "play_backward")
     ("l" . "play_forward")
     ("j" . "decrease_volume")
     ("k" . "increase_volume")
     ("f" . "toggle_fullscreen")
     ("R" . "restart")))
  ;; PDF viewer
  (eaf-pdf-show-progress-on-page nil)
  (eaf-pdf-dark-exclude-image t)
  (eaf-pdf-notify-file-changed t)
  ;; Browser
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (eaf-browser-ignore-history-list '("google.com/search" "file://"))
  (eaf-browser-translate-language "en")
  (eaf-browser-blank-page-url "https://www.duckduckgo.com")
  (eaf-browser-chrome-history-file (expand-file-name "eaf/browser/Default/History" minemacs-var-dir))
  (eaf-browser-default-search-engine "duckduckgo")
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-aria2-auto-file-renaming t)
  :config
  ;; Fix EVIL keybindings
  (with-eval-after-load 'evil
    (defvar eaf-evil-leader-key "C-SPC")
    (define-key
     key-translation-map (kbd "SPC")
     (lambda (prompt)
       (if (derived-mode-p 'eaf-mode)
           (pcase eaf--buffer-app-name
             ("browser" (if (eaf-call-sync "execute_function" eaf--buffer-id "is_focus")
                            (kbd "SPC")
                          (kbd eaf-evil-leader-key)))
             ((or "pdf-viewer"
                  "image-viewer"
                  "music-player"
                  "video-player"
                  "file-sender"
                  "jupyter"
                  "mindmap")
              (kbd eaf-evil-leader-key))
             (_ (kbd "SPC")))
         (kbd "SPC")))))


  ;; Extensions
  (require 'eaf-org)
  (require 'eaf-all-the-icons)
  (when (display-graphic-p)
    (mapc (lambda (v) (eaf-all-the-icons-icon (car v)))
          eaf-all-the-icons-alist))

  ;; Apps
  (require 'eaf-org-previewer)
  (require 'eaf-markdown-previewer)
  (require 'eaf-jupyter)
  (require 'eaf-mindmap)
  (require 'eaf-file-sender)
  (require 'eaf-video-player)

  (require 'eaf-browser)

  (unless feat/xwidgets
    ;; Make EAF Browser my default browser
    (setq browse-url-browser-function #'eaf-open-browser)
    (defalias 'browse-web #'eaf-open-browser))

  (defun +eaf-open-mail-as-html ()
    "Open the html mail in EAF Browser."
    (interactive)
    (let ((msg (mu4e-message-at-point t))
          ;; Bind browse-url-browser-function locally, so it works
          ;; even if EAF Browser is not set as a default browser.
          (browse-url-browser-function #'eaf-open-browser))
      (if msg
          (mu4e-action-view-in-browser msg)
        (message "No message at point."))))

  ;; (with-eval-after-load 'mu4e
  ;;   (me-map-local :keymaps '(mu4e-headers-mode-map mu4e-view-mode-map)
  ;;     "h" '(+eaf-open-mail-as-html :which-key "Open mail as HTML")
  ;;     "o" '(eaf-open-browser :which-key "Open URL (EAF)")))

  (require 'eaf-pdf-viewer)

  (with-eval-after-load 'org
    ;; Use EAF PDF Viewer in Org
    (defun +eaf--org-open-file-fn (file &optional link)
      "An wrapper function on `eaf-open'."
      (eaf-open file))
    ;; use `emacs-application-framework' to open PDF file: link
    (add-to-list 'org-file-apps '("\\.pdf\\'" . +eaf--org-open-file-fn)))

  (with-eval-after-load 'latex
    ;; Link EAF with the LaTeX compiler in emacs. When a .tex file is open,
    ;; the Command>Compile and view (C-c C-a) option will compile the .tex
    ;; file into a .pdf file and display it using EAF. Double clicking on the
    ;; PDF side jumps to editing the clicked section.
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))


(provide 'me-eaf)
