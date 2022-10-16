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
  (+map
    "oo" '(eaf-open :which-key "Open with EAF"))
  (+map-local
    :states 'normal
    :keymaps '(mu4e-headers-mode-map mu4e-view-mode-map)
    "h" '(+eaf-open-mail-as-html :which-key "Open mail as HTML")
    "o" '(eaf-open-browser :which-key "Open URL (EAF)"))
  :custom
  ;; Generic
  (eaf-apps-to-install '(browser mindmap jupyter org-previewer
                         markdown-previewer file-sender video-player))
  (eaf-start-python-process-when-require t)
  (eaf-kill-process-after-last-buffer-closed t)
  (eaf-fullscreen-p nil)
  (eaf-config-location (expand-file-name "eaf" minemacs-local-dir))
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
  (eaf-browser-chrome-history-file (expand-file-name "eaf/browser/Default/History" minemacs-local-dir))
  (eaf-browser-default-search-engine "duckduckgo")
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-aria2-auto-file-renaming t)
  :config
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
        (message "No message at point.")))))


(provide 'me-eaf)
