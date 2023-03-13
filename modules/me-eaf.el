;;; me-eaf.el --- EAF applications -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(unless (+emacs-features-p 'lucid)
  (use-package eaf
    :straight (:host github :repo "emacs-eaf/emacs-application-framework" :files (:defaults "*"))
    :init
    (+map!
      "oo" '(eaf-open :wk "Open with EAF"))
    (with-eval-after-load 'mu4e
      (+map-local!
        :keymaps '(mu4e-headers-mode-map mu4e-view-mode-map)
        "h" '(+eaf-open-mail-as-html :wk "Open mail as HTML")
        "o" '(eaf-open-browser :wy "Open URL (EAF)")))
    :commands eaf-file-sender-qrcode-in-dired +eaf-open-mail-as-html +browse-url-eaf
    :custom
    ;; Generic
    (eaf-apps-to-install
     '(browser mindmap jupyter org-previewer pdf-viewer system-monitor
       markdown-previewer file-sender video-player))
    (eaf-start-python-process-when-require t)
    (eaf-kill-process-after-last-buffer-closed t)
    (eaf-fullscreen-p nil)
    (eaf-config-location (concat minemacs-local-dir "eaf/"))
    ;; Debug
    (eaf-enable-debug nil)
    ;; Web engine
    (eaf-webengine-font-family (plist-get minemacs-fonts :font-family))
    (eaf-webengine-fixed-font-family (plist-get minemacs-fonts :font-family))
    (eaf-webengine-serif-font-family (plist-get minemacs-fonts :variable-pitch-font-family))
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
    ;; Web browser
    (eaf-browser-continue-where-left-off t)
    (eaf-browser-enable-adblocker t)
    (eaf-browser-ignore-history-list '("google.com/search" "file://"))
    (eaf-browser-translate-language "en")
    (eaf-browser-blank-page-url "https://www.duckduckgo.com")
    (eaf-browser-chrome-history-file (concat minemacs-local-dir "eaf/browser/chrome-history"))
    (eaf-browser-default-search-engine "duckduckgo")
    (eaf-browser-continue-where-left-off t)
    (eaf-browser-aria2-auto-file-renaming t)
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
    ;; Jupyter
    (eaf-jupyter-font-family (plist-get minemacs-fonts :font-family))
    (eaf-jupyter-font-size 14)
    ;; PDF viewer
    (eaf-pdf-outline-buffer-indent 2)
    :config
    ;; Try to load enabled apps, and install them if they aren't installed
    (let (not-installed-apps)
      (dolist (app eaf-apps-to-install)
        (unless (require (intern (format "eaf-%s" app)) nil t)
          (push app not-installed-apps)))
      (when not-installed-apps
        (warn "Some apps are not installed: %s" not-installed-apps)))

    (defun +browse-url-eaf (url &rest args)
      "Open URL in EAF Browser."
      (interactive (browse-url-interactive-arg "URL: "))
      (setq url (browse-url-encode-url url))
      (eaf-open-browser url args))

    (defun +eaf-open-mail-as-html ()
      "Open the HTML mail in EAF Browser."
      (interactive)
      (if-let ((msg (mu4e-message-at-point t))
               ;; Bind browse-url-browser-function locally, so it works
               ;; even if EAF Browser is not set as a default browser.
               (browse-url-browser-function #'eaf-open-browser))
          (mu4e-action-view-in-browser msg)
        (message "No message at point.")))))


(provide 'me-eaf)
