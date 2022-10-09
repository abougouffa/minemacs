;; -*- lexical-binding: t; -*-


(use-package eaf
  :straight (:host github :repo "emacs-eaf/emacs-application-framework" :files (:defaults "*"))
  :unless feat/lucid
  :commands (eaf-open
             eaf-open-browser
             eaf-open-jupyter
             eaf-install-and-update
             +eaf-open-mail-as-html)
  :init
  (setq eaf-apps-to-install
        '(org browser mindmap jupyter org-previewer
          markdown-previewer file-sender video-player))

  (defun +eaf-app-p (app-symbol)
    (memq app-symbol eaf-apps-to-install))

  (when (+eaf-app-p 'browser)
    (unless feat/xwidgets
      ;; Make EAF Browser my default browser
      (setq browse-url-browser-function #'eaf-open-browser))
    (defalias 'browse-web #'eaf-open-browser)
    (me-map-local :keymaps '(mu4e-headers-mode-map mu4e-view-mode-map)
      "h" '(+eaf-open-mail-as-html :which-key "Open mail as HTML")
      "o" '(eaf-open-browser :which-key "Open URL (EAF)")))

  (when (+eaf-app-p 'pdf-viewer)
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

  :config
  ;; Generic
  (setq eaf-start-python-process-when-require t
        eaf-kill-process-after-last-buffer-closed t
        eaf-fullscreen-p nil
        eaf-config-location (expand-file-name "eaf" minemacs-var-dir))

  ;; Debug
  (setq eaf-enable-debug nil)

  ;; Web engine
  (setq eaf-webengine-font-family (plist-get me-fonts :font-family)
        eaf-webengine-fixed-font-family (plist-get me-fonts :font-family)
        eaf-webengine-serif-font-family (plist-get me-fonts :variable-pitch-font-family)
        eaf-webengine-font-size 16
        eaf-webengine-fixed-font-size 16
        eaf-webengine-enable-scrollbar t
        eaf-webengine-scroll-step 200
        eaf-webengine-default-zoom 1.25
        eaf-webengine-show-hover-link t
        eaf-webengine-download-path "~/Downloads"
        eaf-webengine-enable-plugin t
        eaf-webengine-enable-javascript t
        eaf-webengine-enable-javascript-access-clipboard t)

  (when (display-graphic-p)
    (require 'eaf-all-the-icons)
    (mapc (lambda (v) (eaf-all-the-icons-icon (car v)))
          eaf-all-the-icons-alist))

  ;; Browser settings
  (when (+eaf-app-p 'browser)
    (setq eaf-browser-continue-where-left-off t
          eaf-browser-dark-mode nil ;; "follow"
          eaf-browser-enable-adblocker t
          eaf-browser-enable-autofill nil
          eaf-browser-remember-history t
          eaf-browser-ignore-history-list '("google.com/search" "file://")
          eaf-browser-text-selection-color "auto"
          eaf-browser-translate-language "en"
          eaf-browser-blank-page-url "https://www.duckduckgo.com"
          eaf-browser-chrome-history-file (expand-file-name "eaf/browser/Default/History" minemacs-var-dir)
          eaf-browser-default-search-engine "duckduckgo"
          eaf-browser-continue-where-left-off t
          eaf-browser-aria2-auto-file-renaming t)

    (require 'eaf-browser)

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

  ;; File manager settings
  (when (+eaf-app-p 'file-manager)
    (setq eaf-file-manager-show-preview nil
          eaf-find-alternate-file-in-dired t
          eaf-file-manager-show-hidden-file t
          eaf-file-manager-show-icon t)
    (require 'eaf-file-manager))

  ;; File Browser
  (when (+eaf-app-p 'file-browser)
    (require 'eaf-file-browser))

  ;; PDF Viewer settings
  (when (+eaf-app-p 'pdf-viewer)
    (setq eaf-pdf-dark-mode "follow"
          eaf-pdf-show-progress-on-page nil
          eaf-pdf-dark-exclude-image t
          eaf-pdf-notify-file-changed t)
    (require 'eaf-pdf-viewer))

  ;; RSS
  (when (+eaf-app-p 'rss-reader)
    (setq eaf-rss-reader-split-horizontally nil
          eaf-rss-reader-web-page-other-window t)
    (require 'eaf-org))

  ;; Org
  (when (+eaf-app-p 'org)
    (require 'eaf-org))

  ;; Org Previewer
  (when (+eaf-app-p 'org-previewer)
    (setq eaf-org-dark-mode "follow")
    (require 'eaf-org-previewer))

  ;; Markdown Previewer
  (when (+eaf-app-p 'markdown-previewer)
    (setq eaf-markdown-dark-mode "follow")
    (require 'eaf-markdown-previewer))

  ;; Jupyter
  (when (+eaf-app-p 'jupyter)
    (setq eaf-jupyter-dark-mode "follow"
          eaf-jupyter-font-family (plist-get me-fonts :font-family)
          eaf-jupyter-font-size 13)
    (require 'eaf-jupyter))

  ;; Mindmap
  (when (+eaf-app-p 'mindmap)
    (setq eaf-mindmap-dark-mode "follow"
          eaf-mindmap-save-path "~/Documents/")
    (require 'eaf-mindmap))

  ;; File Sender
  (when (+eaf-app-p 'file-sender)
    (require 'eaf-file-sender))

  ;; Music Player
  (when (+eaf-app-p 'music-player)
    (require 'eaf-music-player))

  ;; Video Player
  (when (+eaf-app-p 'video-player)
    (setq eaf-video-player-keybinding
          '(("p" . "toggle_play")
            ("q" . "close_buffer")
            ("h" . "play_backward")
            ("l" . "play_forward")
            ("j" . "decrease_volume")
            ("k" . "increase_volume")
            ("f" . "toggle_fullscreen")
            ("R" . "restart")))
    (require 'eaf-video-player))

  ;; Image Viewer
  (when (+eaf-app-p 'image-viewer)
    (require 'eaf-image-viewer))

  ;; Git
  (when (+eaf-app-p 'git)
    (require 'eaf-git))

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
             ("pdf-viewer" (kbd eaf-evil-leader-key))
             ("image-viewer" (kbd eaf-evil-leader-key))
             ("music-player" (kbd eaf-evil-leader-key))
             ("video-player" (kbd eaf-evil-leader-key))
             ("file-sender" (kbd eaf-evil-leader-key))
             ("mindmap" (kbd eaf-evil-leader-key))
             (_ (kbd "SPC")))
         (kbd "SPC"))))))


(provide 'me-eaf)
