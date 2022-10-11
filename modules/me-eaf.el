;; -*- lexical-binding: t; -*-


(use-package eaf
  :straight (:host github :repo "emacs-eaf/emacs-application-framework" :files (:defaults "*"))
  :unless feat/lucid
  :defer t
  :commands (eaf-install-and-update
             eaf-open)
  :custom
  (eaf-apps-to-install '(browser mindmap jupyter org-previewer
                         markdown-previewer file-sender video-player))
  ;; Generic
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
             ("pdf-viewer" (kbd eaf-evil-leader-key))
             ("image-viewer" (kbd eaf-evil-leader-key))
             ("music-player" (kbd eaf-evil-leader-key))
             ("video-player" (kbd eaf-evil-leader-key))
             ("file-sender" (kbd eaf-evil-leader-key))
             ("mindmap" (kbd eaf-evil-leader-key))
             (_ (kbd "SPC")))
         (kbd "SPC"))))))


(use-package eaf-all-the-icons
  :after eaf
  :config
  (when (display-graphic-p)
    (mapc (lambda (v) (eaf-all-the-icons-icon (car v)))
          eaf-all-the-icons-alist)))


(use-package eaf-org
  :defer t)


(use-package eaf-org-previewer
  :defer t)


(use-package eaf-markdown-previewer
  :defer t)


(use-package eaf-jupyter
  :commands (eaf-open-jupyter)
  :custom
  (eaf-jupyter-font-family (plist-get me-fonts :font-family))
  (eaf-jupyter-font-size 14))


(use-package eaf-mindmap
  :commands (eaf-open-mindmap))


(use-package eaf-file-sender
  :commands (eaf-file-sender-qrcode
             eaf-file-sender-qrcode-in-dired))


(use-package eaf-video-player
  :after eaf
  :custom
  (eaf-video-player-keybinding
   '(("p" . "toggle_play")
     ("q" . "close_buffer")
     ("h" . "play_backward")
     ("l" . "play_forward")
     ("j" . "decrease_volume")
     ("k" . "increase_volume")
     ("f" . "toggle_fullscreen")
     ("R" . "restart"))))


(use-package eaf-browser
  :after eaf
  :commands (eaf-open-browser
             +eaf-open-mail-as-html)
  :general
  (me-map-local :keymaps '(mu4e-headers-mode-map mu4e-view-mode-map)
    "h" '(+eaf-open-mail-as-html :which-key "Open mail as HTML")
    "o" '(eaf-open-browser :which-key "Open URL (EAF)"))
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-dark-mode nil) ;; "follow"
  (eaf-browser-enable-adblocker t)
  (eaf-browser-ignore-history-list '("google.com/search" "file://"))
  (eaf-browser-translate-language "en")
  (eaf-browser-blank-page-url "https://www.duckduckgo.com")
  (eaf-browser-chrome-history-file (expand-file-name "eaf/browser/Default/History" minemacs-var-dir))
  (eaf-browser-default-search-engine "duckduckgo")
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-aria2-auto-file-renaming t)
  :config
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


(use-package eaf-pdf-viewer
  :disabled t
  :after eaf
  (eaf-pdf-show-progress-on-page nil)
  (eaf-pdf-dark-exclude-image t)
  (eaf-pdf-notify-file-changed t)
  :config
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
