;;; config.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;; Personal info
(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address "abougouffa@fedoraproject.org")

;; Set the default GPG key ID, see "gpg --list-secret-keys"
;; (setq-default epa-file-encrypt-to '("XXXX"))

(setq
 ;; Set fonts
 minemacs-fonts ;; or Cascadia Code, Fira Code, FiraCode Nerd Font, Iosevka Fixed Curly Slab
 '(:font-family "Iosevka Fixed Curly Slab"
   :font-size 16
   :variable-pitch-font-family "IBM Plex Serif"
   :variable-pitch-font-size 16)

 ;; Set a theme for MinEmacs, supported themes include these from `doom-themes'
 ;; and `modus-themes'.
 minemacs-theme 'doom-one)

;; If you installed Emacs from source, you can add the source code
;; directory to enable jumping to symbols defined in Emacs' C code.
;; (setq source-directory "~/Sources/emacs-git/")

;; I use Brave, and never use Chrome, so I replace chrome program with "brave"
;; (setq browse-url-chrome-program (or (executable-find "brave") (executable-find "chromium")))

;; Module: `me-natural-langs' -- Package: `spell-fu'
(with-eval-after-load 'spell-fu
  ;; We can use MinEmacs' helper macro `+spell-fu-register-dictionaries'
  ;; to enable multi-language spell checking.
  (+spell-fu-register-dictionaries "en" "fr"))

;; Module: `me-rss' -- Package: `elfeed'
(with-eval-after-load 'elfeed
  ;; Add news feeds for `elfeed'
  (setq elfeed-feeds
        '("https://itsfoss.com/feed"
          "https://lwn.net/headlines/rss"
          "https://linuxhandbook.com/feed"
          "https://www.omgubuntu.co.uk/feed"
          "https://this-week-in-rust.org/rss.xml"
          "https://planet.emacslife.com/atom.xml")))

;; Module: `me-email' -- Package: `mu4e'
(with-eval-after-load 'mu4e
  ;; Load personal aliases, a file containing aliases, for example:
  ;; alias gmail "Firstname Lastname <some.user.name@gmail.com>"
  ;; alias work  "Firstname Lastname <some.user.name@work.com>"

  ;; (setq mail-personal-alias-file (concat minemacs-config-dir "private/mail-aliases.mailrc"))

  (setq +mu4e-auto-bcc-address "always.bcc@this.email" ;; Add an email address always included as BCC
        +mu4e-gmail-accounts '(("account1@gmail.com" . "/gmail")
                               ("account@somesite.org" . "/gmail")))

  ;; Register email accounts with mu4e
  ;; Use MinEmacs' `+mu4e-register-account' helper function to register multiple accounts
  (+mu4e-register-account
   "Google mail" ;; Account name
   "gmail" ;; Maildir
   `((user-mail-address     . "account1@gmail.com")
     (mu4e-sent-folder      . "/gmail/Sent Mail")
     (mu4e-drafts-folder    . "/gmail/Drafts")
     (mu4e-trash-folder     . "/gmail/Trash")
     ;; These settings aren't mandatory if a `msmtp' config is used.
     (smtpmail-smtp-server  . "smtp.googlemail.com")
     (smtpmail-smtp-service . 587)
     ;; Define account aliases
     (+mu4e-account-aliases . ("account1-alias@somesite.org"
                               "account1-alias@othersite.org"))
     ;; Org-msg greeting and signature
     (org-msg-greeting-fmt  . "Hi%s,")
     ;; Generate signature
     (org-msg-signature     . ,(+org-msg-make-signature
                                "Regards," ;; Closing phrase
                                "Firstname" ;; First name
                                "Lastname" ;; Last name
                                "/R&D Engineer at Some company/")))))

;; Module: `me-org' -- Package: `org'
(with-eval-after-load 'org
  ;; Set Org-mode directory
  (setq org-directory "~/Org/" ; let's put files here
        org-default-notes-file (concat org-directory "inbox.org"))
  ;; Customize Org stuff
  ;; (setq org-todo-keywords
  ;;       '((sequence "IDEA(i)" "TODO(t)" "NEXT(n)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
  ;;         (sequence "[ ](T)" "[-](S)" "|" "[X](D)")
  ;;         (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

  (setq org-export-headline-levels 5)

  ;; Your Org files to include in the agenda
  (setq org-agenda-files
        (mapcar
         (lambda (f) (concat org-directory f))
         '("inbox.org"
           "agenda.org"
           "projects.org"))))

;; Module: `me-notes' -- Package: `org-roam'
(with-eval-after-load 'org-roam
  (setq org-roam-directory (concat org-directory "slip-box/")
        org-roam-db-location (concat org-roam-directory "org-roam.db"))

  ;; Register capture template (via Org-Protocol)
  ;; Add this as bookmarklet in your browser
  ;; javascript:location.href='org-protocol://roam-ref?template=r&ref=%27+encodeURIComponent(location.href)+%27&title=%27+encodeURIComponent(document.title)+%27&body=%27+encodeURIComponent(window.getSelection())
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :if-new (file+head "web/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+created: %U\n\n${body}\n")
           :unnarrowed t))))

;; Module: `me-media' -- Package: `empv'
(with-eval-after-load 'empv
  ;; Set the radio channels, you can get streams from https://www.radio-browser.info
  (setq empv-radio-channels
        '(("El-Bahdja FM" . "http://webradio.tda.dz:8001/ElBahdja_64K.mp3")
          ("El-Chaabia" . "https://radio-dzair.net/proxy/chaabia?mp=/stream")
          ("Quran Radio" . "http://stream.radiojar.com/0tpy1h0kxtzuv")
          ("Algeria International" . "https://webradio.tda.dz/Internationale_64K.mp3")
          ("JOW Radio" . "https://str0.creacast.com/jowradio")
          ("Europe1" . "http://ais-live.cloud-services.paris:8000/europe1.mp3")
          ("France Iter" . "http://direct.franceinter.fr/live/franceinter-hifi.aac")
          ("France Info" . "http://direct.franceinfo.fr/live/franceinfo-hifi.aac")
          ("France Culture" . "http://icecast.radiofrance.fr/franceculture-hifi.aac")
          ("France Musique" . "http://icecast.radiofrance.fr/francemusique-hifi.aac")
          ("FIP" . "http://icecast.radiofrance.fr/fip-hifi.aac")
          ("Beur FM" . "http://broadcast.infomaniak.ch/beurfm-high.aac")
          ("Skyrock" . "http://icecast.skyrock.net/s/natio_mp3_128k"))
        ;; See https://docs.invidious.io/instances/
        empv-invidious-instance "https://invidious.projectsegfau.lt/api/v1"))

;; Module: `me-ros' -- Package: `ros'
(with-eval-after-load 'ros
  (setq ros-workspaces
        (list
         (ros-dump-workspace
          :tramp-prefix "/docker:ros@ros-machine:"
          :workspace "~/ros_ws"
          :extends '("/opt/ros/noetic/"))
         (ros-dump-workspace
          :tramp-prefix "/docker:ros@ros-machine:"
          :workspace "~/ros2_ws"
          :extends '("/opt/ros/foxy/")))))
