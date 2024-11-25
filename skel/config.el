;;; config.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Abdelhak Bougouffa

;; =============================================================================
;; CAVEAT! PLEASE NOTE THAT THIS CONFIG FILE IS JUST AN EXAMPLE OF HOW TO SET
;; SOME OF MINEMACS' FEATURES. IT IS NOT INTENDED TO BE USED AS IT IS UNLESS YOU
;; UNDERSTAND IT ALL. IF YOU USE IT AS IT IS, YOU CAN SET SOME SETTINGS THAT YOU
;; DON'T WANT TO SET!
;; =============================================================================

;; Personal info
(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address (rot13 "nobhtbhssn@srqbencebwrpg.bet"))

;; Set the default GPG key ID, see "gpg --list-secret-keys"
;; (setq-default epa-file-encrypt-to '("XXXX"))

;; Set a theme for MinEmacs, supported themes include these from `doom-themes'
;; or built-in themes
(setq minemacs-theme 'doom-one) ; `doom-one' is a dark theme, `doom-one-light' is the light one

;; MinEmacs defines the variable `minemacs-fonts-plist' that is used by the
;; `+setup-fonts' function. The function checks and enables the first available
;; font from these defined in `minemacs-fonts-plist'. This variable can be
;; customized to set font specs for specific Emacs faces or to enable some
;; language-specific fonts.

;; You can set a list of fonts to be used, like the snippet below. The first
;; font found on the system will be used:
(plist-put minemacs-fonts-plist
           :default ;; <- applies to the `default' face using `custom-theme-set-faces'
           '((:family "Iosevka Fixed Curly Slab" :height 130) ; <- priority 1
             (:family "JetBrains Mono" :height 110 :weight light) ; <- priority 2
             (:family "Cascadia Code" :height 120 :weight semi-light))) ; <- priority 3

;; To set font for arbitrary Emacs face, you need just to write the face name as
;; a keyword. For example `variable-pitch' -> `:variable-pitch':
(plist-put minemacs-fonts-plist
           :variable-pitch ;; <- applies to the `variable-pitch' face using `custom-theme-set-faces'
           '("Lato"
             "Roboto"
             "Inter"
             "Helvetica"))

;; For example to set custom font for `mode-line' -> `:mode-line':
(plist-put minemacs-fonts-plist
           :mode-line ;; <- applies to the `mode-line' face using `custom-theme-set-faces'
           '((:family "Lato" :weight regular)
             (:family "Roboto" :weight light)))

(plist-put minemacs-fonts-plist
           :mode-line-inactive ;; <- applies to the `mode-line-inactive'
           '((:family "Lato" :weight regular)
             (:family "Roboto" :weight light)))

;; You can also setup some language-specific fonts. For example, to use "Amiri"
;; or "KacstOne" for Arabic script (the first to be found). All scripts
;; supported by Emacs can be found in `+known-scripts'. The value of the extra
;; `:prepend' is passed the last argument to `set-fontset-font'. The extra
;; `:scale' parameter can be used to set a scaling factor for the font in Emacs'
;; `face-font-rescale-alist'.
(plist-put minemacs-fonts-plist
           :arabic ;; <- applies to arabic script using `set-fontset-font'
           '((:family "Amiri" :scale 0.9)
             (:family "KacstOne")))

;; Use "LXGW WenKai Mono" for Han (Chinese) script
(plist-put minemacs-fonts-plist
           :han
           '((:family "LXGW WenKai Mono" :scale 1.3)))

;; When `me-daemon' and `me-email' are enabled, MinEmacs will try to start
;; `mu4e' in background at startup. To disable this behavior, you can set
;; `+mu4e-auto-start' to nil here.
;; (setq +mu4e-auto-start nil)

(+deferred!
 ;; Auto enable Eglot in modes `+eglot-auto-enable-modes' using
 ;; `+eglot-auto-enable' (from the `me-prog' module). You can use
 ;; `+lsp-auto-enable' instead to automatically enable LSP mode in supported
 ;; modes (from the `me-lsp' module).
 (+eglot-auto-enable)

 ;; Add `ocaml-mode' to `eglot' auto-enable modes
 (add-to-list '+eglot-auto-enable-modes 'ocaml-mode)

 (with-eval-after-load 'eglot
   ;; You can use this to fill `+eglot-auto-enable-modes' with all supported
   ;; modes from `eglot-server-programs'
   (+eglot-use-on-all-supported-modes eglot-server-programs)))

;; If you installed Emacs from source, you can add the source code
;; directory to enable jumping to symbols defined in Emacs' C code.
;; (setq source-directory "~/Sources/emacs-git/")

;; I use Brave, and never use Chrome, so I replace chrome program with "brave"
(setq browse-url-chrome-program (or (executable-find "brave") (executable-find "chromium")))

;; Install some third-party packages. MinEmacs uses `use-package' and `straight'
;; for package management. It is recommended to use the same to install
;; additional packages. For example, to install `devdocs' you can use something
;; like:
(use-package devdocs
  ;; The installation recipe (from Github)
  :straight (:host github :repo "astoff/devdocs.el" :files ("*.el"))
  ;; Autoload the package when invoking these commands, note that if the
  ;; commands are already autoloaded (defined with `autoload'), this is not
  ;; needed.
  :commands (devdocs-install)
  ;; MinEmacs sets the `use-package-always-defer' to t, so by default, packages
  ;; are deferred to save startup time. If you want to load a package
  ;; immediately, you need to explicitly use `:demand'.
  ;; :demand
  ;; Set some custom variables, using the `:custom' block is recommended over
  ;; using `setq'. This will ensure calling the right setter function if it is
  ;; defined for the custom variable.
  :custom
  (devdocs-data-dir (concat minemacs-local-dir "devdocs/")))

;; Module: `me-tools' -- Package: `vterm'
;; When the libvterm present in the system is too old, you can face VTERM_COLOR
;; related compilation errors. Thil parameter tells `vterm' to download libvterm
;; for you, see the FAQ at: github.com/akermu/emacs-libvterm.
;; (with-eval-after-load 'vterm
;;   (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=Off"))

;; Module: `me-tags' -- Package: `citre'
(with-eval-after-load 'citre
  ;; In case you get "gtags: Objdir not found." when trying to run `citre-global-update-database' in the *citre-gtags* buffer
  (cl-callf2 remove "--objdir" citre-gtags-args))

;; Module: `me-natural-langs' -- Package: `jinx'
(with-eval-after-load 'jinx
  ;; Check spelling for Arabic, English and French by default.
  (setq-default jinx-languages "ar en fr"))

;; The `spell-fu' configuration is obsolete now (in favor of `jinx'). However,
;; you can force MinEmacs to load obsolete configurations by loading them
;; manually. Here is an example of how to force loading an obsolete module, you
;; can do this here or in modules.el.
;; (+load minemacs-obsolete-modules-dir "me-spell-fu.el")
;; (with-eval-after-load 'spell-fu
;;   ;; We can use MinEmacs' helper macro `+spell-fu-register-dictionaries!'
;;   ;; to enable multi-language spell checking.
;;   (+spell-fu-register-dictionaries! "en" "fr"))

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

  (setq +mu4e-auto-bcc-address "always.bcc@this.email") ;; Add an email address always included as BCC

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
                                "/R&D Engineer at Some company/")))
   'default ;; Use it as default in a multi-accounts setting
   'gmail)) ;; This is a Gmail account, store it and treat it accordingly (see `me-mu4e-gmail')

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

;; Module: `me-notes' -- Package: `denote'
(with-eval-after-load 'denote
  (setq denote-directory "~/Org/notes/"))

;; Module: `me-media' -- Package: `empv'
(with-eval-after-load 'empv
  ;; Set the radio channels, you can get streams from radio-browser.info
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
        empv-invidious-instance "https://invidious.projectsegfau.lt/api/v1")) ; See: https://docs.invidious.io/instances

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

;; Module: `me-vc' -- Package: `forge'
(with-eval-after-load 'forge
  ;; To setup private Gitlab instance
  ;; 1. Add this to your ~/.gitconfig
  ;; [gitlab "gitlab.private.com/api/v4"]
  ;;   user = my.username
  ;; 2. Then create an access token on GitLab. I ticked api and
  ;; write_repository, which seems to work fine so far. Put the token in
  ;; ~/.authinfo.gpg:
  ;; machine gitlab.private.com/api/v4 login my.user^forge password <token>
  ;; 3. Use this in your config:
  (add-to-list 'forge-alist '("gitlab.private.com" "gitlab.private.com/api/v4" "gitlab.private.com" forge-gitlab-repository)))

;; Module: `me-services' -- Package: `jiralib' / `org-jira'
;; When `jiralib2' is enabled, do some extra stuff
(when (+package-configured-p 'jiralib)
  ;; You only need to set `jiralib-url'. The `jiralib-host' and `jiralib-user' are optional
  (setq jiralib-url "https://my-jira-server.tld/"
        jiralib-host "my-jira-server.tld"
        jiralib-user "my-username")

  ;; Add a hook on git-commit, so it automatically prompt for a ticket number to
  ;; add to the commit message
  (add-hook
   'git-commit-mode-hook
   (satch-defun +jira-commit-auto-insert-ticket-id-h ()
     (require 'jiralib)
     (when (and jiralib-url jiralib-token
                ;; Do not auto insert if the commit message is not empty (ex. amend)
                (+first-line-empty-p))
       (goto-char (point-min))
       (insert "\n")
       (goto-char (point-min))
       (+jira-insert-ticket-id)
       (insert ": "))))

  ;; Login automatically using credentials from `auth-source'
  ;; You can achieve this by adding this line in your "~/.authinfo.gpg"
  ;; machine my-jira-server.tld login my-username password MY-pAsSwOrD-123
  (with-eval-after-load 'jiralib
    (+jiralib-auto-login)))
