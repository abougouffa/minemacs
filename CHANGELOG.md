# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## v0.4.0 - 2023-05-27
#### Bug Fixes
- **(citar)** avoid using `all-the-icons` until it is loaded - (960d978) - Abdelhak Bougouffa
- **(code-review)** use fixed fork, unpin closql & forge - (79423c5) - Abdelhak Bougouffa
- **(elfeed)** correct key for +elfeed-download-image - (4b69a74) - DarkBuffalo
- **(flymake)** use custom icons only when suitable - (1e84c48) - Abdelhak Bougouffa
- **(ltex)** use ltex-ls as TCP server, add helpers - (fe290c2) - Abdelhak Bougouffa
#### Documentation
- **(backports)** update function documentation - (c5ab9fe) - Abdelhak Bougouffa
- **(skel)** update comment - (125d82a) - Abdelhak Bougouffa
#### Features
- **(editor)** add header2 support - (0017976) - Abdelhak Bougouffa
- **(elfeed)** add yt-dlp support - (8b8e611) - DarkBuffalo
#### Nitpicks, changes with no side effect
- **(defaults)** add an optional argument - (a02e5cc) - Abdelhak Bougouffa
- **(defaults)** minor edit - (6fc983d) - Abdelhak Bougouffa
- **(evil-collection)** cleanup previous fix - (21ddd4d) - Abdelhak Bougouffa
- **(project)** code formatting - (3827863) - Abdelhak Bougouffa
#### Refactoring
- **(backports)** precise condition - (22a09fa) - Abdelhak Bougouffa
- **(core)** minor edit - (a0861ff) - Abdelhak Bougouffa
- **(ecryptfs)** partial rewrite - (e1e4034) - Abdelhak Bougouffa
- move pcache directory customization to me-defaults - (f10b3cc) - Abdelhak Bougouffa
#### Revert
- **(ltex)** remove server commands - (f02b0a7) - Abdelhak Bougouffa
#### Tweaks
- **(bootstrap)** call build functions interactively - (63b92d4) - Abdelhak Bougouffa
- **(bootstrap)** minor refactor - (cefcdac) - Abdelhak Bougouffa
- **(bootstrap)** do not ask when running build functions - (3d390ea) - Abdelhak Bougouffa
- **(bootstrap)** run build functions on update - (940fdaa) - Abdelhak Bougouffa
- **(clang-format)** add ts modes - (e317bbf) - Abdelhak Bougouffa
- **(compile)** move compilation options to `me-prog` - (f949b46) - Abdelhak Bougouffa
- **(core)** move package updating routines - (5bbfe68) - Abdelhak Bougouffa
- **(core)** update loaddefs - (71eb596) - Abdelhak Bougouffa
- **(dashboard)** add an option to disable it - (ee30640) - Abdelhak Bougouffa
- **(defaults)** set variables with `setopt` - (75fbb43) - Abdelhak Bougouffa
- **(defaults)** better scrolling settings - (f1b4639) - Abdelhak Bougouffa
- **(defaults)** more UI customization - (d7e7f98) - Abdelhak Bougouffa
- **(defaults)** move `password` and `auth-source` to `me-builtin` - (dc3a0da) - Abdelhak Bougouffa
- **(defaults)** correctly set `show-trailing-whitespace` - (997f6b2) - Abdelhak Bougouffa
- **(defaults)** set additional paths - (9e8c7b9) - Abdelhak Bougouffa
- **(eaf)** disable pdf-viewer - (c35356b) - Abdelhak Bougouffa
- **(early-init)** prefer loading newer Elisp files - (9bdf851) - Abdelhak Bougouffa
- **(eglot)** do not assume servers to be executable - (51dd7b8) - Abdelhak Bougouffa
- **(elfeed)** Enhance customizability - (d2f124a) - Abdelhak Bougouffa
- **(highlight-numbers)** enable for `conf-mode`, tweak regexp - (849dba3) - Abdelhak Bougouffa
- **(init)** use `lisp-interaction-mode` for scratch - (5f755e4) - Abdelhak Bougouffa
- **(io)** code refactoring - (c5ef698) - Abdelhak Bougouffa
- **(lexic)** fix local keybindigs - (ab56fb4) - Abdelhak Bougouffa
- **(ltex)** add `tex-mode` - (0e344a8) - Abdelhak Bougouffa
- **(ltex)** minor edits, add documentation - (8e0421c) - Abdelhak Bougouffa
- **(ltex)** use Eglot's server/port syntax - (faec1c5) - Abdelhak Bougouffa
- **(ltex-ls)** additional languages - (ea19fa6) - Abdelhak Bougouffa
- **(macrostep)** remove unneeded hack (fixed upstream) - (0f0b5ef) - Abdelhak Bougouffa
- **(macrostep)** use upstream repo, apply a hack - (c048d3c) - Abdelhak Bougouffa
- **(media)** automatically open Youtube links in MPV - (1fcc323) - Abdelhak Bougouffa
- **(mu4e)** use `nerd-icons` - (5407ab5) - Abdelhak Bougouffa
- **(nerd-icons)** set an icon for matlab/octave files - (972d3a4) - Abdelhak Bougouffa
- **(realgud)** add local binding for treesit modes - (51d8078) - Abdelhak Bougouffa
- **(realgud)** minor tweaks and fixes - (8365877) - Abdelhak Bougouffa
- **(realgud)** define commands - (920ab8d) - Abdelhak Bougouffa
- **(realgud-lldb)** remove unneeded autoload - (689a1da) - Abdelhak Bougouffa
- **(skel)** update the skeleton's modules list - (1e86106) - Abdelhak Bougouffa
- **(tempel)** bind TAB and S-TAB to next/previous - (dff6996) - Abdelhak Bougouffa
- **(ui)** remove unused themes - (d8f4e3b) - Abdelhak Bougouffa
- **(ui)** add `nerd-icons` explicitly - (33e3ffe) - Abdelhak Bougouffa
- **(xclip)** remove useless `+xclip--enable-in-tty-h` - (0481b8e) - Abdelhak Bougouffa
- bump packages versions - (69e9235) - Abdelhak Bougouffa
- use `nerd-icons` instead of `all-the-icons` - (403a3a1) - Abdelhak Bougouffa
- bump packages versions - (6525e40) - Abdelhak Bougouffa
- bump packages versions - (8b90843) - Abdelhak Bougouffa
- bump packages versions - (272bb17) - Abdelhak Bougouffa
- bump packages versions - (bffed94) - Abdelhak Bougouffa
- bump packages versions - (53caf4d) - Abdelhak Bougouffa
- bump packages versions - (c89d5cc) - Abdelhak Bougouffa
- bump packages versions - (693efa0) - Abdelhak Bougouffa
- register package-specific build functions - (ef635c4) - Abdelhak Bougouffa

- - -

## v0.3.0 - 2023-05-03
#### Bug Fixes
- **(aphelia)** adapt to the new upstream changes - (06f7776) - Abdelhak Bougouffa
- **(backports)** fix a bug causing straight to fail (#51) - (ded8596) - Abdelhak Bougouffa
- **(daemon)** avoid running `mu4e` repeatedly - (c784d05) - Abdelhak Bougouffa
- **(dashboard)** do not show when Emacs started with a file - (ebe1a9a) - Abdelhak Bougouffa
- **(io)** fix +html2pdf output file name - (a8d4435) - Abdelhak Bougouffa
- **(meow)** add safety guards to avoid conflict with evil - (db20803) - Abdelhak Bougouffa
- **(straight)** clone the full packages repos - (4f51035) - Abdelhak Bougouffa
- **(straight)** use master on Emacs 28, develop on newer versions - (7f7f33d) - Abdelhak Bougouffa
- **(tempel)** wrong parenthesis disabling corfu in other modes - (1ad3b33) - Abdelhak Bougouffa
- **(use-package)** `:pin-ref` problem on Emacs28 (#49) - (e37b984) - Abdelhak Bougouffa
- **(vc)** pin problematic packages to working versions - (424fd54) - Abdelhak Bougouffa
#### Documentation
- **(readme)** fix broken link - (87247a5) - Abdelhak Bougouffa
#### Features
- **(core)** add `minemacs-update` command - (5692b8d) - Abdelhak Bougouffa
- **(core)** add the `+hook-once!` macro - (1eaa535) - Abdelhak Bougouffa
- **(docs)** initial support for `poly-markdown` - (2460e18) - Abdelhak Bougouffa
- **(io)** add a helper to save URLs to HTML snapshots - (24e03db) - Abdelhak Bougouffa
- **(meow)** WIP optional evil replacement - (954a549) - Abdelhak Bougouffa
- **(modeling)** add `medelica-mode` - (86e8c09) - Abdelhak Bougouffa
- **(prog)** add `hy-mode` - (031b5ba) - Abdelhak Bougouffa
- **(ui)** add all-the-icons for ibuffer - (40d8bb8) - Abdelhak Bougouffa
- **(use-package)** add the `:pin-ref` keyword to use `straight-x` - (cc4f11b) - Abdelhak Bougouffa
#### Miscellaneous Chores
- **(git)** track straight's default pins - (52390b7) - Abdelhak Bougouffa
- provide `make update` - (4d6af7d) - Abdelhak Bougouffa
- save straight's versions when cleaning - (8795bbf) - Abdelhak Bougouffa
#### Nitpicks, changes with no side effect
- **(biblio)** minor edit - (cc742e7) - Abdelhak Bougouffa
- **(core)** simplify a condition - (4a82a91) - Abdelhak Bougouffa
- **(core)** rename a parameter - (4dd3285) - Abdelhak Bougouffa
- **(vc)** remove extra spaces - (b8d9d91) - Abdelhak Bougouffa
- add files headers and footers - (298543e) - Abdelhak Bougouffa
#### Refactoring
- **(mu4e)** remove commented code - (a766768) - Abdelhak Bougouffa
- **(use-package)** small cleanup - (0c3dc30) - Abdelhak Bougouffa
- make use of `+hook-once!` - (43b12bb) - Abdelhak Bougouffa
#### Revert
- **(corfu)** restore in `(org/markdown)-mode` - (be458ac) - Abdelhak Bougouffa
#### Tweaks
- **(bootstrap)** revert to straight's develop branch - (797e115) - Abdelhak Bougouffa
- **(core)** make sure `+emacs-features-p` returns a boolean - (2c194de) - Abdelhak Bougouffa
- **(core)** accept hook symbol in `+hook-once!` - (4a08072) - Abdelhak Bougouffa
- **(core)** fallback to a builtin theme if `minemacs-theme` fails - (48b0b62) - Abdelhak Bougouffa
- **(corfu)** disable in `org-mode` and `markdown-mode` - (9360b69) - Abdelhak Bougouffa
- **(daemon)** ensure keeping `mu4e` alive in background - (2358e3d) - Abdelhak Bougouffa
- **(early-init)** set straight branch to develop - (d9688e1) - Abdelhak Bougouffa
- **(ecryptfs)** better support - (7034976) - Abdelhak Bougouffa
- **(eldoc-box)** remove special case, fixed upstream - (f38adf1) - Abdelhak Bougouffa
- **(email)** refine attachement detection regexp - (7b72d76) - Abdelhak Bougouffa
- **(evil-collection)** restore `mu4e` - (3c60a72) - Abdelhak Bougouffa
- **(gts-translate)** add an option to choose translation langs - (8e4d74f) - Abdelhak Bougouffa
- **(latex)** enable `hs-minor-mode` - (29d22a4) - Abdelhak Bougouffa
- **(latex)** better fontification - (9d31c01) - Abdelhak Bougouffa
- **(latex)** additional tweaks - (ad5adae) - Abdelhak Bougouffa
- **(logview)** set custom files paths - (f817a8f) - Abdelhak Bougouffa
- **(maxima)** use locally installed packages - (024a05e) - Abdelhak Bougouffa
- **(mu4e)** don't ask for the alias when there is only one - (6bda132) - Abdelhak Bougouffa
- **(mu4e)** minor edits - (94a8c1f) - Abdelhak Bougouffa
- **(notes)** better org-roam protocol handling - (9a9748c) - Abdelhak Bougouffa
- **(org)** prefer using `latexmk` or `tectonic` when found - (4e1267d) - Abdelhak Bougouffa
- **(pcache)** create the cache in the cache directory - (28f2c04) - Abdelhak Bougouffa
- **(straight)** update packages - (1dd3044) - Abdelhak Bougouffa
- **(straight)** add pin file - (03e13d2) - Abdelhak Bougouffa
- **(tempel)** restrict `org/markdown` to tempel capf - (439b6aa) - Abdelhak Bougouffa
- **(treemacs)** enable `evil` support - (2176941) - Abdelhak Bougouffa
- bump packages versions - (6a94af1) - Abdelhak Bougouffa
- bump package versions - (8372b89) - Abdelhak Bougouffa
- bump package versions - (0f6a2fd) - Abdelhak Bougouffa
- update loaddefs - (09a9cea) - Abdelhak Bougouffa

- - -

## v0.2.0 - 2023-04-01
#### Bug Fixes
- **(auctex)** require `tex` - (104a41e) - Abdelhak Bougouffa
- **(backports)** add `scratch-buffer` (#41) - (f01f80b) - Abdelhak Bougouffa
- **(compile)** remove accidentally added quote - (69bd4b3) - Abdelhak Bougouffa
- **(consult)** no initial fill when in `minibuffer` #37 - (5c30bcd) - Abdelhak Bougouffa
- **(dashboard)** load after `evil-collection` (#42) - (d3b0976) - Abdelhak Bougouffa
- **(defaults)** setup default hooks early - (720da8c) - Abdelhak Bougouffa
- **(docker)** better handling of `Dockerfile`s - (69544e2) - Abdelhak Bougouffa
- **(ebnf-mode)** fix a typo - (f63014f) - Abdelhak Bougouffa
- **(editor)** temporary disable `unicode-fonts` - (a116b7b) - Abdelhak Bougouffa
- **(eglot-box)** better integration with `tab-bar` and `tool-bar` - (f43d7ff) - Abdelhak Bougouffa
- **(evil)** temporary disable `evil-escape` - (22f9a6d) - Abdelhak Bougouffa
- **(evil)** the right way to use `evil-search` - (a5c61ab) - Abdelhak Bougouffa
- **(evil-mc)** avoid inserting the first `evil-escape` char - (99559a8) - Abdelhak Bougouffa
- **(mu4e)** fixes related to evil-collection - (56533ad) - Abdelhak Bougouffa
- **(mu4e)** mu 1.10 UI and evil fixes - (2398bd0) - Abdelhak Bougouffa
- **(pdf-tools)** make sure to use it to show PDFs - (d8bc950) - Abdelhak Bougouffa
- disable packages causing problems on the last build - (6b2c3ae) - Abdelhak Bougouffa
#### Documentation
- **(skel)** more use cases of `early-config.el` - (b6c2104) - Abdelhak Bougouffa
- add a header image in README - (2130b3c) - Abdelhak Bougouffa
#### Features
- **(dashboard)** add dashboard - (f0d5a10) - Abdelhak Bougouffa
- **(doc-view)** use SVG when available - (b9a7715) - Abdelhak Bougouffa
- **(evil-multiedit)** initial support - (535f2ba) - Abdelhak Bougouffa
- **(lisp)** add support for Clojure (via cider) - (7546f08) - Abdelhak Bougouffa
- **(lisp)** add more geiser backends - (2f95ff1) - Abdelhak Bougouffa
- **(lsp-bridge)** initial support (WIP) - (5820016) - Abdelhak Bougouffa
- **(mermaid)** initial support - (5718db8) - Abdelhak Bougouffa
- add the missing template for `+html2pdf` - (c3c6fd2) - Abdelhak Bougouffa
- add more backends to `+html2pdf` - (8b025d0) - Abdelhak Bougouffa
#### Miscellaneous Chores
- **(changlog)** remove - (8d54ac2) - Abdelhak Bougouffa
- move templates to assets - (4042455) - Abdelhak Bougouffa
- move pictures to `assets` - (2f9809a) - Abdelhak Bougouffa
- add names for the workflows - (1d41152) - Abdelhak Bougouffa
- add `clean_pcache` target in Makefile - (a7e02d1) - Abdelhak Bougouffa
- add the "v" prefix in cocogitto - (0a70fbc) - Abdelhak Bougouffa
- bump cocogitto version to 3.4 - (a1c5ab7) - Abdelhak Bougouffa
#### Nitpicks, changes with no side effect
- **(elisp)** minor edits - (bf3a4e0) - Abdelhak Bougouffa
- **(macrostep)** update recipe - (9054280) - Abdelhak Bougouffa
- **(math)** format recipes - (284c970) - Abdelhak Bougouffa
- **(pdf-tools)** simplify - (427c0c1) - Abdelhak Bougouffa
- **(vc)** remove unneeded package - (6baf7b0) - Abdelhak Bougouffa
- minor edits - (81dc6a6) - Abdelhak Bougouffa
#### Refactoring
- **(robot)** make ROS commands customizable - (c041258) - Abdelhak Bougouffa
- **(writeroom-mode)** hook via `use-package` - (7c0832e) - Abdelhak Bougouffa
- move `transient` to `me-builtin` - (0761b3d) - Abdelhak Bougouffa
- define MinEmacs sub-groups - (a8f563c) - Abdelhak Bougouffa
#### Revert
- restore disabled packages, using emacs@6bf441ff11540 - (6e00f68) - Abdelhak Bougouffa
- replace `writeroom-mode` with simpler config - (4c2255d) - Abdelhak Bougouffa
- replace `yasnippet` with `tempel` - (b1edd7e) - Abdelhak Bougouffa
- remove `lsp-bridge` - (34ce221) - Abdelhak Bougouffa
#### Tweaks
- **(+writing-mode)** increase text scale - (ddfb23a) - Abdelhak Bougouffa
- **(builtin)** pin `map` and `let-alist` - (6eff30b) - Abdelhak Bougouffa
- **(compile)** enable ANSI colors, restore savehist integration - (c701113) - Abdelhak Bougouffa
- **(compile)** add a message on toggle burying buffer - (6c7c28d) - Abdelhak Bougouffa
- **(core)** remove obsolete keybinding aliases - (48c53de) - Abdelhak Bougouffa
- **(core)** define a group for MinEmacs' custom variables - (56c37a8) - Abdelhak Bougouffa
- **(defaults)** set `custom-theme-directory` - (fa621b6) - Abdelhak Bougouffa
- **(defaults)** enable drag and drop of regions - (fb1d75d) - Abdelhak Bougouffa
- **(defaults)** do not use system tooltips - (3adf802) - Abdelhak Bougouffa
- **(doom-modeline)** update the main modeline layout - (f8c367e) - Abdelhak Bougouffa
- **(eaf)** minor edits, start in emacs state - (d818ab6) - Abdelhak Bougouffa
- **(eglot)** disable annoying reporting in echo area - (23af99e) - Abdelhak Bougouffa
- **(ein)** load org-babel the right way - (2a68535) - Abdelhak Bougouffa
- **(init)** simplify the `file-name-handler-alist` hack - (9dd1345) - Abdelhak Bougouffa
- **(latex)** edit keybindings - (f6fbe96) - Abdelhak Bougouffa
- **(mu4e)** disable the new `mu4e-modeline-mode` - (e7af964) - Abdelhak Bougouffa
- **(mu4e)** small UI tweak - (b3588e9) - Abdelhak Bougouffa
- **(mu4e-alert)** better filtering of spams - (0c54543) - Abdelhak Bougouffa
- **(org)** update keybindings - (7675c7a) - Abdelhak Bougouffa
- **(org)** dynamically set latex fragments scale - (26f1fd9) - Abdelhak Bougouffa
- **(straight)** add repo hash to the build directory - (4ad4c3a) - Abdelhak Bougouffa
- **(vars)** decrease default fonts size - (ba61d68) - Abdelhak Bougouffa
- **(window)** wider window for `lexic` - (6d2550c) - Abdelhak Bougouffa
- **(writeroom)** minor edits - (a1bc1a8) - Abdelhak Bougouffa
- make `yasnippet` conf obsolete - (e8025e9) - Abdelhak Bougouffa

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).