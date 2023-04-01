# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

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