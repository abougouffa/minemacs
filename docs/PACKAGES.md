# MinEmacs modules and packages
## `me-ai`
* `llm`: A library abstracting LLM capabilities for Emacs
* `llm-ollama`: `llm` module for integrating with Ollama
* `ellama`: A package for interacting with LLMs from Emacs
* `elisa`: Emacs Lisp Information System Assistant, LLM-based information agent leveraging a Retrieval Augmented Generation (RAG) approach
* `whisper`: Speech-to-Text interface for Emacs using OpenAI's whisper model and whisper.cpp as inference engine

## `me-calendar`
* `org-timeblock`: Schedule your day visually, using timeblocking technique inside Emacs

## `me-checkers`
* `flymake-collection`: Collection of checkers for Flymake
* `flymenu`: Transient menu for Flymake
* `flymake-pmd`: Flymake backend for PMD, the extensible cross-language static code analyzer
* `package-lint-flymake`: A `flymake` integration with the linting library (`package-lint`) for Elisp package metadata

## `me-completion`
* `cape`: Completion at point extensions which can be used in combination with Corfu, Company or the default completion UI
* `corfu`: Corfu enhances in-buffer completion with a small completion popup
* `corfu-popupinfo`: Candidate information popup for Corfu
* `corfu-terminal`: Corfu popup on terminal
* `nerd-icons-corfu`: Icons for Corfu using `nerd-icons`
* `consult`: Consult provides search and navigation commands based on the Emacs completion function `completing-read`
* `consult-dir`: Insert paths into the minibuffer prompt
* `embark`: Choose a command to run based on what is near point, both in minibuffer and in normal buffers
* `embark-consult`: Consult integration for Embark
* `marginalia`: Marginalia (i.e., description) in the minibuffer
* `nerd-icons-completion`: Use nerd-icons for completion
* `orderless`: Emacs completion style that matches multiple regexps in any order
* `vertico`: Vertico provides a performant and minimalistic vertical completion UI based on the default completion system

## `me-daemon`

## `me-debug`
* `dape`: Debug Adapter Protocol for Emacs
* `disaster`: Disassemble C, C++ or Fortran code under cursor
* `rmsbolt`: A compiler output viewer
* `beardbolt`: Compiler Explorer clone (fork of `rmsbolt` optimized for C/C++)
* `objdump-disassemble`: Use "objdump" to display disassembled executable and object files

## `me-docs`
* `pdf-tools`: Emacs support library for PDF files
* `pdfgrep`: PDFGrep is an Emacs module providing "grep" comparable facilities but for PDF files
* `crdt`: Collaborative editing using Conflict-free Replicated Data Types
* `rfc-mode`: An Emacs major mode to read and browse RFC documents

## `me-editor`
* `vundo`: Visualize and navigate the undo tree
* `undo-fu-session`: Persistent undo tree between sessions
* `ws-butler`: Unobtrusively trim extraneous white-space *ONLY* in lines edited
* `dtrt-indent`: Smart guessing the indentation offset originally used in the opened source files
* `wgrep`: Writable grep buffer and apply the changes to files
* `symbol-overlay`: Highlight symbols with keymap-enabled overlays
* `rainbow-delimiters`: Emacs rainbow delimiters mode
* `highlight-numbers`: Highlight numbers in source code
* `selection-highlight-mode`: An Emacs minor mode for highlighting matches to the selection
* `zones`: Zones of text - like multiple regions
* `smartparens`: Minor mode for Emacs that deals with parens pairs and tries to be smart about it
* `expreg`: Your friendly neighborhood expand-region clone
* `drag-stuff`: Drag stuff around in Emacs
* `real-backup`: Perform a backup on each file save, real backup for Emacs!

## `me-emacs-lisp`
* `elisp-plus`: Better Emacs Lisp code viewing
* `parinfer-rust-mode`: Simplifying how you write Lisp
* `macrostep`: Interactive macro-expander for Emacs
* `helpful`: A better Emacs *help* buffer
* `inspector`: Inspection tool for Emacs Lisp objects
* `eros`: Evaluation Result OverlayS for Emacs Lisp
* `relint`: Elisp regexp mistake finder

## `me-email`
* `mu4e`: Emacs Email agent based on the "mu" indexer
* `mu4e-icalendar`: Reply to iCalendar meeting requests
* `varuga`: Send ical calendar invites by email
* `me-mu4e-ui`: My UI tweaks for `mu4e`
* `org-msg`: Global minor mode mixing up `org-mode` and `message-mode` to compose and reply to emails in a Outlook HTML friendly style
* `org-mime`: Send HTML email using Org-mode HTML export (alternative to `org-msg`)
* `mu4e-alert`: Desktop notifications and modeline display for `mu4e`
* `mu4e-crypto`: Encrypt and decrypt mails in `mu4e`

## `me-embedded`
* `embed`: Emacs package with utilities for embedded development with OpenOCD
* `bitbake`: A set of Emacs modes for various Yocto/Bitbake file formats
* `bitbake-ts-mode`: A `treesit`-based Bitbake major mode
* `x86-lookup`: Quickly jump to Intel's x86 documentation from Emacs

## `me-extra`
* `crux`: A Collection of Ridiculously Useful eXtensions for Emacs
* `run-in-dir`: Override the `default-directory` in the next command

## `me-files`
* `dired-hacks`: Collection of useful dired additions
* `disk-usage`: Sort and browse disk usage listings
* `neotree`: A Emacs tree plugin like NerdTree for Vim
* `sr-speedbar`: Same frame speedbar
* `vlf-setup`: View, edit, search and compare very large files in batches, trading memory for processor time
* `guard-lf`: Fast opening of large files
* `sudo-edit`: Utilities for opening files with "sudo"
* `dired-rsync`: Asynchronous "rsync" from `dired`
* `fd-dired`: Same functionality as `find-dired` and `find-grep-dired`, using fd/rg instead
* `ztree`: Directory tree comparison mode for Emacs (inspired by commercial tools like Beyond Compare and Araxis Merge)

## `me-fun`
* `xkcd`: Implementation of an xkcd reader for Emacs
* `speed-type`: Practice touch/speed typing in Emacs
* `wordel`: Play Wordle (a.k.a. Lingo) in Emacs

## `me-god`
* `god-mode`: Global minor mode for entering Emacs commands without modifier keys

## `me-gtd`
* `org-gtd`: GTD workflow with Org mode

## `me-lifestyle`
* `awqat`: Islamic prayer times for Emacs

## `me-math`
* `maxima`: Major modes for writing Maxima code
* `imaxima`: Maxima mode with images
* `ein`: Jupyter notebook client in Emacs
* `code-cells`: Lightweight notebooks in Emacs
* `jupyter`: An interface to communicate with Jupyter kernels
* `julia-mode`: Julia support in Emacs
* `ess`: Emacs Speaks Statistics
* `ess-view`: View R dataframes in a spreadsheet software
* `ess-R-data-view`: Data viewer for GNU R

## `me-media`
* `empv`: An Emacs media player, media library manager, radio player, YouTube frontend
* `ready-player`: An Emacs major mode to open media (audio/video) files like any other file (via `find-file`, `dired`, etc)

## `me-multi-cursors`
* `iedit`: Modify multiple occurrences simultaneously
* `multiple-cursors`: Multiple cursors implementation for Emacs

## `me-natural-langs`
* `jinx`: Just-in-time spell checker based on the Enchanted library
* `lexic`: Fancy Emacs integration with the console version of StarDict
* `reverso`: Emacs client for www.reverso.net for translation, grammar check, context and synonyms search
* `me-eglot-ltex`: Internal package to add support for LTeX-LS specific commands to `eglot`

## `me-notes`
* `denote`: Simple notes for Emacs with an efficient file-naming scheme
* `consult-denote`: Use Consult in tandem with Denote
* `denote-menu`: View and filter Denote files in a tabulated list

## `me-org`
* `org-contrib`: Contributed packages to Org in search for new maintainers
* `engrave-faces`: Convert font-lock faces to other formats
* `ox-hugo`: A carefully crafted Org exporter back-end for Hugo
* `ox-extra`: Convenience functions for Org export
* `org-appear`: Auto-toggle Org elements
* `org-modern`: Modern Org style
* `org-fragtog`: Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
* `org-rich-yank`: Rich text clipboard for org-mode: Paste as a #+BEGIN_SRC block of correct mode, with link to where it came from
* `citar`: Emacs package to quickly find and act on bibliographic references, and edit org, markdown, and latex academic documents
* `citar-embark`: Citar integration with Embark

## `me-prog`
* `treesit-auto`: Automatically manage `treesit` grammars
* `ts-movement`: Move and edit code blocks based on tree-sitter AST
* `treesit-fold`: Tree-sitter based code folding
* `treesitter-context`: A `treesit`-based package to show code context, dim surrouding text, and fold code
* `eglot-x`: Extra non-standard functionalities for Eglot
* `gambol`: Emacs text actions using LSP symbol information
* `combobulate`: Structured editing and navigation in Emacs with Tree-Sitter
* `consult-eglot`: Consult integration with Eglot
* `reformatter`: Define commands which run reformatters on the current Emacs buffer
* `apheleia`: Run code formatter on buffer contents without moving point
* `format-all`: Auto-format source code in many languages with one command
* `quickrun`: Out of the box code execution from editing buffer
* `dumb-jump`: An Emacs "jump to definition" package for 50+ languages
* `xref-union`: Combine multiple Xref backends
* `hl-todo`: Highlight TODO keywords
* `rainbow-mode`: Colorize color names in buffers
* `eglot-booster`: Boost `eglot` using `emacs-lsp-booster` (github.com/blahgeek/emacs-lsp-booster)
* `breadcrumb`: Emacs headerline indication of where you are in a large project
* `devdocs`: Emacs viewer for DevDocs, offline documentation for programming languages and libraries
* `cognitive-complexity`: Show cognitive complexity of code in Emacs 29+ (treesit-based)

## `me-project`
* `otpp`: One tab per project, with unique names - simple implementation of workspaces
* `consult-project-extra`: Consult extension for `project`
* `compile-multi`: Multi target interface to compile
* `compile-multi-embark`: Integration for `compile-multi` and `embark`
* `consult-compile-multi`: Consulting read support for `compile-multi`
* `compile-multi-nerd-icons`: Integration of `compile-multi` with `nerd-icons`
* `projection`: Projectile like project management library built on Emacs' `project`
* `projection-multi`: Projection integration for `compile-multi`
* `projection-multi-embark`: Integration for `projection-multi` and `embark`
* `projection-dape`: Projection integration for `dape`

## `me-robot`
* `ros`: A package to ease the interaction ROS nodes and the development of ROS software
* `rosbag-info`: Show information about ROS bag files in Emacs

## `me-rss`
* `elfeed`: An Emacs RSS web feeds client
* `elfeed-protocol`: Extra `elfeed` protocols to add support for Fever, NewsBlur, Nextcloud/ownCloud News and Tiny Tiny RSS

## `me-search`
* `avy`: Jump to things in Emacs tree-style
* `avy-zap`: Zap to char using `avy`
* `dogears`: Never lose your place in Emacs again
* `gumshoe`: A smart point tracker
* `goto-last-change`: An Emacs package to move point through `buffer-undo-list` positions
* `isearch+`: Extensions to `isearch`
* `isearch-mb`: Control `isearch` from the minibuffer
* `phi-search`: Another incremental search command, compatible with `multiple-cursors`
* `phi-grep`: Interactively-editable recursive "grep" implementation in Elisp
* `deadgrep`: Fast, friendly searching with ripgrep and Emacs
* `affe`: Asynchronous fuzzy finder for Emacs
* `fzf`: An Emacs front-end for "fzf"

## `me-services`
* `jiralib`: Provide connectivity to Jira SOAP/REST services
* `org-jira`: Bring Jira and Org mode together
* `tributary`: Edit Confluence wiki pages in Emacs
* `sx`: Stack Exchange for Emacs
* `webpaste`: Paste text to pastebin-like services

## `me-snippets`
* `yasnippet`: A template system for Emacs
* `yasnippet-capf`: Completion-At-Point Extension for YASnippet
* `yasnippet-snippets`: A collection of yasnippet snippets for many languages
* `doom-snippets`: The Doom Emacs snippets library
* `spdx`: Insert SPDX license header

## `me-tags`
* `ggtags`: Emacs frontend to GNU Global source code tagging system
* `citre`: Ctags IDE on the True Editor!, a superior code reading & auto-completion tool with pluggable backends
* `citre-config`: Apply the default configuration (part of `citre`)
* `xcscope`: Cscope interface for Emacs
* `consult-cscope`: Cscope integration for Emacs' Consult
* `clink`: Clink integration to Emacs
* `rtags`: A client/server indexer for C/C++/Objc[++] with integration for Emacs based on Clang
* `rtags-xref`: RTags backend for `xref`

## `me-tools`
* `ssh-deploy`: github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples
* `incus-tramp`: TRAMP integration for Incus containers
* `app-launcher`: Launch system applications from Emacs
* `emamux`: Manipulate "tmux" from Emacs
* `emacs-everywhere`: System-wide popup Emacs windows for quick edits
* `tldr`: Browse "tldr" pages from Emacs
* `vterm`: Fully-fledged terminal emulator inside Emacs based on "libvterm"
* `multi-vterm`: Managing multiple vterm buffers in Emacs
* `docker`: Manage docker from Emacs
* `systemd`: Major mode for editing systemd units
* `journalctl-mode`: Major mode to view journalctl's output in Emacs
* `logview`: Emacs mode for viewing log files
* `with-editor`: Use the Emacsclient as the "$EDITOR" of child processes
* `envrc`: Buffer-local "direnv" integration for Emacs
* `pet`: Python Executable Tracker
* `add-node-modules-path`: Adds the "node_modules/.bin" directory to the buffer "exec_path"
* `verb`: Organize and send HTTP requests from Emacs' Org mode files
* `impostman`: Import of Postman collections in Emacs (for `verb` and `restclient`)
* `ecryptfs`: Mount/umount eCryptfs private directory from Emacs

## `me-tty`
* `xclip`: Copy&paste GUI clipboard from text terminal

## `me-ui`
* `nerd-icons`: Nerd Font icons for Emacs
* `doom-themes`: A megapack of themes for Emacs
* `ef-themes`: Colourful and legible themes for GNU Emacs
* `doom-modeline`: A fancy and fast mode-line inspired by minimalism design
* `keycast`: Show current command and its key in the mode line
* `enlight`: Highly customizable startup screen for Emacs
* `lacarte`: Execute menu items as commands, with completion
* `mixed-pitch`: Use a variable pitch, keeping fixed pitch where it's sensible
* `page-break-lines`: Display "^L" page breaks as tidy horizontal lines
* `pulsar`: Pulse highlight on demand or after select functions
* `focus`: Dim the font color of text in surrounding sections
* `olivetti`: Minor mode to automatically balance window margins
* `nerd-icons-ibuffer`: Integrate `nerd-icons` with `ibuffer`
* `nerd-icons-multimodal`: Integrate `nerd-icons` with `archive-mode`, `tar-mode`, `dired-mode`, and `ztree`
* `diredfl`: Extra font lock rules for a more colourful `dired`
* `info-colors`: Extra colors for `Info-mode`
* `virtual-format`: Format buffers visually without modification
* `casual`: A collection of opinionated keyboard-driven user interfaces for various built-in Emacs modes
* `casual-avy`: An opinionated `transient` menu for `avy`
* `casual-symbol-overlay`: An opinionated `transient` menu for `symbol-overlay`

## `me-vc`
* `magit`: It's Magit! A Git Porcelain inside Emacs.
* `git-commit`: Edit Git commit messages - part of `magit`
* `magit-todos`: Show source files' TODOs (and FIXMEs, etc) in Magit status buffer
* `magit-iconify`: File icons for Magit based on `nerd-icons`
* `magit-imerge`: Magit extension for "git-imerge"
* `multi-magit`: A set of extensions for `magit` to handle multiple repositories simultaneously
* `closql`: Store EIEIO objects using EmacSQL
* `forge`: Work with Git forges from the comfort of Magit
* `diff-hl`: Emacs package for highlighting uncommitted changes
* `git-timemachine`: Walk through git revisions of a file
* `git-modes`: Emacs major modes for Git configuration files
* `repo`: Running "repo" from Emacs
* `repo-transient`: Transient menus to use some "repo" commands within Magit
* `jujutsushi`: Integrate `vc` and `project` with Jujutsu, a Git-compatible VCS that is both simple and powerful
* `diffview`: View diffs side-by-side in Emacs

## `me-window`
* `ace-window`: Quickly switch windows in Emacs
* `window-purpose`: Manage windows and buffers according to purposes

