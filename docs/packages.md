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

## `me-debug`
* `dape`: Debug Adapter Protocol for Emacs
* `rmsbolt`: A compiler output viewer
* `beardbolt`: Compiler Explorer clone (fork of `rmsbolt` optimized for C/C++)
* `objdump-disassemble`: Use "objdump" to display disassembled executable and object files

## `me-docs`
* `pdf-tools`: Emacs support library for PDF files
* `pdfgrep`: PDFGrep is an Emacs module providing "grep" comparable facilities but for PDF files
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
* `smartparens`: Minor mode for Emacs that deals with parens pairs and tries to be smart about it
* `expreg`: Your friendly neighborhood expand-region clone
* `drag-stuff`: Drag stuff around in Emacs
* `crdt`: Collaborative editing using Conflict-free Replicated Data Types
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
* `me-mu4e-gmail`: My tweaks to use Gmail accounts via `mu4e`
* `me-mu4e-extras`: My extra `mu4e` customizations
* `org-msg`: Global minor mode mixing up `org-mode` and `message-mode` to compose and reply to emails in a Outlook HTML friendly style
* `org-mime`: Send HTML email using Org-mode HTML export (alternative to `org-msg`)
* `mu4e-alert`: Desktop notifications and modeline display for `mu4e`
* `mu4e-crypto`: Encrypt and decrypt mails in `mu4e`

## `me-embedded`
* `embed`: Emacs package with utilities for embedded development with OpenOCD
* `bitbake`: A set of Emacs modes for various Yocto/Bitbake file formats
* `bitbake-ts-mode`: A `treesit`-based Bitbake major mode
* `x86-lookup`: Quickly jump to Intel's x86 documentation from Emacs

## `me-experimental`
* `eglot-inactive-regions`: Highlight inactive code regions with eglot power (mainly C/C++ preprocessor directives)
* `igist`: Work seamlessly with GitHub gists from Emacs

## `me-extra`
* `crux`: A Collection of Ridiculously Useful eXtensions for Emacs
* `run-in-dir`: Override the `default-directory` in the next command
* `pscratch`: Persistent per-project scratch buffers for Emacs

## `me-files`
* `dired-hacks`: Collection of useful dired additions
* `disk-usage`: Sort and browse disk usage listings
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
* `flyspell-correct`: Distraction-free words correction with `flyspell` via `completing-read`
* `lexic`: Fancy Emacs integration with the console version of StarDict
* `reverso`: Emacs client for www.reverso.net for translation, grammar check, context and synonyms search
* `me-eglot-ltex`: Internal package to add support for LTeX-LS specific commands to `eglot`

## `me-nav`
* `avy`: Jump to things in Emacs tree-style
* `avy-zap`: Zap to char using `avy`
* `treesit-jump`: Jump around your source code in emacs using `treesit` and `avy`
* `dogears`: Never lose your place in Emacs again
* `goto-chg`: Go to last change
* `isearch+`: Extensions to `isearch`
* `phi-search`: Another incremental search command, compatible with `multiple-cursors`
* `rg`: Emacs search tool based on "ripgrep"
* `p-search`: Emacs search engine which combines concepts from information retrievial and Bayesian search theory
* `affe`: Asynchronous fuzzy finder for Emacs
* `fzf`: An Emacs front-end for "fzf"

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
* `eglot-booster`: Boost `eglot` using `emacs-lsp-booster` (github.com/blahgeek/emacs-lsp-booster)
* `gambol`: Emacs text actions using LSP symbol information
* `combobulate-setup`: Structured editing and navigation in Emacs with Tree-Sitter
* `consult-eglot`: Consult integration with Eglot
* `apheleia`: Run code formatter on buffer contents without moving point
* `format-all`: Auto-format source code in many languages with one command
* `quickrun`: Out of the box code execution from editing buffer
* `dumb-jump`: An Emacs "jump to definition" package for 50+ languages
* `xref-union`: Combine multiple Xref backends
* `hl-todo`: Highlight TODO keywords
* `breadcrumb`: Emacs headerline indication of where you are in a large project
* `devdocs`: Emacs viewer for DevDocs, offline documentation for programming languages and libraries
* `cocci`: Coccinelle: Complex style-preserving source-to-source transformations

## `me-project`
* `otpp`: One tab per project, with unique names - simple implementation of workspaces
* `compile-multi`: Multi target interface to compile
* `compile-multi-embark`: Integration for `compile-multi` and `embark`
* `consult-compile-multi`: Consulting read support for `compile-multi`
* `compile-multi-nerd-icons`: Integration of `compile-multi` with `nerd-icons`
* `projection`: Projectile like project management library built on Emacs' `project`
* `projection-multi`: Projection integration for `compile-multi`
* `projection-multi-embark`: Integration for `projection-multi` and `embark`
* `projection-dape`: Projection integration for `dape`
* `find-file-in-project`: Quick access to project files using `fd`

## `me-robot`
* `ros`: A package to ease the interaction ROS nodes and the development of ROS software
* `rosbag-info`: Show information about ROS bag files in Emacs

## `me-rss`
* `elfeed`: An Emacs RSS web feeds client
* `elfeed-protocol`: Extra `elfeed` protocols to add support for Fever, NewsBlur, Nextcloud/ownCloud News and Tiny Tiny RSS

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
* `call-graph`: Generate call graph for C/C++ functions

## `me-tools`
* `ssh-deploy`: A deployment plugin via Tramp for Emacs
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
* `page-break-lines`: Display "^L" page breaks as tidy horizontal lines
* `pulsar`: Pulse highlight on demand or after select functions
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
* `magit-imerge`: Magit extension for "git-imerge"
* `multi-magit`: A set of extensions for `magit` to handle multiple repositories simultaneously
* `closql`: Store EIEIO objects using EmacSQL
* `forge`: Work with Git forges from the comfort of Magit
* `lab`: Emacs-GitLab integration
* `diff-hl`: Emacs package for highlighting uncommitted changes
* `git-timemachine`: Walk through Git revisions of a file
* `git-modes`: Emacs major modes for Git configuration files
* `repo`: Running "repo" from Emacs
* `jujutsushi`: Integrate `vc` and `project` with Jujutsu, a Git-compatible VCS that is both simple and powerful
* `diffview`: View diffs side-by-side in Emacs
* `difftastic`: A structural diff that understands syntax

## `me-window`
* `ace-window`: Quickly switch windows in Emacs
* `window-purpose`: Manage windows and buffers according to purposes


# MinEmacs on-demand modules and packages
## `on-demand/me-agda`
* `agda2-mode`: Major mode for the Agda programming language

## `on-demand/me-alloy`
* `alloy-mode`: Emacs major mode for Alloy language

## `on-demand/me-apl`
* `gnu-apl-mode`: Major mode for GNU APL

## `on-demand/me-arduino`
* `arduino-mode`: Major mode for the Arduino language

## `on-demand/me-assembly`
* `mips-mode`: Major mode for MIPS assembly
* `riscv-mode`: Major mode for RISC V assembly
* `fasm-mode`: Major mode for Flat Assembler
* `masm-mode`: Major mode for Microsoft Macro Assembler
* `nasm-mode`: Major mode for Netwide Assembler
* `gas-mode`: Major mode for GNU Assembler

## `on-demand/me-awk`
* `awk-ts-mode`: Major mode for AWK using Tree-sitter

## `on-demand/me-ballerina`
* `ballerina-mode`: A major mode for editing ballerina source code

## `on-demand/me-bqn`
* `bqn-mode`: Major mode for editing BQN grammar files

## `on-demand/me-cc`
* `flymake-cppcheck`: Flymake backend for CppCheck

## `on-demand/me-chapel`
* `chapel-mode`: A major mode for the Chapel programming language

## `on-demand/me-clojure`
* `clojure-mode`: Major mode for Clojure code
* `cider`: Clojure Interactive Development Environment that Rocks

## `on-demand/me-cmake`
* `cmake-mode`: Major mode for editing CMake sources
* `cmake-font-lock`: Advanced, type aware, highlight support for CMake

## `on-demand/me-cobol`
* `cobol-mode`: Major mode for editing COBOL code

## `on-demand/me-coffee`
* `coffee-mode`: Major mode for CoffeeScript code

## `on-demand/me-common-lisp`
* `sly`: Sylvester the Cat's Common Lisp IDE
* `sly-quicklisp`: Quicklisp support for SLY
* `sly-asdf`: ASDF system support for SLY
* `sly-repl-ansi-color`: Add ANSI colors support to the `sly-mrepl`
* `sly-macrostep`: Fancy macro-expansion via `macrostep`

## `on-demand/me-crystal`
* `crystal-mode`: Major mode for editing Crystal files

## `on-demand/me-csv`
* `csv-mode`: Major mode for editing comma/char separated values
* `rainbow-csv`: Highlight CSV and TSV files in different rainbow colors

## `on-demand/me-cuda`
* `cuda-mode`: Major mode for editing Nvidia CUDA C++ files

## `on-demand/me-cue`
* `cue-mode`: Major mode for CUE language files

## `on-demand/me-cypher`
* `cypher-mode`: Major mode for editing Cypher scripts

## `on-demand/me-cython`
* `cython-mode`: Major mode for editing Cython files

## `on-demand/me-d`
* `d-mode`: Major mode for the D programming language

## `on-demand/me-d2`
* `d2-mode`: Major mode for working with D2 graphs
* `ob-d2`: Org Babel code evaluation for the D2 graph lanugage

## `on-demand/me-dart`
* `dart-mode`: Major mode for editing Dart files
* `flutter`: Tools for working with Flutter SDK

## `on-demand/me-demangle`
* `demangle-mode`: Automatically demangle C++, D, and Rust symbols in LLVM code

## `on-demand/me-devicetree`
* `dts-mode`: Major mode for DeviceTree source code
* `virtual-dts-mode`: Major mode for Device Tree Binary (`*.dtb`) files

## `on-demand/me-dhall`
* `dhall-mode`: Major mode for the Dhall configuration language

## `on-demand/me-djvu`
* `djvu`: Edit and view Djvu files via `djvused`
* `djvu3`: Extend `djvu` to display annotations and more

## `on-demand/me-docker`
* `dockerfile-mode`: Major mode for editing Docker's Dockerfiles
* `docker-compose-mode`: Major mode for editing docker-compose files

## `on-demand/me-ebuild`
* `ebuild-mode`: Major mode for editing Gentoo's ebuild and eclass files

## `on-demand/me-elixir`
* `elixir-mode`: Major mode for editing Elixir files
* `ob-elixir`: Org Babel code evaluation for Elixir

## `on-demand/me-elm`
* `elm-mode`: Major mode for Elm
* `elm-test-runner`: Enhanced support for running `elm-test`

## `on-demand/me-epub`
* `nov`: Featureful EPUB reader mode

## `on-demand/me-erlang`
* `erlang`: Major modes for editing and running Erlang files

## `on-demand/me-fish`
* `fish-mode`: Major mode for Fish shell scripts

## `on-demand/me-forth`
* `forth-mode`: Major mode for the Forth programming language

## `on-demand/me-fpga`
* `vhdl-ts-mode`: VHDL Tree-sitter major mode
* `verilog-ts-mode`: Verilog Tree-sitter major mode

## `on-demand/me-franca-idl`
* `franca-idl`: A major mode to edit Franca IDL code

## `on-demand/me-freebasic`
* `fb-mode`: A major mode for the FreeBASIC programming language

## `on-demand/me-fsharp`
* `fsharp-mode`: Support for the F# programming language

## `on-demand/me-gecode`
* `gcode-mode`: Simple G-Code major mode

## `on-demand/me-genexpr`
* `genexpr-mode`: Major mode for editing GenExpr files

## `on-demand/me-gitlab-ci`
* `gitlab-ci-mode`: Mode for editing GitLab CI files

## `on-demand/me-gnuplot`
* `gnuplot`: Major mode and interactive frontend for GNUPlot

## `on-demand/me-godot`
* `gdscript-mode`: Major mode for Godot's GDScript language

## `on-demand/me-graphql`
* `graphql-mode`: Major mode for editing GraphQL schemas

## `on-demand/me-graphviz`
* `graphviz-dot-mode`: Mode for the dot-language used by GraphViz

## `on-demand/me-haskell`
* `haskell-mode`: Major mode for editing Haskell code

## `on-demand/me-haxe`
* `haxe-mode`: Major mode for editing Haxe files

## `on-demand/me-hcl`
* `hcl-mode`: Major mode for Hashicorp Configuration Language (HCL)
* `terraform-mode`: Major mode for Terraform configuration files

## `on-demand/me-hurl`
* `hurl-mode`: Major mode to edit, run and test HTTP requests using Hurl

## `on-demand/me-hy`
* `hy-mode`: Major mode for the Hy programming language
* `ob-hy`: Org Babel code evaluation for the Hy language

## `on-demand/me-idris`
* `idris-mode`: Major mode for the Idris programming language

## `on-demand/me-java`
* `groovy-mode`: Major mode for Groovy source files
* `android-mode`: Minor mode for Android application development

## `on-demand/me-jenkins`
* `jenkinsfile-mode`: Major mode for editing Jenkins declarative pipeline syntax

## `on-demand/me-jira`

## `on-demand/me-json`
* `json-mode`: Major mode for editing JSON files
* `jq-mode`: Major mode for interactively editing jq queries

## `on-demand/me-julia`
* `julia-mode`: Major mode for editing Julia source code
* `julia-ts-mode`: Major mode for Julia source code using Tree-sitter
* `julia-repl`: A minor mode for a Julia REPL
* `julia-snail`: An Emacs development environment for Julia

## `on-demand/me-just`
* `just-mode`: Major mode for editing Justfile
* `justl`: Major mode for driving just files

## `on-demand/me-kotlin`
* `kotlin-mode`: Major mode for the Kotlin programming language
* `kotlin-ts-mode`: Tree-sitter based major mode for the Kotlin programming language

## `on-demand/me-latex`
* `tex`: Integrated environment for TeX
* `latex`: Integrated environment for LaTeX
* `auctex-latexmk`: Add LatexMk support to AUCTeX
* `latex-preview-pane`: Makes LaTeX editing less painful by providing a updatable preview pane

## `on-demand/me-llvm`
* `llvm-ts-mode`: LLVM major mode using Tree-sitter

## `on-demand/me-lua`
* `lua-mode`: Major mode for editing Lua scripts

## `on-demand/me-markdown`
* `markdown-mode`: Major mode for Markdown-formatted text
* `markdown-ts-mode`: Major mode for Markdown using Treesitter

## `on-demand/me-mathematica`
* `wolfram-mode`: Mathematica editing and inferior mode

## `on-demand/me-mercury`
* `metal-mercury-mode`: Concise mercury major mode

## `on-demand/me-mermaid`
* `mermaid-mode`: Major mode for working with Mermaid graphs
* `ob-mermaid`: Org Babel support for Mermaid evaluation

## `on-demand/me-mode-framework`
* `robot-mode`: Major mode for Robot Framework files

## `on-demand/me-modelica`
* `modelica-mode`: Major mode for editing Modelica files

## `on-demand/me-mojo`
* `mojo`: Major mode for the Mojo programming lanugage

## `on-demand/me-nim`
* `nim-mode`: A major mode for the Nim programming language

## `on-demand/me-nix`
* `nix-mode`: Major mode for editing Nix files
* `nix-ts-mode`: Tree-sitter based major mode for editing Nix files

## `on-demand/me-nushell`

## `on-demand/me-ocaml`
* `tuareg`: Major mode and REPL for the OCaml programming language
* `opam-switch-mode`: Select OCaml opam switches via a menu
* `dune`: Integration with the dune build system
* `utop`: Universal toplevel for OCaml

## `on-demand/me-odin`
* `odin-mode`: Major mode for Odin

## `on-demand/me-opencl`
* `opencl-c-mode`: Major mode with for OpenCL kernels

## `on-demand/me-openscad`
* `scad-mode`: A major mode for editing OpenSCAD code

## `on-demand/me-p4`
* `p4-16-mode`: Support for the P4_16 programming language

## `on-demand/me-pandoc`
* `pandoc-mode`: Minor mode for interacting with Pandoc

## `on-demand/me-pcap`
* `pcap-mode`: Major mode for working with PCAP files via Wireshark's `tshark` tool

## `on-demand/me-pdf`
* `pdf-tools`: View and annotate PDF files
* `pdf-isearch`: Isearch support in PDF buffers
* `pdf-view-restore`: Support for opening last known pdf position in `pdf-view-mode`

## `on-demand/me-pkgbuild`
* `pkgbuild-mode`: Edit and run Arch Linux's PKGBUILD recipes

## `on-demand/me-plantuml`
* `plantuml-mode`: Major mode for PlantUML
* `flymake-plantuml`: Add `flymake` support for editing PlantUML files

## `on-demand/me-powershell`
* `powershell`: Mode for editing PowerShell scripts

## `on-demand/me-protobuf`
* `protobuf-mode`: Major mode for editing Protocol Buffers
* `protobuf-ts-mode`: Tree-sitter based major mode for editing Protocol Buffers files

## `on-demand/me-purescript`
* `purescript-mode`: A PureScript editing mode

## `on-demand/me-python`
* `python-docstring`: Smart Python docstring formatting
* `python-pytest`: Helpers to run Python's pytest
* `pip-requirements`: Major mode for editing Python's pip requirements files

## `on-demand/me-qsharp`
* `qsharp-mode`: Major mode for the Q# programming language

## `on-demand/me-qt`
* `qml-mode`: Major mode for editing QT Declarative (QML) code
* `qt-pro-mode`: Major mode for Qt's Pro/Pri files

## `on-demand/me-rust`
* `rust-mode`: Major mode for editing Rust source code

## `on-demand/me-scala`
* `scala-mode`: Major mode for editing Scala
* `sbt-mode`: An Emacs mode for interacting with Scala sbt (Simple build tool) and projects

## `on-demand/me-scheme`
* `racket-mode`: Racket editing, REPL, and more
* `geiser`: Generic Scheme interaction mode with an enhanced REPL and a set of minor modes
* `geiser-chez`: Chez Scheme and Geiser talk to each other
* `geiser-guile`: Guile Scheme and Geiser talk to each other
* `geiser-mit`: MIT Scheme and Geiser talk to each other
* `geiser-racket`: Racket Scheme and Geiser talk to each other
* `macrostep-geiser`: Macrostep for `geiser`
* `flymake-guile`: Guile `flymake` backend

## `on-demand/me-selinux-policy`
* `selinux-policy`: Major mode for editing SELinux TE-RBAC

## `on-demand/me-smalltalk`

## `on-demand/me-sml`
* `sml-mode`: Major mode for editing (Standard) ML

## `on-demand/me-sql`
* `sqlup-mode`: Upcase SQL words for you

## `on-demand/me-stan`
* `stan-mode`: Major mode for editing Stan files
* `eldoc-stan`: Eldoc Eldoc support for Stan functions
* `stan-snippets`: Yasnippets for Stan

## `on-demand/me-swift`
* `swift-mode`: Major-mode for Apple's Swift programming language
* `swift-ts-mode`: Major mode for Swift based on Tree-sitter

## `on-demand/me-textile`
* `textile-mode`: Textile markup editing major mode

## `on-demand/me-toml`
* `toml-mode`: Major mode for editing TOML files

## `on-demand/me-v`
* `v-mode`: Major mode for the V programming language

## `on-demand/me-vala`
* `vala-mode`: Major mode for the Vala programming language
* `vala-snippets`: Yasnippets for Vala

## `on-demand/me-vb`
* `visual-basic-mode`: A mode for editing Visual Basic programs

## `on-demand/me-vimscript`
* `vimrc-mode`: Major mode for vimrc files

## `on-demand/me-web`
* `web-mode`: Major mode for editing web templates
* `emmet-mode`: Support for Emmet, the essential toolkit for web-developers
* `haml-mode`: Major mode for editing Haml files
* `sass-mode`: Major mode for editing Sass files
* `wat-mode`: Major mode for WebAssembly
* `flymake-biome`: Flymake integration for checking JavaScript files using `biome`

## `on-demand/me-wiki`
* `wikitext-mode`: Major mode for editing Wikitexts
* `mediawiki`: MediaWiki frontend

## `on-demand/me-yaml`
* `yaml-mode`: Major mode for editing YAML files
* `yaml-pro`: Parser-aided YAML editing features
* `ansible`: Ansible minor mode

## `on-demand/me-yang`
* `yang-mode`: Major mode for editing YANG files

## `on-demand/me-zig`
* `zig-mode`: Major mode for the Zig programming language
* `zig-ts-mode`: Tree-sitter based major mode for the Zig programming language

