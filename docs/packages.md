# MinEmacs modules and packages
## `me-ai`
* `llm`: A library abstracting LLM capabilities for Emacs
* `llm-ollama`: `llm` module for integrating with Ollama
* `ellama`: A package for interacting with LLMs from Emacs
* `elisa`: Emacs Lisp Information System Assistant, LLM-based information agent leveraging a Retrieval Augmented Generation (RAG) approach
* `aidermacs`: Pair-programming with AI agents using Aider
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
* `nerd-icons-corfu`: Icons for Corfu using `nerd-icons`
* `consult`: Consult provides search and navigation commands based on the Emacs completion function `completing-read`
* `embark`: Choose a command to run based on what is near point, both in minibuffer and in normal buffers
* `embark-consult`: Consult integration for Embark
* `marginalia`: Marginalia (i.e., description) in the minibuffer
* `nerd-icons-completion`: Use nerd-icons for completion
* `orderless`: Emacs completion style that matches multiple regexps in any order
* `vertico`: Vertico provides a performant and minimalistic vertical completion UI based on the default completion system

## `me-debug`
* `dape`: Debug Adapter Protocol for Emacs
* `dape-cortex-debug`: `dape` integration for cortex-debug (https://github.com/Marus/cortex-debug)
* `rmsbolt`: A compiler output viewer
* `beardbolt`: Compiler Explorer clone (fork of `rmsbolt` optimized for C/C++)
* `objdump-disassemble`: Use "objdump" to display disassembled executable and object files

## `me-docs`
* `pdf-tools`: Emacs support library for PDF files
* `pdfgrep`: PDFGrep is an Emacs module providing "grep" comparable facilities but for PDF files
* `rfc-mode`: An Emacs major mode to read and browse RFC documents
* `tldr`: Browse "tldr" pages from Emacs
* `devdocs`: Emacs viewer for DevDocs, offline documentation for programming languages and libraries
* `dash-docs`: Offline documentation browser using Dash/Zeal docsets
* `consult-dash`: Integration of `consult` with `dash-docs`

## `me-editor`
* `vundo`: Visualize and navigate the undo tree
* `undo-fu-session`: Persistent undo tree between sessions
* `iedit`: Modify multiple occurrences simultaneously
* `multiple-cursors`: Multiple cursors implementation for Emacs
* `ws-butler`: Unobtrusively trim extraneous white-space *ONLY* in lines edited
* `dtrt-indent`: Smart guessing the indentation offset originally used in the opened source files
* `combobulate-setup`: Structured editing and navigation in Emacs with Tree-Sitter
* `vim-file-locals`: Parse and respect Vim modeline options (`tab-width`, `fill-column`, etc.)
* `selection-highlight-mode`: An Emacs minor mode for highlighting matches to the selection
* `expreg`: Your friendly neighborhood expand-region clone
* `move-dup`: Eclipse-like moving and duplicating lines or rectangles
* `real-backup`: Perform a backup on each file save, real backup for Emacs!
* `xclip`: Copy&paste GUI clipboard from text terminal

## `me-emacs-lisp`
* `elisp-plus`: Better Emacs Lisp code viewing
* `parinfer-rust-mode`: Simplifying how you write Lisp
* `eros`: Evaluation Result OverlayS for Emacs Lisp

## `me-email`
* `mu4e`: Emacs Email agent based on the "mu" indexer
* `mu4e-icalendar`: Reply to iCalendar meeting requests
* `varuga`: Send ical calendar invites by email
* `me-mu4e-ui`: My UI tweaks for `mu4e`
* `me-mu4e-gmail`: My tweaks to use Gmail accounts via `mu4e`
* `me-mu4e-extras`: My extra `mu4e` customizations
* `org-msg`: Global minor mode mixing up `org-mode` and `message-mode` to compose and reply to emails in a Outlook HTML friendly style
* `mu4e-alert`: Desktop notifications and modeline display for `mu4e`
* `mu4e-crypto`: Encrypt and decrypt mails in `mu4e`

## `me-embedded`
* `embed`: Emacs package with utilities for embedded development with OpenOCD
* `bitbake`: A set of Emacs modes for various Yocto/Bitbake file formats

## `me-experimental`
* `lspce`: LSP Client for Emacs implemented as a module using Rust

## `me-extra`
* `crux`: A Collection of Ridiculously Useful eXtensions for Emacs
* `pscratch`: Persistent per-project scratch buffers for Emacs

## `me-files`
* `dired-hacks`: Collection of useful dired additions
* `dired-rsync`: Asynchronous "rsync" from `dired`
* `fd-dired`: Same functionality as `find-dired` and `find-grep-dired`, using fd/rg instead
* `trashed`: Viewing and editing system trash can
* `disk-usage`: Sort and browse disk usage listings
* `vlf-setup`: View, edit, search and compare very large files in batches, trading memory for processor time
* `guard-lf`: Fast opening of large files
* `ztree`: Directory tree comparison mode for Emacs (inspired by commercial tools like Beyond Compare and Araxis Merge)
* `cascading-dir-locals`: Apply all (!) ".dir-locals.el" from root to current directory

## `me-lifestyle`
* `awqat`: Islamic prayer times for Emacs

## `me-media`
* `empv`: An Emacs media player, media library manager, radio player, YouTube frontend
* `ready-player`: An Emacs major mode to open media (audio/video) files like any other file (via `find-file`, `dired`, etc)

## `me-natural-langs`
* `jinx`: Just-in-time spell checker based on the Enchanted library
* `flyspell-correct`: Distraction-free words correction with `flyspell` via `completing-read`
* `lexic`: Fancy Emacs integration with the console version of StarDict
* `reverso`: Emacs client for Reverso.net for translation, grammar check, context and synonyms search

## `me-nav`
* `ace-window`: Quickly switch windows in Emacs
* `avy`: Jump to things in Emacs tree-style
* `avy-zap`: Zap to char using `avy`
* `dogears`: Never lose your place in Emacs again
* `goto-chg`: Go to last change
* `phi-search`: Another incremental search command, compatible with `multiple-cursors`
* `rg`: Emacs search tool based on "ripgrep"
* `p-search`: Emacs search engine which combines concepts from information retrievial and Bayesian search theory
* `affe`: Asynchronous fuzzy finder for Emacs
* `fzf`: An Emacs front-end for "fzf"

## `me-notes`
* `denote`: Simple notes for Emacs with an efficient file-naming scheme
* `denote-silo`: Convenience functions for working with multiple silos
* `denote-journal`: Convenience functions for daily journaling with Denote
* `denote-sequence`: Sequence notes or Folgezettel with Denote
* `denote-org`: Extensions to better integrate Org with Denote
* `denote-markdown`: Extensions to better integrate Markdown with Denote
* `consult-denote`: Use Consult in tandem with Denote

## `me-org`
* `org-contrib`: Contributed packages to Org in search for new maintainers
* `engrave-faces`: Convert font-lock faces to other formats
* `ox-extra`: Convenience functions for Org export
* `org-appear`: Auto-toggle Org elements
* `org-modern`: Modern Org style
* `org-fragtog`: Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
* `citar`: Emacs package to quickly find and act on bibliographic references, and edit org, markdown, and latex academic documents
* `citar-embark`: Citar integration with Embark

## `me-prog`
* `apheleia`: Run code formatter on buffer contents without moving point
* `dumb-jump`: An Emacs "jump to definition" package for 50+ languages
* `hl-todo`: Highlight TODO keywords
* `macrostep`: Interactive macro-expander for Emacs Lisp and C
* `breadcrumb`: Emacs headerline indication of where you are in a large project
* `simpc-mode`: Simple and fast C mode for amalgamated (big) files
* `cpp-func-impl`: Generate C++ method implementations from declarations using `treesit`

## `me-project`
* `otpp`: One tab per project, with unique names - simple implementation of workspaces
* `compile-multi`: Multi target interface to compile
* `compile-multi-embark`: Integration for `compile-multi` and `embark`
* `consult-compile-multi`: Consulting read support for `compile-multi`
* `compile-multi-nerd-icons`: Integration of `compile-multi` with `nerd-icons`
* `projection`: Projectile like project management library built on Emacs' `project`
* `projection-find`: Projection extension to jump between related files in a project
* `projection-multi`: Projection integration for `compile-multi`
* `projection-multi-embark`: Integration for `projection-multi` and `embark`
* `projection-dape`: Projection integration for `dape`

## `me-rss`
* `elfeed`: An Emacs RSS web feeds client
* `elfeed-protocol`: Extra `elfeed` protocols to add support for Fever, NewsBlur, Nextcloud/ownCloud News and Tiny Tiny RSS

## `me-services`
* `jiralib`: Provide connectivity to Jira SOAP/REST services
* `webpaste`: Paste text to pastebin-like services
* `isgd`: Simply shortening URLs using the is.gd service
* `igist`: Work seamlessly with GitHub gists from Emacs

## `me-snippets`
* `yasnippet`: A template system for Emacs
* `yasnippet-snippets`: A collection of yasnippet snippets for many languages
* `consult-yasnippet`: A consulting-read interface for yasnippet

## `me-tags`
* `citre`: Ctags IDE on the True Editor!, a superior code reading & auto-completion tool with pluggable backends

## `me-tools`
* `ssh-deploy`: A deployment plugin via Tramp for Emacs
* `eat`: Emulate A Terminal, in a region, in a buffer and in Eshell
* `app-launcher`: Launch system applications from Emacs
* `docker`: Manage docker from Emacs
* `devcontainer`: Rudimentary devcontainer support for Emacs
* `journalctl-mode`: Major mode to view journalctl's output in Emacs
* `with-editor`: Use the Emacsclient as the "$EDITOR" of child processes
* `pet`: Python Executable Tracker
* `ecryptfs`: Mount/umount eCryptfs private directory from Emacs

## `me-ui`
* `nerd-icons`: Nerd Font icons for Emacs
* `doom-themes`: A megapack of themes for Emacs
* `doric-themes`: Highly legible minimalist themes with precise typography
* `vim-tab-bar`: Vim-like tab bar
* `minemacs-modeline`: Light, modern and opinionated mode-line for MinEmacs
* `pulsar`: Pulse highlight on demand or after select functions
* `nerd-icons-ibuffer`: Integrate `nerd-icons` with `ibuffer`
* `nerd-icons-multimodal`: Integrate `nerd-icons` with `archive-mode`, `tar-mode`, `dired-mode`, and `ztree`
* `diredfl`: Extra font lock rules for a more colourful `dired`
* `virtual-format`: Format buffers visually without modification
* `casual`: A collection of opinionated keyboard-driven user interfaces for various built-in Emacs modes
* `casual-avy`: An opinionated `transient` menu for `avy`
* `ligature`: Display typographical ligatures in major modes
* `easysession`: Effortlessly persist and restore your Emacs sessions

## `me-vc`
* `magit`: It's Magit! A Git Porcelain inside Emacs.
* `git-commit`: Edit Git commit messages - part of `magit`
* `magit-imerge`: Magit extension for "git-imerge"
* `magit-delta`: Use delta when viewing diffs in `magit`
* `magit-gerrit`: Gerrit integration with Magit
* `closql`: Store EIEIO objects using EmacSQL
* `forge`: Work with Git forges from the comfort of Magit
* `diff-hl`: Emacs package for highlighting uncommitted changes
* `git-timemachine`: Walk through Git revisions of a file
* `git-modes`: Emacs major modes for Git configuration files
* `repo`: Running "repo" from Emacs
* `vc-jj`: Jujutsu (jj) integration with Emacs `vc` and `project`
* `diffview`: View diffs side-by-side in Emacs
* `difftastic`: A structural diff that understands syntax


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
* `arm-mode`: Major mode for editing Advanced RISC Machine (a.k.a. ARM) assembly code
* `x86-lookup`: Quickly jump to Intel's x86 documentation from Emacs

## `on-demand/me-awk`
* `awk-ts-mode`: Major mode for AWK using Tree-sitter

## `on-demand/me-ballerina`
* `ballerina-mode`: A major mode for editing ballerina source code

## `on-demand/me-bazel`

## `on-demand/me-bqn`
* `bqn-mode`: Major mode for editing BQN grammar files

## `on-demand/me-c3`

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

## `on-demand/me-cron`
* `crontab-mode`: Major mode for crontab

## `on-demand/me-crystal`
* `crystal-mode`: Major mode for editing Crystal files

## `on-demand/me-csv`
* `csv-mode`: Major mode for editing comma/char separated values
* `rainbow-csv`: Highlight CSV and TSV files in different rainbow colors
* `eplot`: Interactively generate time series charts, plots and bar charts

## `on-demand/me-cuda`
* `cuda-mode`: Major mode for editing Nvidia CUDA C++ files
* `cuda-ts-mode`: CUDA mode based on tree-sitter

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
* `dart-ts-mode`: A major mode for Dart programming language with tree-sitter supports
* `flutter`: Tools for working with Flutter SDK

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
* `apptainer-mode`: Major mode for Apptainer definition files

## `on-demand/me-dotnet`
* `sln-mode`: A major mode to edit Visual Studio's solution files `*.sln`
* `csproj-mode`: Work with .NET project files (csproj, vbproj, fsproj, vdproj, vcxproj)
* `dotnet`: Interact with dotnet CLI tool
* `sharper`: A dotnet CLI wrapper, using Transient
* `vbnet-mode`: A mode for editing Visual Basic .NET code

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
* `erlang-flymake`: Integrate `erlang` with `flymake`

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

## `on-demand/me-gitlab`
* `gitlab-ci-mode`: Mode for editing GitLab CI files
* `lab`: Emacs-GitLab integration

## `on-demand/me-gnuplot`
* `gnuplot`: Major mode and interactive frontend for GNUPlot

## `on-demand/me-godot`
* `gdscript-mode`: Major mode for Godot's GDScript language

## `on-demand/me-graphql`
* `graphql-mode`: Major mode for editing GraphQL schemas

## `on-demand/me-graphviz`
* `graphviz-dot-mode`: Mode for the dot-language used by GraphViz

## `on-demand/me-hare`
* `hare-mode`: Hare programming lanugage mode
* `hare-ts-mode`: Tree-sitter based mode for Hare

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

## `on-demand/me-javascript`
* `add-node-modules-path`: Adds the "node_modules/.bin" directory to the buffer "exec_path"

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

## `on-demand/me-jupyter`
* `ein`: Jupyter notebook client in Emacs
* `jupyter`: An interface to communicate with Jupyter kernels

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
* `xenops`: An editing environment for LaTeX mathematical documents

## `on-demand/me-linux`
* `kconfig-mode`: Major mode for editing Kconfig files
* `cocci`: Coccinelle: Complex style-preserving source-to-source transformations

## `on-demand/me-llvm`
* `llvm-ts-mode`: LLVM major mode using Tree-sitter
* `demangle-mode`: Automatically demangle C++, D, and Rust symbols in LLVM code

## `on-demand/me-logs`
* `logview`: Emacs mode for viewing log files

## `on-demand/me-lua`
* `lua-mode`: Major mode for editing Lua scripts

## `on-demand/me-markdown`
* `markdown-mode`: Major mode for Markdown-formatted text

## `on-demand/me-mathematica`
* `wolfram-mode`: Mathematica editing and inferior mode

## `on-demand/me-maxima`
* `maxima`: Major modes for writing Maxima code
* `imaxima`: Maxima mode with images

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
* `dune-watch`: Integration with dune --watch tasks
* `dune-flymake`: Integrate `dune` with `flymake`
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
* `rustic`: Rust development environment

## `on-demand/me-scala`
* `scala-mode`: Major mode for editing Scala
* `sbt-mode`: An Emacs mode for interacting with Scala sbt (Simple build tool) and projects

## `on-demand/me-scallop`
* `scallop-mode`: Major mode for editing Scallop programming language

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
* `flymake-sqlfluff`: Flymake backend for sqlfluff

## `on-demand/me-stan`
* `stan-mode`: Major mode for editing Stan files
* `eldoc-stan`: Eldoc Eldoc support for Stan functions
* `stan-snippets`: Yasnippets for Stan

## `on-demand/me-statistics`
* `ess`: Emacs Speaks Statistics
* `ess-view`: View R dataframes in a spreadsheet software
* `ess-R-data-view`: Data viewer for GNU R

## `on-demand/me-swift`
* `swift-mode`: Major-mode for Apple's Swift programming language
* `swift-ts-mode`: Major mode for Swift based on Tree-sitter

## `on-demand/me-systemd`
* `systemd`: Major mode for editing systemd units

## `on-demand/me-textile`
* `textile-mode`: Textile markup editing major mode

## `on-demand/me-toml`
* `toml-mode`: Major mode for editing TOML files

## `on-demand/me-typst`
* `typst-ts-mode`: Typst tree sitter major mode for Emacs
* `typst-preview`: Typst live preview minor mode
* `ox-typst`: Typst back-end for Org export engine

## `on-demand/me-v`
* `v-mode`: Major mode for the V programming language

## `on-demand/me-vala`
* `vala-mode`: Major mode for the Vala programming language
* `vala-snippets`: Yasnippets for Vala

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

