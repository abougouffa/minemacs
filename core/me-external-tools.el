;;; me-external-tools.el - External tools

(defvar
  minemacs-external-dependencies
  '((:tool file
     :link "https://darwinsys.com/file"
     :desc "A tool to determine file types")
    (:tool (mbsync isync)
     :link "https://isync.sourceforge.io"
     :desc "Free IMAP and MailDir mailbox synchronizer")
    (:tool mu
     :link "https://github.com/djcb/mu"
     :desc "Maildir indexer/searcher + Emacs mail client + Guile bindings")
    (:tool msmtp
     :link "https://github.com/marlam/msmtp"
     :desc "SMTP client with sendmail compatible interface")
    (:tool (objdump binutils)
     :link "https://en.wikipedia.org/wiki/Objdump"
     :desc "A tool to display information from object files")
    (:tool (gcc g++)
     :link "https://gcc.gnu.org"
     :desc "The GNU Compiler for C/C++")
    (:tool single-file
     :link "https://github.com/gildas-lormeau/single-file-cli"
     :desc "CLI tool for saving a faithful copy of a complete web page in a single HTML file")
    (:tool jq
     :link "https://github.com/jqlang/jq"
     :desc "A lightweight and flexible command-line JSON processor")
    (:tool yq
     :link "https://github.com/mikefarah/yq"
     :desc "A portable command-line YAML, JSON, XML, CSV, TOML and properties processor")
    (:tool xmllint
     :link "https://github.com/GNOME/libxml2"
     :desc "Parse the XML files and output the result of the parsing")
    (:tool black
     :link "https://github.com/psf/black"
     :desc "The uncompromising code formatter for Python")
    (:tool shfmt
     :link "https://github.com/mvdan/sh"
     :desc "A tool to format shell programs")
    (:tool clang-format
     :link "https://clang.llvm.org/docs/ClangFormat.html"
     :desc "A tool to format C/C++/Java/JavaScript/JSON/Objective-C/Protobuf/C# code")
    (:tool (opengrok clj-opengrok)
     :link "https://github.com/youngker/clj-opengrok"
     :desc "A fast and usable source code search and cross reference engine")
    (:tool rtags
     :link "https://github.com/Andersbakken/rtags"
     :desc "A client/server indexer for C/C++/ObjC[++] with integration for Emacs based on Clang")
    (:tool cscope
     :link "https://cscope.sourceforge.net"
     :desc "Interactively examine a C program")
    (:tool ctags
     :link "https://github.com/universal-ctags/ctags"
     :desc "Universal Ctags (abbreviated as u-ctags) is a maintained implementation of ctags")
    (:tool gawk
     :link "https://www.gnu.org/software/gawk"
     :desc "A domain-specific language designed for text processing")
    (:tool emacs-lsp-booster
     :link "https://github.com/blahgeek/emacs-lsp-booster"
     :desc "Emacs LSP performance booster")
    (:tool cmake
     :link "https://github.com/Kitware/CMake"
     :desc "A Powerful Software Build System")
    (:tool make
     :link "https://www.gnu.org/software/make"
     :desc "GNU Make utility to maintain groups of programs")
    (:tool ipdb
     :link "https://github.com/gotcha/ipdb"
     :desc "Integration of IPython pdb")
    (:tool lldb
     :link "https://lldb.llvm.org"
     :desc "The LLDB Debugger")
    (:tool zshdb
     :link "https://github.com/rocky/zshdb"
     :desc "GDB-like \"trepan\" debugger for Zsh")
    (:tool bashdb
     :link "https://bashdb.sourceforge.net"
     :desc "The Bash Debugger")
    (:tool pdb
     :link "https://docs.python.org/3/library/pdb.html"
     :desc "The Python Debugger")
    (:tool gdb
     :link "https://www.sourceware.org/gdb"
     :desc "The GNU Debugger")
    (:tool java-debug
     :link "https://github.com/microsoft/java-debug"
     :desc "The debug server implementation for Java")
    (:tool js-debug
     :link "https://github.com/microsoft/vscode-js-debug"
     :desc "The VS Code JavaScript debugger")
    (:tool codelldb
     :link "https://github.com/vadimcn/codelldb"
     :desc "A native debugger extension for VSCode based on LLDB")
    (:tool cpptools
     :link "https://github.com/microsoft/vscode-cpptools"
     :desc "Microsoft official C/C++ extension for VS Code, DAP support for C/C++")
    (:tool code-debug
     :link "https://github.com/WebFreak001/code-debug"
     :desc "Native debugging for VSCode, DAP implementation")
    (:tool debugpy
     :link "https://github.com/microsoft/debugpy"
     :desc "An implementation of the Debug Adapter Protocol for Python")
    (:tool (rustc cargo rustup)
     :link "https://github.com/rust-lang/rust"
     :desc "Rust toolchain")
    (:tool pyenv
     :link "https://github.com/pyenv/pyenv"
     :desc "Simple Python version management")
    (:tool virtualenv
     :link "https://docs.python.org/3/library/venv.html"
     :desc "Python module for creating lightweight virtual environments")
    (:tool grep
     :link "https://www.gnu.org/software/grep/manual/grep.html"
     :desc "Prints lines that contain a match for one or more patterns")
    (:tool (find findutils)
     :link "https://www.gnu.org/software/findutils"
     :desc "Search for files in a directory hierarchy")
    (:tool ripgrep
     :link "https://github.com/BurntSushi/ripgrep"
     :desc "Recursively search directories for a regex pattern while respecting your gitignore")
    (:tool fd
     :link "https://github.com/sharkdp/fd"
     :desc "A simple, fast and user-friendly alternative to 'find'")
    (:tool git
     :link "https://github.com/git/git"
     :desc "The stupid content tracker")
    (:tool python
     :link "https://python.org"
     :desc "The Python interpreter")
    (:tool pyright
     :link "https://github.com/microsoft/pyright"
     :desc "LSP server for Python")
    (:tool ccls
     :link "https://github.com/MaskRay/ccls"
     :desc "C/C++/Objective-C language server")
    (:tool clangd
     :link "https://clangd.llvm.org"
     :desc "A C/C++ language server that provides IDE-like features to editors")
    (:tool clang
     :link "https://clang.llvm.org"
     :desc "A C language family frontend for LLVM")
    (:tool luac
     :link "https://www.lua.org"
     :desc "Lua compiler")
    (:tool rust-analyzer
     :link "https://github.com/rust-lang/rust-analyzer"
     :desc "A Rust compiler front-end for IDEs, aka. Rust LSP")
    (:tool yaml-language-server
     :link "https://github.com/redhat-developer/yaml-language-server"
     :desc "Language Server for YAML files")
    (:tool lemminx
     :link "https://github.com/eclipse/lemminx"
     :desc "Eclipse XML language server")
    (:tool bash-language-server
     :link "https://github.com/bash-lsp/bash-language-server"
     :desc "LSP for Bash/Shell")
    (:tool ltex-ls
     :link "https://github.com/valentjn/ltex-ls"
     :desc "LTeX Language Server based on Language Tool")
    (:tool sqlfluff
     :link "https://github.com/sqlfluff/sqlfluff"
     :desc "SQLFluff is a modular SQL linter for humans")
    (:tool sqlformat
     :link "https://github.com/andialbrecht/sqlparse"
     :desc "SQL formatter based on sqlparse")
    (:tool pg_format
     :link "https://github.com/darold/pgFormatter"
     :desc "A PostgreSQL SQL syntax beautifier that can work as a console program or as a CGI")
    (:tool docker
     :link "https://www.docker.com"
     :desc "A self-sufficient runtime for containers")
    (:tool bear
     :link "https://github.com/rizsotto/Bear"
     :desc "A tool that generates a compilation database for Clang tooling")
    (:tool clink
     :link "https://github.com/Smattr/clink"
     :desc "A modern re-implementation of Cscope based on Clang")
    (:tool autoconf
     :link "https://www.gnu.org/software/autoconf"
     :desc "Generate configuration script")
    (:tool aspell
     :link "https://github.com/GNUAspell/aspell"
     :desc "Interactive spell checker")
    (:tool enchant-2
     :link "https://github.com/AbiWord/enchant"
     :desc "A generic spell checker")
    (:tool codespell
     :link "https://github.com/codespell-project/codespell"
     :desc "Check code for common misspellings")
    (:tool bandit
     :link "https://github.com/pycqa/bandit"
     :desc "A tool designed to find common security issues in Python code")
    (:tool guild
     :link "https://www.gnu.org/software/guile"
     :desc "The command-line interface to Guile’s compiler and utilities")
    (:tool pmd
     :link "https://github.com/pmd/pmd"
     :desc "An extensible cross-language static code analyzer")
    (:tool pyre
     :link "https://github.com/facebook/pyre-check"
     :desc "A performant type-checker for Python 3")
    (:tool nasm
     :link "https://github.com/netwide-assembler/nasm"
     :desc "Netwide Assembler (NASM), an assembler for the x86 CPU")
    (:tool cppcheck
     :link "https://github.com/danmar/cppcheck"
     :desc "A tool for static C/C++ code analysis")
    (:tool pycodestyle
     :link "https://github.com/pycqa/pycodestyle"
     :desc "Simple Python style checker in one Python file")
    (:tool mypy
     :link "https://github.com/python/mypy"
     :desc "Optional Static Typing for Python")
    (:tool ruff
     :link "https://github.com/astral-sh/ruff"
     :desc "An extremely fast Python linter and code formatter, written in Rust")
    (:tool pylint
     :link "https://github.com/pylint-dev/pylint"
     :desc "It's not just a linter that annoys you!")
    (:tool flake8
     :link "https://github.com/pycqa/flake8"
     :desc "Glues together pycodestyle, pyflakes, mccabe, and third-party plugins to check the style and quality of some python code")
    (:tool sqlint
     :link "https://github.com/purcell/sqlint"
     :desc "ItSimple SQL linter supporting ANSI and PostgreSQL syntaxes")
    (:tool sql-lint
     :link "https://github.com/joereynolds/sql-lint"
     :desc "An SQL linter")
    (:tool markdownlint
     :link "https://github.com/markdownlint/markdownlint"
     :desc "Markdown lint tool")
    (:tool tidy
     :link "https://github.com/htacg/tidy-html5"
     :desc "The granddaddy of HTML tools, with support for modern standards")
    (:tool luacheck
     :link "https://github.com/mpeterv/luacheck"
     :desc "A tool for linting and static analysis of Lua code")
    (:tool yamllint
     :link "https://github.com/adrienverge/yamllint"
     :desc "A linter for YAML files")
    (:tool eslint
     :link "https://github.com/eslint/eslint"
     :desc "Find and fix problems in your JavaScript code")
    (:tool jsonlint
     :link "https://github.com/zaach/jsonlint"
     :desc "A JSON parser and validator with a CLI")))


(provide 'me-external-tools)
;;; me-external-tools.el ends here
