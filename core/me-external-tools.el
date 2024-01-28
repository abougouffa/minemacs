;;; me-external-tools.el - External tools

(defvar
  minemacs-external-dependencies
  '((:tool file
     :link "https://darwinsys.com/file"
     :desc "A tool to determine file types")
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
     :link "https://jqlang.github.io/jq"
     :desc "A lightweight and flexible command-line JSON processor")
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
     :link "https://cmake.org"
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
     :link "https://git-scm.com"
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
     :link "https://sqlfluff.com"
     :desc "SQLFluff is a modular SQL linter for humans")
    (:tool sqlformat
     :link "https://sqlformat.org"
     :desc "SQL formatter")
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
     :link "https://www.gnu.org/software/make"
     :desc "Generate configuration script")
    (:tool aspell
     :link "http://aspell.net"
     :desc "Interactive spell checker")
    (:tool enchant-2
     :link "https://github.com/AbiWord/enchant"
     :desc "A generic spell checker")
    (:tool clink
     :link "https://github.com/Smattr/clink"
     :desc "A modern re-implementation of Cscope based on Clang")))


(provide 'me-external-tools)
;;; me-external-tools.el ends here
