;;; me-loaddefs.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from ../elisp/goto-last-change.el

(autoload 'goto-last-change "../elisp/goto-last-change" "\
Set point to the position of the last change.

Consecutive calls set point to the position of the previous changes.
With a prefix arg (optional arg MARK-POINT non-nil), this will set mark
so \\[exchange-point-and-mark] will return point to the current
position.

(fn &optional MARK-POINT)" t)
(register-definition-prefixes "../elisp/goto-last-change" '("goto-last-change-"))


;;; Generated autoloads from ../modules/on-demand/me-agda.el

(minemacs-register-on-demand-module 'me-agda :auto-mode '(("\\.l?agda\\'" . agda2-mode)))


;;; Generated autoloads from ../modules/on-demand/me-alloy.el

(minemacs-register-on-demand-module 'me-alloy :auto-mode '(("\\.als\\'" . alloy-mode)))


;;; Generated autoloads from ../modules/on-demand/me-apl.el

(minemacs-register-on-demand-module 'me-apl :auto-mode '(("\\.apl\\'" . gnu-apl-mode)) :interpreter-mode '(("apl" . gnu-apl-mode)))


;;; Generated autoloads from ../modules/on-demand/me-arduino.el

(minemacs-register-on-demand-module 'me-arduino :auto-mode '((("\\.ino\\'" "\\.pde\\'") . arduino-mode)))


;;; Generated autoloads from ../modules/on-demand/me-assembly.el

(minemacs-register-on-demand-module 'me-assembly :auto-mode '(("\\.S\\'" . gas-mode) (("\\.asm\\'" "\\.inc\\'") . masm-mode) ("\\.mips\\'" . mips-mode) ("\\.riscv\\'" . riscv-mode)))
(autoload '+asm-ask-for-mode "../modules/on-demand/me-assembly" "\
Ask the MODE to run.

(fn MODE)" t)


;;; Generated autoloads from ../modules/on-demand/me-awk.el

(minemacs-register-on-demand-module 'me-awk :auto-mode '(("\\.awk\\'" . awk-ts-mode)) :companion-packages '((awk-mode . awk-ts-mode)))


;;; Generated autoloads from ../modules/on-demand/me-cc.el

(minemacs-register-on-demand-module 'me-cc :companion-packages '(((c-mode c++-mode c-ts-mode c++-ts-mode) . flymake-cppcheck)))


;;; Generated autoloads from ../modules/on-demand/me-clojure.el

(minemacs-register-on-demand-module 'me-clojure :auto-mode '(("\\.cljs\\'" . clojurescript-mode) (("\\.cljc\\'" "\\.\\(clj\\|cljd\\|dtm\\|edn\\|lpy\\)\\'" "\\(?:build\\|profile\\)\\.boot\\'") . clojure-mode)) :interpreter-mode '(("bb" . clojure-mode) ("nbb" . clojurescript-mode)))


;;; Generated autoloads from ../modules/on-demand/me-cmake.el

(minemacs-register-on-demand-module 'me-cmake :auto-mode '((("CMakeLists\\.txt\\'" "\\.cmake\\'") . cmake-mode)) :companion-packages '((cmake-ts-mode cmake-font-lock cmake-mode)))


;;; Generated autoloads from ../modules/on-demand/me-cobol.el

(minemacs-register-on-demand-module 'me-cobol :auto-mode '(("\\.c\\(ob\\|bl\\|py\\)\\'" . cobol-mode)))


;;; Generated autoloads from ../modules/extras/me-cocogitto.el

(autoload '+cocogitto-bump "../modules/extras/me-cocogitto" "\
Bump version LEVEL (`auto', `major', `minor' or `patch'), and with PRE if it
is a pre-release.

This command stashes the current workspace before bumping the version, and
restores it after that.

(fn LEVEL &optional PRE)" t)
(register-definition-prefixes "../modules/extras/me-cocogitto" '("+cocogitto-buffer-name"))


;;; Generated autoloads from ../modules/on-demand/me-coffee.el

(minemacs-register-on-demand-module 'me-coffee :auto-mode '((("\\.coffee\\'" "\\.iced\\'" "Cakefile\\'" "\\.cson\\'") . coffee-mode)) :interpreter-mode '(("coffee" . coffee-mode)))


;;; Generated autoloads from ../modules/on-demand/me-common-lisp.el

(minemacs-register-on-demand-module 'me-common-lisp :companion-packages '((lisp-mode sly sly-macrostep sly-quicklisp sly-asdf sly-macrostep)))


;;; Generated autoloads from ../modules/on-demand/me-csv.el

(minemacs-register-on-demand-module 'me-csv :auto-mode '(("\\.[Cc][Ss][Vv]\\'" . csv-mode) ("\\.[Tt][Ss][Vv]\\'" . tsv-mode)) :companion-packages '((csv-mode . rainbow-csv)))


;;; Generated autoloads from ../modules/on-demand/me-cuda.el

(minemacs-register-on-demand-module 'me-cuda :auto-mode '(("\\.cu[h]?\\'" . cuda-mode)))


;;; Generated autoloads from ../modules/on-demand/me-cython.el

(minemacs-register-on-demand-module 'me-cython :auto-mode '((("\\.pyx\\'" "\\.px[di]\\'") . cython-mode)))


;;; Generated autoloads from ../modules/on-demand/me-d.el

(minemacs-register-on-demand-module 'me-d :auto-mode '(("\\.d[i]?\\'" . d-mode)))


;;; Generated autoloads from ../modules/on-demand/me-d2.el

(minemacs-register-on-demand-module 'me-d2 :auto-mode '(("\\.d2\\'" . d2-mode)))


;;; Generated autoloads from ../modules/on-demand/me-dart.el

(minemacs-register-on-demand-module 'me-dart :auto-mode '(("\\.dart\\'" . dart-mode)) :companion-packages '((dart-mode . flutter)))


;;; Generated autoloads from ../modules/on-demand/me-demangle.el

(minemacs-register-on-demand-module 'me-demangle :companion-packages '(((llvm-mode llvm-ts-mode) . demangle-mode)))


;;; Generated autoloads from ../modules/on-demand/me-devicetree.el

(minemacs-register-on-demand-module 'me-devicetree :auto-mode '(("\\.dtsi?\\'" . dts-mode) (".+\\.dtb\\|dtbo\\'" . virtual-dts-mode)))


;;; Generated autoloads from ../modules/on-demand/me-dhall.el

(minemacs-register-on-demand-module 'me-dhall :auto-mode '(("\\.dhall\\'" . dhall-mode)))


;;; Generated autoloads from ../modules/on-demand/me-djvu.el

(minemacs-register-on-demand-module 'me-djvu :auto-mode '(("\\.[dD][jJ][vV][uU]?\\'" . djvu-init-mode)) :magic-mode '(("%DJVU" . djvu-read-mode)))


;;; Generated autoloads from ../modules/on-demand/me-docker.el

(minemacs-register-on-demand-module 'me-docker :auto-mode `((("\\.dockerfile\\'" "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'") . dockerfile-mode) ("docker-compose[^/]*\\.ya?ml\\'" . docker-compose-mode)))


;;; Generated autoloads from ../modules/on-demand/me-ebuild.el

(minemacs-register-on-demand-module 'me-ebuild :auto-mode '(("\\.ebuild\\'" . ebuild-mode) ("\\.eclass\\'" . ebuild-eclass-mode)))


;;; Generated autoloads from ../modules/extras/me-eglot-ltex.el

(put 'eglot-ltex-language 'safe-local-variable 'stringp)
(register-definition-prefixes "../modules/extras/me-eglot-ltex" '("eglot-ltex-"))


;;; Generated autoloads from ../modules/on-demand/me-elixir.el

(minemacs-register-on-demand-module 'me-elixir :auto-mode '((("\\.elixir\\'" "\\.exs?\\'" "/mix\\.lock") . elixir-mode)) :companion-packages '((elixir-mode . ob-elixir)))


;;; Generated autoloads from ../modules/on-demand/me-elm.el

(minemacs-register-on-demand-module 'me-elm :auto-mode '(("\\.elm\\'" . elm-mode)) :companion-packages '((elm-mode . elm-test-runner)))


;;; Generated autoloads from ../modules/on-demand/me-epub.el

(minemacs-register-on-demand-module 'me-epub :auto-mode '(("\\.[eE][pP][uU][bB]\\'" . nov-mode)))


;;; Generated autoloads from ../modules/on-demand/me-erlang.el

(minemacs-register-on-demand-module 'me-erlang :auto-mode '((("\\.erl$" "\\.app\\.src$" "\\.escript" "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app") . erlang-mode)))


;;; Generated autoloads from me-external-tools.el

(register-definition-prefixes "me-external-tools" '("minemacs-external-dependencies"))


;;; Generated autoloads from ../modules/on-demand/me-fish.el

(minemacs-register-on-demand-module 'me-fish :auto-mode '((("\\.fish\\'" "/fish_funced\\..*\\'") . fish-mode)) :interpreter-mode '(("fish" . fish-mode)))


;;; Generated autoloads from ../modules/on-demand/me-forth.el

(minemacs-register-on-demand-module 'me-forth :auto-mode '(("\\.\\(fth\\|4th\\)\\'" . forth-mode)))


;;; Generated autoloads from ../modules/on-demand/me-fpga.el

(minemacs-register-on-demand-module 'me-fpga :auto-mode '(("\\.vhdl?\\'" . vhdl-ts-mode) ("\\.[ds]?va?h?\\'" . verilog-ts-mode)) :companion-packages '((vhdl-mode . vhdl-ts-mode) (verilog-mode . verilog-ts-mode)))


;;; Generated autoloads from ../modules/on-demand/me-franca-idl.el

(minemacs-register-on-demand-module 'me-franca-idl :auto-mode '(("\\.fidl\\'" . franca-idl-mode)))


;;; Generated autoloads from ../modules/on-demand/me-freebasic.el

(minemacs-register-on-demand-module 'me-freebasic :auto-mode '(("\\.b\\(i\\|as\\)\\'" . fb-mode)))


;;; Generated autoloads from ../modules/on-demand/me-fsharp.el

(minemacs-register-on-demand-module 'me-fsharp :auto-mode '(("\\.fs[iylx]?\\'" . fsharp-mode) ("\\.fsproj\\'" . nxml-mode)))


;;; Generated autoloads from ../modules/on-demand/me-gitlab-ci.el

(minemacs-register-on-demand-module 'me-gitlab-ci :auto-mode '(("\\.gitlab-ci.ya?ml\\'" . gitlab-ci-mode)) :companion-packages '(((yaml-mode yaml-ts-mode) . gitlab-ci-mode)))


;;; Generated autoloads from ../modules/on-demand/me-gnuplot.el

(minemacs-register-on-demand-module 'me-gnuplot :auto-mode '((("\\.plot\\'" "\\.gpi\\'" "\\.gplt\\'" "\\.plt\\'" "\\.gnuplot\\'") . gnuplot-mode)))


;;; Generated autoloads from ../modules/on-demand/me-godot.el

(minemacs-register-on-demand-module 'me-godot :auto-mode '(("\\.gd\\'" . gdscript-mode) ("\\.tres\\'" . conf-toml-mode) ("\\.tscn\\'" . conf-toml-mode)))


;;; Generated autoloads from ../modules/on-demand/me-graphql.el

(minemacs-register-on-demand-module 'me-graphql :auto-mode '((("\\.gql\\'" "\\.graphql\\'") . graphql-mode)))


;;; Generated autoloads from ../modules/on-demand/me-graphviz.el

(minemacs-register-on-demand-module 'me-graphviz :auto-mode '(("\\.gv\\'" . graphviz-dot-mode) ("\\.dot\\'" . graphviz-dot-mode)))


;;; Generated autoloads from ../modules/on-demand/me-haskell.el

(minemacs-register-on-demand-module 'me-haskell :auto-mode '((("\\.hsig\\'" "\\.[gh]s\\'" "\\.hsc\\'") . haskell-mode) ("\\.l[gh]s\\'" . haskell-literate-mode)) :interpreter-mode '(("runghc" . haskell-mode) ("runhaskell" . haskell-mode)))


;;; Generated autoloads from ../modules/on-demand/me-haxe.el

(minemacs-register-on-demand-module 'me-haxe :auto-mode '(("\\.hx\\'" . haxe-mode)))


;;; Generated autoloads from ../modules/on-demand/me-hurl.el

(minemacs-register-on-demand-module 'me-hurl :auto-mode '(("\\.hurl\\'" . hurl-mode)))


;;; Generated autoloads from ../modules/on-demand/me-hy.el

(minemacs-register-on-demand-module 'me-hy :auto-mode '(("\\.hy\\'" . hy-mode)) :interpreter-mode '(("hy" . hy-mode)) :companion-packages '((hy-mode . ob-hy)))


;;; Generated autoloads from ../modules/on-demand/me-idris.el

(minemacs-register-on-demand-module 'me-idris :auto-mode '(("\\.l?idr\\'" . idris-mode)))


;;; Generated autoloads from ../modules/on-demand/me-java.el

(minemacs-register-on-demand-module 'me-java :auto-mode '((("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" "/Jenkinsfile\\'") . groovy-mode)) :interpreter-mode '(("groovy" . groovy-mode)) :companion-packages '(((java-mode java-ts-mode) groovy-mode android-mode)))


;;; Generated autoloads from ../modules/on-demand/me-jenkins.el

(minemacs-register-on-demand-module 'me-jenkins :auto-mode '(("Jenkinsfile\\'" . jenkinsfile-mode)))


;;; Generated autoloads from ../modules/on-demand/me-json.el

(minemacs-register-on-demand-module 'me-json :auto-mode '((("\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'" "/.babelrc\\'" "/.bowerrc\\'" "/composer.lock\\'") . json-mode)) :companion-packages '((json-ts-mode jq-mode json-mode) ((nxml-mode yaml-mode yaml-ts-mode) . jq-mode)))


;;; Generated autoloads from ../modules/on-demand/me-julia.el

(minemacs-register-on-demand-module 'me-julia :auto-mode '(("\\.jl\\'" . julia-mode)) :companion-packages '(((julia-mode julia-ts-mode ess-julia-mode) julia-repl julia-snail julia-ts-mode)))


;;; Generated autoloads from ../modules/on-demand/me-just.el

(minemacs-register-on-demand-module 'me-just :auto-mode '((("/[Jj]ustfile\\'" "\\.[Jj]ust\\(file\\)?\\'") . just-mode)) :companion-packages '(((just-mode just-ts-mode) . justl)))


;;; Generated autoloads from ../modules/on-demand/me-kotlin.el

(minemacs-register-on-demand-module 'me-kotlin :auto-mode '(("\\.kts?\\'" kotlin-mode kotlin-ts-mode)))


;;; Generated autoloads from ../modules/on-demand/me-latex.el

(minemacs-register-on-demand-module 'me-latex :companion-packages '(((latex-mode tex-mode doctex-mode bibtex-mode bibtex-style-mode) auctex auctex-latexmk latex-preview-pane)))


;;; Generated autoloads from me-lib.el

(register-definition-prefixes "me-lib" '("+a" "+binary-hexl-enable" "+c" "+d" "+e" "+f" "+i" "+known-scripts" "+l" "+m" "+p" "+quoted-p" "+res" "+s" "+un" "+varplist-get" "+with-" "minemacs-"))


;;; Generated autoloads from me-lib-extra.el

(autoload 'minemacs-run-build-functions "me-lib-extra" "\
Run all build functions in `minemacs-build-functions'.

Call functions without asking when DONT-ASK-P is non-nil.

(fn &optional DONT-ASK-P)" t)
(autoload 'minemacs-bump-packages "me-lib-extra" "\
Update MinEmacs packages to the last revisions (can cause breakages)." t)
(autoload 'minemacs-bump-packages-async "me-lib-extra" "\
Asynchronous version of `minemacs-bump-packages'." t)
(autoload 'minemacs-restore-locked-packages "me-lib-extra" "\
Restore lockfile packages list. Takes into account the pinned ones.
When called with \\[universal-argument] or with RESTORE-FROM-BACKUP, it will
restore the lockfile from backups, not Git.

(fn RESTORE-FROM-BACKUP)" t)
(autoload 'minemacs-upgrade "me-lib-extra" "\
Upgrade MinEmacs and its packages to the latest pinned versions (recommended).

When PULL-MINEMACS is non-nil, run a \"git pull\" in MinEmacs' directory.

This calls `minemacs-update-restore-locked' asynchronously.

(fn PULL-MINEMACS)" t)
(autoload 'minemacs-root-dir-cleanup "me-lib-extra" "\
Cleanup MinEmacs' root directory.")
(autoload '+straight-prune-build-cache "me-lib-extra" "\
Prune straight.el build directories for old Emacs versions.")
(autoload 'minemacs-cleanup-emacs-directory "me-lib-extra" "\
Cleanup unwanted files/directories from MinEmacs' directory." t)
(autoload 'minemacs-apply-performance-tweaks "me-lib-extra" "\
Set some Emacs variables for better (!) performance." t)
(autoload 'minemacs-load-module "me-lib-extra" "\
Interactively install and load MODULES that aren't enabled in \"modules.el\".

When called with \\[universal-argument], it prompts also for on-demand modules.
When called with \\[universal-argument] \\[universal-argument], it prompts also for obsolete modules.

(fn &rest MODULES)" t)
(autoload '+file-mime-type "me-lib-extra" "\
Get MIME type for FILE based on magic codes provided by the \"file\" command.
Return a symbol of the MIME type, ex: `text/x-lisp', `text/plain',
`application/x-object', `application/octet-stream', etc.

(fn FILE)")
(autoload '+file-name-incremental "me-lib-extra" "\
Return a unique file name for FILENAME.
If \"file.ext\" exists, returns \"file-0.ext\".

(fn FILENAME)")
(autoload '+delete-this-file "me-lib-extra" "\
Delete PATH.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation.

(fn &optional PATH FORCE-P)" t)
(autoload '+delete-file-or-directory "me-lib-extra" "\
Delete FILE-OR-DIRECTORY with `delete-file' or `delete-directory'.

Move to trash when TRASH is non-nil, delete directories recursively when
RECURSIVE is non-nil.

(fn FILE-OR-DIRECTORY &optional TRASH RECURSIVE)")
(autoload '+delete-this-file-and-buffer "me-lib-extra" "\
Delete FILENAME and its associated visiting buffer.

(fn &optional FILENAME)" t)
(autoload '+tramp-sudo-file-path "me-lib-extra" "\
Construct a Tramp sudo path to FILE. Works for both local and remote files.

(fn FILE)")
(autoload '+sudo-find-file "me-lib-extra" "\
Open FILE as root.

(fn FILE)" t)
(autoload '+sudo-this-file "me-lib-extra" "\
Open the current file as root." t)
(autoload '+sudo-save-buffer "me-lib-extra" "\
Save this buffer as root. Save as new file name if called with prefix." t)
(autoload '+yank-this-file-name "me-lib-extra" "\
Yank the file name of this buffer." t)
(autoload '+clean-file-name "me-lib-extra" "\
Clean FILENAME, optionally convert to DOWNCASE-P.

(fn FILENAME &optional DOWNCASE-P)")
(autoload '+html2pdf "me-lib-extra" "\
Convert HTML file INFILE to PDF and save it to OUTFILE.
When BACKEND is provided, the corresponding program is used, otherwise, the
value of `+html2pdf-default-backend' is used.

(fn INFILE OUTFILE &optional BACKEND)")
(autoload '+txt2html "me-lib-extra" "\
Convert plain-text file INFILE to HTML and save it to OUTFILE.
When MAIL-MODE-P is non-nil, --mailmode is passed to \"txt2html\".

(fn INFILE OUTFILE &optional MAIL-MODE-P)")
(autoload '+save-as-pdf "me-lib-extra" "\
Save URL as PDF.
This function's signature is compatible with `browse-url-browser-function'
so it can be used to save HTML pages or emails to PDF.
When MAIL-MODE-P is non-nil, treat INFILE as a mail.

(fn INFILE &optional MAIL-MODE-P)")
(autoload '+single-file "me-lib-extra" "\
Save URL into OUT-FILE as a standalone HTML file.

(fn URL OUT-FILE)" t)
(autoload '+browse-html-file "me-lib-extra" "\
Browser HTML FILE following `+browse-html-file-browser-priority'.

If no function from `+browse-html-file-browser-priority' is available,
use `browse-url'.

When called with universal argument, open the current buffer's file.

(fn FILE)" t)
(autoload '+serial-running-p "me-lib-extra" "\
Is there a serial port terminal running?")
(autoload '+serial-run-commands "me-lib-extra" "\
Run COMMANDS on a device via serial communication.

If PORT or BAUD are nil, use values from `+serial-port' and `+serial-baudrate'.

(fn COMMANDS &optional PORT BAUD)" t)
(autoload '+net-get-ip-address "me-lib-extra" "\
Get the IP-address for device DEV (default: eth0) of the current machine.

(fn &optional DEV)")
(autoload '+github-latest-release "me-lib-extra" "\
Get the latest release of REPO. Strips the \"v\" at left.

Fallback to FALLBACK-RELEASE when it can't get the last one.

(fn REPO &optional FALLBACK-RELEASE)")
(autoload '+dir-locals-reload-for-this-buffer "me-lib-extra" "\
Reload directory-local for the current buffer." t)
(autoload '+dir-locals-reload-for-all-buffers-in-this-directory "me-lib-extra" "\
Reload dir-locals for all buffers in the current `default-directory'." t)
(autoload '+dir-locals-toggle-autoreload "me-lib-extra" "\
Toggle autoloading dir-local variables after editing the \".dir-locals\" file.

If ENABLE is non-nil, force enabling autoreloading.

(fn &optional ENABLE)" t)
(autoload '+dir-locals-open-or-create "me-lib-extra" "\
Open or create the dir-locals.el for the current project." t)
(autoload '+what-faces "me-lib-extra" "\
Get the font faces at POS.

(fn POS)" t)
(autoload '+screenshot-svg "me-lib-extra" "\
Save a screenshot of the current frame as an SVG image to OUTFILE.

If launched with a prefix or universal argument, it waits for a moment (defined
by `+screenshot-delay') before taking the screenshot.

(fn OUTFILE)" t)
(autoload '+minibuffer-kill-minibuffer "me-lib-extra" "\
Kill the minibuffer when switching to window with mouse." t)
(autoload '+region-or-thing-at-point "me-lib-extra" "\
Return the region or the thing at point.

If LEAVE-REGION-MARKED is no-nil, don't call `desactivate-mark'
when a region is selected.

(fn &optional LEAVE-REGION-MARKED)")
(autoload '+insert-thing-at-point "me-lib-extra" "\
Insert region or symbol in the minibuffer." t)
(autoload '+kill-region-or-backward-word "me-lib-extra" "\
Kill selected region if region is active. Otherwise kill a backward word." t)
(autoload '+kill-whitespace-or-word "me-lib-extra" "\
Kill forward whitespace or word.
With argument ARG, do this that many times.
Restricts the effect of `kill-word' to the current line.

(fn ARG)" t)
(autoload '+backward-kill-whitespace-or-word "me-lib-extra" "\
Kill backward whitespace or word.
With argument ARG, do this that many times.
Restricts the effect of `backward-kill-word' to the current line.

(fn ARG)" t)
(autoload '+set-indent-width "me-lib-extra" "\
Change the indentation size to WIDTH of the current buffer.

The effectiveness of this command is significantly improved if
you have `editorconfig' or `dtrt-indent' installed.

(fn WIDTH)" t)
(autoload '+webjump "me-lib-extra" "\
Like `webjump', with initial query filled from `+region-org-thing-at-point'." t)
(autoload '+eglot-ccls-inheritance-hierarchy "me-lib-extra" "\
Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point.

(fn &optional DERIVED)" t)
(autoload '+eglot-help-at-point "me-lib-extra" "\
Request documentation for the thing at point." t)
(autoload '+server-restart "me-lib-extra" "\
Restart the Emacs server." t)
(autoload '+binary-buffer-p "me-lib-extra" "\
Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least one null byte.

Returns either nil, or the position of the first null byte.

(fn &optional BUFFER)")
(autoload '+binary-file-p "me-lib-extra" "\
Is FILE a binary?

This checks the first CHUNK of bytes, defaults to 1024.

(fn FILE &optional CHUNK)")
(autoload '+binary-hexl-buffer-p "me-lib-extra" "\
Does BUFFER (defaults to the current buffer) should be viewed using `hexl-mode'.

(fn &optional BUFFER)")
(autoload '+binary-hexl-mode-maybe "me-lib-extra" "\
Activate `hexl-mode' if relevant for the current buffer." t)
(autoload '+kill-buffer-and-its-windows "me-lib-extra" "\
Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string).

(fn BUFFER &optional MSGP)" t)
(autoload '+region-to-buffer "me-lib-extra" "\
Copy region to BUFFER: At beginning (prefix >= 0), end (< 0), or replace.
START and END are the region boundaries.
BUFFER is a buffer or its name (a string).
With prefix ARG >= 0: `append-to-buffer':
  Append contents of region to end of BUFFER.
  (Point is moved to end of BUFFER first.)
With prefix ARG < 0:  `prepend-to-buffer':
  Prepend contents of region to beginning of BUFFER.
  (Point is moved to beginning of BUFFER first.)
With no prefix ARG (nil): `copy-to-buffer'.
  Write region to BUFFER, replacing any previous contents.

(fn START END BUFFER ARG)" t)
(autoload '+region-to-file "me-lib-extra" "\
With prefix arg, this is `append-to-file'.  Without, it is `write-region'.
START and END are the region boundaries.
Prefix ARG non-nil means append region to end of file FILENAME.
Prefix ARG nil means write region to FILENAME, replacing contents.

(fn START END FILENAME ARG)" t)
(autoload '+kill-some-buffers "me-lib-extra" "\
Kill some buffers.  Asks the user whether to kill the modified ones.
Non-interactively, if optional argument LIST is non-nil, it
specifies the list of buffers to kill, asking for approval for each one.
See `kill-some-buffers'.

(fn &optional LIST)" t)
(autoload '+kill-buffer-ask-if-modified "me-lib-extra" "\
Like `kill-buffer-ask', but kills BUFFER without confirmation when unmodified.
Kill without asking for buffer names in `+kill-buffer-no-ask-list'.

(fn BUFFER)")
(autoload '+delete-extra-windows-for-buffer "me-lib-extra" "\
Delete all other windows showing the selected window's buffer." t)
(autoload '+delete-window-maybe-kill-buffer "me-lib-extra" "\
Delete selected window.
If no other window shows its buffer, kill the buffer too." t)
(autoload '+replace-in-buffer "me-lib-extra" "\
Replace OLD with NEW in the current buffer.

(fn OLD NEW)")
(autoload '+clear-frenchy-ponctuations "me-lib-extra" "\
Replace french ponctuations (like unsectable space) by regular ones." t)
(autoload '+save-buffer-preserving-modtime "me-lib-extra" "\
Call `save-buffer', but keep the visited file's modtime the same." t)
(autoload '+kill-region-as-paragraph "me-lib-extra" "\
Kill (copy) region as one paragraph.
This command removes new line characters between lines." t)
(autoload '+first-line-empty-p "me-lib-extra" "\
Return t when the first line of the buffer is empty.")
(autoload '+project-add-project "me-lib-extra" "\
Switch to another project at DIR.
When DIR is not detected as a project, ask to force it to be by adding a
\".project.el\" file. When DONT-ASK is non-nil, create the file without asking.

(fn DIR &optional DONT-ASK)" t)
(autoload '+project-forget-zombie-projects "me-lib-extra" "\
Forget all known projects that don't exist any more.

Like `project-forget-zombie-projects', but handles remote projects differently,
it forget them only when we are sure they don't exist." t)
(autoload '+project-gdb "me-lib-extra" "\
Invoke `gdb' in the project's root." t)
(autoload '+project-list-cleanup "me-lib-extra" "\
Forget all duplicate known projects (/home/user/proj, ~/proj)." t)
(autoload '+xref-find-references-at-point "me-lib-extra" "\
Find references to the identifier at or around point." t)
(autoload '+systemd-running-p "me-lib-extra" "\
Check if the systemd SERVICE is running.

(fn SERVICE)")
(autoload '+systemd-command "me-lib-extra" "\
Call systemd with COMMAND and SERVICE.

(fn SERVICE COMMAND &optional PRE-FN POST-FN)")
(autoload '+systemd-start "me-lib-extra" "\
Start systemd SERVICE. Optionally run PRE-FN and POST-FN.

(fn SERVICE &optional PRE-FN POST-FN)")
(autoload '+systemd-stop "me-lib-extra" "\
Stops the systemd SERVICE. Optionally run PRE-FN and POST-FN.

(fn SERVICE &optional PRE-FN POST-FN)")
(autoload '+list-external-dependencies "me-lib-extra" "\
Show the list of declared external dependencies." t)
(register-definition-prefixes "me-lib-extra" '("+browse-html-file-browser-priority" "+dir-locals--autoreload-" "+eglot--help-buffer" "+html2pdf-" "+kill-buffer-no-ask-list" "+net-default-device" "+s" "+webjump-read-string-"))


;;; Generated autoloads from ../modules/on-demand/me-llvm.el

(minemacs-register-on-demand-module 'me-llvm :auto-mode '(("\\.ll\\'" . llvm-ts-mode)))


;;; Generated autoloads from ../modules/on-demand/me-lua.el

(minemacs-register-on-demand-module 'me-lua :auto-mode '(("\\.lua\\'" . lua-mode)) :interpreter-mode '(("lua" . lua-mode)))


;;; Generated autoloads from ../modules/on-demand/me-markdown.el

(minemacs-register-on-demand-module 'me-markdown :auto-mode '(("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)) :companion-packages '((markdown-mode . markdown-ts-mode)))


;;; Generated autoloads from ../modules/on-demand/me-mathematica.el

(minemacs-register-on-demand-module 'me-mathematica)


;;; Generated autoloads from ../modules/on-demand/me-mermaid.el

(minemacs-register-on-demand-module 'me-mermaid :auto-mode '(("\\.mmd\\'" . mermaid-mode)))


;;; Generated autoloads from ../modules/on-demand/me-mode-framework.el

(minemacs-register-on-demand-module 'me-mode-framework :auto-mode '(("\\.\\(robot\\|resource\\)\\'" . robot-mode)))


;;; Generated autoloads from ../modules/on-demand/me-modelica.el

(minemacs-register-on-demand-module 'me-modelica :auto-mode '(("\\.mo\\'" . modelica-mode)))


;;; Generated autoloads from me-modules.el

(register-definition-prefixes "me-modules" '("minemacs-modules"))


;;; Generated autoloads from ../modules/extras/me-mu4e-extras.el

(register-definition-prefixes "../modules/extras/me-mu4e-extras" '("+mu4e-" "+org-msg-make-signature"))


;;; Generated autoloads from ../modules/extras/me-mu4e-gmail.el

(register-definition-prefixes "../modules/extras/me-mu4e-gmail" '("+mu4e-"))


;;; Generated autoloads from ../modules/extras/me-mu4e-ui.el

(register-definition-prefixes "../modules/extras/me-mu4e-ui" '("+mu4e-"))


;;; Generated autoloads from ../modules/on-demand/me-nim.el

(minemacs-register-on-demand-module 'me-nim :auto-mode '(("\\.nim\\'" . nim-mode) ("\\.nim\\(ble\\|s\\)\\'" . nimscript-mode-maybe)))


;;; Generated autoloads from ../modules/on-demand/me-nix.el

(minemacs-register-on-demand-module 'me-nix :auto-mode '(("\\.nix\\'" . nix-mode) ("\\.nix\\'" . nix-ts-mode)))


;;; Generated autoloads from ../modules/on-demand/me-ocaml.el

(minemacs-register-on-demand-module 'me-ocaml :auto-mode '(("\\.mly\\'" . tuareg-menhir-mode) (("\\.eliomi?\\'" "\\.ml[ip]?\\'") . tuareg-mode) ("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\|\\-workspace\\)?\\'" . dune-mode)) :interpreter-mode '(("ocamlrun" . tuareg-mode) ("ocaml" . tuareg-mode)))


;;; Generated autoloads from ../modules/on-demand/me-odin.el

(minemacs-register-on-demand-module 'me-odin :auto-mode '(("\\.odin\\'" . odin-mode)))


;;; Generated autoloads from ../modules/on-demand/me-opencl.el

(minemacs-register-on-demand-module 'me-opencl :auto-mode '(("\\.cl\\'" . opencl-c-mode)))


;;; Generated autoloads from ../modules/on-demand/me-openscad.el

(minemacs-register-on-demand-module 'me-openscad :auto-mode '(("\\.scad\\'" . scad-mode)))


;;; Generated autoloads from ../modules/extras/me-org-extras.el

(register-definition-prefixes "../modules/extras/me-org-extras" '("+org-"))


;;; Generated autoloads from ../modules/on-demand/me-p4.el

(minemacs-register-on-demand-module 'me-p4 :auto-mode '(("\\.p4\\(info\\)?\\'" . p4-16-mode)))


;;; Generated autoloads from ../modules/on-demand/me-pandoc.el

(minemacs-register-on-demand-module 'me-pandoc :companion-packages '(((markdown-mode markdown-ts-mode org-mode tex-mode latex-mode) . pandoc-mode)))


;;; Generated autoloads from ../modules/on-demand/me-pcap.el

(minemacs-register-on-demand-module 'me-pcap :auto-mode '(("\\.\\(?:ntar\\|pcap\\(?:ng\\)?\\)\\'" . pcap-mode)))


;;; Generated autoloads from ../modules/on-demand/me-pdf.el

(minemacs-register-on-demand-module 'me-pdf :auto-mode '(("\\.[pP][dD][fF]\\'" . pdf-view-mode)) :magic-mode '(("%PDF" . pdf-view-mode)) :companion-packages '((doc-view-mode pdf-view-mode pdf-isearch-minor-mode pdf-view-restore-mode)))


;;; Generated autoloads from ../modules/on-demand/me-pkgbuild.el

(minemacs-register-on-demand-module 'me-pkgbuild :auto-mode '(("/PKGBUILD\\'" . pkgbuild-mode)) :companion-packages '(((sh-mode bash-ts-mode) . pkgbuild-mode)))


;;; Generated autoloads from ../modules/on-demand/me-plantuml.el

(minemacs-register-on-demand-module 'me-plantuml :auto-mode '(("\\.\\(plantuml\\|pum\\|plu\\)\\'" . plantuml-mode)))


;;; Generated autoloads from ../modules/on-demand/me-powershell.el

(minemacs-register-on-demand-module 'me-powershell :auto-mode '(("\\.ps[dm]?1\\'" . powershell-mode)))


;;; Generated autoloads from ../modules/on-demand/me-protobuf.el

(minemacs-register-on-demand-module 'me-protobuf :auto-mode '(("\\.proto\\'" . protobuf-mode)))


;;; Generated autoloads from ../modules/on-demand/me-purescript.el

(minemacs-register-on-demand-module 'me-purescript :auto-mode '(("\\.purs\\'" . purescript-mode)))


;;; Generated autoloads from ../modules/on-demand/me-python.el

(minemacs-register-on-demand-module 'me-python :companion-packages '(((python-mode python-ts-mode) . python-docstring-mode)))


;;; Generated autoloads from ../modules/on-demand/me-qt.el

(minemacs-register-on-demand-module 'me-qt :auto-mode '(("\\.qml\\'" . qml-mode) ("\\.pr[io]\\'" . qt-pro-mode)))
(add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode))


;;; Generated autoloads from ../modules/on-demand/me-rust.el

(minemacs-register-on-demand-module 'me-rust :auto-mode '(("\\.rs\\'" . rust-mode)) :companion-packages '((rust-ts-mode . rust-mode)))


;;; Generated autoloads from ../modules/on-demand/me-scala.el

(minemacs-register-on-demand-module 'me-scala :auto-mode '(("\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'" . scala-mode)) :companion-packages '(((scala-mode scala-ts-mode) . sbt-mode)))


;;; Generated autoloads from ../modules/on-demand/me-scheme.el

(minemacs-register-on-demand-module 'me-scheme :auto-mode '(("\\.rkt[dl]?\\'" . racket-mode)) :interpreter-mode '(("racket" . racket-mode)) :companion-packages '(((racket-mode scheme-mode) geiser-mode flymake-guile)))


;;; Generated autoloads from ../modules/on-demand/me-sml.el

(minemacs-register-on-demand-module 'me-sml :auto-mode '(("\\.s\\(?:ml\\|ig\\)\\'" . sml-mode) ("\\.cm\\'" . sml-cm-mode) ("\\.grm\\'" . sml-yacc-mode)))


;;; Generated autoloads from ../modules/on-demand/me-sql.el

(minemacs-register-on-demand-module 'me-sql :companion-packages '((sql-mode . sqlup-mode)))


;;; Generated autoloads from ../modules/on-demand/me-stan.el

(minemacs-register-on-demand-module 'me-stan :auto-mode '(("\\.stan\\'" . stan-mode)) :companion-packages '(((stan-mode stan-ts-mode) eldoc-stan stan-snippets)))


;;; Generated autoloads from ../modules/on-demand/me-toml.el

(minemacs-register-on-demand-module 'me-toml :auto-mode '(("\\.toml\\'" . toml-mode)) :companion-packages '((toml-ts-mode . toml-mode)))


;;; Generated autoloads from ../modules/on-demand/me-v.el

(minemacs-register-on-demand-module 'me-v :auto-mode '(("\\(\\.v?v\\|\\.vsh\\)$" . v-mode)))


;;; Generated autoloads from ../modules/on-demand/me-vala.el

(minemacs-register-on-demand-module 'me-vala :auto-mode '(("\\.vala\\'" . vala-mode) ("\\.vapi\\'" . vala-mode)) :companion-packages '((vala-mode . vala-snippets)))


;;; Generated autoloads from me-vars.el

(register-definition-prefixes "me-vars" '("+env-" "+load" "emacs/features" "minemacs-" "os/" "sys/arch"))


;;; Generated autoloads from ../modules/on-demand/me-vb.el

(minemacs-register-on-demand-module 'me-vb :auto-mode '(("\\.vbs?\\'" . visual-basic-mode)))


;;; Generated autoloads from ../modules/on-demand/me-vimscript.el

(minemacs-register-on-demand-module 'me-vimscript :auto-mode '((("\\.vim\\'" "[._]?g?vimrc\\'" "\\.exrc\\'") . vimrc-mode)))


;;; Generated autoloads from ../modules/on-demand/me-web.el

(minemacs-register-on-demand-module 'me-web :auto-mode '((("\\.[px]?html?\\'" "\\.tpl\\.php\\'" "\\.[lh]?eex\\'" "\\.[agj]sp\\'" "\\.ejs\\'" "\\.hbs\\'" "\\.svelte\\'" "\\.twig\\'" "\\.jinja2?\\'" "\\.eco\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.vue\\'" "wp-content/themes/.+/.+\\.php\\'" "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'" "/\\(views\\|html\\|templates\\)/.*\\.php\\'") . web-mode) ("\\.haml\\'" . haml-mode) ("\\.sass\\'" . sass-mode)) :companion-packages '(((php-mode php-ts-mode html-mode html-ts-mode css-mode) web-mode haml-mode emmet-mode sass-mode)))


;;; Generated autoloads from ../modules/extras/me-writing-mode.el

(autoload '+writing-mode "../modules/extras/me-writing-mode" "\
A mode for writing without distraction.

This is a minor mode.  If called interactively, toggle the `+Writing
mode' mode.  If the prefix argument is positive, enable the mode, and if
it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `+writing-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(put '+writing-global-mode 'globalized-minor-mode t)
(defvar +writing-global-mode nil "\
Non-nil if +Writing-Global mode is enabled.
See the `+writing-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `+writing-global-mode'.")
(custom-autoload '+writing-global-mode "../modules/extras/me-writing-mode" nil)
(autoload '+writing-global-mode "../modules/extras/me-writing-mode" "\
Toggle +Writing mode in all buffers.
With prefix ARG, enable +Writing-Global mode if ARG is positive; otherwise,
disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

+Writing mode is enabled in all buffers where `+turn-on-writing-mode' would do
it.

See `+writing-mode' for more information on +Writing mode.

(fn &optional ARG)" t)
(register-definition-prefixes "../modules/extras/me-writing-mode" '("+turn-on-writing-mode" "+writing-"))


;;; Generated autoloads from ../modules/on-demand/me-yaml.el

(minemacs-register-on-demand-module 'me-yaml :auto-mode '(("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)) :magic-mode '(("^%YAML\\s-+[0-9]+\\.[0-9]+\\(\\s-+#\\|\\s-*$\\)" . yaml-mode)) :companion-packages '((yaml-ts-mode yaml-mode yaml-pro-ts-mode)))


;;; Generated autoloads from ../modules/on-demand/me-yang.el

(minemacs-register-on-demand-module 'me-yang :auto-mode '(("\\.[Yy][Aa][Nn][Gg]\\'" . yang-mode)))


;;; Generated autoloads from ../modules/on-demand/me-zig.el

(minemacs-register-on-demand-module 'me-zig :auto-mode '(("\\.\\(zig\\|zon\\)\\'" . zig-mode)))


;;; Generated autoloads from minemacs-lazy.el

(register-definition-prefixes "minemacs-lazy" '("minemacs--lazy-"))


;;; Generated autoloads from ../elisp/valgrind.el

(autoload 'valgrind "../elisp/valgrind" "\
Run valgrind.
Runs a shell COMMAND in a separate process asynchronously with output going to
the buffer `*valgrind*'.
You can then use the command \\[next-error] to find the next error message and
move to the source code that caused it.

(fn COMMAND)" t)
(register-definition-prefixes "../elisp/valgrind" '("valgrind-"))

;;; End of scraped data

(provide 'me-loaddefs)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8
;; End:

;;; me-loaddefs.el ends here
