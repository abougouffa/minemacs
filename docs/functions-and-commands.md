<img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right">

# MinEmacs - a minimalist & lightweight Emacs configuration framework

Load and hooks order:
- `~/.emacs.d/early-init.el`
  * `~/.emacs.d/core/me-lib.el`
    + `~/.emacs.d/core/me-vars.el`
  * `$MINEMACSDIR/early-config.el`           (unless disabled in `$MINEMACS_IGNORE_USER_CONFIG`)
  * `$MINEMACSDIR/local/early-config.el`     (unless disabled)
- `before-init-hook`
- `~/.emacs.d/init.el`
  * `~/.emacs.d/early-init.el`               (ensure it is loaded, in case we started Emacs without early-init)
  * `$MINEMACSDIR/custom-vars.el`
  * `~/.emacs.d/core/me-loaddefs.el`
  * `$MINEMACSDIR/init-tweaks.el`            (unless disabled)
  * `$MINEMACSDIR/local/init-tweaks.el`      (unless disabled)
  * `$MINEMACSDIR/local/modules.el`          (unless disabled)
  * `~/.emacs.d/core/me-builtin.el`
  * `~/.emacs.d/core/me-bootstrap.el`        (unless `MINEMACS_BUILTIN_ONLY`)
  * `~/.emacs.d/modules/<MODULE>.el`         (for <MODULE> in `minemacs-modules`, unless `MINEMACS_BUILTIN_ONLY)
  * `$MINEMACSDIR/config.el`                 (unless disabled)
  * `$MINEMACSDIR/local/config.el`           (unless disabled)
- `after-init-hook`
- `emacs-startup-hook`
  * `minemacs-after-load-theme-hook`         (after applying `minemacs-theme`)
  * `minemacs-after-startup-hook`
    + `minemacs-lazy-hook`                   (hooks are incrementally loaded via a timer)

Special hooks defined with `+make-first-file-hook!`
- `minemacs-first-file-hook`
- `minemacs-first-elisp-file-hook`
- `minemacs-first-python-file-hook`
- `minemacs-first-org-file-hook`
- `minemacs-first-c/c++-file-hook`



### Customization Documentation

#### `+use-package-keep-checking-for-disabled-p`

If you want to keep the advice that skip disabled packages.
You need to set in in your "early-config.el".

#### `minemacs-msg-level`

Level of printed messages.
1 - `+error!`
2 - `+info!`
3 - `+log!`
4 - `+debug!`

#### `minemacs-theme`

The theme of MinEmacs.

#### `minemacs-disabled-packages`

List of packages to be disabled when loading MinEmacs modules.
This can be useful if you want to enable a module but you don't want a package
of being enabled.

#### `minemacs-after-setup-fonts-hook`

Runs after setting MinEmacs fonts, runs at the end of `+setup-fonts`.

#### `minemacs-after-load-theme-hook`

Runs after loading MinEmacs theme, runs at the end of `+load-theme`.

#### `minemacs-after-startup-hook`

This hook will be run after loading Emacs.

MinEmacs hooks will be run in this order:
1. `minemacs-after-startup-hook`
2. `minemacs-lazy-hook`

#### `minemacs-lazy-hook`

This hook will be run after loading Emacs, with laziness.

MinEmacs hooks will be run in this order:
1. `minemacs-after-startup-hook`
2. `minemacs-lazy-hook`

#### `minemacs-proxies`

MinEmacs proxies.

Example, set it to:

\='(("no" . "localhost,127.0.0.1,.local,.mylocaltld")
  ("ftp" . "http://myproxy.local:8080/")
  ("http" . "http://myproxy.local:8080/")
  ("https" . "http://myproxy.local:8080/")))

These will set the environment variables "no_proxy", "ftp_proxy", ...

When set in "early-config.el" or in "init-tweaks.el", MinEmacs will enable
it automatically.

#### `minemacs-modules`

MinEmacs enabled modules.

#### `+env-file`

The file in which the environment variables will be saved.

#### `+env-deny-vars`

Environment variables to omit.
Each string is a regexp, matched against variable names to omit from
`+env-file` when saving evnironment variables in `+env-save`.

#### `minemacs-on-demand-modules-alist`

List of extra on-demand modules.

#### `minemacs-on-demand-enable-plist`

Enable loading on-demand packages when needed.

#### `+clang-format-command`

The "clang-format" command to use by default.
This allows us to use a specific Clang-format version (like
"clang-format-12"). This command will be used in
`+editorconfig-guess-style-from-clang-format`,
`reformatter`, `apheleia`, etc.

#### `+eval-when-idle-delay`

The default delay (in seconds) to consider in `+eval-when-idle!` macro.

#### `+lazy-delay`

The default delay (in seconds) to consider in `+lazy!` macro.

#### `+first-file-hook-ignore-list`

A list of files to ignore in the `minemacs-first-*-file-hook` hooks.

It can be a filename, a filename with wildcard or a function that
returns one of the two.

#### `+first-file-hooks`

A list of hooks defined using `+make-first-file-hook!`.

#### `+eglot-auto-enable-modes`

Modes for which Eglot can be automatically enabled by `+eglot-auto-enable`.

#### `+super-project-root-markers`

List of super-project markers.

#### `+serialized-symbols-directory`

Default directory to store serialized symbols.

#### `minemacs-fonts-plist`

MinEmacs fonts used by `+setup-fonts`.

The function checks and enables the first available font from these defined in
this plist. This variable can be customized to set font specs for specific Emacs
faces or to enable some language-specific fonts. The plist keywords are either
face names or script names expressed as keywords (with the ":" prefix).

For example to set `default` face, use `:default`, to set the `mode-line` face,
use `:mode-line`, and so on. The parameters for each font in these cases (ie.
for face names) are used in the `custom-theme-set-faces` function, so you can
pass any specs (key value pairs) supported by `custom-theme-set-faces` (like
`:weight`, `:slant`, `:foreground`, ...). A list of supported keywords are
available in the variable `+face-attributes`.

You can also setup some language-specific fonts. All scripts supported by Emacs
can be found in `+known-scripts`. The keyword in this plist will be the script
name expressed as a keyword, for example, for `latin` use `:latin`, for `arabic`
use `:arabic`, for `emoji` use `:emoji`, and so on. In this case, the parameters
are used with `set-fontset-font`, so you can send any key value pair supported
by `set-fontset-font`. A list of supported keywords in this case is available in
`+font-spec-keywords`.

The value of the extra `:prepend` keyword is passed the last argument to
`set-fontset-font`. The value of the extra `:scale` keyword can be used to set a
scaling factor for the font in Emacs' `face-font-rescale-alist`. See the
`+setup-fonts` implementation for more details.

#### `+html2pdf-default-backend`

The default backend to convert HTML files to PDFs in `+html2pdf`.

#### `+html2pdf-backend-config-file`

A config file to use with the backend tool (pandoc, weasyprint, ...).

#### `+single-file-executable`

The executable for "single-file" which is used archive HTML pages.

#### `+serial-port`

The default port (device) to use.

#### `+serial-baudrate`

The default baudrate to use.

#### `+serial-first-commands`

A list of commands to run in the serial terminal after creation.

#### `+kill-buffer-no-ask-list`

A list of buffer names to be killed without confirmation.

#### `+mpv-command`

The MPV command.

#### `eglot-ltex-user-rules-path`

Path to save user rules.

#### `eglot-ltex-ngrams-path`

Path the language model's ngrams.

#### `eglot-ltex-ls-path`

Path to LTeX+ LS.

#### `eglot-ltex-ls-program`

The path or executable name of the LTeX+ LS.

#### `+mu4e-account-aliases`

Per-account alias list.

#### `+mu4e-auto-bcc-address`

Auto add this/these addresses as BCC.

#### `+mu4e-gmail-accounts`

Gmail accounts that do not contain "gmail" in address and maildir.
An alist of Gmail addresses of the format
\='(("username@domain.com" . "account-maildir"))
to which Gmail integrations (behind the `+gmail` flag of the `mu4e`
module) should be applied. See `+mu4e-msg-gmail-p` and
`mu4e-sent-messages-behavior`.

#### `+org-responsive-image-percentage`

Maximum image width as a percentage of the window width.

#### `+org-responsive-image-width-limits`

The minimum and maximum width of a displayed image.

#### `+org-use-lower-case-keywords-and-properties`

Automatically convert Org keywords and properties to lowercase on save.

### Function and Macro Documentation

#### `(+with-delayed! &rest BODY)` (macro)

Delay evaluating BODY with priority 0 (high priority).

#### `(+with-delayed-1! &rest BODY)` (macro)

Delay evaluating BODY with priority 1.

#### `(+varplist-get VPLIST KEYWORD &optional CAR-P)`

Get KEYWORD's value from variable value length VPLIST.
Ex: (+varplist-get '(:a 'a :b 'b1 'b2) :b) -> '(b1 b2).

#### `(+plist-keys PLIST)`

Return the keys of PLIST.

#### `(+plist-push! PLIST &rest KEY-VALS)` (macro)

Push KEY-VALS to PLIST.

#### `(+plist-combine &rest PLISTS)`

Create a single property list from all plists in PLISTS.
Modified from `org-combine-plists`. This supposes the values to be vectors,
and concatenate them.

#### `(+plist-delete PLIST PROP)`

Delete property PROP from PLIST.
Adapted from `org-plist-delete`.

#### `(+plist-to-alist PLIST &optional TRIM-COL)`

Convert PLIST to an alist, trim first colon when TRIM-COL.

#### `(+alist-to-plist ALIST &optional ADD-COL)`

Convert ALIST to a plist, add colon to the keys when ADD-COL.

#### `(+alist-set KEY VAL ALIST &optional SYMBOL)`

Set property KEY to VAL in ALIST. Return new alist.
This creates the association if it is missing, and otherwise sets the cdr of the
first matching association in the list. It does not create duplicate
associations. By default, key comparison is done with `equal`. However, if
SYMBOL is non-nil, then `eq` is used instead.
This method may mutate the original alist, but you still need to use the return
value of this method instead of the original alist, to ensure correct results.

#### `(+add-to-list-at LIST-VAR ELEMENT INDEX)`

Insert into LIST-VAR an ELEMENT at INDEX.
If INDEX is 0, ELEMENT is inserted before the first element.

#### `(+unquote EXPR)`

Return EXPR unquoted.
  This function does not change global state, including the match data.

#### `(+quoted-p EXPR)`

Return t when EXPR is quoted.

#### `(+apply-partially-right FUN &rest ARGS)`

Like `apply-partially`, but apply the ARGS to the right of FUN.

#### `(+reverse-args FUN)`

Return a function that calls FUN with arguments in the reversed order.

#### `(+error! MSG &rest VARS)` (macro)

Log error MSG and VARS using `message`.

#### `(+info! MSG &rest VARS)` (macro)

Log info MSG and VARS using `message`.

#### `(+log! MSG &rest VARS)` (macro)

Log MSG and VARS using `message` when `minemacs-verbose-p` is non-nil.

#### `(+debug! MSG &rest VARS)` (macro)

Log debug MSG and VARS using `message` when `minemacs-msg-level` is 4.

#### `(+shutup! &rest BODY)` (macro)

Suppress new messages temporarily while evaluating BODY.
This inhebits both the echo area and the `*Messages*` buffer. If `:log` is
provided as the first argument, inhibit messages but keep writing them to the
`*Messages*` buffer.

#### `(+apply-inhibit-messages FN &rest ARGS)`

Call FN with ARGS while to suppressing the messages in echo area.
If `minemacs-verbose-p` is non-nil, do not print any message to
*Messages* buffer.

#### `(+load-theme)`

Load Emacs' theme from `minemacs-theme`.

#### `(+eval-when-idle DELAY &rest FNS)`

Queue FNS to be processed when Emacs becomes idle after DELAY seconds.

#### `(+eval-when-idle! &rest BODY)` (macro)

Evaluate BODY when Emacs becomes idle.

#### `(+eval-when-idle-for! DELAY &rest BODY)` (macro)

Evaluate BODY after DELAY seconds from Emacs becoming idle.

#### `(+deferred! &rest BODY)` (macro)

Run BODY after Emacs gets loaded, a.k.a. after `minemacs-loaded`.

#### `(+lazy! &rest BODY)` (macro)

Run BODY as a lazy block (see `minemacs-lazy`).

#### `(+make-first-file-hook! FILETYPE EXT-REGEXP)` (macro)

Make a hook which run on the first FILETYPE file of a particular extensions.
The extension should matches EXT-REGEXP.
This will creates a function named `+first-file--FILETYPE-h` which gets executed
before `after-find-file`. This function will run on the first file that matches
EXT-REGEXP. When it runs, this function provides a feature named
`minemacs-first-FILETYPE-file` and a run all hooks in
`minemacs-first-FILETYPE-file-hook`.

#### `(+resolve-hook-forms HOOKS)`

Convert a list of modes into a list of hook symbols.
If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is.
  This function does not change global state, including the match data.

#### `(+setq-hook-fns HOOKS REST &optional SINGLES ADVICE-HOW)`

HOOKS REST SINGLES ADVICE-HOW.

#### `(+setq-hook! HOOKS &rest [SYM VAL]...)` (macro)

Set buffer-local variables on HOOKS.
HOOKS can be expect receiving arguments (like in `enable-theme-functions`), the
`args` variable can be used inside VAR-VALS forms to get the arguments passed
the the function.
(+setq-hook! 'enable-theme-functions
  current-theme (car args))

#### `(+setq-advice! FUNCS HOW &rest [SYM VAL]...)` (macro)

Set buffer-local variables as HOW advices for FUNCS.
FUNCS can be expect receiving arguments, the `args` variable can
be used inside VAR-VALS forms to get the arguments passed the the
function.
(+setq-advice! #'revert-buffer :before
  revert-buffer-function #'ignore)

#### `(+unsetq-hook! HOOKS &rest VAR1 VAR2...)` (macro)

Unbind setq hooks on HOOKS for VARS.

#### `(+fn-sans-advice SYM)`

Get original function defined at SYM, sans advices.

#### `(+ignore-root &rest ROOTS)`

Add ROOTS to ignored projects, recentf, etc.

#### `(+package-disabled-p PACKAGE &optional MODULE)`

Is package PACKAGE disabled in `minemacs-disabled-packages`.
Optionally, check also for the containing MODULE.

#### `(+package-configured-p PACKAGE)`

Check if the PACKAGE has been configured by MinEmacs.
This is only valable after loading all modules (in the user's "config.el").

#### `(minemacs-modules &optional INCLUDE-ON-DEMAND INCLUDE-OBSOLETE)`

List all the available modules.
With optional INCLUDE-ON-DEMAND and INCLUDE-OBSOLETE.

#### `(minemacs-load-module &rest MODULES)`

Interactively install and load MODULES that aren't enabled in "modules.el".
When called with C-u, it prompts also for on-demand modules.
When called with C-u C-u, it prompts also for obsolete modules.

#### `(+shell-command-to-string-ignore-stderr COMMAND)`

Execute shell command COMMAND and return its output as a string.
Works like `shell-command-to-string` with three differences:
1. It uses `+shell-command-switch` instead of `shell-command-switch`.
2. It returns only stdout and ignore the output of stderr.
3. It sets TERM to "smart" instead of "dumb", to be able to escape from
Emacs-specific early exit in ".bashrc".

#### `(+env-save)`

Load environment variables from shell and save them to `+env-file`.

#### `(+env-load)`

Load environment variables from `+env-file`.

#### `(+file-read-to-string FILENAME)`

Return a string with the contents of FILENAME.

#### `(+directory-subdirs DIR)`

Return a list of sub-directories in DIR.

#### `(+directory-ensure &rest PATH-PARTS)`

Concatenate PATH-PARTS to construct a path and return it.
Ensure the path exists, if not create it. The exact behavior is to create the
parent directory if the path is a file, and if the path is a directory, create
that directory.

#### `(+lockedp NAME)`

Return non-nil if the resource NAME is locked.

#### `(+locked-by-current-process-p NAME)`

Return non-nil if the resource NAME locked by the current Emacs instance.

#### `(+lock NAME)`

Lock the resource named NAME.

#### `(+unlock NAME &optional FORCE-P)`

Unlock the resource named NAME if locked by this process.
If FORCE-P is non-nil, force unlocking even if the resource is not locked by the
current process.

#### `(+def-dedicated-tab! NAME [[:exit-hook HOOK] [:exit-func FUNC]]FORMS...)` (macro)

Define +CMD command to run BODY in a dedicated tab.
If not specified, BODY defaults to `(CMD)`.
You can pass an exit hook or exit function on which, the created workspace will
be deleted.

#### `(+eglot-auto-enable)`

Auto-enable Eglot in configured modes in `+eglot-auto-enable-modes`.

#### `(+eglot-use-on-all-supported-modes &optional MODE-LIST)`

Add all modes in MODE-LIST to `+eglot-auto-enable-modes`.

#### `(+eglot-register MODES &rest SERVERS)`

Register MODES with LSP SERVERS.
Examples:
(+eglot-register 'vhdl-mode "vhdl_ls")
(+eglot-register 'lua-mode "lua-language-server" "lua-lsp")
(+eglot-register '(c-mode c++-mode) '("clangd" "--clang-tidy" "-j=12") "ccls")

#### `(+project-super-project-try DIR)`

Find super-project root starting from DIR.

#### `(+super-project-current &optional DIR)`

Return the current super-project instance in DIR.

#### `(+super-project-define-commands PACKAGE &rest COMMANDS)`

Define PACKAGE's COMMANDS for super-project context.

#### `(+project-safe-root &optional PROJ)`

Return the root of PROJ using several backends, don't fail.

#### `(minemacs-get-enabled-proxies)`

Get a list of enabled proxies.

#### `(minemacs-set-enabled-proxies PROXIES)`

Set PROXIES.

#### `(minemacs-enable-proxy PROXIES)`

Set *_proxy Linux environment variables from PROXIES.

#### `(minemacs-disable-proxy)`

Unset *_proxy Linux environment variables.

#### `(+with-no-proxies! &rest BODY)` (macro)

Run BODY without proxies. Doesn't work with `emacs-async`.
Example:
  (+with-no-proxies! (async-shell-command "git fetch --all")).

#### `(+serialize-sym SYM &optional DIR FILENAME-FORMAT)`

Serialize SYM to DIR.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex.
"file-%s.el"). Return the written file name, or nil if SYM is not bound.

#### `(+deserialize-sym SYM &optional DIR MUTATE FILENAME-FORMAT)`

Deserialize SYM from DIR, if MUTATE is non-nil, assign the object to SYM.
If FILENAME-FORMAT is non-nil, use it to as a format (ex. "file-%s.el").
Return the deserialized object, or nil if the SYM.el
file dont exist.

#### `(+font-installed-p FONT-FAMILY)`

Check if FONT-FAMILY is installed on the system.

#### `(+apply-font-or-script SCRIPT-OR-FACE)`

Set font for SCRIPT-OR-FACE from `minemacs-fonts-plist`.

#### `(+setup-fonts)`

Setup fonts.

#### `(minemacs-reduce-font-size &optional RESET)`

Reduce the font size in the buffer by `minemacs-reduce-font-size-ratio`.
When RESET is non-nil, restore the original font size.

#### `(+subtle-mode-line)`

Subtle look for the mode-line.

#### `(+color-subtle BASE-COLOR PERCENTAGE &optional FACE-ATTR)`

Make a more subtle color based on BASE-COLOR and PERCENTAGE.
We mean by subtle here, a darker color in dark themes and a lighter
color in light themes.
BASE-COLOR can be a color (string) or a face.
When it is a face, the FACE-ATTR needs to be provided, otherwise, the
:background attribute will be used.

#### `(+nerd-icons-icon NAME &rest ARGS)`

Generic function to get icons by NAME, with ARGS.

#### `(+make-buffer-conds &rest CONDITIONS)`

Return a lambda that matches CONDITIONS.
To be used as a predicate generator for `display-buffer-alist`.

#### `(minemacs-on-demand-try)`

Loop over on-demand modules and load the ones available for the buffer.

#### `(minemacs-on-demand-try-load-companion-packages)`

Load companion packages for the current buffer's mode.

#### `(minemacs-load-companion-packages-for-buffer)`

Load companion packages applicables to the current's buffer mode.

#### `(+prog-mode-run-hooks)`

Run the hooks in `prog-mode-hook`.

#### `(minemacs-generate-loaddefs)`

Generate MinEmacs' loaddefs file.

#### `(+load-user-configs &rest CONFIGS)`

Load user configurations CONFIGS.

#### `(+load &rest FILENAME-PARTS)`

Load a file, the FILENAME-PARTS are concatenated to form the file name.

#### `(minemacs-run-build-functions &optional DONT-ASK-P)`

Run all build functions in `minemacs-build-functions`.
Call functions without asking when DONT-ASK-P is non-nil.

#### `(minemacs-root-dir-cleanup)`

Cleanup MinEmacs' root directory.

#### `(minemacs-cleanup-emacs-directory)`

Cleanup unwanted files/directories from MinEmacs' directory.

#### `(minemacs-user-config ASK)`

Open MinEmacs user configuration.
When ASK is non-nil (C-u), ask about which file to open.

#### `(+clean-file-name FILENAME &optional DOWNCASE-P)`

Clean FILENAME, optionally convert to DOWNCASE-P.

#### `(+file-name-incremental FILENAME)`

Return a unique file name for FILENAME.
If "file.ext" exists, returns "file-0.ext".

#### `(+delete-this-file &optional PATH FORCE-P)`

Delete PATH.
If PATH is not specified, default to the current buffer's file.
If FORCE-P, delete without confirmation.

#### `(+delete-file-or-directory FILE-OR-DIRECTORY &optional TRASHRECURSIVE)`

Delete FILE-OR-DIRECTORY with `delete-file` or `delete-directory`.
Move to trash when TRASH is non-nil, delete directories recursively when
RECURSIVE is non-nil.

#### `(+delete-current-file-and-buffer &optional FILENAME)`

Delete FILENAME and its associated visiting buffer.

#### `(+copy-current-file-name)`

Save (copy) the file name of this buffer to the kill ring.

#### `(+patch-get-patched-files PATCH-BUFF)`

Get the list of the patches A/B files mentioned in PATCH-BUFF.

#### `(+apply-patch-dwim PATCH-BUF &optional PROJ-DIR)`

Apply PATCH-BUF to the relevant file in PROJ-DIR.
When a region is active, propose to use it as the patch buffer.

#### `(+html2pdf INFILE OUTFILE &optional BACKEND)`

Convert HTML file INFILE to PDF and save it to OUTFILE.
When BACKEND is provided, the corresponding program is used, otherwise, the
value of `+html2pdf-default-backend` is used.

#### `(+txt2html INFILE OUTFILE &optional MAIL-MODE-P)`

Convert plain-text file INFILE to HTML and save it to OUTFILE.
When MAIL-MODE-P is non-nil, --mailmode is passed to "txt2html".

#### `(+save-as-pdf INFILE &optional MAIL-MODE-P)`

Save URL as PDF.
This function's signature is compatible with `browse-url-browser-function`
so it can be used to save HTML pages or emails to PDF.
When MAIL-MODE-P is non-nil, treat INFILE as a mail.

#### `(+single-file URL OUT-FILE)`

Save URL into OUT-FILE as a standalone HTML file.

#### `(+browse-html-file FILE)`

Browser HTML FILE following `+browse-html-file-browser-priority`.
If no function from `+browse-html-file-browser-priority` is available,
use `browse-url`.
When called with universal argument, open the current buffer's file.

#### `(+serial-running-p)`

Is there a serial port terminal running?

#### `(+serial-run-commands COMMANDS &optional PORT BAUD)`

Run COMMANDS on a device via serial communication.
If PORT or BAUD are nil, use values from `+serial-port` and `+serial-baudrate`.

#### `(+net-get-ip-address &optional DEV)`

Get the IP-address for device DEV of the current machine.

#### `(+github-latest-release REPO &optional FALLBACK-RELEASE TRIM-V-PREFIX)`

Get the latest release of REPO. Strips the "v" at left.
Fallback to FALLBACK-RELEASE when it can't get the last one.
When TRIM-V-PREFIX is non-nil, trim the "v" prefix from the version.

#### `(+dir-locals-reload-for-this-buffer)`

Reload directory-local for the current buffer.

#### `(+dir-locals-reload-for-all-buffers-in-this-directory)`

Reload dir-locals for all buffers under `default-directory`.

#### `(+dir-locals-autoreload)`

Is it relevant to auto reload dir-locals for his buffer.

#### `(+ansi-color-apply-on-buffer)`

Decode and apply ANSI color chars in the curernt buffer.

#### `(+what-faces POS)`

Get the font faces at POS.

#### `(+completion-mark-category SEQ CATEGORY)`

Mark SEQ as being in CATEGORY for use with `completing-read`.

#### `(+region-or-thing-at-point &optional LEAVE-REGION-MARKED)`

Return the region or the thing at point.
If LEAVE-REGION-MARKED is no-nil, don't call `desactivate-mark`
when a region is selected.

#### `(+insert-thing-at-point)`

Insert region or symbol in the minibuffer.

#### `(+kill-region-or-backward-word)`

Kill selected region if region is active. Otherwise kill a backward word.

#### `(+kill-whitespace-or-word ARG)`

Kill forward whitespace or word.
With argument ARG, do this that many times.
Restricts the effect of `kill-word` to the current line.

#### `(+backward-kill-whitespace-or-word ARG)`

Kill backward whitespace or word.
With argument ARG, do this that many times.
Restricts the effect of `backward-kill-word` to the current line.

#### `(+set-indent-width WIDTH)`

Change the indentation size to WIDTH of the current buffer.
The effectiveness of this command is significantly improved if
you have `editorconfig` or `dtrt-indent` installed.

#### `(+goto-line N)`

Go to line N, like `goto-line` but for Lisp code.

#### `(+autoload-region BEG END)`

Add the ;;;###autoload to region (BEG . END).

#### `(+webjump-read-string-with-initial-query PROMPT)`

To be used as a replacement for `webjump-read-string`, PROMPT.

#### `(+webjump)`

Like `webjump`, with initial query filled from `+region-or-thing-at-point`.

#### `(+eglot-ccls-inheritance-hierarchy &optional DERIVED)`

Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point.

#### `(+eglot-help-at-point)`

Request documentation for the thing at point.

#### `(+shellcheck-describe-error &rest CODES)`

Describe a ShellCheck message CODES.

#### `(+explainshell COMMAND)`

Get an explanation of the shell COMMAND from explainshell.com.

#### `(+fetch-json-from-url URL)`

Get an Emacs JSON object from a specified URL.

#### `(+json-schemas-for-file FILENAME)`

Get a list of JSON Schemas that apply to FILENAME.

#### `(+json-schemas-catalog &optional REFRESH)`

Get the catalog of schemas from JSON Schemas Store.
When REFRESH is non-nil, don't use the cached version and force
reloading the JSON file.

#### `(+yaml-insert-schema &optional ASK)`

Insert a schema for the current buffer file.
When ASK is non-nil, ask which schema to insert without trying to guess
the schema from the file name.

#### `(+clang-format-get-style &optional NO-OPT)`

Get the "-style=XXX" argument for clang-format.
When NO-OPT isn non-nil, don't return the "-style=" part.

#### `(+editorconfig-guess-style-from-clang-format)`

Set the editor tab and indent widths from ".clang-format".

#### `(+server-restart)`

Restart the Emacs server.

#### `(+kill-buffer-and-its-windows BUFFER &optional MSGP)`

Kill BUFFER and delete its windows.
Default is `current-buffer`. When MSGP is non-nil, signal an error when
the buffer isn't alive. BUFFER may be either a buffer or its name (a
string).

#### `(+region-to-buffer START END BUFFER ARG)`

Copy region to BUFFER: At beginning (prefix >= 0), end (< 0), or replace.
START and END are the region boundaries.
BUFFER is a buffer or its name (a string).
With prefix ARG >= 0: `append-to-buffer`:
  Append contents of region to end of BUFFER.
  (Point is moved to end of BUFFER first.)
With prefix ARG < 0:  `prepend-to-buffer`:
  Prepend contents of region to beginning of BUFFER.
  (Point is moved to beginning of BUFFER first.)
With no prefix ARG (nil): `copy-to-buffer`.
  Write region to BUFFER, replacing any previous contents.

#### `(+region-to-file START END FILENAME ARG)`

With prefix arg, this is `append-to-file`.  Without, it is `write-region`.
START and END are the region boundaries.
Prefix ARG non-nil means append region to end of file FILENAME.
Prefix ARG nil means write region to FILENAME, replacing contents.

#### `(+kill-some-buffers &optional LIST)`

Kill some buffers.  Asks the user whether to kill the modified ones.
Non-interactively, if optional argument LIST is non-nil, it
specifies the list of buffers to kill, asking for approval for each one.
See `kill-some-buffers`.

#### `(+kill-buffer-ask-if-modified BUFFER)`

Like `kill-buffer-ask`, but don't ask if BUFFER isn't modified.
Kill without asking for buffer names in `+kill-buffer-no-ask-list`.

#### `(+delete-extra-windows-for-buffer)`

Delete all other windows showing the selected window's buffer.

#### `(+delete-window-maybe-kill-buffer)`

Delete selected window.
If no other window shows its buffer, kill the buffer too.

#### `(+replace-in-buffer OLD NEW)`

Replace OLD with NEW in the current buffer.

#### `(+clear-frenchy-ponctuations)`

Replace french ponctuations (like unsectable space) by regular ones.

#### `(+save-buffer-preserving-modtime)`

Call `save-buffer`, but keep the visited file's modtime the same.

#### `(+kill-region-as-paragraph)`

Kill (copy) region as one paragraph.
This command removes new line characters between lines.

#### `(+first-line-empty-p)`

Return t when the first line of the buffer is empty.

#### `(+project-forget-zombie-projects)`

Forget all known projects that don't exist any more.
Like `project-forget-zombie-projects`, but handles remote projects differently,
it forget them only when we are sure they don't exist.

#### `(+project-gdb)`

Invoke `gdb` in the project's root.

#### `(+project-list-cleanup)`

Forget all duplicate known projects (like /home/user/proj and ~/proj).

#### `(+project-root-initialize)`

Initialize project list from `+project-root-wildcards`.

#### `(+xref-find-references-at-point)`

Find references to the identifier at or around point.

#### `(minemacs-extract-packages-descriptions)`

Extract the descriptions of MinEmacs packages.

#### `(+list-external-dependencies)`

Show the list of declared external dependencies.

#### `(+adb-run-command &rest ARGS)`

Run adb with command ARGS.

#### `(+adb-push SRC DEST)`

Run adb push SRC DEST.

#### `(+adb-remount AUTO-REBOOT-DEVICE)`

Run adb remount, with -R when AUTO-REBOOT-DEVICE is non-nil.

#### `(+adb-reboot MODE)`

Run adb reboot MODE.

#### `(+adb-root &optional ARG)`

Run adb root (or unroot with C-u).

#### `(+cocogitto-bump LEVEL &optional PRE)`

Bump version LEVEL (`auto`, `major`, `minor` or `patch`).
When PRE is provided, it is used as pre-release suffix.
Call with C-u for applying an `auto` bump.
This command stashes the current workspace before bumping the version, and
restores it after that.

#### `(eglot-ltex-ls-install PRE)`

Download the latest release of LTeX+ LS.
When PRE is non-nil, allow downloading the latest prerelease.

#### `(eglot-ltex-workspace-config-fn &optional SERVER)`

A function to use as a value of `eglot-workspace-configuration`.
It generates the workspace configuration dynamically, taking into account
changed values of `eglot-ltex-language`, `eglot-ltex-dictrionary`, and so on.

#### `(eglot-ltex-enable-handling-client-commands)`

Enable Eglot hack to handle code actions of LTeX-LS.

#### `(eglot-ltex-disable-handling-client-commands)`

Disable Eglot hack to handle code actions of LTeX-LS.

#### `(+mu4e-part-selectors PARTS)`

Generate selection strings for PARTS.

#### `(+mu4e-view-select-attachment)`

Use `completing-read` to select a single attachment.
Acts like a singular `mu4e-view-save-attachments`, without the saving.

#### `(+mu4e-view-open-attachment)`

Select an attachment, and open it.

#### `(+mu4e-view-select-mime-part-action)`

Select a MIME part, and perform an action on it.

#### `(+mu4e-view-save-all-attachments &optional ASK-DIR)`

Save all files from the current view buffer.
With ASK-DIR is non-nil, user can specify the target-directory; otherwise
one is determined using `mu4e-attachment-dir`.

#### `(+mu4e-register-account LABEL MAILDIR LETVARS &optional DEFAULT-PGMAIL-P)`

Register a mu4e context named LABEL, located in MAILDIR.
LETVARS contains the alist of local variables with their values.
If DEFAULT-P is non-nil, the context is placed first and considered the default
one. If GMAIL-P is non-nil, addresses are saved to `+mu4e-gmail-accounts` to be
used later for Gmail specific actions.

#### `(+mu4e-save-message-at-point &optional MSG)`

Copy MSG at point to somewhere else as <date>_<subject>.eml.

#### `(+mu4e-view-save-mail-as-pdf &optional MSG SKIP-HEADERS)`

Save current MSG as PDF.
If SKIP-HEADERS is set, do not show include message headers.

#### `(+mu4e-extras-locks-setup)`

Setup locks for mu4e's server.

#### `(+mu4e-sent-from-gmail-p &optional MSG)`

Return the "from" address if it is in the registred Gmail accounts.
If MSG is provided, use it, else, extract the "from" field
from the envelope of the current message.

#### `(+mu4e-colorize-str STR &optional UNIQUE HERRING)`

Apply a face from `+mu4e-header-colorized-faces` to STR.
If HERRING is set, it will be used to determine the face instead of STR.
Will try to make unique when non-nil UNIQUE,
a quoted symbol for a alist of current strings and faces provided.

#### `(+mu4e-ui-setup)`

Apply UI setup.

#### `(+mu4e-ui-modeline-tweaks)`

Apply UI tweaks based on `nerd-icons`.

#### `(+org-extras-toggle-latex-equation-numbering &optional ENABLE)`

Toggle whether LaTeX fragments are numbered.
Force enabling when ENABLE is non-nil.

#### `(+org-extras-inject-latex-fragment:around-a ORIG-FUNC &rest ARGS)`

Advice function to inject latex code before and/or after the equation in a latex fragment.
You can use this to set \mathversion{bold} for example to make it bolder.
The way it works is by defining :latex-fragment-pre-body and/or
:latex-fragment-post-body in the variable `org-format-latex-options`. These
strings will then be injected before and after the code for the fragment before
it is made into an image.

#### `(+org-extras-inject-latex-fragments)`

Toggle whether you can insert latex in fragments.

#### `(+org-lower-case-keywords-and-properties)`

Lower case Org keywords and properties and block identifiers.
Example: "#+TITLE" -> "#+title"
         "#+BEGIN_EXAMPLE" -> "#+begin_example"
         ":PROPERTIES:" -> ":properties:".

#### `(+org-extras-responsive-images-setup)`

Enable responsive images' size.

#### `(+org-extras-equation-numbering-setup)`

Enable LaTeX equations renumbering.

#### `(+org-extras-multifile-document-setup)`

Enable multi-files documents.

#### `(+org-extras-latex-classes-setup)`

Setup some extra LaTeX classes.

#### `(+org-extras-outline-path-setup)`

Fix the font size issue in Org's outline in the echo area.

#### `(+org-extras-pretty-latex-fragments-setup)`

Enable prettifing Org's LaTeX fragments.

#### `(+org-extras-lower-case-keywords-and-properties-setup)`

Automatically convert KEYWORDS to lower case on save.

#### `(+org-extras-setup)`

Enable all Org-mode extra tweaks.

#### `(+viper-operate-inside-delimiters OPEN CLOSE OP)`

Perform OP inside delimiters OPEN and CLOSE (e.g., (), {}, '', or "").

#### `(+viper-delete-inside-delimiters OPEN CLOSE)`

Delete text inside delimiters OPEN and CLOSE, saving it to the kill ring.

#### `(+viper-yank-inside-delimiters OPEN CLOSE)`

Copy text inside delimiters OPEN and CLOSE to the kill ring.

#### `(+viper-delete-line-or-region)`

Delete the current line or the selected region in Viper mode.
The deleted text is saved to the kill ring.

#### `(+viper-yank-line-or-region)`

Yank the current line or the selected region and highlight the region.

#### `(+viper-visual-select)`

Start visual selection from the current position.

#### `(+viper-visual-select-line)`

Start visual selection from the beginning of the current line.

#### `(+viper-delete-inner-word)`

Delete the current word under the cursor, handling edge cases.

#### `(+viper-change-inner-word)`

Change the current word under the cursor, handling edge cases.

#### `(+viper-yank-inner-word)`

Yank (copy) the current word under the cursor, handling edge cases.

#### `(+viper-delete-inner-compound-word)`

Delete the entire compound word under the cursor, including `-` and `_`.

#### `(viper-change-inner-compound-word)`

Change the entire compound word under the cursor, including `-` and `_`.

#### `(+viper-yank-inner-compound-word)`

Yank the entire compound word under the cursor into the kill ring.

#### `(+viper-compound-word-bounds)`

Get the bounds of a compound word under the cursor.
A compound word includes letters, numbers, `-`, and `_`.

#### `(+viper-go-to-nth-or-first-line ARG)`

Go to the first line of the document, or the ARG-nth.

#### `(+viper-go-to-last-line)`

Go to the last line of the document.

#### `(+viper-window-split-horizontally)`

Split the window horizontally (mimics Vim's `C-w s`).

#### `(+viper-window-split-vertically)`

Split the window vertically (mimics Vim's `C-w v`).

#### `(+viper-window-close)`

Close the current window (mimics Vim's `C-w c`).

#### `(+viper-window-maximize)`

Maximize the current window (mimics Vim's `C-w o`).

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
