<img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right">

           ..                        ...                                   ..OOOOO.....       .OOOO.     ..
        .OOO.                       OOOOO                                  OOOOOOOOOOOOOOOOOOOOOOOOO     .OOO.
       OOO                          .OOO.                                  .OOOOOO.        ...OOOOO.        OOO
      OO.                                                                   .OOOOO                           .OO
     OO.       OOOOOOOO..OOO    OOOOOOO       .OOOO.OOOOO.                    .OOOO.                          .OO
    .OO        .OOO .OOO OOO.   ...OOOO        OOOOO..OOOO                       .OOO.                         OO.
    OO.        .OO. OOO. OOO       OOO.        OOO.    OOO.               ..........OOOO.                      .OO
    OO.        OOO  OOO  OOO       OOO        .OOO     OOO          ..OOOOOOOOOOOOOOOOOOOO.                    .OO
    OO.       .OOO .OOO .OO.      .OOO        OOOO    .OOO        .OOOOOOOOOO...                               .OO
    OOO       OOO. OOO. OOO       OOO.   ..   OOO.    OOO.        OOOOOOOOO                                    OOO
     OO       OOO  OOO  OOO.      OOOO.OOOO  .OOO    .OOOO         OOOOOOOO                                    OO
     .OO      OO.  OO. .OOOO       .OOOOO.   .OO.    .OOOO          .OOOOOOOO.                                OO.
      OOO                                                              .OOOOOOOO...             O.           OOO
       .OO.                                                               ..OOOOOOOOOOOO.......OOO.        .OO.
         .OO.                                                                   ..OOOOOOOOOOOOOOO.       .OO.

                                MINIMALIST & LIGHTWEIGHT EMACS CONFIGURATION FRAMEWORK
                                                         abougouffa.github.io/minemacs

Load and hooks order:
- `~/.emacs.d/early-init.el`
- `$MINEMACSDIR/early-config.el` (unless disabled in `$MINEMACS_IGNORE_USER_CONFIG`)
- `$MINEMACSDIR/local/early-config.el` (unless disabled)
- `~/.emacs.d/init.el`
  * `before-init-hook`
  * `~/.emacs.d/core/me-vars.el`
  * `~/.emacs.d/core/backports/*.el` (when Emacs < 29)
  * `~/.emacs.d/core/me-loaddefs.el`
  * `$MINEMACSDIR/init-tweaks.el` (unless disabled)
  * `$MINEMACSDIR/local/init-tweaks.el` (unless disabled)
  * `$MINEMACSDIR/modules.el` (unless disabled)
  * `$MINEMACSDIR/local/modules.el` (unless disabled)
  * `~/.emacs.d/core/<module>.el` (for module in `minemacs-core-modules`)
  * `~/.emacs.d/modules/<module>.el` (for module in `minemacs-modules`)
  * `minemacs-after-loading-modules-hook`
  * `$MINEMACSDIR/custom-vars.el`
  * `$MINEMACSDIR/config.el` (unless disabled)
  * `$MINEMACSDIR/local/config.el` (unless disabled)
  * `after-init-hook`
  * `emacs-startup-hook`
  * `minemacs-after-startup-hook`
    + `minemacs-lazy-hook` (delayed)

Special hooks defined with `+make-first-file-hook!`
- `minemacs-first-file-hook`
- `minemacs-first-elisp-file-hook`
- `minemacs-first-org-file-hook`



### Customization Documentation

#### `minemacs-msg-level`

Level of printed messages.
1 - `+error!`
2 - `+info!`
3 - `+log!`

#### `minemacs-leader-key`

MinEmacs leader key.

#### `minemacs-localleader-key`

MinEmacs local leader (a.k.a. mode specific) key sequence.

#### `minemacs-global-leader-prefix`

MinEmacs general leader key.

#### `minemacs-global-mode-prefix`

MinEmacs general local leader (a.k.a. mode specific) key sequence.

#### `minemacs-theme`

The theme of MinEmacs.

#### `minemacs-disabled-packages`

List of packages to be disabled when loading MinEmacs modules.
This can be useful if you want to enable a module but you don't want a package
of being enabled.

#### `minemacs-after-loading-modules-hook`

This hook will be run after loading MinEmacs modules.
It is used internally to remove the `+use-package--check-if-disabled:around-a`
advice we set on `use-package` in `me-bootstrap`.

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

#### `+env-file`

The file in which the environment variables will be saved.

#### `+env-deny-vars`

Environment variables to omit.
Each string is a regexp, matched against variable names to omit from
`+env-file` when saving evnironment variables in `+env-save`.

#### `+eval-when-idle-delay`

The default delay (in seconds) to consider in `+eval-when-idle!` macro.

#### `+lazy-delay`

The default delay (in seconds) to consider in `+lazy!` macro.

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

#### `+screenshot-delay`

A delay to wait before taking the screenshot.
Applicable only when calling `+screenshot-svg` with a prefix.

#### `+eglot-auto-enable-modes`

Modes for which Eglot can be automatically enabled by `+eglot-auto-enable`.

#### `+binary-objdump-executable`

The "objdump" command.

#### `+binary-objdump-enable`

Enable or disable disassembling suitable files with objdump.

#### `+binary-hexl-enable`

Enable or disable opening suitable files in `hexl-mode`.

#### `+kill-buffer-no-ask-list`

A list of buffer names to be killed without confirmation.

#### `+project-scan-dir-paths`

A list of paths to scan and add to known projects list.
It can be a list of strings (paths) or a list of (cons "~/path" recursive-p) to scan directories recursively.

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

#### `+mpv-command`

The MPV command.

#### `+ros-mcap-command`

ROS 2 MCAP command.

#### `+ros-rosbag-command`

ROS 1 "rosbag" command.

#### `+ros-ros2-command`

ROS 2 "ros2" command.

#### `eglot-ltex-user-rules-path`

Path to save user rules.

#### `+mu4e-account-aliases`

Per-account alias list.

#### `+mu4e-auto-bcc-address`

Auto add this/these addresses as BCC.

#### `+mu4e-gmail-accounts`

Gmail accounts that do not contain "gmail" in address and maildir.
An alist of Gmail addresses of the format (("username@domain.com" . "account-maildir"))
to which Gmail integrations (behind the `+gmail` flag of the `mu4e` module) should be applied.
See `+mu4e-msg-gmail-p` and `mu4e-sent-messages-behavior`.

#### `+org-responsive-image-percentage`

Maximum image width as a percentage of the window width.

#### `+org-responsive-image-width-limits`

The minimum and maximum width of a displayed image.

#### `+org-use-lower-case-keywords-and-properties`

Automatically convert Org keywords and properties to lowercase on save.

#### `+writing-mode-enable-hook`

Execute hooks on enable.

#### `+writing-mode-disable-hook`

Execute hooks on disable.

#### `+writing-text-scale`

The text-scaling level for `+writing-mode`.

#### `+writing-text-width`

Like `visual-fill-column-width`.

#### `+writing-mixed-pitch-enable`

Enable `mixed-pitch-mode` with `+writing-mode` for some modes defined in `+writing-mixed-pitch-modes`.

#### `+writing-mixed-pitch-modes`

Enable `mixed-pitch-mode` with `+writing-mode` for these modes.

#### `+writing-major-modes`

Enable `+writing-global-mode` for these exact modes.

#### `+writing-derived-major-modes`

Enable `+writing-global-mode` for modes derived these modes.

### Function and Macro Documentation

#### `(minemacs-generate-loaddefs)`

Generate MinEmacs' loaddefs file.

#### `(+load-user-configs &rest CONFIGS)`

Load user configurations CONFIGS.

#### `(+load &rest FILENAME-PARTS)`

Load a file, the FILENAME-PARTS are concatenated to form the file name.

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

#### `(+set-standard-value VARIABLE VALUE)`

Set the standard value of VARIABLE to VALUE.

#### `(+standard-value VARIABLE)`

Return the standard value for VARIABLE.

#### `(+reset-sym SYM)`

Reset SYM to its standard value.

#### `(+reset-var! VAR)` (macro)

Reset VAR to its standard value.

#### `(+unquote EXPR)`

Return EXPR unquoted.
  This function does not change global state, including the match data.

#### `(+quoted-p EXPR)`

Return t when EXPR is quoted.

#### `(+apply-partially-right FUN &rest ARGS)`

Like `apply-partially`, but apply the ARGS to the right of FUN.

#### `(+error! MSG &rest VARS)` (macro)

Log error MSG and VARS using `message`.

#### `(+info! MSG &rest VARS)` (macro)

Log info MSG and VARS using `message`.

#### `(+log! MSG &rest VARS)` (macro)

Log MSG and VARS using `message` when `minemacs-verbose-p` is non-nil.

#### `(+emacs-features-p &rest FEATS)`

Is features FEATS are enabled in this Emacs build.

#### `(+fn-inhibit-messages! FN &optional NO-MESSAGE-LOG)` (macro)

Add an advice around the function FN to suppress messages in echo area.
If NO-MESSAGE-LOG is non-nil, do not print any message to *Messages* buffer.

#### `(+shutup! &rest BODY)` (macro)

Suppress new messages temporarily while evaluating BODY.
This inhebits both the echo area and the `*Messages*` buffer.

#### `(+cmdfy! &rest BODY)` (macro)

Convert BODY to an interactive command.

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

#### `(+deferred-when! CONDITION &rest BODY)` (macro)

Like `+deferred!`, with BODY executed only if CONDITION is non-nil.

#### `(+deferred-unless! CONDITION &rest BODY)` (macro)

Like `+deferred!`, with BODY executed only if CONDITION is nil.

#### `(+lazy! &rest BODY)` (macro)

Run BODY as a lazy block (see `minemacs-lazy`).

#### `(+lazy-when! CONDITION &rest BODY)` (macro)

Like `+lazy!`, with BODY executed only if CONDITION is non-nil.

#### `(+lazy-unless! CONDITION &rest BODY)` (macro)

Like `+lazy!`, with BODY executed only if CONDITION is nil.

#### `(+after-load! FEATURES &rest BODY)` (macro)

Execute BODY after FEATURES have been loaded.

#### `(+hook-with-delay! HOOK SECS FUNCTION &optional DEPTH LOCAL)` (macro)

Add the FUNCTION to the value of HOOK.
The FUNCTION is delayed to be evaluated in SECS once HOOK is
triggered.
DEPTH and LOCAL are passed as is to `add-hook`.

#### `(+hook-once! HOOK &rest BODY)` (macro)

Hook BODY in HOOK, and make it run only once.

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

#### `(+add-hook! HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)` (macro)

A convenience macro for adding N functions to M hooks.
This macro accepts, in order:
  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun` or `cl-defun` forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).
If the hook function should receive an argument (like in
`enable-theme-functions`), the `args` variable can be expanded in the forms
  (+add-hook! 'enable-theme-functions
    (message "Enabled theme: %s" (car args)))

#### `(+remove-hook! HOOKS [:append :local] FUNCTIONS)` (macro)

A convenience macro for removing N functions from M hooks.
Takes the same arguments as `add-hook!`.
If N = 1 and M = 1, there's no benefit to using this macro over `remove-hook`.

#### `(+setq-hook! HOOKS &rest [SYM VAL]...)` (macro)

Set buffer-local variables on HOOKS.
HOOKS can be expect receiving arguments (like in `enable-theme-functions`), the
`args` variable can be used inside VAR-VALS forms to get the arguments passed
the the function.
  (+setq-hook! 'enable-theme-functions
    current-theme (car args))

#### `(+unsetq-hook! HOOKS &rest VAR1 VAR2...)` (macro)

Unbind setq hooks on HOOKS for VARS.

#### `(+compile-functions &rest FNS)`

Queue FNS to be byte/natively-compiled after a brief delay.

#### `(+shell-command-to-string-ignore-stderr COMMAND)`

Execute shell command COMMAND and return its output as a string.
Works like `shell-command-to-string` with two differences:
1. It uses `+shell-command-switch` instead of `shell-command-switch`.
2. It returns only stdout and ignore the output of stderr.

#### `(+env-save)`

Load environment variables from shell and save them to `+env-file`.

#### `(+env-load)`

Load environment variables from `+env-file`.

#### `(+ignore-root &rest ROOTS)`

Add ROOTS to ignored projects, recentf, etc.

#### `(+package-disabled-p PACKAGE &optional MODULE)`

Is package PACKAGE disabled in `minemacs-disabled-packages`.
Optionally, check also for the containing MODULE.

#### `(minemacs-run-build-functions &optional DONT-ASK-P)`

Run all build functions in `minemacs-build-functions`.
Call functions without asking when DONT-ASK-P is non-nil.

#### `(minemacs-bump-packages)`

Update MinEmacs packages to the last revisions (can cause breakages).

#### `(minemacs-restore-locked-packages RESTORE-FROM-BACKUP)`

Restore lockfile packages list. Takes into account the pinned ones.
When called with C-u or with RESTORE-FROM-BACKUP, it will
restore the lockfile from backups, not Git.

#### `(minemacs-upgrade PULL-MINEMACS)`

Upgrade MinEmacs and its packages to the latest pinned versions (recommended).
When PULL-MINEMACS is non-nil, run a "git pull" in MinEmacs' directory.
This calls `minemacs-update-restore-locked` asynchronously.

#### `(+minemacs-root-dir-cleanup)`

Cleanup MinEmacs' root directory.

#### `(+straight-prune-build-cache)`

Prune straight.el build directories for old Emacs versions.

#### `(+minemacs-cleanup-emacs-directory)`

Cleanup unwanted files/directories from MinEmacs' directory.

#### `(+file-mime-type FILE)`

Get MIME type for FILE based on magic codes provided by the "file" command.
Return a symbol of the MIME type, ex: `text/x-lisp`, `text/plain`,
`application/x-object`, `application/octet-stream`, etc.

#### `(+file-name-incremental FILENAME)`

Return a unique file name for FILENAME.
If "file.ext" exists, returns "file-0.ext".

#### `(+file-read-to-string FILENAME)`

Return a string with the contents of FILENAME.

#### `(+directory-subdirs DIR)`

Return a list of sub-directories in DIR.

#### `(+directory-ensure &rest PATH-PARTS)`

Concatenate PATH-PARTS to construct a path and return it.
Ensure the path exists, if not create it. The exact behavior is to create the
parent directory if the path is a file, and if the path is a directory, create
that directory.

#### `(+directory-root-containing-file FILES &optional START-PATH)`

Return the path containing a file from FILES starting from START-PATH.

#### `(+delete-this-file &optional PATH FORCE-P)`

Delete PATH.
If PATH is not specified, default to the current buffer's file.
If FORCE-P, delete without confirmation.

#### `(+delete-this-file-and-buffer &optional FILENAME)`

Delete FILENAME and its associated visiting buffer.

#### `(+delete-file-or-directory FILE-OR-DIRECTORY &optional TRASHRECURSIVE)`

Delete FILE-OR-DIRECTORY with `delete-file` or `delete-directory`.
Move to trash when TRASH is non-nil, delete directories recursively when
RECURSIVE is non-nil.

#### `(+tramp-sudo-file-path FILE)`

Construct a Tramp sudo path to FILE. Works for both local and remote files.

#### `(+sudo-find-file FILE)`

Open FILE as root.

#### `(+sudo-this-file)`

Open the current file as root.

#### `(+sudo-save-buffer)`

Save this buffer as root. Save as new file name if called with prefix.

#### `(+yank-this-file-name)`

Yank the file name of this buffer.

#### `(+clean-file-name FILENAME &optional DOWNCASE-P)`

Clean FILENAME, optionally convert to DOWNCASE-P.

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

#### `(+serial-running-p)`

Is there a serial port terminal running?

#### `(+serial-run-commands COMMANDS &optional PORT BAUD)`

Run COMMANDS on a device via serial communication.
If PORT or BAUD are nil, use values from `+serial-port` and `+serial-baudrate`.

#### `(+net-get-ip-address &optional DEV)`

Get the IP-address for device DEV (default: eth0) of the current machine.

#### `(+github-latest-release REPO &optional FALLBACK-RELEASE)`

Get the latest release of REPO. Strips the "v" at left.
Fallback to FALLBACK-RELEASE when it can't get the last one.

#### `(+lockedp NAME)`

Return non-nil if the resource NAME is locked.

#### `(+locked-by-this-process-p NAME)`

Return non-nil if the resource NAME locked by this Emacs instance.

#### `(+lock NAME)`

Lock the resource named NAME.

#### `(+unlock NAME &optional FORCE-P)`

Unlock the resource named NAME if locked by this process.
If FORCE-P is non-nil, force unlocking even if the resource is not locked by the
current process.

#### `(+dir-locals-reload-for-this-buffer)`

Reload directory-local for the current buffer.

#### `(+dir-locals-reload-for-all-buffers-in-this-directory)`

Reload dir-locals for all buffers in the current `default-directory`.

#### `(+dir-locals-toggle-autoreload &optional ENABLE)`

Toggle autoloading dir-local variables after editing the ".dir-locals" file.
If ENABLE is non-nil, force enabling autoreloading.

#### `(+dir-locals-open-or-create)`

Open or create the dir-locals.el for the current project.

#### `(+what-faces POS)`

Get the font faces at POS.

#### `(+screenshot-svg OUTFILE)`

Save a screenshot of the current frame as an SVG image to OUTFILE.
If launched with a prefix or universal argument, it waits for a moment (defined
by `+screenshot-delay`) before taking the screenshot.

#### `(+minibuffer-kill-minibuffer)`

Kill the minibuffer when switching to window with mouse.

#### `(+region-or-thing-at-point)`

Return the region or the thing at point.

#### `(+webjump)`

Like `webjump`, with initial query filled from `+region-org-thing-at-point`.

#### `(+def-dedicated-tab! NAME [[:exit-hook HOOK] [:exit-func FUNC]]FORMS...)` (macro)

Define +CMD command to run BODY in a dedicated tab.
If not specified, BODY defaults to `(CMD)`.
You can pass an exit hook or exit function on which, the created workspace will
be deleted.

#### `(+eglot-auto-enable)`

Auto-enable Eglot in configured modes in `+eglot-auto-enable-modes`.

#### `(+eglot-use-on-all-supported-modes MODE-LIST)`

Add all modes in MODE-LIST to `+eglot-auto-enable-modes`.

#### `(+eglot-register MODES &rest SERVERS)`

Register MODES with LSP SERVERS.
Examples:
  (+eglot-register 'vhdl-mode "vhdl_ls")
  (+eglot-register 'lua-mode "lua-language-server" "lua-lsp")
  (+eglot-register '(c-mode c++-mode) '("clangd" "--clang-tidy" "-j=12") "ccls")

#### `(+eglot-ccls-inheritance-hierarchy &optional DERIVED)`

Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point.

#### `(+binary-objdump-p FILENAME)`

Can FILENAME be recognized by "objdump".

#### `(+binary-objdump-buffer-p &optional BUFFER)`

Can the BUFFER be viewed as a disassembled code with objdump.

#### `(+binary-buffer-p &optional BUFFER)`

Return whether BUFFER or the current buffer is binary.
A binary buffer is defined as containing at least one null byte.
Returns either nil, or the position of the first null byte.

#### `(+binary-file-p FILE &optional CHUNK)`

Is FILE a binary?
This checks the first CHUNK of bytes, defaults to 1024.

#### `(+binary-hexl-buffer-p &optional BUFFER)`

Does BUFFER (defaults to the current buffer) should be viewed using `hexl-mode`.

#### `(+binary-hexl-mode-maybe)`

Activate `hexl-mode` if relevant for the current buffer.

#### `(+binary-setup-modes)`

Setup binary modes (objdump and hexl) for relevant buffers and file types.

#### `(+kill-buffer-and-its-windows BUFFER &optional MSGP)`

Kill BUFFER and delete its windows.  Default is `current-buffer`.
BUFFER may be either a buffer or its name (a string).

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

Like `kill-buffer-ask`, but kills BUFFER without confirmation when unmodified.
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

#### `(+kill-region-as-paragraph)`

Kill (copy) region as one paragraph.
This command removes new line characters between lines.

#### `(+first-line-empty-p)`

Return t when the first line of the buffer is empty.

#### `(+project-scan-for-projects &optional DIR)`

Scan and remember projects under DIR or `+project-scan-dir-paths`.

#### `(+project-add-project DIR &optional DONT-ASK)`

Switch to another project at DIR.
When DIR is not detected as a project, ask to force it to be by adding a
".project.el" file. When DONT-ASK is non-nil, create the file without asking.

#### `(+project-forget-zombie-projects)`

Forget all known projects that don't exist any more.
Like `project-forget-zombie-projects`, but handles remote projects differently,
it forget them only when we are sure they don't exist.

#### `(+project-gdb)`

Invoke `gdb` in the project's root.

#### `(+project-list-cleanup)`

Forget all duplicate known projects (/home/user/proj, ~/proj).

#### `(+systemd-running-p SERVICE)`

Check if the systemd SERVICE is running.

#### `(+systemd-command SERVICE COMMAND &optional PRE-FN POST-FN)`

Call systemd with COMMAND and SERVICE.

#### `(+systemd-start SERVICE &optional PRE-FN POST-FN)`

Start systemd SERVICE. Optionally run PRE-FN and POST-FN.

#### `(+systemd-stop SERVICE &optional PRE-FN POST-FN)`

Stops the systemd SERVICE. Optionally run PRE-FN and POST-FN.

#### `(minemacs-enable-proxy)`

Set *_proxy Linux environment variables from `minemacs-proxies`.

#### `(minemacs-disable-proxy)`

Unset *_proxy Linux environment variables.

#### `(+map! &rest ARGS)` (macro)

A wrapper around `+minemacs--internal-map!`.
It is deferred until `general` gets loaded and configured.

#### `(+map-local! &rest ARGS)` (macro)

A wrapper around `+minemacs--internal-map-local!`.
It is deferred until `general` gets loaded and configured.

#### `(+nmap! &rest ARGS)` (macro)

A wrapper around `general-nmap`.
It is deferred until `general` gets loaded and configured.

#### `(+vmap! &rest ARGS)` (macro)

A wrapper around `general-vmap`.
It is deferred until `general` gets loaded and configured.

#### `(+mmap! &rest ARGS)` (macro)

A wrapper around `general-mmap`.
It is deferred until `general` gets loaded and configured.

#### `(+imap! &rest ARGS)` (macro)

A wrapper around `general-imap`.
It is deferred until `general` gets loaded and configured.

#### `(+emap! &rest ARGS)` (macro)

A wrapper around `general-emap`.
It is deferred until `general` gets loaded and configured.

#### `(+omap! &rest ARGS)` (macro)

A wrapper around `general-omap`.
It is deferred until `general` gets loaded and configured.

#### `(+rmap! &rest ARGS)` (macro)

A wrapper around `general-rmap`.
It is deferred until `general` gets loaded and configured.

#### `(+iemap! &rest ARGS)` (macro)

A wrapper around `general-iemap`.
It is deferred until `general` gets loaded and configured.

#### `(+nvmap! &rest ARGS)` (macro)

A wrapper around `general-nvmap`.
It is deferred until `general` gets loaded and configured.

#### `(+serialize-sym SYM &optional DIR FILENAME-FORMAT)`

Serialize SYM to DIR.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex.
"file-%s.el"). Return the written file name, or nil if SYM is not bound.

#### `(+deserialize-sym SYM &optional DIR MUTATE FILENAME-FORMAT)`

Deserialize SYM from DIR, if MUTATE is non-nil, assign the object to SYM.
If FILENAME-FORMAT is non-nil, use it to as a format (ex. "file-%s.el").
Return the deserialized object, or nil if the SYM.el
file dont exist.

#### `(+scratch-buffer &optional DONT-RESTORE-P MODE DIRECTORY PROJ-NAME)`

Return a scratchpad buffer in major MODE.
When DONT-RESTORE-P, do not load the previously saved persistent buffer. Load
persistent buffer dedicated to PROJ-NAME when provided.
When provided, set the `default-directory` to DIRECTORY.

#### `(+scratch-persist-buffer-h &rest _)`

Save the current buffer to `+scratch-dir`.

#### `(+scratch-persist-buffers-h &rest _)`

Save all scratch buffers to `+scratch-dir`.

#### `(+scratch-persist-buffers-after-switch-h &rest _)`

Kill scratch buffers when they are no longer visible, saving them to disk.

#### `(+scratch-open-buffer &optional ARG PROJECT-P SAME-WINDOW-P)`

Pop up a persistent scratch buffer.
If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
current project. When SAME-WINDOW-P is non-nil, open in the current window.

#### `(+switch-to-scratch-buffer &optional ARG PROJECT-P)`

Like `+scratch-open-buffer`, but switch to it in the current window.
If passed the prefix ARG, do not restore the last scratch buffer. If PROJECT-P,
open the persistent buffer dedicated to the current project.

#### `(+scratch-open-project-scratch-buffer &optional ARG SAME-WINDOW-P)`

Opens the (persistent) project scratch buffer in a popup.
If passed the prefix ARG, do not restore the last scratch buffer. When
SAME-WINDOW-P is non-nil, open in the same window.

#### `(+scratch-switch-to-project-scratch-buffer &optional ARG)`

Like `+scratch-open-project-scratch-buffer`, but switch in the current window.
If passed the prefix ARG, do not restore the last scratch buffer.

#### `(+scratch-revert-scratch-buffer)`

Revert scratch buffer to last persistent state.

#### `(+scratch-delete-persistent-scratch-file &optional ARG)`

Deletes a scratch buffer file in `+scratch-dir`.
If prefix ARG, delete all persistent scratches.

#### `(+scratch-replace-with-persistent-scratch &optional ARG PROJECT-P)`

Replace the *scratch* buffer with a persistent one.
ARG and PROJECT-P are passed to `+scratch-open-buffer`.

#### `(+font-installed-p FONT-FAMILY)`

Check if FONT-FAMILY is installed on the system.

#### `(+apply-font-or-script SCRIPT-OR-FACE)`

Set font for SCRIPT-OR-FACE from `minemacs-fonts-plist`.

#### `(+setup-fonts)`

Setup fonts.

#### `(+list-external-dependencies)`

Show the list of declared external dependencies.

#### `(+spellcheck-correct)`

Correct word at point.

#### `(+spellcheck-mode)`

Toggle spellchecking.

#### `(+cocogitto-bump LEVEL &optional PRE)`

Bump version LEVEL (`auto`, `major`, `minor` or `patch`), and with PRE if it
is a pre-release.
This command stashes the current workspace before bumping the version, and
restores it after that.

#### `(eglot-ltex-workspace-config-fn &optional SERVER)`

A function to use as a value of `eglot-workspace-configuration`.
It generates the workspace configuration dynamically, taking into account
changed values of `eglot-ltex-language`, `eglot-ltex-dictrionary`, and so on.

#### `(eglot-ltex-enable-handling-client-commands)`

Enable Eglot hack to handle code actions of LTeX-LS.

#### `(eglot-ltex-disable-handling-client-commands)`

Disable Eglot hack to handle code actions of LTeX-LS.

#### `(+gdb-set-layout)`

Enable custom window layout for gdb.

#### `(+gdb-reset-layout)`

Enable custom window layout for gdb.

#### `(+emacs-gdb-enable)`

Load a faster "gdb" command from "emacs-gdb".
This will overwrite the built-in "gdb-mi" for this session.

#### `(+mu4e-view-select-attachment)`

Use `completing-read` to select a single attachment.
Acts like a singular `mu4e-view-save-attachments`, without the saving.

#### `(+mu4e-view-open-attachment)`

Select an attachment, and open it.

#### `(+mu4e-view-select-mime-part-action)`

Select a MIME part, and perform an action on it.

#### `(+mu4e-part-selectors PARTS)`

Generate selection strings for PARTS.

#### `(+mu4e-view-save-all-attachments &optional MSG)`

Save all MIME parts from current mu4e gnus view buffer.

#### `(+mu4e-register-account LABEL MAILDIR LETVARS &optional DEFAULT-PGMAIL-P)`

Register a mu4e context named LABEL, located in MAILDIR.
LETVARS contains the alist of local variables with their values.
If DEFAULT-P is non-nil, the context is placed first and considered the default
one. If GMAIL-P is non-nil, addresses are saved to `+mu4e-gmail-accounts` to be
used later for Gmail specific actions.

#### `(+mu4e-save-message-at-point &optional MSG)`

Copy message at point to somewhere else as <date>_<subject>.eml.

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

#### `(+org-extras-multifiles-document-setup)`

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

#### `(+realgud:cmd-run ARG)`

Run.

#### `(+realgud:cmd-start ARG)`

start => break main; run.

#### `(+realgud:cmd-reverse-next ARG)`

Reverse next.

#### `(+realgud:cmd-reverse-step ARG)`

Reverse step.

#### `(+realgud:cmd-reverse-continue ARG)`

Reverse continue.

#### `(+realgud:cmd-reverse-finish ARG)`

Reverse finish.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
