# MinEmacs FAQ
This file assembles solutions to some of the known issues and answers some of
the frequently asked questions about MinEmacs.

## MinEmacs (with Evil) keybindings cheat sheet

MinEmacs defines several keybindings, mainly using [**general.el**](https://github.com/noctuid/general.el). To see the full
list of keybindings defined using general, you can type `SPC h g` or `M-x
general-describe-keybindings`.

This is usable only when you have `me-evil` enabled.

## Trump gets stuck when trying to open a file via SSH
Sometimes, when trying to open a distant file via SSH on Tramp, it can get
stuck. A common reason for this can be the prompt pattern on the distant
machine. Tramp have a variable `tramp-shell-prompt-pattern` that gets used to
detect if tramp got a valid shell prompt or not (checking for $ or #, among
other stuff). In machines using fancy shell configurations (Oh-my-Zsh,
Oh-my-Bash, ...), the prompt can be different from the defaults. In these cases,
you need to add this to your **distant** machine `.zshrc` or `.bashrc`:

```shell
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
```

More information [can be found
here](https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html).

## Using `parinfer-rust-mode` in a non `x86_64` platform
The `parinfer-rust-mode` package provide precompiled libraries for Linux, Windows
and MacOS, but only for x86\_64 machines. If you have a CPU (like M1 or other ARM
based CPUs) of another architecture (`aarch64`, `arm32`, `arm64`, &#x2026;), you can
compile the library from source [as described in the project's documentation](https://github.com/justinbarclay/parinfer-rust-mode#option-2-building-library-from-sources), and
then add this to your `config.el`:

```elisp
(setq parinfer-rust-library "/path/to/your/parinfer-rust-library.so")
```

## Exporting Org documents in background ends with errors
### User configuration related errors
In some circumstances, you can get an error when trying to export an Org file to
PDF. For example, after hitting `SPC m e l p`, you get short after, the message
`"Org async export finised, see Org Export Process for more details."`.

With something like this in the `*Org Export Process*` buffer:

Using MinEmacs’ "me-org-export-async-init.el" as init file.
Loading "init.el" in an org-export-async context.
Symbol s function definition is void: evil-snipe-mode

```elisp
Error: void-function (evil-snipe-mode)
  mapbacktrace(#f(compiled-function (evald func args flags) #<bytecode 0x187e658465ae87e8>))
  debug-early-backtrace()
  debug-early(error (void-function evil-snipe-mode))
  (evil-snipe-mode 1)
  eval-buffer(#<buffer  *load*-90736> nil "/home/abougouffa/.minemacs.d/config.el" nil t)
  load-with-code-conversion("/home/abougouffa/.minemacs.d/config.el" "/home/abougouffa/.minemacs.d/config.el" nil t)
  load("~/.minemacs.d/config.el" nil t)
  (progn nil (load user-config nil (not minemacs-verbose-p)))
  (if (file-exists-p user-config) (progn nil (load user-config nil (not minemacs-verbose-p))))
  (let ((user-config (concat minemacs-config-dir "config.el"))) (if (file-exists-p user-config) (progn nil (load user-config nil (not minemacs-verbose-p)))))
  eval-buffer(#<buffer  *load*-220865> nil "/home/abougouffa/.emacs.d/init.el" nil t)
  load-with-code-conversion("/home/abougouffa/.emacs.d/init.el" "/home/abougouffa/.emacs.d/init.el" nil t)
  load("~/.emacs.d/init.el" nil t)
  eval-buffer(#<buffer  *load*> nil "/home/abougouffa/.emacs.d/modules/extras/me-org-export-async-init.el" nil t)
  load-with-code-conversion("/home/abougouffa/.emacs.d/modules/extras/me-org-export-async-init.el" "/home/abougouffa/.emacs.d/modules/extras/me-org-export-async-init.el" nil t)
  load("/home/abougouffa/.emacs.d/modules/extras/me-org-export-async-init.el" nil t)
  command-line-1(("-l" "/home/abougouffa/.emacs.d/modules/extras/me-org-export-async-init.el" "-l" "/tmp/org-export-processktJyzH"))
  command-line()
      normal-top-level()
```

To resolve this, we need to understand how MinEmacs configures Org-mode's
asynchronous export. By default, MinEmacs enables `org-export-in-background` to
export documents asynchronously (in a child Emacs process). As this process do
not need all of MinEmacs installed package, so there is a minimalist init file
in `modules/extras/me-org-export-async-init.el` which will be used by default for
the `org-export-async-init-file` variable.

Therefore, if you invoke -in your `config.el`- a feature that is not enabled in
the minimalist init file, you will get random errors related to that. In the
example above, Emacs is complaining about `evil-snipe-mode` because we have call
to `(evil-snipe-mode 1)` in the `config.el`, however, evil and it's related packages
are not enabled in the minimalist init file.

The minimalist init file provides a feature named `me-org-export-async-init`, so
you can add conditional blocks in your `config.el` to be executed (or not) when
this feature is present (*i.e.*, when running in an Org async export context).

So, to resolve this issue, you can either:

- Fix your `config.el` by using `with-eval-after-load` blocks and by conditioning
  the problematic code.

```elisp
(with-eval-after-load 'projectile
  (do-some-extra-stuff))

;; Avoid running this block in org-export context
(unless (featurep 'me-org-export-async-init)
  (do-something))
```

- Disable `org-export-in-background` using:

```elisp
(with-eval-after-load 'org
  (setq org-export-in-background nil))
```

- Or, you can revert to Org-mode's default behavior for the child export
  process (will use MinEmacs' `init.el` file):

```elisp
(with-eval-after-load 'org
  ;; You can also set it to your own custom init.el, just do not forget to
  ;; provide `me-org-export-async-init' in it.
  (setq org-export-async-init-file nil))
```

### The `Odd length text property list` error
For some reason, exporting Org documents in background can fail with this error:

```elisp
(error "Odd length text property list")
```

In such a case, you can remove the Org cache directory then try to export again.

```shell
# Remove the Org cache directory
rm -rf ~/.emacs.d/local/cache/org/
```

See [this comment](https://github.com/org-roam/org-roam/issues/2155#issuecomment-1145388814) for more information.

## `vterm-module` compilation issue
In some old systems, you can run into a compilation issue of `vterm-module`. By
default, `vterm` will try to use the `libvterm` installed in your system,
however, the version provided by your system might be incompatible with
`vterm-module`. In similar cases, you can try to add this to your `config.el`:

```elisp
;; To be able to compile vterm-module
(with-eval-after-load 'vterm
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=Off"))
```

## Debugging MinEmacs
Sometimes Emacs might freezes and stop responding, the reflex in this case is to
use `C-g` to call `keyboard-quit` which signals a "quit" condition. If Emacs
stay frozen, you can send `SIGUSR2` via a terminal, this should stop the running
code and show a backtrace.

```shell
kill -s SIGUSR2 $(pidof emacs)
```
