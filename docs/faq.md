# MinEmacs FAQ
This file assembles solutions to some of the known issues and answers some of
the frequently asked questions about MinEmacs.

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
