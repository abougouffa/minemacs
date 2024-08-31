# MinEmacs (NG)

[![CI-Linux](https://github.com/abougouffa/minemacs/actions/workflows/ci-linux.yaml/badge.svg?branch=minemacs-ng)](https://github.com/abougouffa/minemacs/actions/workflows/ci-linux.yaml)
[![CI-MacOS](https://github.com/abougouffa/minemacs/actions/workflows/ci-macos.yaml/badge.svg?branch=minemacs-ng)](https://github.com/abougouffa/minemacs/actions/workflows/ci-macos.yaml)
[![CI-Windows](https://github.com/abougouffa/minemacs/actions/workflows/ci-windows.yaml/badge.svg?branch=minemacs-ng)](https://github.com/abougouffa/minemacs/actions/workflows/ci-windows.yaml)

**MinEmacs** is a complete and fast Emacs configuration framework.

![MinEmacs banner](/docs/images/minemacs-cover.svg)

![MinEmacs screenshot](/docs/images/minemacs-screenshot.png)

## Why?

[Long story](/docs/STORY.md) short, I'm relaying on Emacs for my everyday's
work, so I needed a fast and stable configuration that fits my needs. MinEmacs
is changing constantly, please refer to the [change log](/docs/CHANGELOG.md)
for more information.

> [!NOTE]
> Please note that I have no intent or availability to create an alternative to
> Doom Emacs or Spacemacs. While I find joy (like every other Emacser out there)
> in tinkering with Emacs, MinEmacs remains just a tool that I use in my
> everyday work, and that I like to share with other Emacsers.

## Install

Open a shell and run:

```shell
git clone --recursive https://github.com/abougouffa/minemacs.git ~/.emacs.d && emacs
```

By executing this command, the repository will be cloned, and Emacs will be
launched. During the initial run, Emacs will automatically install the necessary
packages. You might need to run `M-x minemacs-run-build-functions` when Emacs
loads up to install some extra stuff (build some libraries, install Nerd Fonts,
etc.)

> [!IMPORTANT]
> Please note that I'm using a fresh Emacs 29.4.50 *(recommended version)* built
> from the `emacs-29` branch mainly on two machines, one based on Manjaro Linux
> and the other on (the quite old) Debian 10. However, I have set up some basic
> [Github CI actions](https://github.com/abougouffa/minemacs/actions) that
> automatically test running this configuration on Emacs 29 and 30 in [Ubuntu
> Linux](https://github.com/abougouffa/minemacs/actions/workflows/ci-linux.yaml)
> and
> [MacOS](https://github.com/abougouffa/minemacs/actions/workflows/ci-macos.yaml)
> and on Emacs 29 in
> [Windows](https://github.com/abougouffa/minemacs/actions/workflows/ci-windows.yaml).
> These actions ensure that MinEmacs is "runnable" on these systems; with all
> its modules enabled. However, more testing should be done to validate the
> configuration as a whole on systems other than Linux.

## Customization

To personalize MinEmacs, you can add a specific set of files within the default
user configuration directory, which is located by default at `~/.minemacs.d/` or
`.emacs.d/user-config/` (the first to be found). However, if you prefer to use a
different directory, you have the flexibility to do so by setting the
`MINEMACSDIR` environment variable.

### Main configuration files

There are two main files that can be added in the `~/.minemacs.d` directory:

1. The `~/.minemacs.d/modules.el` file contains a list of enabled modules and/or
   a list of disabled packages (`minemacs-modules` and
   `minemacs-disabled-packages` can be set in this file). So if you want to
   enable a module (_ex._ `me-prog`) but you need to exclude a particular
   package (_ex._ `ts-movement`), you can add the latter to
   `minemacs-disabled-packages`.
2. The `~/.minemacs.d/config.el` file contains the user configuration and
   customization, you can think of it as your `init.el`, which gets loaded at
   the end of MinEmacs' `init.el`!

This repository contains skeleton files for [`modules.el`](/skel/modules.el) and
[`config.el`](/skel/config.el) (under [`skel/`](/skel)). We highly recommend
following the same structure as in the skeleton files, specially the use of
`with-eval-after-load` and `use-package` instead of using `require` directly
(`require` loads the packages immediately, which increases the startup time of
Emacs).

### Machine-specific configuration files

In my workflow, I use mainly the same configuration files across all my machines
(which, following the tradition, are shared in my
[dotfiles](https://github.com/abougouffa/dotfiles) repository). However, I have
some machine-specific (local) configurations that contain some private and
machine-specific configurations. For example, I use them to overwrite the email
address on my workstation, to setup my Email accounts, to setup Forge and Jira
integration in my workstation, and so on.

For this purpose, MinEmacs will also check for files in
`~/.minemacs.d/local/{early-config,init-tweaks,modules,config}.el` and load
them, after the `~/.minemacs.d/{early-config,init-tweaks,modules,config}.el` if
they exists.

### Advanced configuration files

MinEmacs provides also some advanced customization files, these files can be
used to tweak MinEmacs' behavior, add some early initialization code, make
MinEmacs runnable on older Emacs versions, etc.

1. The `~/.minemacs.d/early-config.el` file is loaded at the end of MinEmacs'
   `early-init.el`. You can use it to set up some early stuff like tweaking the
   UI, overwrite the variables set by MinEmacs in `~/.emacs.d/early-init.el`,
   and so on.
2. The `~/.minemacs.d/init-tweaks.el` file is loaded at an early stage of the
   [`init.el`](/init.el) file. You can use it to do some useful stuff before
   MinEmacs starts to customize packages and load modules. See the comments in
   [`init.el`](/init.el) for more information.

### Environment variables

You can customize MinEmacs' behavior via some environment variables.

- `MINEMACS_DIR` or `MINEMACSDIR`: Path for MinEmacs user configuration
  directory, if not set, `~/.minemacs.d/` is used.
- `MINEMACS_MSG_LEVEL`: Change message log level, from 1 (only errors) to 4 (all
  messages).
- `MINEMACS_VERBOSE`: Be more verbose (useful for debugging).
- `MINEMACS_DEBUG`: Enable debugging at startup (and be verbose).
- `MINEMACS_ALPHA`: Set frame `background-alpha` to percentage (value from 0 to
  100).
- `MINEMACS_NOT_LAZY`: Load lazy packages immediately after loading Emacs.
- `MINEMACS_ALWAYS_DEMAND`: Load all packages immediately (this works by setting
  `use-package-always-demand` to `t` and `use-package-always-defer` to `nil` (by
  default, MinEmacs sets `use-package` to always defer, unless explicit
  `:demand` is added).
- `MINEMACS_IGNORE_USER_CONFIG`: space-separated values, used to disables
  loading `~/.minemacs.d/<file>.el` user configuration files. Accepted values
  for `<file>` are: `early-config`, `init-tweaks`, `modules`, `config`,
  `local/early-config`, `local/init-tweaks`, `local/modules` and `local/config`.
  Use `all` to disable all user configuration files.
- `MINEMACS_LOAD_ALL_MODULES`: Load all modules (without taking
  `~/.minemacs.d/modules.el` into account).
- `MINEMACS_BENCHMARK`: Run a benchmark at initialization of Emacs (using
  [`benchmark-init.el`](https://github.com/dholm/benchmark-init-el)) and display
  the results after startup (including lazy packages).
- `MINEMACS_NO_PROXIES`: Set if you have `minemacs-proxies` setup in your
  `early-config.el` but you want to start Emacs without passing by these proxies
  (useful if you use some proxies for workplace but you want Emacs to start
  without passing by them to be able to download packages).

## Extra documentation
For more information about customization variables, functions and commands
defined by MinEmacs, you can refer to the [documentation generated from the
source code](/docs/DOCS.md).

## Troubleshooting

If you experienced an issue with MinEmacs, you can take a look at the
[FAQ](/docs/FAQ.md), consult the
[discussions](https://github.com/abougouffa/minemacs/discussions), check [open
issues or open a new one](https://github.com/abougouffa/minemacs/issues).
