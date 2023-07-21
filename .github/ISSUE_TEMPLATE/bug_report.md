---
name: Bug report
about: Create a report to help us improve
title: "[BUG] "
labels: ''
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**Emacs version**
Run `M-x emacs-version` and put its output here:

```elisp
;; For example
GNU Emacs XX.0.33 (build 1, x86_64-pc-linux-gnu, GTK+ Version 3.24.38, cairo version 1.17.8) of 2023-07-19
```

**Checkup list**
- [ ] You've tested disabling your `config.el` using `MINEMACS_IGNORE_CONFIG_EL=1 emacs`?

**To Reproduce**
Steps to reproduce the behavior:
1. Go to '...'
2. Click on '....'
3. Scroll down to '....'
4. See error

**Expected behavior**
A clear and concise description of what you expected to happen.

**Backtrace**
If the bug rises an error, please attach the full backtrace. To be sure to obtain a backtrace, you should launch Emacs with `MINEMACS_DEBUG=1 emacs` or `emacs --debug-init`.

```elisp
;; Put the backtrace here!
```

**Screenshots**
If applicable, add screenshots to help explain your problem.

**Additional context**
Add any other context about the problem here.
