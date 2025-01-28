# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## [v12.5.1](https://github.com/abougouffa/minemacs/compare/39fce901abb318cc3fe88a29a19a44048c8407db..v12.5.1) - 2025-01-28
#### Bug Fixes
- **(magit-file-icons)** temporary disable, not working with new Magit version - ([be40918](https://github.com/abougouffa/minemacs/commit/be40918b6769f86c9dd91ee0805f9372118bf7e3)) - [@abougouffa](https://github.com/abougouffa)
- use `if-let*` instead of the obsolete `if-let` - ([fe5342f](https://github.com/abougouffa/minemacs/commit/fe5342f0ec7fedb28205ac300d0801c8e3624a1b)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** document the `MINEMACS_BUILTIN_ONLY` environment variable - ([0077b51](https://github.com/abougouffa/minemacs/commit/0077b5127e558b0d96aa6d684f81769568d23177)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(make)** minor updates - ([39fce90](https://github.com/abougouffa/minemacs/commit/39fce901abb318cc3fe88a29a19a44048c8407db)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- add an example script for monitoring a directory - ([807debb](https://github.com/abougouffa/minemacs/commit/807debb5c3499aa799c41dc09190f41639c99da5)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move obsolete definitions to `me-obsolete-defs` - ([2b610c9](https://github.com/abougouffa/minemacs/commit/2b610c9ef962cc1a702e7ee3631cdff5dd5dc060)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult)** bind `fd` to `M-s F` to avoid conflict in `dired-mode` - ([a27c385](https://github.com/abougouffa/minemacs/commit/a27c38588b179c0a93101cd82763264e179f1d9c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** reuse the same buffer in `+shellcheck-describe-error` (WIP) - ([47b6021](https://github.com/abougouffa/minemacs/commit/47b6021dd58ae61337119a3aa06ece20936f849f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1c1c048](https://github.com/abougouffa/minemacs/commit/1c1c0486f27eee1e81a1db32d9a642cac4b98c41)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.5.0](https://github.com/abougouffa/minemacs/compare/67de6736d3e5ced35bbf5531698806fe2bd391c6..v12.5.0) - 2025-01-15
#### Bug Fixes
- **(core)** better file matching in `+json-schema-for-file` - ([a75eab0](https://github.com/abougouffa/minemacs/commit/a75eab00d115742c895af5c667bdbe1ca7db29ef)) - [@abougouffa](https://github.com/abougouffa)
- **(recentf)** explicitly set the file path - ([4ec182e](https://github.com/abougouffa/minemacs/commit/4ec182e725183378f4256b1027dff1f330a79dc4)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** make links relative - ([33ce5ea](https://github.com/abougouffa/minemacs/commit/33ce5ea15f38016e870fef7421d99ffeb8a34d24)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([8b5c152](https://github.com/abougouffa/minemacs/commit/8b5c15273a72551941b5fabedc6e02fd70401ca4)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([a6a441e](https://github.com/abougouffa/minemacs/commit/a6a441e529878a325bc2ea831777709dfbfdd9d9)) - [@abougouffa](https://github.com/abougouffa)
- regenerate documentation - ([67de673](https://github.com/abougouffa/minemacs/commit/67de6736d3e5ced35bbf5531698806fe2bd391c6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+explainshell` - ([3052104](https://github.com/abougouffa/minemacs/commit/30521047dbc41108254368babc1718836c131dfa)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib)** add `+jira-commit-auto-insert-ticket-id` - ([c16e786](https://github.com/abougouffa/minemacs/commit/c16e786c326907bf8d6ff90754c326fe6e15295e)) - [@abougouffa](https://github.com/abougouffa)
- generic implementation of inserting YAML schemas - ([6cc5ebe](https://github.com/abougouffa/minemacs/commit/6cc5ebe599ac50ba645188d0e474ade656ef1ac3)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(docs)** rename documentation files - ([2e867d1](https://github.com/abougouffa/minemacs/commit/2e867d157baa2662dc2d9cbcf36c42bbfeeb6179)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- cleanup - ([c48737c](https://github.com/abougouffa/minemacs/commit/c48737c15ef6a2072248589be5a2a00d238be14d)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- make `+ansi-color-apply-on-buffer` a command, move to lib - ([e65402a](https://github.com/abougouffa/minemacs/commit/e65402af9e67ae17489ae588199d12da0cf4da3e)) - [@abougouffa](https://github.com/abougouffa)
- move subtle color hack to `+color-subtle` and fix an edge case - ([c39dc7f](https://github.com/abougouffa/minemacs/commit/c39dc7fe5d09f5fecf32c82e1d7ff46f834a6d84)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(copilot)** complete with TAB - ([b181e8b](https://github.com/abougouffa/minemacs/commit/b181e8bf4083029bcdcd8ce7a4f85229722ef6f9)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edits - ([9ca0aaf](https://github.com/abougouffa/minemacs/commit/9ca0aafa38b3d770ea35f4fd25b33e9c51a211d0)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `minemacs-default-org-dir` - ([d6aee19](https://github.com/abougouffa/minemacs/commit/d6aee19903d38142691e446cab0d5a3dfb1e0596)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** don't mess with YASnippet - ([c39ed24](https://github.com/abougouffa/minemacs/commit/c39ed24a36e5fba373d4786f307e094476a49ba9)) - [@abougouffa](https://github.com/abougouffa)
- **(nav)** move `p-search` from `me-experimental` to `me-nav` - ([7e178be](https://github.com/abougouffa/minemacs/commit/7e178bedf21328bffab31c2011222d418aedb125)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** accept multiple dirs in `+project-scan-for-projects` - ([d38da3d](https://github.com/abougouffa/minemacs/commit/d38da3d63e66251475360a4245acb94ad6faa0bc)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** move config example for `jiralib` to `skel/config.el` - ([005870c](https://github.com/abougouffa/minemacs/commit/005870c02ad6d3fa6a92fadf71c7a01e2db45b08)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bf303dd](https://github.com/abougouffa/minemacs/commit/bf303dde470468a08f8fde46b89a91526c000a5e)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([aa23913](https://github.com/abougouffa/minemacs/commit/aa239131d96fc57b6dbbc08f23d1e1344f4b418c)) - [@abougouffa](https://github.com/abougouffa)
- better implementation of `+jira-commit-auto-insert-ticket-id` - ([ceec2f3](https://github.com/abougouffa/minemacs/commit/ceec2f314e39eea42a2bdf4b65acd19197ce4fab)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([65b86e1](https://github.com/abougouffa/minemacs/commit/65b86e1052d9bbfc321bcbc8d972c4e355539b02)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.4.0](https://github.com/abougouffa/minemacs/compare/a2a8f7fc6a9d8400f26a0062631b4e54b823d465..v12.4.0) - 2024-12-26
#### Bug Fixes
- **(ansible)** fix the recipe - ([821d7c4](https://github.com/abougouffa/minemacs/commit/821d7c4172229ddcc15b09be8772699e9fa7e47f)) - [@abougouffa](https://github.com/abougouffa)
- **(transient)** install a newer version at the right place - ([2540cbb](https://github.com/abougouffa/minemacs/commit/2540cbb85fbf004b6d0e7e64c0f9ec036462093d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ai)** initial support for Copilot - ([009f7a2](https://github.com/abougouffa/minemacs/commit/009f7a2bbb74d9119d423bc4c98727388f2895fd)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add a command to display ShellCheck errors description - ([7be3de3](https://github.com/abougouffa/minemacs/commit/7be3de35405a5fcda7073a24e53e3447cc9e2669)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/gitlab-ci)** add a helper command `+gitlab-ci-add-schema` - ([b45c89a](https://github.com/abougouffa/minemacs/commit/b45c89a523e28e31effedbd5149940d250946017)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/yaml)** add `ansible-mode` - ([7b82935](https://github.com/abougouffa/minemacs/commit/7b82935c19b9536454ed50ec8deef9d8c0ffbba4)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** inhibit compacting font caches - ([2e777e0](https://github.com/abougouffa/minemacs/commit/2e777e02a5dce27a5d15c0ccf951d2830ef0075b)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** larger kill ring size - ([d1eb039](https://github.com/abougouffa/minemacs/commit/d1eb0393d7e9126eeda9aea0ef114956b7fd634b)) - [@abougouffa](https://github.com/abougouffa)
- **(copilot)** customize the installation directory - ([705d6c6](https://github.com/abougouffa/minemacs/commit/705d6c652de7ef38f981fafa04db688f33f6a0e9)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** minor tweaks in the integration with `symbol-overlay` - ([55eb96d](https://github.com/abougouffa/minemacs/commit/55eb96de8654d7ee779e556aa23c0b4274a2d1c3)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** change the ControlPath socket name format - ([7788f5b](https://github.com/abougouffa/minemacs/commit/7788f5bf066bf01e5d28a43afba0f85ecb229f49)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** more responsive SSH editing - ([a2a8f7f](https://github.com/abougouffa/minemacs/commit/a2a8f7fc6a9d8400f26a0062631b4e54b823d465)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([baff206](https://github.com/abougouffa/minemacs/commit/baff2069a08c4b475b9129fa90320b6be8d7f036)) - [@abougouffa](https://github.com/abougouffa)
- edit the message displayed after loading Emacs - ([a975058](https://github.com/abougouffa/minemacs/commit/a975058a04ab23c9b06aed30a397d4cb98bffa40)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([f665741](https://github.com/abougouffa/minemacs/commit/f66574105a12c66d82301f175c7941482b439a9a)) - [@abougouffa](https://github.com/abougouffa)
- better implementation of `+shellcheck-describe-error` - ([46a4064](https://github.com/abougouffa/minemacs/commit/46a406443a9720a21f802a8c4cfa4ef1477c42b6)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([ea65ade](https://github.com/abougouffa/minemacs/commit/ea65adea4ecc5bc0b3973e57c4faf68cbebec70d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.3.0](https://github.com/abougouffa/minemacs/compare/b943fe9bf5e768c38fe6c9535116abe9ead50cfe..v12.3.0) - 2024-12-17
#### Bug Fixes
- **(builtin)** fix a couple of issues with Emacs 29 - ([95ce8e9](https://github.com/abougouffa/minemacs/commit/95ce8e9948606409b0618385e8098c42cdd35232)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** compatibility with the built-in version of Eglot in Emacs 29 - ([77d6ac2](https://github.com/abougouffa/minemacs/commit/77d6ac297a04fd84f3d2f80ee588ab71c7a62f45)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** require `compat`, fixes issues on Emacs 29 - ([1692e34](https://github.com/abougouffa/minemacs/commit/1692e3431359de8daad2392e019bd73ac2d503fe)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** cleanup references to `straight-x` - ([008e789](https://github.com/abougouffa/minemacs/commit/008e789b4ca2924cadeb1620ec0ba78a765eda86)) - [@abougouffa](https://github.com/abougouffa)
- **(transient)** `forge` needs a recent version - ([5ad1189](https://github.com/abougouffa/minemacs/commit/5ad1189aacb7da9c4eb9ebfb415e393290922a2f)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** update information about the supported version of Emacs - ([64c2e4b](https://github.com/abougouffa/minemacs/commit/64c2e4b711d49b21b84e41d0d4123146b2dff24e)) - [@abougouffa](https://github.com/abougouffa)
- update comments - ([effcd05](https://github.com/abougouffa/minemacs/commit/effcd059bf310eb573da4a8694bd492e40e51291)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([f6ca225](https://github.com/abougouffa/minemacs/commit/f6ca225f5e17f2adbd7a58f95ff563ec2a2b5d5b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- add an option to load only built-in package, cleanup the config - ([362b6ee](https://github.com/abougouffa/minemacs/commit/362b6ee13756165a35c07f208a824306961991af)) - [@abougouffa](https://github.com/abougouffa)
- remove `me-daemon` - ([b943fe9](https://github.com/abougouffa/minemacs/commit/b943fe9bf5e768c38fe6c9535116abe9ead50cfe)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** only test Emacs 29.4 - ([9cc5b8f](https://github.com/abougouffa/minemacs/commit/9cc5b8fcb4061df39f381a51e0ebe94ce432a66e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(mu4e)** move the daemon integration to `me-mu4e` - ([ef3ac20](https://github.com/abougouffa/minemacs/commit/ef3ac2076af2dc5a6414189e3edf62bffd069af6)) - [@abougouffa](https://github.com/abougouffa)
- include `satch` and `once` as internal dependencies - ([52661b2](https://github.com/abougouffa/minemacs/commit/52661b2d49ec103a6380f7ca010f2acbc8e7c721)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(pcap-mode)** bind `n` and `p` + add more extensions - ([06031b7](https://github.com/abougouffa/minemacs/commit/06031b744439637182798f5ee13480ef308ca871)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([38146f8](https://github.com/abougouffa/minemacs/commit/38146f864bf07275faef2cc734b86a8f885c13ef)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([965e59f](https://github.com/abougouffa/minemacs/commit/965e59f34d3d3f6d81d6d353b51dd45f3fd15777)) - [@abougouffa](https://github.com/abougouffa)
- better and simpler error catching in `+apply-patch-dwim` - ([d882484](https://github.com/abougouffa/minemacs/commit/d88248408768ddc3466dc78e8bdee7a2d2932e64)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9cf35f1](https://github.com/abougouffa/minemacs/commit/9cf35f128c92655d68877b011feae6dad3e17946)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([115cbd8](https://github.com/abougouffa/minemacs/commit/115cbd87ce199065296249005312f66dabde1efc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.2.0](https://github.com/abougouffa/minemacs/compare/03098105a8938d03db8606333fae4d767491cd74..v12.2.0) - 2024-12-11
#### Bug Fixes
- **(corfu)** binding `corfu-send` to `RET` isn't a good option (ex. ielm) - ([c240749](https://github.com/abougouffa/minemacs/commit/c240749afdef1e327db756fd2d14355bc0021b4b)) - [@abougouffa](https://github.com/abougouffa)
- **(repo)** correctly treat the ANSI colors in `repo-status` buffer - ([5691ade](https://github.com/abougouffa/minemacs/commit/5691ade77f1d2328d0fbd52a5b61ed4fc8ef3800)) - [@abougouffa](https://github.com/abougouffa)
- remove references to the obsolete `me-god` module - ([5053cf5](https://github.com/abougouffa/minemacs/commit/5053cf5e341cc311724b765baf6a2c94b014aebe)) - [@abougouffa](https://github.com/abougouffa)
- don't use unlimited GC threshold, cases too much memory usage - ([0309810](https://github.com/abougouffa/minemacs/commit/03098105a8938d03db8606333fae4d767491cd74)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(nav)** replace `goto-last-change` with `goto-chg` - ([f0c984a](https://github.com/abougouffa/minemacs/commit/f0c984ac4e4cffa7c43010371b3c95d971845bad)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for SmallTalk - ([d50c7fd](https://github.com/abougouffa/minemacs/commit/d50c7fdf30f1726fbceca93458a2100aaae15668)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for `jira-markup-mode` - ([8291d2c](https://github.com/abougouffa/minemacs/commit/8291d2c0b78b2d424a525df571851f4fc13b64da)) - [@abougouffa](https://github.com/abougouffa)
- **(valgrind)** initial `valgrind-mode` (WIP) - ([8640ba5](https://github.com/abougouffa/minemacs/commit/8640ba5f17c070f596c7667bc6526c43ba523691)) - [@abougouffa](https://github.com/abougouffa)
- add the `me-experimental` module - ([e11138d](https://github.com/abougouffa/minemacs/commit/e11138db6d85ff42ce85f3e35d55bf90b3edfbaa)) - [@abougouffa](https://github.com/abougouffa)
- make `me-god` obsolete, not used from a long time - ([f5aeaf4](https://github.com/abougouffa/minemacs/commit/f5aeaf44e3c54fc29dbad827cd95cf7f50dd631f)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(core)** minor edits - ([526451a](https://github.com/abougouffa/minemacs/commit/526451a876813f4372121bc9b0fb2c04cd88a718)) - [@abougouffa](https://github.com/abougouffa)
- **(god)** which-key integration is no longer experimental - ([c721298](https://github.com/abougouffa/minemacs/commit/c7212988bd59f65b348a2b5494beb155c22326a1)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** cleanup and remove dependency on `satch` and `once` - ([7ba5522](https://github.com/abougouffa/minemacs/commit/7ba5522933c3d04dbcdbe1aa75a26d134bcd46c9)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** cleanup and refactor `init.el` and `early-ini.el` - ([2a07e41](https://github.com/abougouffa/minemacs/commit/2a07e411f77c4904725d8b4cfcab7e082c47f461)) - [@abougouffa](https://github.com/abougouffa)
- move `crdt` from `me-docs` to `me-editor` - ([22c97d0](https://github.com/abougouffa/minemacs/commit/22c97d03c3202fb551a32b0b9d57f1d9ec543f7b)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(multiple-cursors)** use with `parinfer-rust` - ([bd14cf8](https://github.com/abougouffa/minemacs/commit/bd14cf829badd6445a843c13373a0f38a9f30bde)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** better defaults for fallback packages - ([ffac8b6](https://github.com/abougouffa/minemacs/commit/ffac8b6a59eaa824bb1be333336008be84e43191)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** use the default cursor type - ([e07c0ed](https://github.com/abougouffa/minemacs/commit/e07c0eddca24068d942a3226f9195d6062160d30)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** restore the default behavior for `ESC` - ([dbdf163](https://github.com/abougouffa/minemacs/commit/dbdf16350ce61f3afff6a898314976a928c8672a)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** use `modus-operandi` as fallback theme - ([7400c6e](https://github.com/abougouffa/minemacs/commit/7400c6e58d020e2bd27d9a3b63b44f7d7e833428)) - [@abougouffa](https://github.com/abougouffa)
- **(valgrind)** use the new `valgrind-mode` in `valgrind` - ([ae24e1d](https://github.com/abougouffa/minemacs/commit/ae24e1de22c1c58b0b38634f6b5aca377fb03e00)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e8c17d1](https://github.com/abougouffa/minemacs/commit/e8c17d13eeb889a5ee8edcf5e249b0c4e16b08e8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7a43831](https://github.com/abougouffa/minemacs/commit/7a43831e43784979d8cd8f2a33d867c1f4cb534f)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([6e54cc6](https://github.com/abougouffa/minemacs/commit/6e54cc6e4a89ce7b4500158e12daa7b43dfc4759)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1c30caa](https://github.com/abougouffa/minemacs/commit/1c30caa0fb207594893c1ee87b75cd7cc85acb7d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.1.0](https://github.com/abougouffa/minemacs/compare/ade2563d8313b346fda001abb743a8f9c1e073b3..v12.1.0) - 2024-12-05
#### Bug Fixes
- **(citre)** fix the problem with SSHFS and other protocols - ([ea595f2](https://github.com/abougouffa/minemacs/commit/ea595f27b4b8210d615d6eb4303f33ad41e565f0)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** fail silently when the mode don't support `hs-minor-mode` - ([ade2563](https://github.com/abougouffa/minemacs/commit/ade2563d8313b346fda001abb743a8f9c1e073b3)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(mu4e)** beautify the UI using `nerd-icons` - ([ab41a3d](https://github.com/abougouffa/minemacs/commit/ab41a3dcf536619433d1fdeae1635fcd09658f9d)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** add a generic function `+nerd-icons-icon` - ([d0ebccd](https://github.com/abougouffa/minemacs/commit/d0ebccdc321587c1a36b9b3ce584d0decba2e9ec)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** load Coccinelle integration when available - ([fb8f7f2](https://github.com/abougouffa/minemacs/commit/fb8f7f2c8ab58f28d62d9b8297258cb4722c835e)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** add initial support for `call-graph` - ([53c6a3f](https://github.com/abougouffa/minemacs/commit/53c6a3fdfbf38a9de7ad44e90130bc35c02dd535)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `focus` obsolete - ([e847c74](https://github.com/abougouffa/minemacs/commit/e847c749e178085b833975cd11bebc897bfe9f35)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `olivetti` obsolete - ([3e81d9f](https://github.com/abougouffa/minemacs/commit/3e81d9fa23d7136c06ce301969bf83d6af3b1ef6)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add Gitlab integration - ([898ccc5](https://github.com/abougouffa/minemacs/commit/898ccc59e6bf486fd8cc4eac4ebbbc71cef09640)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(citar)** remove unneeded stuff - ([ff385e0](https://github.com/abougouffa/minemacs/commit/ff385e045264337666e23eb07acdbe40811870f0)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib)** minor edit - ([e2d714a](https://github.com/abougouffa/minemacs/commit/e2d714a564e6cc2dd51e5a3fa9ff7ca18c8a33b7)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(tools)** restore `emamux`, pretty useful - ([dd7a45f](https://github.com/abougouffa/minemacs/commit/dd7a45f5a712fd0a068cb36a11d51b5db56483f9)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** minor edits, make byte-compiler happy - ([cdc39bb](https://github.com/abougouffa/minemacs/commit/cdc39bbf1bf0cb5c1756165b2a42f2d2673787fb)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** better deferring of Org related packages - ([5edcdd9](https://github.com/abougouffa/minemacs/commit/5edcdd9d37a3d2e2c35bc6c73752b4f7b84b81e9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.0.1](https://github.com/abougouffa/minemacs/compare/db487f79dfa03f64e3ad65c2eb850241517eba8d..v12.0.1) - 2024-12-01
#### Bug Fixes
- **(mu4e)** refactor and fix some deprecated stuff - ([c303c96](https://github.com/abougouffa/minemacs/commit/c303c96cd0880ef30022a120e8f0da5e27bf7e26)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** load the right LaTeX module when exporting Org files - ([db487f7](https://github.com/abougouffa/minemacs/commit/db487f79dfa03f64e3ad65c2eb850241517eba8d)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- some code formatting - ([8138a9a](https://github.com/abougouffa/minemacs/commit/8138a9a943c9b16e007e60d75ce874ab72a62ff4)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.0.0](https://github.com/abougouffa/minemacs/compare/c66f43b94c799b49c2f3cdb7bbf5be1c6bff1d89..v12.0.0) - 2024-12-01
#### Bug Fixes
- **(core)** rename the module `me-search` to `me-nav` - ([39c9ea7](https://github.com/abougouffa/minemacs/commit/39c9ea752fc17f30e73b6fb704a280c80cb25a8a)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fix a mistake in `minemacs-load-module` - ([1d53ac8](https://github.com/abougouffa/minemacs/commit/1d53ac80f6feacf13edfbce7fb3354a5ba2e3fe0)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** fix the crash on invalid color - ([d3582c9](https://github.com/abougouffa/minemacs/commit/d3582c9fb661bb2e690846d233cc7a8fe3980440)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** regression when `early-init.el` isn't loaded - ([a64fd03](https://github.com/abougouffa/minemacs/commit/a64fd0386f31e0f14426d4eed26f5133f909cb09)) - [@abougouffa](https://github.com/abougouffa)
- **(media)** use `browse-url-handlers` to handle YouTube links - ([caa5684](https://github.com/abougouffa/minemacs/commit/caa5684cb76040626c35d3d4b879eb287750b63b)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** fix the Org export init script - ([2fcd076](https://github.com/abougouffa/minemacs/commit/2fcd076f9f2bc13bf29b97f7d6881d845a6871e7)) - [@abougouffa](https://github.com/abougouffa)
- **(smergs)** remove reference to deleted command, autoload the others - ([6e72f12](https://github.com/abougouffa/minemacs/commit/6e72f121e03e5a74a43d5df070d1e09034f830ff)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([a30e68c](https://github.com/abougouffa/minemacs/commit/a30e68c6bfc18f4ceafddaa2042bbf284d842618)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(bitbake-ts-mode)** temporary disable, too poor font lock - ([85b2301](https://github.com/abougouffa/minemacs/commit/85b230179c043065d596c7eda3205843d91d3e6a)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for Nushell - ([614fc62](https://github.com/abougouffa/minemacs/commit/614fc62fe9938e66037cf6570067051b1f4ffb82)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** add `treesit-jump` - ([3b90d77](https://github.com/abougouffa/minemacs/commit/3b90d7713810846b0984fd3adf58963d26197446)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- use the default indentation function - ([c84363d](https://github.com/abougouffa/minemacs/commit/c84363dd7ab4c19527c455fd74f18af99ca816bb)) - [@abougouffa](https://github.com/abougouffa)
- update links in comments to cope with `goto-addr` - ([6c19e9c](https://github.com/abougouffa/minemacs/commit/6c19e9c954e1202e4b2ca2a44b86548b5b4cc7bf)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** several small fixes - ([68b8ff0](https://github.com/abougouffa/minemacs/commit/68b8ff0189503adbd1c2edb46b9f5e6c7a2f3351)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** minor tweak to make the byte-compiler happy - ([f8066bf](https://github.com/abougouffa/minemacs/commit/f8066bf2d79942c2f539dc5baa6f691a3a03e451)) - [@abougouffa](https://github.com/abougouffa)
- **(electric)** extract sh keywords the right way - ([1cae438](https://github.com/abougouffa/minemacs/commit/1cae4386510017aa2b1428c12af6d603364849d5)) - [@abougouffa](https://github.com/abougouffa)
- **(sudo-edit)** minor edit - ([53adef3](https://github.com/abougouffa/minemacs/commit/53adef3d3b650ddd1433f77b402c06717c1ae45d)) - [@abougouffa](https://github.com/abougouffa)
- cleanup and simplify - ([d8b071d](https://github.com/abougouffa/minemacs/commit/d8b071df6b43e10fba7669dbd388e3e92d70d7c3)) - [@abougouffa](https://github.com/abougouffa)
- rename module `me-search` to `me-nav` - ([6b34b4b](https://github.com/abougouffa/minemacs/commit/6b34b4bcbb2f1d7d2c7d90543c880a042fba47d5)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(citre)** better defaults + Xref `elisp` backend - ([e7843f8](https://github.com/abougouffa/minemacs/commit/e7843f881613fe833df2a94e977f7515894e3e5d)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** better `gtags` defaults - ([c248633](https://github.com/abougouffa/minemacs/commit/c24863304aeeefb1b71d3638b2d04ee89b387cca)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** minor keybinding tweaks - ([e20d366](https://github.com/abougouffa/minemacs/commit/e20d36646bdd637654d78fc3bccdbd102bd4a506)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** shorten the generated description in `+make-first-file-hook!` - ([74ea75c](https://github.com/abougouffa/minemacs/commit/74ea75c8ff03a3ef1630e509a631f87d31a11d67)) - [@abougouffa](https://github.com/abougouffa)
- **(elisp-plus)** use the default lisp-indent - ([f42cc92](https://github.com/abougouffa/minemacs/commit/f42cc9247626fcc0ec0c47c5431c028f0aaa3553)) - [@abougouffa](https://github.com/abougouffa)
- **(gambol)** enable the `embark` integration - ([d5bd8ea](https://github.com/abougouffa/minemacs/commit/d5bd8ea8a007fe1d04e9169c933fa7f7b64e3053)) - [@abougouffa](https://github.com/abougouffa)
- **(ggtags)** better defaults - ([2c25dbf](https://github.com/abougouffa/minemacs/commit/2c25dbf2b1f0d3242283b66bf27586317d85a0a9)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** remove reference to obsolete `make-directory-autoloads` - ([b937022](https://github.com/abougouffa/minemacs/commit/b937022688f68ba296a3d24c068b26701a5c2401)) - [@abougouffa](https://github.com/abougouffa)
- **(octave)** better integration of octave with eros - ([a73cc7d](https://github.com/abougouffa/minemacs/commit/a73cc7d29586d635ab84ec49fcc4f9f6d745319a)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** reduce animation time (iterations) - ([5004a72](https://github.com/abougouffa/minemacs/commit/5004a72449329318888d7f9d3fed37f8eb8354e3)) - [@abougouffa](https://github.com/abougouffa)
- **(recentf)** add serialized variables' path to ignored list - ([c66f43b](https://github.com/abougouffa/minemacs/commit/c66f43b94c799b49c2f3cdb7bbf5be1c6bff1d89)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** reduce disk usage by `--filter=tree:0` - ([19f3c9b](https://github.com/abougouffa/minemacs/commit/19f3c9bd7a95aa8ba94033838cac587adb104255)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** simplify the advice and consider `:disabled` - ([0a17989](https://github.com/abougouffa/minemacs/commit/0a17989f7fa4c411c8a0c6e65314aa01243294c3)) - [@abougouffa](https://github.com/abougouffa)
- **(valgrind)** make the buffer name customizable - ([73b19ef](https://github.com/abougouffa/minemacs/commit/73b19eff3c1494c93fb582587db94b9f4e3226d7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([65a42b0](https://github.com/abougouffa/minemacs/commit/65a42b00376cf279fc6f45d3d9dd656b76c70803)) - [@abougouffa](https://github.com/abougouffa)
- use `global-completion-preview-mode` when `corfu` isn't enabled - ([bb5134d](https://github.com/abougouffa/minemacs/commit/bb5134de427f706b378c0615cfb10d82b9c7b52c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e330724](https://github.com/abougouffa/minemacs/commit/e3307245c5b8d0855910c65a041776b1f1f4be7a)) - [@abougouffa](https://github.com/abougouffa)
- minor tweaks - ([077c83b](https://github.com/abougouffa/minemacs/commit/077c83b67f5d2f199491338d9967f35bbda26d9a)) - [@abougouffa](https://github.com/abougouffa)
- enable `bug-reference-prog-mode` and set its face - ([ad03775](https://github.com/abougouffa/minemacs/commit/ad037758ecb1d1116a741e874aa5b50eaf80f9d7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.7.0](https://github.com/abougouffa/minemacs/compare/187d7f0cb1d2106d85b413d9a0ba4555c5b13cc3..v11.7.0) - 2024-11-27
#### Bug Fixes
- **(enlight)** `pulsar` cases annoying flashed in `enlight` - ([deea54b](https://github.com/abougouffa/minemacs/commit/deea54b49f68f8002abadc788cb724b2fe74659f)) - [@abougouffa](https://github.com/abougouffa)
- defer some packages - ([93efe42](https://github.com/abougouffa/minemacs/commit/93efe4237a6e9cec6e52a6ea4f7ce80ca5014118)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** list some of the main features of MinEmacs - ([fca43c6](https://github.com/abougouffa/minemacs/commit/fca43c67a92c7fdd2ec6daf7ddffd678fc18aca1)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([26009b6](https://github.com/abougouffa/minemacs/commit/26009b698b1a9088b3a4a028586e43a03b1f7c81)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** reduce flickering by loading the theme's background-color early - ([55d1891](https://github.com/abougouffa/minemacs/commit/55d18918cc6678152df921b117a7e66a8ae669bf)) - [@abougouffa](https://github.com/abougouffa)
- **(dtrt-indent)** add `+dtrt-indent-tab-to-tab-stop` - ([9b2703e](https://github.com/abougouffa/minemacs/commit/9b2703e78fd35b8ec1e909ff26dc0a5b136cfd88)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/python)** add `pip-requirements` - ([ed5000b](https://github.com/abougouffa/minemacs/commit/ed5000b9d73b321dfcdfde36e3c242b97f84b7a9)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add support for `difftastic` - ([8d77f7e](https://github.com/abougouffa/minemacs/commit/8d77f7e6638598067cadd2b2b8c6a5503716e543)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(treesit)** enhance `+treesit-create-parser-in-buffer` and remove redundant code - ([daffc36](https://github.com/abougouffa/minemacs/commit/daffc36a1d73da94312767094dc8dee801d680bb)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** rename a variable and a command - ([d0499c8](https://github.com/abougouffa/minemacs/commit/d0499c818e5955b8daf217b34d4f5c7b89232d76)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(eglot)** don't automatically enable `eglot-inlay-hints-mode` - ([cc41559](https://github.com/abougouffa/minemacs/commit/cc41559df75daa791cf66aca4c432afa659b0c9f)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot-booster)** automatically enable when available - ([3644eb0](https://github.com/abougouffa/minemacs/commit/3644eb08febd8bb25088c1b74ea0b5770bef2a5b)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** minor tweak - ([187d7f0](https://github.com/abougouffa/minemacs/commit/187d7f0cb1d2106d85b413d9a0ba4555c5b13cc3)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([3bee1e4](https://github.com/abougouffa/minemacs/commit/3bee1e4e94eb5237fd0b7fd4a2e0f0fb85ef0024)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.6.0](https://github.com/abougouffa/minemacs/compare/86bc9fd07d7917bb3dee1978d1e79bf2cd77ce28..v11.6.0) - 2024-11-26
#### Bug Fixes
- **(pet)** use `fd` to avoid freezing Emacs in large codebases - ([e0e6e7a](https://github.com/abougouffa/minemacs/commit/e0e6e7a37c8c7b84b30a83b34e999f497ac8f546)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** return values of `projectile` emulation primitives - ([dc1a199](https://github.com/abougouffa/minemacs/commit/dc1a199bb8c453cf8a4d2bf2f90b120b7c546444)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** update the configuration examples - ([a612f01](https://github.com/abougouffa/minemacs/commit/a612f01f1fbfb1adf7a25dbfc7a4c57d1afdc5cd)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+package-configured-p` - ([2dd5d4d](https://github.com/abougouffa/minemacs/commit/2dd5d4dfb0fa546990bec81348c6e133d68fdfbd)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+with-no-proxies!` - ([86bc9fd](https://github.com/abougouffa/minemacs/commit/86bc9fd07d7917bb3dee1978d1e79bf2cd77ce28)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** restore `ffip` - ([0bc5f8b](https://github.com/abougouffa/minemacs/commit/0bc5f8b5b7e3b09ba96b317935c6a5fb75dab9c4)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(pet)** remove the `fd` hack, merged upstream - ([859f487](https://github.com/abougouffa/minemacs/commit/859f4871fffd13a3899af57bf6e23d25ed5b44ec)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** restore `treesit-fold` obsolete - ([63c2092](https://github.com/abougouffa/minemacs/commit/63c20921e329f85a16f15239d93ae6f925169e27)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(ffip)** integrate with `nerd-icons-completion-mode` - ([66702a5](https://github.com/abougouffa/minemacs/commit/66702a52285260ab4c13be0a4940af8ec5137f77)) - [@abougouffa](https://github.com/abougouffa)
- **(gud)** remove useless customization - ([9af93fe](https://github.com/abougouffa/minemacs/commit/9af93fe64d296d9b5e7d93f3ff183c15797a9ef6)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** update for the latest version - ([f43f8cc](https://github.com/abougouffa/minemacs/commit/f43f8cc6c827ea25946e49ba1062f7f18c238555)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([385d49a](https://github.com/abougouffa/minemacs/commit/385d49ade632e121fbf01a3e4a1643f4a37ebf76)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.5.0](https://github.com/abougouffa/minemacs/compare/c964e6ef85a5714ec222db8d4532a7d528b8c54c..v11.5.0) - 2024-11-24
#### Documentation
- regenerate the documentation - ([d941b72](https://github.com/abougouffa/minemacs/commit/d941b727e34c693be3cf41dc625201d313eae756)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(debug)** make `disaster` obsolete - ([8248eef](https://github.com/abougouffa/minemacs/commit/8248eef17dd52c3959acae29f234b7d1a8a2ee47)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `neotree` obsolete - ([079d60c](https://github.com/abougouffa/minemacs/commit/079d60c55aaa5d5332dcd229c3cca9db0a1ccf47)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** make `consult-project-extra` obsolete - ([27a7d91](https://github.com/abougouffa/minemacs/commit/27a7d91fa630732acd2d11f5924f5f7dbbaef3cd)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `gumshoe` obsolete - ([2ef76ca](https://github.com/abougouffa/minemacs/commit/2ef76cadd738579c8eae27471efd9a3fe544b20e)) - [@abougouffa](https://github.com/abougouffa)
- spin off persistent scratch buffers as a new package `pscratch` - ([a251ba2](https://github.com/abougouffa/minemacs/commit/a251ba2370a047506290d17dd3e29a5d9ddf2f1f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(ace-window)** restore the default for `aw-dispatch-always` - ([92d14bd](https://github.com/abougouffa/minemacs/commit/92d14bd61698739e106e813a71e4f56b995b1054)) - [@abougouffa](https://github.com/abougouffa)
- **(forge)** lazier loading - ([c964e6e](https://github.com/abougouffa/minemacs/commit/c964e6ef85a5714ec222db8d4532a7d528b8c54c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.4.0](https://github.com/abougouffa/minemacs/compare/76ee765a119d40b852afb5977191a4e9e4efa96f..v11.4.0) - 2024-11-23
#### Bug Fixes
- **(on-demand/hurl)** ensure loading dependencies - ([dcf07fd](https://github.com/abougouffa/minemacs/commit/dcf07fd330955b542210478a82cd0a3e2734b344)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** make `eglot-x` obsolete - ([0d7fc67](https://github.com/abougouffa/minemacs/commit/0d7fc6759050f729095cb6fb0d330f9a58b3f445)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `reformatter` obsolete - ([01b62dc](https://github.com/abougouffa/minemacs/commit/01b62dcf3a2a186a3ee220530aa4e99a1ae47082)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `rainbow-mode` obsolete - ([329490f](https://github.com/abougouffa/minemacs/commit/329490ff7ecbb9e2294922812fb8147532989221)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `cognitive-complexity` obsolete - ([4194f03](https://github.com/abougouffa/minemacs/commit/4194f035e5aba7d7b49cd75783aa41b39194ea9a)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `treesitter-context` obsolete - ([79f0cf2](https://github.com/abougouffa/minemacs/commit/79f0cf21b53c0e30e008d1d3c47cb14be5471d25)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `treesit-fold` obsolete - ([50b160c](https://github.com/abougouffa/minemacs/commit/50b160c1bd13478e17ff718c5df795c2ccd5d7df)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- cleanup some parts - ([f8c8a1e](https://github.com/abougouffa/minemacs/commit/f8c8a1e2aec11984a70edbe2649f42e9f5f9b149)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(awqat)** colorize the icon in modeline - ([76ee765](https://github.com/abougouffa/minemacs/commit/76ee765a119d40b852afb5977191a4e9e4efa96f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** load all modules before bumping packages - ([56ab415](https://github.com/abougouffa/minemacs/commit/56ab4153d7d226394a1f6d0f3b1ed34b80fd3b24)) - [@abougouffa](https://github.com/abougouffa)
- **(jujutsushi)** remove unneeded customization - ([9d730c1](https://github.com/abougouffa/minemacs/commit/9d730c1cc32667a4954c1c1603f7a1b8f728c818)) - [@abougouffa](https://github.com/abougouffa)
- **(man)** select the `*man*` buffer when opened - ([74df4d7](https://github.com/abougouffa/minemacs/commit/74df4d7c0076437cdf75921e938eb1c5f81f993c)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** run comment commands for all cursors - ([4b71512](https://github.com/abougouffa/minemacs/commit/4b71512e203a91bf0472956191f686217f776db0)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** temporary use my fork - ([afe7fc0](https://github.com/abougouffa/minemacs/commit/afe7fc0d3709f0777eea306ca679e726aeb33ae7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([894cb84](https://github.com/abougouffa/minemacs/commit/894cb8477f6aa4c364f92716b2871778a5b0eb2e)) - [@abougouffa](https://github.com/abougouffa)
- enable `fido-vertical-mode` when `vertico` isn't available - ([c5195ff](https://github.com/abougouffa/minemacs/commit/c5195ff46316f16e1485c85913517caa74984713)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cd5c5c4](https://github.com/abougouffa/minemacs/commit/cd5c5c44ec22672bbfd18a0beef9725cc4da5a24)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.3.2](https://github.com/abougouffa/minemacs/compare/3ecee921fe08a70e58425ca59e4bfc8cf13679f5..v11.3.2) - 2024-11-21
#### Bug Fixes
- **(autorevert)** make sure to save the initial mtime on opening the file - ([94d02d9](https://github.com/abougouffa/minemacs/commit/94d02d90498d29e37c44760ecbf7638fee99e71b)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- use full links in comments to play well with `goto-addr` - ([0566438](https://github.com/abougouffa/minemacs/commit/056643887bd50563a7cfbf1f3d1202e65ee24483)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(auctex)** remove unneeded/outdated customization - ([dba5527](https://github.com/abougouffa/minemacs/commit/dba55270109f4975bf6ba564ad162b6ae39b1606)) - [@abougouffa](https://github.com/abougouffa)
- **(calendar)** set the default date style to ISO - ([535db5d](https://github.com/abougouffa/minemacs/commit/535db5d6b168dc223b045c8d79c9916460397153)) - [@abougouffa](https://github.com/abougouffa)
- **(git-timemachine)** show revision in header line, only do font lock - ([3ecee92](https://github.com/abougouffa/minemacs/commit/3ecee921fe08a70e58425ca59e4bfc8cf13679f5)) - [@abougouffa](https://github.com/abougouffa)
- **(goto-addr)** customize the URL face - ([5e9b8f5](https://github.com/abougouffa/minemacs/commit/5e9b8f50912b9e1dffec9e8383914ee04c7161d7)) - [@abougouffa](https://github.com/abougouffa)
- **(isearch)** restore the default `C-[M]-s` binding, bind `C-[M]-ù` - ([1d878e9](https://github.com/abougouffa/minemacs/commit/1d878e92ae3a373d0a789d937d94acc1179a4820)) - [@abougouffa](https://github.com/abougouffa)
- **(re-builder)** add `+reb-replace-regexp` - ([db9c6c2](https://github.com/abougouffa/minemacs/commit/db9c6c24605d71decc4cdcec0e21cbb31475e5e7)) - [@abougouffa](https://github.com/abougouffa)
- enable `goto-address-prog-mode` in `prog-mode` & `conf-mode` - ([e88f508](https://github.com/abougouffa/minemacs/commit/e88f50881e79674249123e0d26fdd18709b970bb)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.3.1](https://github.com/abougouffa/minemacs/compare/c774165b58cd736073c337ba95959b9cd4dccbf7..v11.3.1) - 2024-11-21
#### Bug Fixes
- **(flymake-collection)** use the upstream repo, new checkers merged - ([c774165](https://github.com/abougouffa/minemacs/commit/c774165b58cd736073c337ba95959b9cd4dccbf7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.3.0](https://github.com/abougouffa/minemacs/compare/dc2eca529b07d87afd5d0f4d668b664dd10db7ce..v11.3.0) - 2024-11-21
#### Bug Fixes
- **(multi-magit)** fix `+multi-magit-discover-repos` - ([b2ac632](https://github.com/abougouffa/minemacs/commit/b2ac63294ae578175dc180ef421c58368312d885)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** declare it as unsupported for `multiple-cursors` - ([569dccc](https://github.com/abougouffa/minemacs/commit/569dccc3956916326767bfb4d1bada701af7bb00)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** respect the projects list in `+project-list-cleanup` - ([7ca7d5d](https://github.com/abougouffa/minemacs/commit/7ca7d5db78471bde9a84a20a5df02491804ce4a0)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([292a5ac](https://github.com/abougouffa/minemacs/commit/292a5ac342b62b2042cb6737636dd6168d15c6d0)) - [@abougouffa](https://github.com/abougouffa)
- more comments - ([fc39ddd](https://github.com/abougouffa/minemacs/commit/fc39ddd78ac9f460185fbd80440d4d88e04a510e)) - [@abougouffa](https://github.com/abougouffa)
- generate the documentation - ([9bf607c](https://github.com/abougouffa/minemacs/commit/9bf607c85ed4e33dcfa883a6dfb8d1fa4496d845)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** make `zones` obsolete - ([fc92c66](https://github.com/abougouffa/minemacs/commit/fc92c66fd1e0ad279d28bd3d08f28fa4d3c6e02c)) - [@abougouffa](https://github.com/abougouffa)
- **(embedded)** make `platformio` obsolete - ([bc8763c](https://github.com/abougouffa/minemacs/commit/bc8763c29cf107e5d3677eef27bca3b501693995)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `sr-speedbar` obsolete - ([abdbe82](https://github.com/abougouffa/minemacs/commit/abdbe82befb3be6279027b88c45e0116fcdcaabf)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `phi-grep` obsolete - ([9a1bed3](https://github.com/abougouffa/minemacs/commit/9a1bed34e6525d3cf0fb2a9415d31abb57167925)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `isearch-mb` obsolete - ([fd3c63d](https://github.com/abougouffa/minemacs/commit/fd3c63d3e3c0aee6b3fb0711ecdb008eaa828cb1)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `deadgrep` obsolete - ([0eef649](https://github.com/abougouffa/minemacs/commit/0eef64933d826b00706f5022e7dbf831cac59df8)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `rtags` obsolete - ([73a3e39](https://github.com/abougouffa/minemacs/commit/73a3e39119b6f4e09386b0224cfa5c9a7e9cb978)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `mixed-pitch` obsolete - ([60624cb](https://github.com/abougouffa/minemacs/commit/60624cbe9c29169eb5feb30e68045822b97b9222)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** make `repo-transient` obsolete - ([aa0cc80](https://github.com/abougouffa/minemacs/commit/aa0cc80903ab24d7828f514f6e78cab86b5c5fb7)) - [@abougouffa](https://github.com/abougouffa)
- make `me-writing-mode` obsolete - ([6fe9d42](https://github.com/abougouffa/minemacs/commit/6fe9d422ccc3de4ca3adf171dfbf46040168e82d)) - [@abougouffa](https://github.com/abougouffa)
- remove useless `+project-add-project` - ([d8246ac](https://github.com/abougouffa/minemacs/commit/d8246ac2f70ae3688e24252d41d8b4c6ac8d4944)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- several performance improvements, remove unused functions - ([dc2eca5](https://github.com/abougouffa/minemacs/commit/dc2eca529b07d87afd5d0f4d668b664dd10db7ce)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- restore the `:package` keyword in `:bind` blocks - ([6680fa5](https://github.com/abougouffa/minemacs/commit/6680fa5ab0d3c9bb7fff42078e127652945ab57c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** optionally print logs in `+shutup!` - ([7a0a468](https://github.com/abougouffa/minemacs/commit/7a0a4684e2d7c1f6ae0288f2d483e7ac26cdf83f)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** performance tweaks - ([d6ce5e0](https://github.com/abougouffa/minemacs/commit/d6ce5e01da976a707ab2efd2fe20650933fabb34)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-magit)** remember projects in `+multi-magit-discover-repos` - ([e072c33](https://github.com/abougouffa/minemacs/commit/e072c330d66f7c5127787ed67e97e17bd09ddd34)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([de24b16](https://github.com/abougouffa/minemacs/commit/de24b16c38cf4fd95c50947a09ea6fc5ab4dc4cf)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([a0b6be4](https://github.com/abougouffa/minemacs/commit/a0b6be4884a67a182ccc3f990f5b5ec37912a901)) - [@abougouffa](https://github.com/abougouffa)
- minor edits in `goto-last-change` - ([c5bd045](https://github.com/abougouffa/minemacs/commit/c5bd045a640dd97933195b019e80534081e53498)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.2.0](https://github.com/abougouffa/minemacs/compare/59776f36e39064734719127b8b30c67001dcc119..v11.2.0) - 2024-11-19
#### Bug Fixes
- **(citre)** don't use global cache directory - ([ed5a5d8](https://github.com/abougouffa/minemacs/commit/ed5a5d870df8c515e92fcadaade4b66c432c0193)) - [@abougouffa](https://github.com/abougouffa)
- **(combobulate)** fix problematic combobulate loading - ([2b75eda](https://github.com/abougouffa/minemacs/commit/2b75edae22ebb9a660bf5f0e4626a011487d2d3e)) - [@abougouffa](https://github.com/abougouffa)
- **(gambol)** fix the implementation `+gambol:occur-dwim` - ([22e18af](https://github.com/abougouffa/minemacs/commit/22e18afe9c8873612d1a722635360739738a72eb)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** temporary fixes for problematic pulsing - ([35c7803](https://github.com/abougouffa/minemacs/commit/35c780317f75b7fd2b421ac84b8e86623a9d668c)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- update the documentation - ([580731e](https://github.com/abougouffa/minemacs/commit/580731e357f9025dba1801574198988eb7549dfe)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+describe-random-command` - ([2bd4e05](https://github.com/abougouffa/minemacs/commit/2bd4e05d139c85b06e5173d822e2fab86c4822af)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** add `flyspell-correct`, remove usage of `spell-fu` - ([5af36df](https://github.com/abougouffa/minemacs/commit/5af36dfee1441525b115d75c76f10736314ddbf3)) - [@abougouffa](https://github.com/abougouffa)
- **(sudo-edit)** add the missing `+sudo-edit-save` - ([ac27c2e](https://github.com/abougouffa/minemacs/commit/ac27c2e72fbfaafc2dd1577294772a8658930fdd)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `emamux` obsolete - ([59776f3](https://github.com/abougouffa/minemacs/commit/59776f36e39064734719127b8b30c67001dcc119)) - [@abougouffa](https://github.com/abougouffa)
- remove some commands, alternatives in `crux` and `sudo-edit` - ([7bd5239](https://github.com/abougouffa/minemacs/commit/7bd5239c099a59b0945dc41af79a51c5e6cc0d5c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** simplify the implementation of `+autoload-region` - ([c3175ec](https://github.com/abougouffa/minemacs/commit/c3175ecc46ae0faeb3e4a382036f0f7202cdbeb0)) - [@abougouffa](https://github.com/abougouffa)
- remove confusing `:package` keyword from `:bind` blocks - ([3c089c5](https://github.com/abougouffa/minemacs/commit/3c089c5ea5f55b2c65c8f0657479228bd3867809)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(vc)** replace `magit-file-icons` with `magit-iconify` - ([386c2c4](https://github.com/abougouffa/minemacs/commit/386c2c4b175113106d88d0b532828ba37435dbc5)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** better initial value for `+html2pdf-default-backend` - ([e31e147](https://github.com/abougouffa/minemacs/commit/e31e14778f4251458afc47af663ba5b2da2c427b)) - [@abougouffa](https://github.com/abougouffa)
- **(projection)** bind `projection-dape` & `projection-multi-compile` - ([adc199b](https://github.com/abougouffa/minemacs/commit/adc199b000d4f3f2d9f4d3d107f12b9d94a3d2e6)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** use `plink` on Windows when available - ([479e425](https://github.com/abougouffa/minemacs/commit/479e4257c41dd7bba55e84315960b8b4fd6868df)) - [@abougouffa](https://github.com/abougouffa)
- **(ztree)** draw Unicode lines - ([c7ec30e](https://github.com/abougouffa/minemacs/commit/c7ec30e43fb5ef00c23898f712b56d466245d438)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d587fb5](https://github.com/abougouffa/minemacs/commit/d587fb5210211c67f43a775407be807307169fc1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([60ce18b](https://github.com/abougouffa/minemacs/commit/60ce18bb6a7e9f8b2ab4983d6ba48b9e09505837)) - [@abougouffa](https://github.com/abougouffa)
- remove unused `+compile-functions` - ([77200fd](https://github.com/abougouffa/minemacs/commit/77200fd3577fcd29f4caf9c4374462b775354035)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.1.0](https://github.com/abougouffa/minemacs/compare/724cdefd3e12d3615cadca0f01780aed4076649c..v11.1.0) - 2024-11-17
#### Bug Fixes
- **(combobulate)** fix bindings for obsolete commands - ([ed70d24](https://github.com/abougouffa/minemacs/commit/ed70d24f9c46e5e7d07448a242f735b5a1094349)) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed-protocol)** properly initialize after loading `elfeed` - ([780cc62](https://github.com/abougouffa/minemacs/commit/780cc62b8a2c2d6809d1d5e3618b76f10bf9f9b4)) - [@abougouffa](https://github.com/abougouffa)
- **(selinux-policy)** fix the recipe - ([c6858b4](https://github.com/abougouffa/minemacs/commit/c6858b4c0b758f03b2e0637fe8e70ef802f2512a)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(natural-langs)** update the description of `me-eglot-ltex` - ([60361c8](https://github.com/abougouffa/minemacs/commit/60361c8c217afb23ed2194c0002cdd4f6fca353a)) - [@abougouffa](https://github.com/abougouffa)
- update the documentation - ([0407a87](https://github.com/abougouffa/minemacs/commit/0407a87d8b2fb45dd393f526d48dafe6506cbbd0)) - [@abougouffa](https://github.com/abougouffa)
- generate the modules list - ([391205a](https://github.com/abougouffa/minemacs/commit/391205aabf170f4e5f4bb8d9c76eb853d6b88f6f)) - [@abougouffa](https://github.com/abougouffa)
- add descriptions for on-demand packages - ([d42fb84](https://github.com/abougouffa/minemacs/commit/d42fb84e8fed39dc0c96ac71642b68a7811f2371)) - [@abougouffa](https://github.com/abougouffa)
- add a list of modules/packages - ([4aa707d](https://github.com/abougouffa/minemacs/commit/4aa707dcc9856c1083d7557534bbe43cd684af02)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(elfeed)** enable again + bind `+elfeed` to `C-c C-o f` - ([e14faa4](https://github.com/abougouffa/minemacs/commit/e14faa4f2db3703d7dc412ea19754f8b7de78dee)) - [@abougouffa](https://github.com/abougouffa)
- **(rss)** add support for extra protocols via `elfeed-protocol` - ([138d889](https://github.com/abougouffa/minemacs/commit/138d8898bb9464b51d63518383285e1fd125b066)) - [@abougouffa](https://github.com/abougouffa)
- add `minemacs-extract-packages-descriptions` - ([be95e29](https://github.com/abougouffa/minemacs/commit/be95e290338124b0fefb875c5c2b04139685a011)) - [@abougouffa](https://github.com/abougouffa)
- move `yasnippet` and the like to the new module `me-snippets` - ([3eb87ad](https://github.com/abougouffa/minemacs/commit/3eb87ad49bc840065fa3ddd032388b561e7e4d74)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** automatically generate all the docs - ([39b7ec1](https://github.com/abougouffa/minemacs/commit/39b7ec1123b4aac8129e4e3791c18e69ac0b07f8)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(elfeed)** minor edits - ([1edcb85](https://github.com/abougouffa/minemacs/commit/1edcb85bea1db829ec95d33879b3a3178c85af2e)) - [@abougouffa](https://github.com/abougouffa)
- move a command to `me-lib-extra` - ([9c787ea](https://github.com/abougouffa/minemacs/commit/9c787ea567ebb550400789b33ea2710f8a8cab51)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(jiralib)** better documentation for my custom commands - ([c9a3235](https://github.com/abougouffa/minemacs/commit/c9a323582a432fa9e553a04bfd08e5714c86ad0d)) - [@abougouffa](https://github.com/abougouffa)
- **(sx)** correctly set the cache directory - ([2f4dfc3](https://github.com/abougouffa/minemacs/commit/2f4dfc352783311f8d00b9feeddc6c4b00147909)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** add display rule for `envrc` warnings buffer - ([13c1b79](https://github.com/abougouffa/minemacs/commit/13c1b79009a51a236792cc34ac5d2c7afbd2c96a)) - [@abougouffa](https://github.com/abougouffa)
- add on-demands mods in `minemacs-extract-packages-descriptions` - ([1c19e11](https://github.com/abougouffa/minemacs/commit/1c19e115e5239e020f8533919581a6714180c2b6)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([724cdef](https://github.com/abougouffa/minemacs/commit/724cdefd3e12d3615cadca0f01780aed4076649c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.0.1](https://github.com/abougouffa/minemacs/compare/bc3690bef6b525eab1564ea33dedefd442afdf4b..v11.0.1) - 2024-11-17
#### Bug Fixes
- **(ztree)** fix the keybindings for `n` and `p` - ([c540c80](https://github.com/abougouffa/minemacs/commit/c540c8083d0228a96efc3d4bc3d872b65acda756)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([bc3690b](https://github.com/abougouffa/minemacs/commit/bc3690bef6b525eab1564ea33dedefd442afdf4b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dired)** enable wrapping next-line at top/bottom - ([b8d5df5](https://github.com/abougouffa/minemacs/commit/b8d5df5e3fbd9eda3de05807f56916da9307e915)) - [@abougouffa](https://github.com/abougouffa)
- **(dired)** remove unneeded hack - ([07b85b1](https://github.com/abougouffa/minemacs/commit/07b85b1e2674c4908732d4b92d2df54ce5f182b0)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-multimodal)** enable in `ztree-mode` - ([e01aa0b](https://github.com/abougouffa/minemacs/commit/e01aa0bd2da2c1e2674f7aa6dddd680815821e3b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.0.0](https://github.com/abougouffa/minemacs/compare/3b688e146b3929a9cdb60c31d694861bcbfbeffb..v11.0.0) - 2024-11-16
#### Bug Fixes
- **(desktop)** fix saving/reading desktop session files - ([e683b05](https://github.com/abougouffa/minemacs/commit/e683b05f55a86259e01a8ffbc6611d42886b8cd5)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** correctly configure the new `flymake-indicator-type` - ([7f2fd4a](https://github.com/abougouffa/minemacs/commit/7f2fd4ab5f79f3c8b24cb240045a1b28fdaf6238)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** better implementation of `+parinfer-rust-mode-maybe` - ([2f7d842](https://github.com/abougouffa/minemacs/commit/2f7d842e6b078cbdbd01c1ccd8df861879e22877)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** temporary disable for `multiple-cursors` - ([dbe43d8](https://github.com/abougouffa/minemacs/commit/dbe43d818922487c1438e1e9ace56e10193af8d8)) - [@abougouffa](https://github.com/abougouffa)
- fix some keymap bindings - ([33074f0](https://github.com/abougouffa/minemacs/commit/33074f0f412be2bb996185f21a3fe7e08328a270)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(extra)** add support for my new package `run-in-dir` - ([8d6adb9](https://github.com/abougouffa/minemacs/commit/8d6adb958cd0b603fcd9ab5fc1457aa39a437e58)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add initial support for `fd-dired` - ([fc57ef1](https://github.com/abougouffa/minemacs/commit/fc57ef13cd4fc126e1c42d82b26a5d8c23e403da)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-magit)** better implementation of `+multi-magit-discover-repos` - ([a765730](https://github.com/abougouffa/minemacs/commit/a76573037561cfcbe7a5f22c6c069354a9e594e7)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-magit)** add `+multi-magit-select-repos-under-directory` - ([8644ccf](https://github.com/abougouffa/minemacs/commit/8644ccff88179aa3080406c5997c85917799af71)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** remove `git-undo`, not stable - ([b0e1b10](https://github.com/abougouffa/minemacs/commit/b0e1b10ce1335046bd985944f0ada47df30a53d6)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** additional support for `multi-magit` - ([b5cd6c2](https://github.com/abougouffa/minemacs/commit/b5cd6c2de2fe06c6b36fa8c392978f2d5899a4ed)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(pages)** fix the branch - ([3b688e1](https://github.com/abougouffa/minemacs/commit/3b688e146b3929a9cdb60c31d694861bcbfbeffb)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(desktop)** rename a variable - ([9e69fe7](https://github.com/abougouffa/minemacs/commit/9e69fe76ddd634c0e1108d9d925f73b8f034d377)) - [@abougouffa](https://github.com/abougouffa)
- use `+emacs-options-p` to detect all options - ([eab3369](https://github.com/abougouffa/minemacs/commit/eab336916406292c52f6d7d841f5e4ee39d7ea67)) - [@abougouffa](https://github.com/abougouffa)
- make use of `directory-files-no-dot-files-regexp` - ([9f3c060](https://github.com/abougouffa/minemacs/commit/9f3c0603f149fe34e200038c7866ce3d761b001c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(desktop)** autoload `+desktop-read-session` - ([34aa1e8](https://github.com/abougouffa/minemacs/commit/34aa1e8719fa5217ed21096229200c491a640b90)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** remove old hack, merged upstream - ([8fe6c67](https://github.com/abougouffa/minemacs/commit/8fe6c673d042b0d4cf88a76e828018b711354e65)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-vterm)** implement `+multi-vterm-toggle-dedicated-dwim` - ([a62216d](https://github.com/abougouffa/minemacs/commit/a62216db8efaada0914e49ac51d901842f5a09f5)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** whitelist extra commands, add transient menu - ([eec057f](https://github.com/abougouffa/minemacs/commit/eec057f6791da599d8ab4cf5df5adf9312cbac55)) - [@abougouffa](https://github.com/abougouffa)
- **(ztree)** add some keybindings - ([cdd0620](https://github.com/abougouffa/minemacs/commit/cdd062028bcc1a7e26ec00d67e7b0c6cbc44a23b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2d81f58](https://github.com/abougouffa/minemacs/commit/2d81f584f3102258f6a24192ca8ce73bf2fc5a4f)) - [@abougouffa](https://github.com/abougouffa)
- minor keymap edits - ([6b5398e](https://github.com/abougouffa/minemacs/commit/6b5398e82b27e2ceb78b86f66662e822c47a75a4)) - [@abougouffa](https://github.com/abougouffa)
- better setup of custom keybindings - ([7feb5a9](https://github.com/abougouffa/minemacs/commit/7feb5a9261d8f001d97f65749ce9a96557b5a70c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.6.0](https://github.com/abougouffa/minemacs/compare/a27c957437533c3dad8e3c43d719a9aa4c55c051..v10.6.0) - 2024-11-12
#### Bug Fixes
- **(flymake)** properly setup the custom fringe symbols - ([fc2abde](https://github.com/abougouffa/minemacs/commit/fc2abdecbc9fe4b3396a290764cec6303bd7cb86)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(desktop)** automatically make timestamped copies of saved sessions - ([b2e20e5](https://github.com/abougouffa/minemacs/commit/b2e20e5d7c32c51e5e47b16a20434fb04c81d8f8)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add initial support for `org-rich-yank` - ([2616f99](https://github.com/abougouffa/minemacs/commit/2616f99daf0416f6aa7202aa1a9965552a4682f1)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add support for `git-undo` - ([3484d08](https://github.com/abougouffa/minemacs/commit/3484d0875707bfb8015d2bac66244d0f3f0b4f95)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(dogears)** minor edit - ([af2f678](https://github.com/abougouffa/minemacs/commit/af2f678f602f3b26c174d9e843a8eccf2be5cfd2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(desktop)** automatically save the session - ([3558643](https://github.com/abougouffa/minemacs/commit/35586435a013959cb65f978630d4a45c6990ce96)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** add an option for `+desktop-read-session` - ([901d9c1](https://github.com/abougouffa/minemacs/commit/901d9c1c8d5d18b2c9b9423801e7c0a0cfa04148)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** provide an option to restore the session - ([7ba12d5](https://github.com/abougouffa/minemacs/commit/7ba12d5c76ed9551120c8ee3d8caeeb590416414)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** simpler implementation of `+mu4e-view-save-all-attachments` - ([2abbec4](https://github.com/abougouffa/minemacs/commit/2abbec481865ab15a1c44d28e82edf9e1da3e377)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** minor tweaks - ([d9da42b](https://github.com/abougouffa/minemacs/commit/d9da42bcd013bc98fbb417c144f56afb7e5e50ba)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-vterm)** add `+multi-vterm-project-toggle` - ([718c0c7](https://github.com/abougouffa/minemacs/commit/718c0c7152596558ccf2fb08104f19e3eb1b9b05)) - [@abougouffa](https://github.com/abougouffa)
- **(smartparens)** disable on `minibuffer-inactive-mode`, add a rule - ([c8ad7c5](https://github.com/abougouffa/minemacs/commit/c8ad7c511f23155110ac7cadfa565874683d583a)) - [@abougouffa](https://github.com/abougouffa)
- **(smartparens)** several tweaks - ([a27c957](https://github.com/abougouffa/minemacs/commit/a27c957437533c3dad8e3c43d719a9aa4c55c051)) - [@abougouffa](https://github.com/abougouffa)
- **(ssh-deploy)** bind `C-c C-z` to prefix map instead of hydra - ([0281946](https://github.com/abougouffa/minemacs/commit/028194664a24a9161fb2a4b07a56a922447089cc)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([063e336](https://github.com/abougouffa/minemacs/commit/063e336e6a5869bfd1b2056736b00deb54f8d379)) - [@abougouffa](https://github.com/abougouffa)
- better use of `once-x-call` - ([2656553](https://github.com/abougouffa/minemacs/commit/26565534ccc3468cf8160eef2aa406f024afcdd4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4994361](https://github.com/abougouffa/minemacs/commit/4994361b3fe0dbf54f40f25fb230947e22bfb35c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.5.1](https://github.com/abougouffa/minemacs/compare/d0259dcf5297053fe4be7fc381b3075d9dbf9155..v10.5.1) - 2024-11-11
#### Bug Fixes
- **(cape/corfu)** fixes for Emacs 31 - ([ef72ffa](https://github.com/abougouffa/minemacs/commit/ef72ffa60ee8dd231261fb05de35fe43b3729f4c)) - [@abougouffa](https://github.com/abougouffa)
- fix several bugs (detected on Emacs 31) - ([c396c63](https://github.com/abougouffa/minemacs/commit/c396c632ed73a184a62c92a066a9723dace80d6f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- stick to recommended version of Ditaa & PlantUML - ([bd9802d](https://github.com/abougouffa/minemacs/commit/bd9802d191c9f92d0d0d089a0e52bf36e2e62203)) - [@abougouffa](https://github.com/abougouffa)
- replace `when/if-let` with `when/if-let*` (deprecated in v31) - ([d0259dc](https://github.com/abougouffa/minemacs/commit/d0259dcf5297053fe4be7fc381b3075d9dbf9155)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.5.0](https://github.com/abougouffa/minemacs/compare/6e012e942590997eb8dcfa6fecb0abb43c77007e..v10.5.0) - 2024-11-10
#### Bug Fixes
- **(combobulate)** hack to prevent `combobulate` from loading at startup - ([67b9fb2](https://github.com/abougouffa/minemacs/commit/67b9fb27968a0f295d7ce07bc9f503788f2c5008)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** fix `(error "Selecting deleted buffer")` - ([69cfc7c](https://github.com/abougouffa/minemacs/commit/69cfc7c33044267e20f7b0646f864c09b8424b9e)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** ensure enabling the mode in the right buffer - ([5d097d3](https://github.com/abougouffa/minemacs/commit/5d097d3f4fc9706049ce54df11af38f968582412)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** add a sample debug trick to `early-config.el` - ([bf59ef2](https://github.com/abougouffa/minemacs/commit/bf59ef2f99c13f94cf195cb33616082e9f5791c3)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([469a5f8](https://github.com/abougouffa/minemacs/commit/469a5f84d95fd7592e126132355dd70062d27a6d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(calendar)** add initial support for `org-caldav` - ([ffa68bc](https://github.com/abougouffa/minemacs/commit/ffa68bcce13ed7664237e902aa161d67a564fa9c)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** add support for StackExchange via `sx` - ([8ce9313](https://github.com/abougouffa/minemacs/commit/8ce9313eb94d56fcfaadb823f86f68e4e51792d0)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** replace `magit-file-icons` with `magit-iconify` - ([392dcb4](https://github.com/abougouffa/minemacs/commit/392dcb4a1a1c4878f50f137e993494c934c93194)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(calendar)** set the week starting day - ([e5d4dd2](https://github.com/abougouffa/minemacs/commit/e5d4dd2066880021e895ede599f5440d15bd7cd0)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-eglot)** cleanup old `lsp-mode` related code - ([1c5a603](https://github.com/abougouffa/minemacs/commit/1c5a603998bf0cebf7b37af12f2fb387da91afcf)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unused/useless commands and functions - ([128a211](https://github.com/abougouffa/minemacs/commit/128a211be6f7b0c83b7de1c5d52f7b3fa409d89c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** propose to create existent paths when renaming files - ([f3dbd99](https://github.com/abougouffa/minemacs/commit/f3dbd9954fa14839836445766ae11640949722d7)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** use UTF-8 for language environment - ([f6380c3](https://github.com/abougouffa/minemacs/commit/f6380c3df1ebf9a09a44178b3ad53fe5e5450d13)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** enhance the `tool-bar-setup` hack - ([7c2a7f7](https://github.com/abougouffa/minemacs/commit/7c2a7f73e4ddbf3fc85b9c4a18aaba79061a74a7)) - [@abougouffa](https://github.com/abougouffa)
- **(newcomment)** minor tweaks - ([f3e63e0](https://github.com/abougouffa/minemacs/commit/f3e63e0e30795b1b3ae980d0007bc15bd6c09ff9)) - [@abougouffa](https://github.com/abougouffa)
- **(paren)** better defaults for `show-paren-mode` - ([e44a98a](https://github.com/abougouffa/minemacs/commit/e44a98aca417a69836b6cda5ce7d702d7ac58194)) - [@abougouffa](https://github.com/abougouffa)
- **(plantuml)** prefer executable, correctly use the JAR file - ([7df0234](https://github.com/abougouffa/minemacs/commit/7df0234a4f14dc7b7be4654a23336f01bc4dd88c)) - [@abougouffa](https://github.com/abougouffa)
- **(recentf)** increase the maximum number of saved items - ([45c0703](https://github.com/abougouffa/minemacs/commit/45c07037669c14f40b7a758a99c277f6cb5b5834)) - [@abougouffa](https://github.com/abougouffa)
- **(smartparens)** enable in the minibuffer - ([65c7131](https://github.com/abougouffa/minemacs/commit/65c7131d497a7cfecb938ad19035c399f23c2380)) - [@abougouffa](https://github.com/abougouffa)
- **(vundo)** bind `vundo` to `C-c o u` - ([6e012e9](https://github.com/abougouffa/minemacs/commit/6e012e942590997eb8dcfa6fecb0abb43c77007e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([efe0022](https://github.com/abougouffa/minemacs/commit/efe0022bb0aa82ef8f890567904fc97023b37617)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7c3a535](https://github.com/abougouffa/minemacs/commit/7c3a535f3847cc1ff9a506986b6593bf7850e26d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.4.0](https://github.com/abougouffa/minemacs/compare/3d038df4b4a7e5ca9847753e77cc253c5fd9989f..v10.4.0) - 2024-11-08
#### Features
- **(core)** bind `M-:` to `pp-eval-expression` - ([b3abd1b](https://github.com/abougouffa/minemacs/commit/b3abd1b71b8c0eb78018fa4e714e462810c1c47a)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `disk-usage` - ([d99f1f0](https://github.com/abougouffa/minemacs/commit/d99f1f08555b708cee297afe6caebb2158bbc858)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `rscope` obsolete - ([d46370a](https://github.com/abougouffa/minemacs/commit/d46370a279cacdc32fc58d37b61805133336a909)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `package-lint` to `me-checkers` - ([4f58497](https://github.com/abougouffa/minemacs/commit/4f58497f9a1fd26ce64ba369b19afca27870bd3b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(elfeed)** temporary disable `elfeed` since it is unusable - ([189d18d](https://github.com/abougouffa/minemacs/commit/189d18ddf5b25e45fd8ff6c9d94d1e9972605b7b)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit)** enable `show-paren-local-mode` in tree explorer - ([65d69e2](https://github.com/abougouffa/minemacs/commit/65d69e2a247d14bc95236693b4d005829c75b8ee)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([410f4d7](https://github.com/abougouffa/minemacs/commit/410f4d78571d82fa81ec7a036f1321132039353f)) - [@abougouffa](https://github.com/abougouffa)
- restore default `C-x k` - ([0cb409e](https://github.com/abougouffa/minemacs/commit/0cb409ec330d022687e6e55187dced57da78f2e7)) - [@abougouffa](https://github.com/abougouffa)
- bump `parinfer-rust-mode` - ([1365843](https://github.com/abougouffa/minemacs/commit/136584389e957fc19069a0b12b6bd714e7308f09)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3d038df](https://github.com/abougouffa/minemacs/commit/3d038df4b4a7e5ca9847753e77cc253c5fd9989f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.3.0](https://github.com/abougouffa/minemacs/compare/eb0fe53b9c9f2ac9ab3e1dc52a48b167dff16719..v10.3.0) - 2024-11-07
#### Bug Fixes
- **(citre)** simplify loading the default configuration - ([6e0abca](https://github.com/abougouffa/minemacs/commit/6e0abca830db85e26c3338b3f092937a521b3805)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(citre)** more customization, add Bitbake aware list of files - ([b10db3c](https://github.com/abougouffa/minemacs/commit/b10db3cb7ef56a90b9578d9225345df8d3278710)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+github-download-release` and tweak `+github-latest-release` - ([8079515](https://github.com/abougouffa/minemacs/commit/8079515f4eca8f613f5199e7ffa6ed6c4e876d9d)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `cmake-build` `czm-cpp` obsoletes - ([c4d6e51](https://github.com/abougouffa/minemacs/commit/c4d6e515738a69a47e868aafff70e63c43321efd)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `irony-mode` obsolete - ([6e0aff7](https://github.com/abougouffa/minemacs/commit/6e0aff7c645be6b76fc6abfc7526097c14a20db5)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- minor edit - ([eaea01c](https://github.com/abougouffa/minemacs/commit/eaea01ccffb0e9e52e2053cd92f267551577dd54)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bitbake)** autoload my custom commands - ([d436db9](https://github.com/abougouffa/minemacs/commit/d436db9579b95d7c5aebf04541af3b7f56bd132b)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-cscope)** use `+region-or-thing-at-point` for initial input - ([d6b665c](https://github.com/abougouffa/minemacs/commit/d6b665c45b89d7aeaac164dc7e7a0aaf0afeb32c)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-cscope)** better integration with super-projects - ([eb0fe53](https://github.com/abougouffa/minemacs/commit/eb0fe53b9c9f2ac9ab3e1dc52a48b167dff16719)) - [@abougouffa](https://github.com/abougouffa)
- **(ditaa)** auto download using `+github-download-release` - ([25efe9f](https://github.com/abougouffa/minemacs/commit/25efe9fa1d037c8ca93dca49f0765e282ae237ab)) - [@abougouffa](https://github.com/abougouffa)
- **(ditaa)** automatically download the latest version from GitHub - ([b1b56ac](https://github.com/abougouffa/minemacs/commit/b1b56aca89a2a5cc8e0b50e101b9b3bf36b05236)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/plantuml)** auto download PlantUML JAR file - ([ec16177](https://github.com/abougouffa/minemacs/commit/ec161774226a193df7d1180a34b2b1d461a728ec)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** auto load `ol-man` (Org link to man pages) - ([9b4c27b](https://github.com/abougouffa/minemacs/commit/9b4c27b44481da85062e74c7792a4ea17724fd89)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.2.0](https://github.com/abougouffa/minemacs/compare/51d3158d67f915105eb3a0dee18630b3d8c89e02..v10.2.0) - 2024-11-05
#### Bug Fixes
- **(cmake-build)** set `cmake-build-local-options-file` very early - ([6e6893e](https://github.com/abougouffa/minemacs/commit/6e6893e52705b8965c03ae523fdd5fab3cbe9a9e)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** restore `tool-bar-setup` after startup - ([7bd7693](https://github.com/abougouffa/minemacs/commit/7bd769330a746c81c001f772a7ca1a159251fb35)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-pmd)** correct the version detection - ([a4908ec](https://github.com/abougouffa/minemacs/commit/a4908ecad2a3cde14ecfc5a2a5d83fb999d09583)) - [@abougouffa](https://github.com/abougouffa)
- **(gambol)** fix implementation of `+gambol:occur-dwim` - ([3ba07fb](https://github.com/abougouffa/minemacs/commit/3ba07fb8b11ba90e86484260195da028db8bd80c)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(bitbake-ts-mode)** add initial support - ([95b0d44](https://github.com/abougouffa/minemacs/commit/95b0d44ef18c0e38872643ac8f8b525c1412c54c)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** enable `visual-wrap-prefix-mode` in Emacs 30+ - ([c96e25a](https://github.com/abougouffa/minemacs/commit/c96e25a438169d2f4bd785cc57ee7c3c7f2d3c71)) - [@abougouffa](https://github.com/abougouffa)
- **(compile-multi)** add `nerd-icons` support - ([1cc372d](https://github.com/abougouffa/minemacs/commit/1cc372d157b0673c13b95a20d1be6480f5cd11aa)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** add `+consult-xref-history` - ([51d3158](https://github.com/abougouffa/minemacs/commit/51d3158d67f915105eb3a0dee18630b3d8c89e02)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+project-root-initialize` - ([e7f1ca8](https://github.com/abougouffa/minemacs/commit/e7f1ca842ebf03b1e3337fc3201b8b786ca60723)) - [@abougouffa](https://github.com/abougouffa)
- **(email)** add support for sending invitations via `varuga` - ([5bb1ff0](https://github.com/abougouffa/minemacs/commit/5bb1ff0da9454dac1474cb443ef5479e7ffcbcec)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for GenExpr files - ([3fee077](https://github.com/abougouffa/minemacs/commit/3fee07704578630265efa3e4be6c5921ba179dce)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for `selinuxpolicy-mode` - ([72c082a](https://github.com/abougouffa/minemacs/commit/72c082a5e12f04d484f4bc3b242063b1f33fa3dc)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/web)** add support for `flymake-biome` - ([a4aca87](https://github.com/abougouffa/minemacs/commit/a4aca8719e7ea19cb8751a5398486895b8d6a584)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/zig)** add support for `zig-ts-mode` - ([4cac851](https://github.com/abougouffa/minemacs/commit/4cac8510683f4c9242b41209f3c22625f1c4bd47)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** restore `treesitter-context` with better UI integration - ([f6aca22](https://github.com/abougouffa/minemacs/commit/f6aca22d6f767acbf7835083c47bb80446533a1d)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `gambol` support - ([9f469ae](https://github.com/abougouffa/minemacs/commit/9f469ae1664845b08f169be8a9697bf4d7edc5de)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add TRAMP support for Incus containers - ([212838c](https://github.com/abougouffa/minemacs/commit/212838c8a632774ae01d392e5598abb3b7f9a53f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([ed49e73](https://github.com/abougouffa/minemacs/commit/ed49e730041e25500c30f7db9dc16f4eff5c09b4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([da603cd](https://github.com/abougouffa/minemacs/commit/da603cd7df1934e33fbf84c5cac73b3dc0ff4b73)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.1.0](https://github.com/abougouffa/minemacs/compare/616765da01c698ecebeb6ab27db7cb7a301999fc..v10.1.0) - 2024-11-03
#### Features
- **(on-demand/python)** add support for `python-pytest` - ([9c56a6f](https://github.com/abougouffa/minemacs/commit/9c56a6f8f56e9f6497f7597ac4b6398dd167e8d0)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- make use of new (29+) base modes - ([2d2ffb2](https://github.com/abougouffa/minemacs/commit/2d2ffb2403229cb96ab19b75ef4cf7dbb2149e22)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(pet)** add integration for `quickrun` - ([4345fcf](https://github.com/abougouffa/minemacs/commit/4345fcf8003624bfa937b46a81f73c6c45170f05)) - [@abougouffa](https://github.com/abougouffa)
- **(pet)** make sure the `pet-mode` hook is executed before other hooks - ([616765d](https://github.com/abougouffa/minemacs/commit/616765da01c698ecebeb6ab27db7cb7a301999fc)) - [@abougouffa](https://github.com/abougouffa)
- **(quickrun)** bind `quickrun` to F5 - ([516ad41](https://github.com/abougouffa/minemacs/commit/516ad4102f83c910e8588e6e4124fd7a80e01775)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.0.0](https://github.com/abougouffa/minemacs/compare/da0728d179d6d4b5d6cdb46d652998d4e97f2a2f..v10.0.0) - 2024-11-02
#### Bug Fixes
- **(core)** fix the `:trigger-commands` argument of `use-package` - ([3aed505](https://github.com/abougouffa/minemacs/commit/3aed505de5d7a6085cbb227562aa53fa436c4e90)) - [@abougouffa](https://github.com/abougouffa)
- **(jujutsushi)** use my mirror (sourcehut is often offline) - ([60dd738](https://github.com/abougouffa/minemacs/commit/60dd738aa27383299bc0946fe8e77820b8a39ad9)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-file-icons)** load before `magit`, but not too early - ([543724c](https://github.com/abougouffa/minemacs/commit/543724c37ae423d27ce995a58ed93afdfb4eacf8)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- minor edits - ([9442493](https://github.com/abougouffa/minemacs/commit/944249306386417d4033156fd8619a4849642a2f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** officially drop support for Emacs 28 - ([6572063](https://github.com/abougouffa/minemacs/commit/65720630d9735c1e6d320f0a50ff1bc2b1a00183)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+file-type` - ([71afd8e](https://github.com/abougouffa/minemacs/commit/71afd8e9dad77915badb90beb3892b8e363c7dce)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** add initial support for `gumshoe` - ([fbf36fd](https://github.com/abougouffa/minemacs/commit/fbf36fdebae3f552f78d2cb487fe1bc4698edbd2)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** add initial support for `deadgrep` - ([38b7eec](https://github.com/abougouffa/minemacs/commit/38b7eeca2dcbfddee9b78083a15d055713f4e5d8)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add Jujutsu (`jj`) support - ([f96e481](https://github.com/abougouffa/minemacs/commit/f96e481c8306ae004016ff0fb9f5ff7295638f4d)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** remove unneeded code - ([9f89b99](https://github.com/abougouffa/minemacs/commit/9f89b99c3ddae0e63b5f8737ee3815d1523a2d9e)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** make `minemacs-load-module` more generic - ([da0728d](https://github.com/abougouffa/minemacs/commit/da0728d179d6d4b5d6cdb46d652998d4e97f2a2f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(awqat)** change the icon - ([30e8a9c](https://github.com/abougouffa/minemacs/commit/30e8a9c38317ee8206f725dab0b3b2fac5c6d85f)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** remove hack, the change has been merged upstream - ([4654975](https://github.com/abougouffa/minemacs/commit/46549754557fafb8692cb9d014f6a4e6bd9df8c8)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor tweaks - ([7f0c960](https://github.com/abougouffa/minemacs/commit/7f0c960fbed7b4854a8d4eb11fca5a134db6e725)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `os/android` variable - ([ad951d5](https://github.com/abougouffa/minemacs/commit/ad951d5d4d914b27c47d1d6068058dc31c225c26)) - [@abougouffa](https://github.com/abougouffa)
- **(ob-ditaa)** include JAR files for Ditaa - ([901d93d](https://github.com/abougouffa/minemacs/commit/901d93de435a984a6d5c9273f1b9be1312da8271)) - [@abougouffa](https://github.com/abougouffa)
- **(with-editor)** add support for `JJ_EDITOR` - ([d701a39](https://github.com/abougouffa/minemacs/commit/d701a39f6fb3e190bb866e91932987023e9eacfa)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([be77906](https://github.com/abougouffa/minemacs/commit/be779066812ff1849cf795bc0a8835db224bcbf3)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c84e461](https://github.com/abougouffa/minemacs/commit/c84e4611dc77308fd243687b828371a1b3c86013)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0162893](https://github.com/abougouffa/minemacs/commit/016289373b9efcb4257d4141e5989bc5e5fba8d5)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([f37723d](https://github.com/abougouffa/minemacs/commit/f37723ddd9d857033acf00e354e8558996731106)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.4.1](https://github.com/abougouffa/minemacs/compare/c72e9292214f9f2e305b27cb1c34d78a124d1fc7..v9.4.1) - 2024-10-27
#### Documentation
- **(documentation)** regenerate the documentation - ([c72e929](https://github.com/abougouffa/minemacs/commit/c72e9292214f9f2e305b27cb1c34d78a124d1fc7)) - [@abougouffa](https://github.com/abougouffa)
- update external dependencies - ([34722b1](https://github.com/abougouffa/minemacs/commit/34722b1b3ce3412f72ecaff93825467f455b3291)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(cape)** use `satch-advice-add` when possible - ([44847d9](https://github.com/abougouffa/minemacs/commit/44847d93da544725b0d5c9ef40f671775c057196)) - [@abougouffa](https://github.com/abougouffa)
- move pseudo-packages declaration to `me-bootstrap` - ([c682141](https://github.com/abougouffa/minemacs/commit/c6821416f64d40c921f7c4ee9d82b421890517af)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cape)** remove a tweak for the unsupported Emacs 28 - ([6bce8b3](https://github.com/abougouffa/minemacs/commit/6bce8b30c3de13ddfb28845587f3e4206e17974d)) - [@abougouffa](https://github.com/abougouffa)
- **(casual)** bind `casual-editkit-main-tmenu` to `C-o` - ([d05429a](https://github.com/abougouffa/minemacs/commit/d05429a76fe0aced5fb99f6f977898a3e131ac7f)) - [@abougouffa](https://github.com/abougouffa)
- **(compat)** force using the latest version from GNU ELPA mirror - ([ae6cb1e](https://github.com/abougouffa/minemacs/commit/ae6cb1eb86657eb65c30015b2958e97f1fc9e07b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3d3b95b](https://github.com/abougouffa/minemacs/commit/3d3b95b691c9db92491114a682101cbd4426c3d0)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.4.0](https://github.com/abougouffa/minemacs/compare/90ee195695f9f77639830b092d2f106abb86f3b8..v9.4.0) - 2024-10-26
#### Bug Fixes
- **(ui)** upgrade the Casual suite, merged into one package - ([02e3845](https://github.com/abougouffa/minemacs/commit/02e38458840addf67d30f249265a49db9c9e7083)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(completion)** add descriptions for each package - ([7c6e0be](https://github.com/abougouffa/minemacs/commit/7c6e0bec9a98979ac925eb40c7260ca94de64343)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add descriptions for each package - ([0fcebe6](https://github.com/abougouffa/minemacs/commit/0fcebe6f1f33bb4e605a03cb2358fdf1afebf92f)) - [@abougouffa](https://github.com/abougouffa)
- add descriptions for included packages - ([4dd8e63](https://github.com/abougouffa/minemacs/commit/4dd8e633471718d7e60c61f558412fb9cb15a981)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** better support for CMake via `cmake-build` & `czm-cpp` - ([c540e73](https://github.com/abougouffa/minemacs/commit/c540e7384e1ec9dff7b7af4bbab9f0643fe6bf34)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** restore `magit-file-icons` - ([90ee195](https://github.com/abougouffa/minemacs/commit/90ee195695f9f77639830b092d2f106abb86f3b8)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `editorconfig` to `me-builtin` - ([93be81d](https://github.com/abougouffa/minemacs/commit/93be81d171585548ec4436527234d574acc5ac59)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cmake-build)** set the file path - ([b0e583d](https://github.com/abougouffa/minemacs/commit/b0e583d206cd63d25a9c23e30c855da93e3f23e2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cbc5068](https://github.com/abougouffa/minemacs/commit/cbc506844ed6268c56a7b4dfdd57e81432d19266)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d89b7b2](https://github.com/abougouffa/minemacs/commit/d89b7b291401175e47e888de3591b8dde945cd47)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.6](https://github.com/abougouffa/minemacs/compare/993d21efbd24829226c4cd19276906f5bbfc3ce9..v9.3.6) - 2024-10-18
#### Bug Fixes
- **(core)** fix the regexp for failed patches in `+apply-patch-dwim` - ([8e2b367](https://github.com/abougouffa/minemacs/commit/8e2b3674a733428664945f0c69f6c3bbb05ef388)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** restore accidentally removed hook - ([103a556](https://github.com/abougouffa/minemacs/commit/103a55609dbd30ae08debe62a5d7a705abb1e7e0)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult)** don't preview GPG files - ([9f193f2](https://github.com/abougouffa/minemacs/commit/9f193f2a15d547e3a5de01457cf78f13bdf5db31)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** enable view mode in patch output - ([135d269](https://github.com/abougouffa/minemacs/commit/135d2692f15eda3e845610ae92f54b97a0d47376)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better UX for `+apply-patch-dwim` - ([6b7414c](https://github.com/abougouffa/minemacs/commit/6b7414cebe735f15170441361cb33888c78a3546)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better user experience in `+apply-patch-dwim` - ([361eba1](https://github.com/abougouffa/minemacs/commit/361eba1137f4d5f2ef81564fb5b6fdb61f413fd2)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simple and efficient implementation of `+apply-patch-dwim` - ([993d21e](https://github.com/abougouffa/minemacs/commit/993d21efbd24829226c4cd19276906f5bbfc3ce9)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8dcf56c](https://github.com/abougouffa/minemacs/commit/8dcf56c6e91757cf67c7acba3001cbc7016df89b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.5](https://github.com/abougouffa/minemacs/compare/aead8e0ab54b34abd816252fa84f778f60497d31..v9.3.5) - 2024-10-06
#### Documentation
- **(readme)** update the branch on CI badges - ([aead8e0](https://github.com/abougouffa/minemacs/commit/aead8e0ab54b34abd816252fa84f778f60497d31)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([256c145](https://github.com/abougouffa/minemacs/commit/256c1453d2e3166954032bce7762de9317f53701)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.4](https://github.com/abougouffa/minemacs/compare/43659edb28f9db2d7cab1d2628f53bf131d0a98b..v9.3.4) - 2024-10-03
#### Bug Fixes
- **(core)** edge case in `+apply-patch-dwim` - ([40922d8](https://github.com/abougouffa/minemacs/commit/40922d8f6fb770416de3d07f8605a3ed21370bcd)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add pre/post functions for `+apply-patch-dwim` - ([b7d64e5](https://github.com/abougouffa/minemacs/commit/b7d64e598749ee45758c802f18c8d8c327239061)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** enable `--ignore-whitespace` for `+apply-patch-dwim` - ([43659ed](https://github.com/abougouffa/minemacs/commit/43659edb28f9db2d7cab1d2628f53bf131d0a98b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.3](https://github.com/abougouffa/minemacs/compare/9ece6787b9c365bfcbebb9da5a935cc3773e5fd7..v9.3.3) - 2024-09-30
#### Bug Fixes
- **(forge)** ensure `markdown-mode` is available - ([4733196](https://github.com/abougouffa/minemacs/commit/47331968b4cf0d9a01d985a62a0edd5d5c6c28a6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** complete the implementation of `+apply-patch-dwim` - ([2e810ec](https://github.com/abougouffa/minemacs/commit/2e810ec5ff6e306f56a81e09a1cbb3ed3bcfc687)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+apply-patch-dwim` - ([a975f88](https://github.com/abougouffa/minemacs/commit/a975f88dd319c6bad18dcdf3e06620e68ba75af7)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** initial support for `causal-editkit` and `casual-symbol-overlay` - ([6b92988](https://github.com/abougouffa/minemacs/commit/6b92988690616d901263cc046eea4243186cc63f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bitbake)** add a meaningful prompt to `+bitbake-insert-poky-sources` - ([9ece678](https://github.com/abougouffa/minemacs/commit/9ece6787b9c365bfcbebb9da5a935cc3773e5fd7)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** regenerate loaddefs - ([fe555c0](https://github.com/abougouffa/minemacs/commit/fe555c00d640446acbadf7aace9b9f088696ca2b)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete/code-review)** require `on-demand/markdown` when used - ([1b316fc](https://github.com/abougouffa/minemacs/commit/1b316fc5d372a57f997b5744b4b8dd17b359ae0b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([88c531b](https://github.com/abougouffa/minemacs/commit/88c531b1ee2b7569c88244212ef6d957d5d2e361)) - [@abougouffa](https://github.com/abougouffa)
- rename `+yank-this-file-name` to `+copy-this-file-name` - ([b0dcadc](https://github.com/abougouffa/minemacs/commit/b0dcadc4aad8610b7237d346156e8e81720f4c8d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.2](https://github.com/abougouffa/minemacs/compare/5b40821c703cf01b59869738b8eb6a566cf36a12..v9.3.2) - 2024-09-15
#### Documentation
- **(readme)** remove note about new/legacy MinEmacs - ([5b40821](https://github.com/abougouffa/minemacs/commit/5b40821c703cf01b59869738b8eb6a566cf36a12)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** ensure `minemacs-try-load-extra-mode` is called the called - ([161cfd0](https://github.com/abougouffa/minemacs/commit/161cfd09205cf0e7b965b6579137248660f2b296)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f36751e](https://github.com/abougouffa/minemacs/commit/f36751eafab17f896fec9b8d00f64397973d7d92)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.1](https://github.com/abougouffa/minemacs/compare/61296654b1fd1830d4fb907858b88c890106a6a5..v9.3.1) - 2024-08-31
#### Features
- **(core)** add `+autoload-region` - ([89b95c6](https://github.com/abougouffa/minemacs/commit/89b95c6b183342fb372098e0b820deac97f943af)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add `gcode-mode` - ([c98d9bc](https://github.com/abougouffa/minemacs/commit/c98d9bc89fd5ad9d111cd7bef6b1d506074ce31a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dired)** better file listing - ([4fd0421](https://github.com/abougouffa/minemacs/commit/4fd04212196fbdf0a8923b6600788f50c76acaa6)) - [@abougouffa](https://github.com/abougouffa)
- **(diredfl)** add more compressed files extensions - ([d0b8523](https://github.com/abougouffa/minemacs/commit/d0b85237c436b28e0b73f922d0ec5a20856ad447)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** run `+mc/mark-all-symbol-overlays` only once - ([dd9998c](https://github.com/abougouffa/minemacs/commit/dd9998c35304a3f355a369e1c179203af90f11de)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** add more pulsing commands - ([6129665](https://github.com/abougouffa/minemacs/commit/61296654b1fd1830d4fb907858b88c890106a6a5)) - [@abougouffa](https://github.com/abougouffa)
- **(xclip)** log errors to `*Messages*` but don't display them - ([33b05ff](https://github.com/abougouffa/minemacs/commit/33b05ff8524295537e1f3c47e6093e12abaeafab)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8b4e0e2](https://github.com/abougouffa/minemacs/commit/8b4e0e2ae23fa0e14843237303973bb4b536cfc3)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.0](https://github.com/abougouffa/minemacs/compare/19c83ef67ad86490d172beeb93f5258a6387e772..v9.3.0) - 2024-08-29
#### Bug Fixes
- **(mojo)** fix the extension binding and the autoloads - ([7e46bc2](https://github.com/abougouffa/minemacs/commit/7e46bc2c40d210a8a3a4661680a0c005db7a3505)) - [@abougouffa](https://github.com/abougouffa)
- **(platformio-mode)** avoid permanently disabling `projectile` - ([ed3177e](https://github.com/abougouffa/minemacs/commit/ed3177ec36d66ed71326d1ce65f6083c101a4df3)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(embedded)** initial support for `platformio-mode` - ([0241abd](https://github.com/abougouffa/minemacs/commit/0241abd0970e1b3833c86a2da3e9f08b94f65a84)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `dired-hacks` - ([a3ec35b](https://github.com/abougouffa/minemacs/commit/a3ec35b455df4d0b92a2e79a2c4a232ee3cde5af)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `dired+` - ([63045ec](https://github.com/abougouffa/minemacs/commit/63045ec30a54966e52ae7786087347741db2b6f8)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `dirvish` obsolete, too much problems! - ([e18ff4d](https://github.com/abougouffa/minemacs/commit/e18ff4df8f3df8637f2b53e35b1e228784f3f5d2)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** make `matlab-mode` obsolete - ([8047d47](https://github.com/abougouffa/minemacs/commit/8047d477b790ba94503ad23a9e80f9611b7646fc)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for the Mojo language - ([b237a9c](https://github.com/abougouffa/minemacs/commit/b237a9c42517209addbefe99805aa1dbfea9c74b)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** add initial support for `irony-mode` and `irony-eldoc` - ([6a53610](https://github.com/abougouffa/minemacs/commit/6a536101548ea0269d1b187b4dbb5350bbc00d82)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** add support for `consult-cscope` - ([7b9dc33](https://github.com/abougouffa/minemacs/commit/7b9dc3387e827bb7c1b75633939215a7a23e5cc2)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `diredfl` - ([d07c103](https://github.com/abougouffa/minemacs/commit/d07c103d88b0a1d6847219c230674c80ab43c47f)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** replace `nerd-icons-archive` with `nerd-icons-multimodal` - ([41d7f30](https://github.com/abougouffa/minemacs/commit/41d7f30ec0533f457343021139e55e0624b42dbf)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `nerd-icons-dired` - ([9ec9f12](https://github.com/abougouffa/minemacs/commit/9ec9f12024086420b8b89fd4dd755eef62d8c14f)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- remove `me-biblio`, move `citar` & `citar-embark` to `me-org` - ([02f433e](https://github.com/abougouffa/minemacs/commit/02f433e2403f7420858d485456d3c45f7a45e1dc)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(files)** remove `dired+`, too much opinionated - ([7a7d2fa](https://github.com/abougouffa/minemacs/commit/7a7d2fa1e789dd259c523a28da76ac949ab7f4ec)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** remove `nerd-icons-dired` - ([864119f](https://github.com/abougouffa/minemacs/commit/864119f8422e26de0afdca5863f1fee5a40373ad)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bitbake)** use `completing-read` instead of `widget-choose` - ([a993a20](https://github.com/abougouffa/minemacs/commit/a993a202d0ca8dd36978d38a8124c7e5fe174db2)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete/projectile)** check if the package hasn't been disabled - ([0f544c2](https://github.com/abougouffa/minemacs/commit/0f544c2b003ebea034e7cc1041e78f625162a510)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** better fake functions for `projectile` - ([d342a70](https://github.com/abougouffa/minemacs/commit/d342a70f9e5bfe717e48433802813e3561aa090b)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** don't auto-create parser for Org buffers - ([19c83ef](https://github.com/abougouffa/minemacs/commit/19c83ef67ad86490d172beeb93f5258a6387e772)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([40e6237](https://github.com/abougouffa/minemacs/commit/40e6237835be85e14db5bd78c8eec57739c0de00)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.2.1](https://github.com/abougouffa/minemacs/compare/678f6a884981de12ef4292c1f9508dc8c2b422f9..v9.2.1) - 2024-08-25
#### Bug Fixes
- **(git-timemachine)** add a hack to fix the font lock issue - ([95eef36](https://github.com/abougouffa/minemacs/commit/95eef36cd1fdbdfe245ef3a7cf204d65b04f1d90)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `compat` to `me-builtin` - ([709e28c](https://github.com/abougouffa/minemacs/commit/709e28cf168483e7140abc4e1db4e92ba9fd745e)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(+browse-html-file)** create a temporary file for non-file visiting buffers - ([df66315](https://github.com/abougouffa/minemacs/commit/df663150f45d97fa7f5b49620a9fa33d5e246951)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([678f6a8](https://github.com/abougouffa/minemacs/commit/678f6a884981de12ef4292c1f9508dc8c2b422f9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.2.0](https://github.com/abougouffa/minemacs/compare/0438bb980bf9e860eb4c4d24128b8074d849f1be..v9.2.0) - 2024-08-24
#### Features
- **(emacs-lisp)** make `elisp-demos` obsolete - ([7835185](https://github.com/abougouffa/minemacs/commit/7835185690f25dd34ae6728d4183735f7853a58e)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `casual-agenda` - ([410519c](https://github.com/abougouffa/minemacs/commit/410519c80015c079007a7008a59461ce40abb97d)) - [@abougouffa](https://github.com/abougouffa)
- more on-demand modes - ([bb8f43d](https://github.com/abougouffa/minemacs/commit/bb8f43daca52101216d7562d93d5b2d579f15e67)) - [@abougouffa](https://github.com/abougouffa)
- more on-demand modes - ([0438bb9](https://github.com/abougouffa/minemacs/commit/0438bb980bf9e860eb4c4d24128b8074d849f1be)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(ui)** move `info-colors` from `me-emacs-lisp` to `me-ui` - ([874a32b](https://github.com/abougouffa/minemacs/commit/874a32b7a390666b38d70efa23f210fc5e3a3532)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(docs)** restore `pdf-tools` to `me-docs` - ([2bf2baa](https://github.com/abougouffa/minemacs/commit/2bf2baae462f725769d957503b56a62e580dc2ed)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(multiple-cursors)** add some commands to run once or on all cursors - ([f4ab2e3](https://github.com/abougouffa/minemacs/commit/f4ab2e3943d6f2d56bb0ed540104e7b2cfdaca47)) - [@abougouffa](https://github.com/abougouffa)
- **(x86-lookup)** use the available PDF reader, set the cache directory - ([a11ab2b](https://github.com/abougouffa/minemacs/commit/a11ab2b5b1866aa9169a11130c2d368576e0c6e6)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.1.0](https://github.com/abougouffa/minemacs/compare/476ae1011076d380f10c32b3d9c0b870f4196594..v9.1.0) - 2024-08-22
#### Features
- more on-demand modes - ([fb8bcd5](https://github.com/abougouffa/minemacs/commit/fb8bcd5ecea740a966133a783cb204df0e809aea)) - [@abougouffa](https://github.com/abougouffa)
- more on-demand modes - ([7c9daa3](https://github.com/abougouffa/minemacs/commit/7c9daa37a6c04e9017a067e8fc3af88b9cdcd9cf)) - [@abougouffa](https://github.com/abougouffa)
- more on-demand packages - ([476ae10](https://github.com/abougouffa/minemacs/commit/476ae1011076d380f10c32b3d9c0b870f4196594)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- change the mail address in some files - ([61f6970](https://github.com/abougouffa/minemacs/commit/61f69702bd3b5a6dfbd63629fc560d5ca4af120e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(obsolete/realgud)** avoid using Evil stuff unless used, simplify code - ([9706d4e](https://github.com/abougouffa/minemacs/commit/9706d4e7884688f8892f821bfab9c30b52aef31c)) - [@abougouffa](https://github.com/abougouffa)
- add `+prog-mode-run-hooks` - ([50bf659](https://github.com/abougouffa/minemacs/commit/50bf659a126abf427e179a9d854dce149579686b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(align)** fix OpenCL integration - ([2c36085](https://github.com/abougouffa/minemacs/commit/2c36085a476c50cac472e2e9fcfb8c9a09706fc9)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-completion)** remove a fix, merged upstream - ([b294ace](https://github.com/abougouffa/minemacs/commit/b294acee6ff3802888dc512c3daf6c9621727318)) - [@abougouffa](https://github.com/abougouffa)
- **(visual-basic)** enable line numbers in `visual-basic-mode` - ([10126ae](https://github.com/abougouffa/minemacs/commit/10126ae795bc107c36a0d9d84b01c58b6ac364ef)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ae97767](https://github.com/abougouffa/minemacs/commit/ae97767289b724868567d3a00b104e6273d41c84)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.0.0](https://github.com/abougouffa/minemacs/compare/v9.0.0-rc2..v9.0.0) - 2024-08-16
#### Bug Fixes
- ensure asking for loading on-demand packages when choosen - ([06c4fda](https://github.com/abougouffa/minemacs/commit/06c4fdac07afbc03c5bd892b57e9bd943d0f91dd)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([43918a1](https://github.com/abougouffa/minemacs/commit/43918a1c8b2e063976e024ee8c5446e0f412099d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ai)** add initial support for `whisper` - ([9f7fa77](https://github.com/abougouffa/minemacs/commit/9f7fa77142c9078fe8e84717c1fbf65a353734f6)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+project-safe-root` - ([c8f2976](https://github.com/abougouffa/minemacs/commit/c8f297660c60fd4b62bc463cc7d263a7fa64f98c)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for DjVu files - ([cc0ab8f](https://github.com/abougouffa/minemacs/commit/cc0ab8f2089adeedbcdeb0ee4e2f03cbbef18e84)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `pandoc-mode` to an on-demand module - ([1a4e279](https://github.com/abougouffa/minemacs/commit/1a4e2794c5722cc1d0613f77b19ed7bc75af9dc9)) - [@abougouffa](https://github.com/abougouffa)
- move PDF related packages to on-demand module `me-pdf` - ([1029365](https://github.com/abougouffa/minemacs/commit/102936540b4c16d7070497cf4ec7bac7e8002b35)) - [@abougouffa](https://github.com/abougouffa)
- make use of `+project-safe-root` - ([d12e0af](https://github.com/abougouffa/minemacs/commit/d12e0af8667dd172c2e6c73a50faaecab0f4e773)) - [@abougouffa](https://github.com/abougouffa)
- load `flymake-cppcheck` and `flymake-guile` on-demand - ([2ce317d](https://github.com/abougouffa/minemacs/commit/2ce317d17b1f4777fcd4c1a83e96f9d3bd211194)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(ltex-ls)** use for `markdown-ts-mode` - ([9b1d84f](https://github.com/abougouffa/minemacs/commit/9b1d84f3591d6b5e0c1de6f12174195404afc998)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([49230a8](https://github.com/abougouffa/minemacs/commit/49230a804ea6a48f2e50a83772da5a46324cb529)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bced43c](https://github.com/abougouffa/minemacs/commit/bced43cf12aa2aa1ff53074196cec9fc002c21f3)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.0.0-rc2](https://github.com/abougouffa/minemacs/compare/v9.0.0-rc1..v9.0.0-rc2) - 2024-08-16
#### Bug Fixes
- **(obsolete/evil)** fix configurations due to modules changes - ([2e5214d](https://github.com/abougouffa/minemacs/commit/2e5214d9311337553d806b0503338e71687c4089)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([8eedb7e](https://github.com/abougouffa/minemacs/commit/8eedb7e23b2d05d1055e70406aff298e73b211ca)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** regenerate documentation - ([d231057](https://github.com/abougouffa/minemacs/commit/d2310573e06cfeeb33eb256740a5e3113ee42811)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** document loading `on-demand` modules - ([8d65455](https://github.com/abougouffa/minemacs/commit/8d65455fd0cad7bbc059af29d6d3510bba25a549)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(on-demand)** more on-demand packages, remove `me-data` - ([ecefe3b](https://github.com/abougouffa/minemacs/commit/ecefe3b757670b3a86e21350455a2ea1f9cf75a3)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `clang-format` obsolete (use `apheleia` and `format-all`) - ([63a0ca2](https://github.com/abougouffa/minemacs/commit/63a0ca2230e802458e9f1ae1281d607c3c6cbb66)) - [@abougouffa](https://github.com/abougouffa)
- remove `me-keybindings` (contains only unused `key-chord`) - ([c9fb57f](https://github.com/abougouffa/minemacs/commit/c9fb57fd392003776f3933c92c1268d83b4dc6ca)) - [@abougouffa](https://github.com/abougouffa)
- support more modes as on-demand modules - ([8093802](https://github.com/abougouffa/minemacs/commit/8093802717beb1a6ab4a0224bbbc644c5d9b6f81)) - [@abougouffa](https://github.com/abougouffa)
- support more modes as on-demand modules - ([898a10b](https://github.com/abougouffa/minemacs/commit/898a10bcb723d2fc7bcb096ddcf11c8178648ede)) - [@abougouffa](https://github.com/abougouffa)
- support more modes as on-demand modules - ([35630d2](https://github.com/abougouffa/minemacs/commit/35630d2b1ed3ad6b7d4a821516aeeb335c0127df)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(version)** v9.0.0-rc2 - ([8f681cb](https://github.com/abougouffa/minemacs/commit/8f681cb2de7c45ba557e664d2328549457f0934e)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(editor)** minor edit - ([af3691c](https://github.com/abougouffa/minemacs/commit/af3691c4a58d9e24a56a8b1c685145d97010b452)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(obsolete/me-evil)** minor edits - ([e0f5d1f](https://github.com/abougouffa/minemacs/commit/e0f5d1f8870f886f55e579cf2e88d0230b2b120d)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** implement `:interpreter-mode` and refactor the code - ([92052f6](https://github.com/abougouffa/minemacs/commit/92052f62ec03884ddd436bf5d82eb52457469789)) - [@abougouffa](https://github.com/abougouffa)
- move `otpp` to `me-project`, remove module `me-workspaces` - ([4e27e89](https://github.com/abougouffa/minemacs/commit/4e27e8934ff780e5ed5631367d5b0370fe093fc8)) - [@abougouffa](https://github.com/abougouffa)
- make `me-latex` an on-demand module - ([6486845](https://github.com/abougouffa/minemacs/commit/6486845b6484f0a04fe7dbb71bc155ee62315a74)) - [@abougouffa](https://github.com/abougouffa)
- merge `me-undo` into `me-editor` - ([e2a5ad1](https://github.com/abougouffa/minemacs/commit/e2a5ad176ad05c55f9ecfcfe9e3fc8c89ef36fbf)) - [@abougouffa](https://github.com/abougouffa)
- move `add-node-modules-path` to `me-tools` - ([9a9a712](https://github.com/abougouffa/minemacs/commit/9a9a712bb557c1d695f1922d0d92a1033ad1fbc5)) - [@abougouffa](https://github.com/abougouffa)
- make Markdown integration an on-demand module - ([ce12126](https://github.com/abougouffa/minemacs/commit/ce1212651333bd80003edeb1a914e8074d1edba0)) - [@abougouffa](https://github.com/abougouffa)
- change the signature of `minemacs-modules` - ([c80f8d3](https://github.com/abougouffa/minemacs/commit/c80f8d3cfbce7eb74f3053913d9a641e021d5e70)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- add an option to disable all on-demand modules - ([d0d7d7f](https://github.com/abougouffa/minemacs/commit/d0d7d7fb5416d66a3bc6a3ed0e377fe0537710e2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(guard-lf)** don't trigger on IPython notebooks - ([fa7f60f](https://github.com/abougouffa/minemacs/commit/fa7f60fc0495c1c6641201161444bbafc64b18e5)) - [@abougouffa](https://github.com/abougouffa)
- **(matlab)** install `matlab-mode` unconditionally, exclude `company` backend - ([0e2e0dc](https://github.com/abougouffa/minemacs/commit/0e2e0dcdb83b8ebae6270ef5ee09d52238c50e1b)) - [@abougouffa](https://github.com/abougouffa)
- **(pcap-mode)** truncate lines by default - ([e05c8d3](https://github.com/abougouffa/minemacs/commit/e05c8d3773b7ebbb37f5d79b44d93ef25ed21f46)) - [@abougouffa](https://github.com/abougouffa)
- **(rust)** make `rust-mode` an on-demand module, remove `rustic` and `cargo` - ([1ae67d5](https://github.com/abougouffa/minemacs/commit/1ae67d55bc8e24ed427de6e951265d3f3dc708af)) - [@abougouffa](https://github.com/abougouffa)
- rename `one-tab-per-project` to `otpp` - ([856b054](https://github.com/abougouffa/minemacs/commit/856b054a5d0b49ad7ac65828f3ad2744daafd9ae)) - [@abougouffa](https://github.com/abougouffa)
- update the modules list - ([0118173](https://github.com/abougouffa/minemacs/commit/0118173c314f82612246d068339c256cfeb62d2f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4e42677](https://github.com/abougouffa/minemacs/commit/4e42677b45061a55d9d61363594f62b67df7b60a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.0.0-rc1](https://github.com/abougouffa/minemacs/compare/c0fca518755420fc6be055b50271364c3285dd2b..v9.0.0-rc1) - 2024-08-16
#### Bug Fixes
- **(julia)** restore `julia-mode` in `me-math` - ([8ba7b50](https://github.com/abougouffa/minemacs/commit/8ba7b50275f877c84f2dc9f42d4d7d2789462c76)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(calendar)** make `calfw` obsolete - ([cccfff2](https://github.com/abougouffa/minemacs/commit/cccfff2ce539f62c21f93120eb1898f842c8b8c1)) - [@abougouffa](https://github.com/abougouffa)
- **(media)** remove `emms` - ([c0fca51](https://github.com/abougouffa/minemacs/commit/c0fca518755420fc6be055b50271364c3285dd2b)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add more on-demand modules - ([6e41226](https://github.com/abougouffa/minemacs/commit/6e412264b27e052d78718528de0fcc11718151c1)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** make `project-x` obsolete - ([dbb7cc9](https://github.com/abougouffa/minemacs/commit/dbb7cc9e5369bce7686515eef64aba8adad99952)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `eopengrok` obsolete - ([a0b7a94](https://github.com/abougouffa/minemacs/commit/a0b7a9438550c3cd7d5b29a861277e8f543f1dce)) - [@abougouffa](https://github.com/abougouffa)
- add more on-demand modules - ([a4e5d0b](https://github.com/abougouffa/minemacs/commit/a4e5d0b4a7cd90f58035d4d77e2909ac8fbc6428)) - [@abougouffa](https://github.com/abougouffa)
- add Nim support as on-demand module - ([2138f30](https://github.com/abougouffa/minemacs/commit/2138f308a0b9c7cd1dc22d91d0117551c1203c40)) - [@abougouffa](https://github.com/abougouffa)
- support OCaml as an on-demand module - ([05d836b](https://github.com/abougouffa/minemacs/commit/05d836b7aac146af60668898fc65f4c0e413246b)) - [@abougouffa](https://github.com/abougouffa)
- add `demangle-mode` as an on-demand package - ([8718235](https://github.com/abougouffa/minemacs/commit/87182353a1e29411b82cd4012e2c2d3bcee3c52b)) - [@abougouffa](https://github.com/abougouffa)
- move `alloy-mode` from `obsolete/me-formal` to `modes/me-alloy` - ([f4de723](https://github.com/abougouffa/minemacs/commit/f4de723087108c4272dc66ca53eef8e87412c959)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(snippets)** rename the on-demand module snippet - ([0616038](https://github.com/abougouffa/minemacs/commit/061603892a8e405fb5033a421ea1087d66a9214d)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** fix `memode` snippet - ([edcbd69](https://github.com/abougouffa/minemacs/commit/edcbd6975baf6877cd50ce62400a55466bbd4a5e)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** add a snippet for creating extra modes modules - ([0a280bc](https://github.com/abougouffa/minemacs/commit/0a280bcf64551ed92fbbdd84126371282a2bfcb5)) - [@abougouffa](https://github.com/abougouffa)
- **(version)** v9.0.0-rc1 - ([27033f4](https://github.com/abougouffa/minemacs/commit/27033f4131f2b45d0e5ecb0929dbb2fb68496f0b)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(empv)** tweak the download playlist command - ([dba318c](https://github.com/abougouffa/minemacs/commit/dba318c94728f639ffef58b89ebe4ced3aa455e5)) - [@abougouffa](https://github.com/abougouffa)
- make Zig and CMake integration into on-demand modules - ([87fdf43](https://github.com/abougouffa/minemacs/commit/87fdf4355edf321f0f4f1fb5c4adaa36abf7f550)) - [@abougouffa](https://github.com/abougouffa)
- remove unneeded arguments `minemacs-register-on-demand-module` - ([cbe9afa](https://github.com/abougouffa/minemacs/commit/cbe9afabac45f3d36407bed074991f2117f92c10)) - [@abougouffa](https://github.com/abougouffa)
- move `modules/modes` to `modules/on-demand` - ([ef39e7c](https://github.com/abougouffa/minemacs/commit/ef39e7c9b029bdc5145f5071b29d47fe7ece71f7)) - [@abougouffa](https://github.com/abougouffa)
- load less used modes only on-demand when needed - ([5fadc62](https://github.com/abougouffa/minemacs/commit/5fadc6220532bf5abc2eba646b2fc5d99c30d8ca)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- rebuild all packages in `minemacs-restore-locked-packages` - ([e005059](https://github.com/abougouffa/minemacs/commit/e005059494ee5606e14a806f55e61723cb6b5bcc)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** allow loading on-demand modules via `minemacs-load-module` - ([afbc799](https://github.com/abougouffa/minemacs/commit/afbc799ab5aa744be169bbd652f5d5c240641c9a)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** ask only for visible buffers - ([d69f181](https://github.com/abougouffa/minemacs/commit/d69f181fc835ada5970ace35dd7c586c604ecc31)) - [@abougouffa](https://github.com/abougouffa)
- add an option to disable all on-demand modules - ([2644605](https://github.com/abougouffa/minemacs/commit/26446055aa4761ba26443f7a065faef537d8a08b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2ed8d2e](https://github.com/abougouffa/minemacs/commit/2ed8d2e925fbc3c952176d2879f334dd9d2749d5)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.8.3](https://github.com/abougouffa/minemacs/compare/9ab3077ab61aed18b08f297f0bfe7cfc27c28b20..v8.8.3) - 2024-08-09
#### Features
- make `org-re-reveal` obsolete (never used!) - ([9ab3077](https://github.com/abougouffa/minemacs/commit/9ab3077ab61aed18b08f297f0bfe7cfc27c28b20)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([fb7a2ef](https://github.com/abougouffa/minemacs/commit/fb7a2ef0e32613f574cbe067e1552696b75d51a7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.8.2](https://github.com/abougouffa/minemacs/compare/814946e4bf52baa878238fe380f81b94f09d17cc..v8.8.2) - 2024-08-09
#### Bug Fixes
- **(init)** fix the format in error messages - ([022244a](https://github.com/abougouffa/minemacs/commit/022244a6f58c5aa134e986187511bc8b839fb77e)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate documentation - ([eabcf86](https://github.com/abougouffa/minemacs/commit/eabcf862c9b8cce5cfd4c68b7412c1434f004233)) - [@abougouffa](https://github.com/abougouffa)
- update the logo - ([814946e](https://github.com/abougouffa/minemacs/commit/814946e4bf52baa878238fe380f81b94f09d17cc)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ui)** initial support for `ef-themes` - ([19492e2](https://github.com/abougouffa/minemacs/commit/19492e29ad3d319fd2b36cf1932f9d283242bab1)) - [@abougouffa](https://github.com/abougouffa)
- add `+browse-html-file` - ([17254c7](https://github.com/abougouffa/minemacs/commit/17254c7c02aecdbf3b3a0e1385c608faf961b95c)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** use `me-lib-extra.el` to generate documentation - ([a254550](https://github.com/abougouffa/minemacs/commit/a254550b8909fcbeb86a5a228ee85be5a00dfa72)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** change fonts priority - ([21209fe](https://github.com/abougouffa/minemacs/commit/21209fe1536ed93a709529c895061207a9cb28aa)) - [@abougouffa](https://github.com/abougouffa)
- **(hurl-mode)** restore the upstream repo, issue fixed - ([250698b](https://github.com/abougouffa/minemacs/commit/250698bc94384659adeb7c6105fbbf0cb5e0070b)) - [@abougouffa](https://github.com/abougouffa)
- **(mlscroll)** restore `+mlscroll-right-mode` to the obsolete module - ([8f22cbf](https://github.com/abougouffa/minemacs/commit/8f22cbf9fd2968e91a406134ce72628b28bdfbf9)) - [@abougouffa](https://github.com/abougouffa)
- **(pet)** minor tweak for Org advice - ([9d963bc](https://github.com/abougouffa/minemacs/commit/9d963bc2aba918c7067648847024029e69ac74d9)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add Hurl grammar - ([7d2b0db](https://github.com/abougouffa/minemacs/commit/7d2b0db87b8796725c61c910f9ca08dbeba7967b)) - [@abougouffa](https://github.com/abougouffa)
- prefer `Iosevka Comfy Motion Duo` for `:variable-pitch` - ([de081de](https://github.com/abougouffa/minemacs/commit/de081de074034e8811dfb8d2571cecf6c3f112ed)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([4ddf408](https://github.com/abougouffa/minemacs/commit/4ddf408e68b5e573f822e81ed12ca4930a524457)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.8.1](https://github.com/abougouffa/minemacs/compare/ec56cc07c37b027ef08c38512cde6bac8390ac5e..v8.8.1) - 2024-08-04
#### Documentation
- **(documentation)** regenerate the documentation - ([23838fc](https://github.com/abougouffa/minemacs/commit/23838fc43528a4fdcc8492a84ff1aef00fb3412d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(elisp)** make `flymake-relint` obsolete and keep `relint` - ([511481a](https://github.com/abougouffa/minemacs/commit/511481af995c0fdd76dc3ac48803830cdecb56f4)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** add a comment - ([ec56cc0](https://github.com/abougouffa/minemacs/commit/ec56cc07c37b027ef08c38512cde6bac8390ac5e)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** remove `me-gc` from the core modules - ([5cd0d21](https://github.com/abougouffa/minemacs/commit/5cd0d215fc8e9cf30106aa9a73b3d343ff4f2737)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- add a script to generate Cppcheck rules from MISRA PDF - ([0bce095](https://github.com/abougouffa/minemacs/commit/0bce0958916f919f80f58ab6489b2f7cc219a331)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- rename some functions - ([d6e565e](https://github.com/abougouffa/minemacs/commit/d6e565e7e614f02896506aa1797f5a96ea5be2ae)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dape)** disable buggy GUD-like window arrangement - ([14acc80](https://github.com/abougouffa/minemacs/commit/14acc80f0612bb3cdadc9199bcb362dbb46ae07f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.8.0](https://github.com/abougouffa/minemacs/compare/3ca7982a15839426bb53206bbbd209184e493f2d..v8.8.0) - 2024-08-03
#### Bug Fixes
- **(hurl-mode)** temporary switch to my fork until PR gets merged - ([9fcd26e](https://github.com/abougouffa/minemacs/commit/9fcd26eb436de392f36d3b9a9a254321c821a7ef)) - [@abougouffa](https://github.com/abougouffa)
- **(mlscroll)** make sure `mlscroll` is loaded on `+mlscroll-right-mode` - ([3e5c17e](https://github.com/abougouffa/minemacs/commit/3e5c17efee7f76f39ae071888ffcc1872972fec7)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** correct the `+clang-format-get-style` for classic C/C++ modes - ([1acc7fd](https://github.com/abougouffa/minemacs/commit/1acc7fd0b6f8e0745766bc84141e935c0a7fb8cd)) - [@abougouffa](https://github.com/abougouffa)
- **(restclient)** correct the recipe - ([3ca7982](https://github.com/abougouffa/minemacs/commit/3ca7982a15839426bb53206bbbd209184e493f2d)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** correct interactive `+treesit-create-parser-in-buffer` - ([529b54a](https://github.com/abougouffa/minemacs/commit/529b54a48a3ff338ff3807a95d179e102ab228c4)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(treesit-auto)** update some comments - ([1b3f45f](https://github.com/abougouffa/minemacs/commit/1b3f45f4f44d485b0c9faff114a04c61b72f3346)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(keybindings)** remove unused `hydra` stuff - ([1bb0e7b](https://github.com/abougouffa/minemacs/commit/1bb0e7bce94cca7db2e7783e26f5203345290f45)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** add support for `denote-menu` - ([c379699](https://github.com/abougouffa/minemacs/commit/c37969962e05868e6ee1e2fb5de74a6cc905c397)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `restclient` obsolete (use `hurl-mode` or `verb`) - ([e4945df](https://github.com/abougouffa/minemacs/commit/e4945dfc1317e4c088682c3d3e73ba8e01743c6b)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `mlscroll` with tweaked mode `+mlscroll-right-mode` - ([50eb555](https://github.com/abougouffa/minemacs/commit/50eb5556448c87d77b779fad94f8e2ad7eb17232)) - [@abougouffa](https://github.com/abougouffa)
- make `emacs-gdb` obsolete - ([b8a731c](https://github.com/abougouffa/minemacs/commit/b8a731c4c81b131b2ce83f582286b17cc50272c8)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(prog)** minor edit in `+clang-format-get-style` - ([6eb9723](https://github.com/abougouffa/minemacs/commit/6eb9723bc973316562104e9c4b4b62674f8f1dda)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([6445517](https://github.com/abougouffa/minemacs/commit/6445517699e43771665a3e4d3a8600e7469f54d0)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- tweak conditional installation for `obsolete/me-evil` - ([147bb70](https://github.com/abougouffa/minemacs/commit/147bb7001d01ee72bbc3790ac81625daf5154472)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(treesit-auto)** the grammar of C++ seems Ok in Emacs 30 - ([714f4e1](https://github.com/abougouffa/minemacs/commit/714f4e11bbcbd6f19c95bca85c2c47680909f453)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** remove `+mlscroll-right-mode` - ([4224ae7](https://github.com/abougouffa/minemacs/commit/4224ae7f4f2c362cfb19abc0633a45d5bce058ed)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(app-launcher)** bind `C-c o a` to `app-launcher-run-app` - ([5fb4473](https://github.com/abougouffa/minemacs/commit/5fb44739972428786898302c461cbab4e5e0bafa)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** bigger fonts for variable pitch - ([b63c00c](https://github.com/abougouffa/minemacs/commit/b63c00c7fe497abd11793f3fd8e1086364c7bda2)) - [@abougouffa](https://github.com/abougouffa)
- **(gdb-mi)** better defaults for Emacs 30 - ([f836d5d](https://github.com/abougouffa/minemacs/commit/f836d5dd9e69425a2a743f4294f085eb7a7eeac3)) - [@abougouffa](https://github.com/abougouffa)
- **(gdb-mi)** remove old unnecessary tweaks - ([b09bb6c](https://github.com/abougouffa/minemacs/commit/b09bb6c6ea6be40cc8d66aab592ce7feff9a3f81)) - [@abougouffa](https://github.com/abougouffa)
- **(grep)** use Emacs 30 rg-like headings view - ([790d612](https://github.com/abougouffa/minemacs/commit/790d612116d0bfd953d62542dcdfc07a5d1bc96b)) - [@abougouffa](https://github.com/abougouffa)
- **(gud)** Emacs 30 supports highlighting the current line - ([2e9c6b9](https://github.com/abougouffa/minemacs/commit/2e9c6b9fc64a288ad86b9a2793f998eb4ea7d93a)) - [@abougouffa](https://github.com/abougouffa)
- **(hurl-mode)** use for the `*.hurl` extension - ([633e5a5](https://github.com/abougouffa/minemacs/commit/633e5a5017a696073ec3163e9ce5c22cb529cfc9)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** warn on Emacs v28 - ([be9415b](https://github.com/abougouffa/minemacs/commit/be9415b7b92c02722918a00640e26687a543f5c3)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** mark some commands to run for all cursors - ([69165dd](https://github.com/abougouffa/minemacs/commit/69165dde2f464fe536c6a00b92126aec9c7f7320)) - [@abougouffa](https://github.com/abougouffa)
- **(org-jira)** set the default working directory - ([ef5741b](https://github.com/abougouffa/minemacs/commit/ef5741be2b687f11b4a90367d3e2e3377ce55b9d)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** set background for the alternate face also - ([61496e3](https://github.com/abougouffa/minemacs/commit/61496e33ac81b9d401b67f5d1aad5646ee1d9d9d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([737d9ab](https://github.com/abougouffa/minemacs/commit/737d9ab4e7745901efff8459a8d2f4bffe8e6ec2)) - [@abougouffa](https://github.com/abougouffa)
- remove `gcmh` and simplify GC management - ([a6e6082](https://github.com/abougouffa/minemacs/commit/a6e6082c36c3262666f71973f0483593f072a642)) - [@abougouffa](https://github.com/abougouffa)
- rebuild only when necessary in `minemacs-restore-locked-packages` - ([51ab896](https://github.com/abougouffa/minemacs/commit/51ab896bd991e9ac07897d3d13f69f3f3076640f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.7.0](https://github.com/abougouffa/minemacs/compare/18cece308ba6246b8fb632d95fd6df6bc5267896..v8.7.0) - 2024-07-31
#### Bug Fixes
- **(jiralib)** use the right host name argument in auto-login - ([ff92e7f](https://github.com/abougouffa/minemacs/commit/ff92e7f92761e92f01480ca44d8d45fe3e835939)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** require `color` before using `color-*-name` - ([33c971a](https://github.com/abougouffa/minemacs/commit/33c971ad60fe67ce716f97797cc1a5cae781e409)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(bitbake)** add some helper commands - ([a2d6892](https://github.com/abougouffa/minemacs/commit/a2d6892fbc30b9f6d879a723d32736f9e404cf90)) - [@abougouffa](https://github.com/abougouffa)
- **(completion)** remove `consult-web` - ([b1481f6](https://github.com/abougouffa/minemacs/commit/b1481f623695d6795b92f4f103bb6b9425363e7c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add a `spacious-padding` like subtle mode-line look - ([a187ece](https://github.com/abougouffa/minemacs/commit/a187ece7ff5085607ae798295a8871c2c2305d02)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `format-all-the-code` - ([f8b548d](https://github.com/abougouffa/minemacs/commit/f8b548d008f05640c2fa43ffc00fd1b397ca6499)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `loccur` obsolete, `consult-focus-lines` does the job! - ([e62767e](https://github.com/abougouffa/minemacs/commit/e62767e2fa3f29cf2796c532ff4c4585d71abd0b)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `visual-format` - ([8f47c7a](https://github.com/abougouffa/minemacs/commit/8f47c7a9ed7531cdf99ba395af49df3a08bf3b5f)) - [@abougouffa](https://github.com/abougouffa)
- visual format buffer (early WIP) - ([c7483ab](https://github.com/abougouffa/minemacs/commit/c7483ab831a348c65f7946593a06401f1e30b633)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- mark my temporary forks of packages as forks - ([bfd3422](https://github.com/abougouffa/minemacs/commit/bfd3422199436feb45a8e1972e0e1bea661ef5fa)) - [@abougouffa](https://github.com/abougouffa)
- prefer using the list form for `:commands` - ([c677dfc](https://github.com/abougouffa/minemacs/commit/c677dfc5748c268bc4159fe41ff7c348bc391c87)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** add the `-style` option for the `clang-format` command - ([725ae78](https://github.com/abougouffa/minemacs/commit/725ae78e1f17715722b8d68f924a748f02ecf2b9)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** better handling of the `-style` argument - ([36f0cfe](https://github.com/abougouffa/minemacs/commit/36f0cfea4ce9a3e87365dfff02e9546d6cf0f3bb)) - [@abougouffa](https://github.com/abougouffa)
- **(combobulate)** don't enable by default (very annoying in Python) - ([18cece3](https://github.com/abougouffa/minemacs/commit/18cece308ba6246b8fb632d95fd6df6bc5267896)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** customize more commands with initial input - ([868842b](https://github.com/abougouffa/minemacs/commit/868842b49a082e0744d62e765e13f13ac4626bea)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** don't load until needed - ([fb4cd11](https://github.com/abougouffa/minemacs/commit/fb4cd1175243e3108a3fa8d0e6e2becd2f1f32e7)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** setup some hooks for better integration - ([52f6a84](https://github.com/abougouffa/minemacs/commit/52f6a844ab070573f3f80d4a804db9300ebdf18e)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** simplify and restore the vertical bar - ([e66b37c](https://github.com/abougouffa/minemacs/commit/e66b37ccc477ff22f7d26d2e89b2376b77555009)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** add a shortcut to browse Org directory in Dired - ([1662fc1](https://github.com/abougouffa/minemacs/commit/1662fc1e9920cc77483e7b13d58e7ed75b7cf465)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-selection)** use my fork until it gets merged - ([a3567e3](https://github.com/abougouffa/minemacs/commit/a3567e3ed3ed7e073feb8c2eb9e68455fd95b919)) - [@abougouffa](https://github.com/abougouffa)
- **(opencl-c-mode)** use the correct package/mode name - ([fc3e53f](https://github.com/abougouffa/minemacs/commit/fc3e53faaa658792402f2ab9f1b1de2c551410c9)) - [@abougouffa](https://github.com/abougouffa)
- **(reformatter)** define some formatters - ([6dfecd4](https://github.com/abougouffa/minemacs/commit/6dfecd41df93f4c2549c62e49df6987556d0aeff)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** rename `visual-format` the `virtual-format` - ([4beb68d](https://github.com/abougouffa/minemacs/commit/4beb68d25b039a14f547b5fa366f71ec905e9d13)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([fee238e](https://github.com/abougouffa/minemacs/commit/fee238e69815c62817c0a20cc44e4f64c23cf031)) - [@abougouffa](https://github.com/abougouffa)
- apply a lighter/darker color for trailing whitespace - ([7892c51](https://github.com/abougouffa/minemacs/commit/7892c513481ff38d491ee5ec70fdc2965b89e418)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6e1c330](https://github.com/abougouffa/minemacs/commit/6e1c330d778b05e14ef7cd7125dfbee3da5bd5ff)) - [@abougouffa](https://github.com/abougouffa)
- don't reinvent the wheel, `color-lighten-name` is a thing! - ([fce9e4e](https://github.com/abougouffa/minemacs/commit/fce9e4ee28ec590862349a9c2de3c723802ae5d7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.6.4](https://github.com/abougouffa/minemacs/compare/1b82605345a3098a1b690405897e73f0dc16d986..v8.6.4) - 2024-07-22
#### Bug Fixes
- **(flymake)** properly defer fringe tweaks - ([1b82605](https://github.com/abougouffa/minemacs/commit/1b82605345a3098a1b690405897e73f0dc16d986)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** fix buggy case on Emacs 30 - ([5a98de3](https://github.com/abougouffa/minemacs/commit/5a98de3b920e449cfde860d56a494f3eb7180dfc)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** server crashing silently due to face setting - ([d7ba61f](https://github.com/abougouffa/minemacs/commit/d7ba61fd88a21e1f7fb5e94b23f87b34be24a30a)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** dynamically set the face color - ([026eee0](https://github.com/abougouffa/minemacs/commit/026eee0a959ef3605436feaca0665bb0a486544d)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** fix the `__elpkg` snippet - ([59e4995](https://github.com/abougouffa/minemacs/commit/59e49958c509e741d511ec287e3da51b403bc827)) - [@abougouffa](https://github.com/abougouffa)
- **(xref-union)** make sure we disable `etags` in `xref-union` - ([fcc092d](https://github.com/abougouffa/minemacs/commit/fcc092dcb50b85d40063e3fa3641f2c599a171d9)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+color-brighter-or-darker` - ([2a5b320](https://github.com/abougouffa/minemacs/commit/2a5b32096f691800acc975f2bf9763793690277f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+set-indent-width` - ([87f7fc0](https://github.com/abougouffa/minemacs/commit/87f7fc0de899100fc2edccca48f5fdec07b45f92)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** remove `license-snippets` (outdated and messy) - ([a45a1b4](https://github.com/abougouffa/minemacs/commit/a45a1b4eda6e1044f8cecbeb0c4a4f5c70b98018)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `casual-bookmarks` - ([34e38ea](https://github.com/abougouffa/minemacs/commit/34e38ea49081b88f84bb2852bb467e8e1867f84b)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- update logo - ([d756545](https://github.com/abougouffa/minemacs/commit/d7565451ac395491bb30eddb90558d6be31e8c15)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(ecryptfs)** move the `ecryptfs` integration to separate package - ([cd8f0d9](https://github.com/abougouffa/minemacs/commit/cd8f0d90a4ed292c00958e9f8e5bf0e5114298fe)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** make use of `+color-brighter-or-darker` - ([33eca80](https://github.com/abougouffa/minemacs/commit/33eca802f08c513f80bcd49f2f179a640929407f)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(dumb-jump)** no need to add `dumb-jump` for `xref-union` to work - ([7fc842d](https://github.com/abougouffa/minemacs/commit/7fc842d751b595a8d404ac3295d12813ff18f26b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** respect the percentage in `+color-brighter-or-darker` - ([b36a71b](https://github.com/abougouffa/minemacs/commit/b36a71baa67cf6b06c88e43f4b3b2bbf863d53ae)) - [@abougouffa](https://github.com/abougouffa)
- **(dtrt-indent)** better defaults and triggering predicate - ([cf12878](https://github.com/abougouffa/minemacs/commit/cf12878b9f6935c76f16a0f130c6ce60f53a2b78)) - [@abougouffa](https://github.com/abougouffa)
- **(ecryptfs)** add bindings for `ecryptfs-toggle-mount-private` - ([b0119a9](https://github.com/abougouffa/minemacs/commit/b0119a9cc3183c24b5261e967436938c06558035)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** add a dedicated mode for `guard-lf` files - ([2cfdabc](https://github.com/abougouffa/minemacs/commit/2cfdabcd13390406f00affb957a7349d8bc8dac1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([14f2c33](https://github.com/abougouffa/minemacs/commit/14f2c33e5180030443c12b6b5284a215298eaca9)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([bd207b3](https://github.com/abougouffa/minemacs/commit/bd207b3ce2dde812a3103d272358a653bdf3cc45)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.6.3](https://github.com/abougouffa/minemacs/compare/a6f7ec61e90e2f1224410c533420717cd45ab72b..v8.6.3) - 2024-07-21
#### Features
- **(editor)** restore `spdx` - ([a6f7ec6](https://github.com/abougouffa/minemacs/commit/a6f7ec61e90e2f1224410c533420717cd45ab72b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(autorevert)** auto revert files immediately - ([4933b0c](https://github.com/abougouffa/minemacs/commit/4933b0c0a8cfb030cc4c7652a7a590d8e515ffdc)) - [@abougouffa](https://github.com/abougouffa)
- **(csv-mode)** add the vertical bar `|` to the separators list - ([82734d5](https://github.com/abougouffa/minemacs/commit/82734d52b266f1f86d8b51e742f63a236e2367c4)) - [@abougouffa](https://github.com/abougouffa)
- **(neotree)** switch to upstream (`nerd-icons` integration merged) - ([0bc58f8](https://github.com/abougouffa/minemacs/commit/0bc58f85be67ea84cd5c34183f6c206fac3d917c)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** remove `unique-dir-name`, it is currently integrated - ([0f7758c](https://github.com/abougouffa/minemacs/commit/0f7758ce72dfba9a67f55c7e3fdff6661eb30abe)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6f8e5af](https://github.com/abougouffa/minemacs/commit/6f8e5af3038207f02370f698005da3b68b3b0842)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.6.2](https://github.com/abougouffa/minemacs/compare/a0303d9c63bbcf6ab7e5c536ecd598ef737b8d44..v8.6.2) - 2024-07-20
#### Features
- **(goto-last-change)** rewrite `goto-last-change` based on the original - ([1d9444c](https://github.com/abougouffa/minemacs/commit/1d9444c2f060df91108bc41db6f007f9cb0292d5)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(parinfer-rust)** move the hooks to `:hook` - ([a0303d9](https://github.com/abougouffa/minemacs/commit/a0303d9c63bbcf6ab7e5c536ecd598ef737b8d44)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(goto-last-change)** bind `goto-last-change` to `M-é` - ([c8db33a](https://github.com/abougouffa/minemacs/commit/c8db33a901c367c629e9ed2565606258e2455a92)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-file-icons)** disable temporary, seems broken - ([7ad7363](https://github.com/abougouffa/minemacs/commit/7ad73634caa179e696b029358847ec037deb8a3e)) - [@abougouffa](https://github.com/abougouffa)
- **(rainbow-csv)** switch back to upstream - ([81bcca4](https://github.com/abougouffa/minemacs/commit/81bcca4c7b3f0112c9525a1bfbfae796f27989c1)) - [@abougouffa](https://github.com/abougouffa)
- **(rainbow-csv)** switch to my fork until it gets merged - ([b868881](https://github.com/abougouffa/minemacs/commit/b86888187f55ef6fdb24c23e0fd09dd80125f64d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e78427e](https://github.com/abougouffa/minemacs/commit/e78427ec49998f14126bb4264c8222ecfd7cafa3)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.6.1](https://github.com/abougouffa/minemacs/compare/5cf31589065ef0a66f2bac58319aa771c45cd6d2..v8.6.1) - 2024-07-20
#### Bug Fixes
- **(org-msg)** use a different fork that fixes `mu` 1.12 issues - ([ed852e8](https://github.com/abougouffa/minemacs/commit/ed852e879c233ec09017b1027a4f51ddeb443cc8)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** try to fix the mode being not taken into account - ([75f9805](https://github.com/abougouffa/minemacs/commit/75f9805f0b46522edb148b8df96fba23d982fa53)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(workspaces)** remove unused `burly` and `bufler` - ([ca36e9b](https://github.com/abougouffa/minemacs/commit/ca36e9bd9095cb989b3da24dce2552e640414f8b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cognitive-complexity)** update the repo link - ([39e32bf](https://github.com/abougouffa/minemacs/commit/39e32bfe52cb4bb9662f960a89fe86c9b201afa0)) - [@abougouffa](https://github.com/abougouffa)
- **(dumb-jump)** make sure it is locally added to the xref backends - ([5bf2777](https://github.com/abougouffa/minemacs/commit/5bf2777eeee8020f102ce9b3885c912ae36907f2)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** add keybindings to skip cursors - ([5cf3158](https://github.com/abougouffa/minemacs/commit/5cf31589065ef0a66f2bac58319aa771c45cd6d2)) - [@abougouffa](https://github.com/abougouffa)
- **(org-msg)** replace `+org-msg-make-signature` with `+org-msg-signature` - ([1a30a35](https://github.com/abougouffa/minemacs/commit/1a30a351fedc8a25dee02911b269bfb74c8d2df2)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** display info messages on disable/enable - ([e3152fb](https://github.com/abougouffa/minemacs/commit/e3152fbbae2535f0348863a888bf1d0dfb2040e2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f9222c9](https://github.com/abougouffa/minemacs/commit/f9222c91145788a31156c511fbf47fac331875e7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.6.0](https://github.com/abougouffa/minemacs/compare/0c697be6deb27c0c6bf20e7e5948a2a126918d96..v8.6.0) - 2024-07-19
#### Bug Fixes
- **(citre)** don't signal an error when `find-references` fails - ([17438cf](https://github.com/abougouffa/minemacs/commit/17438cfe70bedec8e9c3e0751e981eec43cf4016)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more generic args propagation - ([94364c8](https://github.com/abougouffa/minemacs/commit/94364c85b276809ccbbf1e376a4a7349350a3181)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** correctly propagate args in `+super-project-define-commands` - ([2299224](https://github.com/abougouffa/minemacs/commit/2299224cd9b25f56380671deceab3122e3d92dc4)) - [@abougouffa](https://github.com/abougouffa)
- **(empv)** change invidious instance - ([3a6ae2d](https://github.com/abougouffa/minemacs/commit/3a6ae2d142691738cd65d36d62c28ef389154001)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-completion)** restore the icons on the new `marginalia` - ([6e05d4a](https://github.com/abougouffa/minemacs/commit/6e05d4a6c8b87a73e935470a5ab33e1cac1119cb)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** load the consult tweaks in the right order - ([18a7315](https://github.com/abougouffa/minemacs/commit/18a73155ce5fa23a456aa0c131e3f03ad693e50e)) - [@abougouffa](https://github.com/abougouffa)
- **(systemd)** prefer the builtin capf backend over the `company` one - ([4012f63](https://github.com/abougouffa/minemacs/commit/4012f63b8b2c603e065946fb4da5c46f3f378d76)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** fix the tramp auto-save directory - ([21bce01](https://github.com/abougouffa/minemacs/commit/21bce01db1aff5ad6b3ba74e5aab4b1d79802368)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+save-buffer-preserving-modtime` - ([8649350](https://github.com/abougouffa/minemacs/commit/8649350ef792d6f54a9114acf1d0d2ffd75729e8)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+setq-advice!` - ([32ed6e2](https://github.com/abougouffa/minemacs/commit/32ed6e28e633a97a96aecd5818f856ae579c07ee)) - [@abougouffa](https://github.com/abougouffa)
- **(data)** add support for GraphQL via `graphql-mode` - ([9232fed](https://github.com/abougouffa/minemacs/commit/9232fedc90da54599591b0b0f15e93ee0e62d117)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add initial support for `symbol-overlay` - ([3a3deea](https://github.com/abougouffa/minemacs/commit/3a3deea1bc0408ce01b8b1405e3596a4f8fc52ea)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** add support for `package-lint` - ([828715e](https://github.com/abougouffa/minemacs/commit/828715e2c24338077935d5a6fc23c9f6e9f21f89)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** add initial support for `inspector` - ([252abd3](https://github.com/abougouffa/minemacs/commit/252abd3f39436c3621e1079a077147ebd2b741f2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `cognitive-complexity` - ([e091d79](https://github.com/abougouffa/minemacs/commit/e091d79a94dab62618551d4459be083aa1fae0ff)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `emacs-everywhere` - ([0c697be](https://github.com/abougouffa/minemacs/commit/0c697be6deb27c0c6bf20e7e5948a2a126918d96)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `casual-re-builder` - ([b672beb](https://github.com/abougouffa/minemacs/commit/b672beb954586bd3673961a0317601ac906cfa49)) - [@abougouffa](https://github.com/abougouffa)
- add initial support for `goto-last-change` - ([b13a223](https://github.com/abougouffa/minemacs/commit/b13a223c19c52d55b3424757458ff7fdb1105a3b)) - [@abougouffa](https://github.com/abougouffa)
- add initial support for `inotify-revert` - ([739cce8](https://github.com/abougouffa/minemacs/commit/739cce8261884bc2ee6ed13647aa6d351d69f32b)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- correct a typo - ([4ee7712](https://github.com/abougouffa/minemacs/commit/4ee7712027e5879d18e58a03d29808104a312a8e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** remove unnecessary `+move-this-file` - ([7bed09c](https://github.com/abougouffa/minemacs/commit/7bed09c9d3b2c4ccce4ac96dbbd0caf0dce7fd09)) - [@abougouffa](https://github.com/abougouffa)
- **(empv)** cleanup and refactor - ([7dad83d](https://github.com/abougouffa/minemacs/commit/7dad83da45b07216b841923cc2b659360ef0e592)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- remove `inotify-revert`, no gain over `auto-revert` - ([db7ad96](https://github.com/abougouffa/minemacs/commit/db7ad9678f91d8afbf73be598dc10ca6bd9f777c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(casual)** use default MELPA recipes - ([50efb6c](https://github.com/abougouffa/minemacs/commit/50efb6c4bfc5f7cf63beb6825f1733652e3a846f)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** add initial input for `consult-fd` - ([9933fcc](https://github.com/abougouffa/minemacs/commit/9933fcc4fb482aa901cfe0ccaed8cb8c609cc707)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** bind `+consult-insert-thing-at-point` to `M-i` - ([dc7033d](https://github.com/abougouffa/minemacs/commit/dc7033d85377562203f128700b0184f68b84bf23)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-denote)** prefer `ripgrep` and `fd` - ([0642ba1](https://github.com/abougouffa/minemacs/commit/0642ba1cafdc28615cd5f9b82dd92ced05e9130f)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** bind `corfu-send` to `RET` - ([58e81d5](https://github.com/abougouffa/minemacs/commit/58e81d5a8b784d60c9c94d220128c85d61fc7e51)) - [@abougouffa](https://github.com/abougouffa)
- **(csv-mode)** correctly guess the separator character - ([1e69dae](https://github.com/abougouffa/minemacs/commit/1e69dae7e5d786b24cfbfcb9ea55b30599064e75)) - [@abougouffa](https://github.com/abougouffa)
- **(isearch)** use regexp based search by default - ([a8a7b9a](https://github.com/abougouffa/minemacs/commit/a8a7b9a03c8835c94539807c9435c2e1752bf171)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib)** consider `markdown-ts-mode` in `+jira-insert-ticket-link` - ([8a1dad8](https://github.com/abougouffa/minemacs/commit/8a1dad88d1a76dcdba4ab971af78d88c0d977d03)) - [@abougouffa](https://github.com/abougouffa)
- **(markdown)** use `markdown-mode` unless the file is big - ([778d17b](https://github.com/abougouffa/minemacs/commit/778d17be1de4cc088a7bfda07baad823c9b40bfc)) - [@abougouffa](https://github.com/abougouffa)
- **(media)** minor changes in MPV `browse-url` integration - ([4f6850e](https://github.com/abougouffa/minemacs/commit/4f6850e82de017d23d79e136dd99ad1c1d8f1253)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** integrate with `symbol-overlay` - ([59e7b0f](https://github.com/abougouffa/minemacs/commit/59e7b0fc9a32e484c2d441afa3865fb5abaf6c9b)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete/blamer)** decrease font size for the blame - ([47cd5d1](https://github.com/abougouffa/minemacs/commit/47cd5d19ab97f19e478cf38e617d46cc35ece4eb)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** update the configuration for v2.0.0 - ([08ac3a8](https://github.com/abougouffa/minemacs/commit/08ac3a844a192778a3a5d6a56f155b65ec8bb0cf)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** add some keybindings - ([92e1581](https://github.com/abougouffa/minemacs/commit/92e15812de9fe338b00899d6e170b3fadbc3c791)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** bump package version - ([b008d2a](https://github.com/abougouffa/minemacs/commit/b008d2a01d64cabc74510db97a12463c2552e6ac)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** add initial input for `consult` commands - ([caeb001](https://github.com/abougouffa/minemacs/commit/caeb001cccb415157c3334c54f1bc91579a0f93d)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** enable `otpp-remap-commands-mode` - ([1ea123b](https://github.com/abougouffa/minemacs/commit/1ea123bfe4fc91b2c08d91bf68005efaef491682)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** better implementation of `project-name` - ([c045cb8](https://github.com/abougouffa/minemacs/commit/c045cb8c3364d8b4ced5c302bfdf7994ae85b839)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add some functions/commands for compatibility with `projectile` - ([69e9460](https://github.com/abougouffa/minemacs/commit/69e94603c49631815c7cdb06d93031e3934b986e)) - [@abougouffa](https://github.com/abougouffa)
- **(rainbow-csv)** make use of `+setq-advice!` - ([b2ac604](https://github.com/abougouffa/minemacs/commit/b2ac604ee9f28debd66e9d2e734a873eb4e6c580)) - [@abougouffa](https://github.com/abougouffa)
- **(rainbow-csv)** automatically guess the separator - ([39ba664](https://github.com/abougouffa/minemacs/commit/39ba664ebab1b09f91ab82d3a13b6d79a0f0c4c9)) - [@abougouffa](https://github.com/abougouffa)
- **(ssh-deploy)** change the revisions directory - ([2041506](https://github.com/abougouffa/minemacs/commit/20415069c4265215479e7e9e8a7068c1fe2bf430)) - [@abougouffa](https://github.com/abougouffa)
- **(systemd)** switch to my fork - ([9a3e668](https://github.com/abougouffa/minemacs/commit/9a3e6686e9f14f63b136c7b20fca780f60960b08)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** add group to a custom variable - ([179b321](https://github.com/abougouffa/minemacs/commit/179b321915211ac21dcd02777e0cbf335f9473ca)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** restore Elisp grammar - ([618f93c](https://github.com/abougouffa/minemacs/commit/618f93c584b57dbca4cf47717f891795e554d594)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3f813b8](https://github.com/abougouffa/minemacs/commit/3f813b8b4f402c4adfd0443c2eda917260e14501)) - [@abougouffa](https://github.com/abougouffa)
- move & rename `+insert-thing-at-point`, integrate with `isearch` - ([2096426](https://github.com/abougouffa/minemacs/commit/20964266093471e9feb46b6d7c901d94551da478)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([ff998b7](https://github.com/abougouffa/minemacs/commit/ff998b788f6da5993fc4a1243d0990fec120097b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7577005](https://github.com/abougouffa/minemacs/commit/75770053536c8a14ea316fcaa6d3ee3b60decb24)) - [@abougouffa](https://github.com/abougouffa)
- bump `one-tab-per-project` version - ([e689ff0](https://github.com/abougouffa/minemacs/commit/e689ff0d467c92d57a0471be4ffe2cc8fe0c1451)) - [@abougouffa](https://github.com/abougouffa)
- bump `one-tab-per-project` version - ([d31fe73](https://github.com/abougouffa/minemacs/commit/d31fe7313f17cb07949e8b24fd21535efc176aa2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0090e94](https://github.com/abougouffa/minemacs/commit/0090e943892acc089aeaa2b7e8347184e50715d2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.5.0](https://github.com/abougouffa/minemacs/compare/5ca0374eda9e75fe0ed1f8c743ca178c34c7a882..v8.5.0) - 2024-07-10
#### Documentation
- update external tools - ([b966126](https://github.com/abougouffa/minemacs/commit/b96612627e0be07db072eacae8a86b5b314dd096)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(media)** initial support for `ready-player` - ([334b4c8](https://github.com/abougouffa/minemacs/commit/334b4c8e229d6a6418f7202ab8681a3848fbc671)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** restore `lexic` integration - ([6ea0e69](https://github.com/abougouffa/minemacs/commit/6ea0e69433b1d06f6dcafe7b6fe8a6ecd8f8d21b)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `casual-ibuffer` - ([ec6a345](https://github.com/abougouffa/minemacs/commit/ec6a3454647237e3e8310a96c5da8b27aa15f9f8)) - [@abougouffa](https://github.com/abougouffa)
- **(workspace)** replace `project-tab-groups` with `one-tab-per-project` - ([cc3a31a](https://github.com/abougouffa/minemacs/commit/cc3a31aff38f9d425a388b6e7d739f3f1657e3a3)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** rebuild pages when the related CI configuration changes - ([1edf96b](https://github.com/abougouffa/minemacs/commit/1edf96ba0c9b947e99f5fbdb10f8d71f4a3b2426)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** change branch and update pages actions - ([bc762cc](https://github.com/abougouffa/minemacs/commit/bc762cc5d49815d191116d4f54f0d21d3b24ef17)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** remove testing on Emacs 28.2, add timeout - ([5ca0374](https://github.com/abougouffa/minemacs/commit/5ca0374eda9e75fe0ed1f8c743ca178c34c7a882)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(media)** minor edits - ([7f36f98](https://github.com/abougouffa/minemacs/commit/7f36f981b45298d906694c520ef99e87d5fc2e04)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** better code - ([faca25c](https://github.com/abougouffa/minemacs/commit/faca25ca60fe08de94f73c47a8cbc6c7fbe5a984)) - [@abougouffa](https://github.com/abougouffa)
- extract unique naming utilities to an external package - ([cfb649c](https://github.com/abougouffa/minemacs/commit/cfb649c2b4ff0a69421d9653d55fc8a53aa8ebb3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bookmark)** truncate long lines when creating bookmark title - ([b4e7eb1](https://github.com/abougouffa/minemacs/commit/b4e7eb187aba0ba605090285d34c4f2f63d2ef29)) - [@abougouffa](https://github.com/abougouffa)
- **(browse-url)** don't overwrite the default browser - ([64d7b6b](https://github.com/abougouffa/minemacs/commit/64d7b6b5b24a19c0bc7df04184bcadb17977db87)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** enable `narrow-to-page` and `narrow-to-region` - ([bc01d74](https://github.com/abougouffa/minemacs/commit/bc01d744b14db87841c8f2db5e2c10d58a45b41a)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** drop support for Emacs 28 - ([f1a79dc](https://github.com/abougouffa/minemacs/commit/f1a79dcad6ab721e274000927cf0cf529a12dddf)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** update and correct the transient list - ([9f99d05](https://github.com/abougouffa/minemacs/commit/9f99d053910d52863de157923327f02aa6b2aeb9)) - [@abougouffa](https://github.com/abougouffa)
- **(journalctl-mode)** autoload `journalctl-mode` to be used with saved logs - ([5479a44](https://github.com/abougouffa/minemacs/commit/5479a4495ae5abeb1a10538f583781625f9176a6)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete/eaf)** enable more apps - ([e77ebeb](https://github.com/abougouffa/minemacs/commit/e77ebeb430abc040dc76f5dc17fa478275426b7d)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** better sentinel messages for org async export - ([f881b24](https://github.com/abougouffa/minemacs/commit/f881b2430f43e53b3f5f30e8ea59738b55f14ffc)) - [@abougouffa](https://github.com/abougouffa)
- **(project-tab-groups)** unregister unique name and rename tabs on closing - ([b8fc926](https://github.com/abougouffa/minemacs/commit/b8fc9269426ff45eb0c1090ed68a47be768435fc)) - [@abougouffa](https://github.com/abougouffa)
- **(smerge)** extract the custom `+smerge-vc-next-conflict-recenter` - ([31ef19a](https://github.com/abougouffa/minemacs/commit/31ef19ac660fb72db633e7188a64f44226007425)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** rename the first tab to `*default*` - ([0e9d695](https://github.com/abougouffa/minemacs/commit/0e9d6955e42431f1de40d359181f321f87d16094)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([fa340c7](https://github.com/abougouffa/minemacs/commit/fa340c74d68ce78b52baf018de88bd429b86bdfe)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.4.0](https://github.com/abougouffa/minemacs/compare/7fa7e17f67fbe3429f70e6e42ac1530fd149b016..v8.4.0) - 2024-07-07
#### Bug Fixes
- **(nerd-icons-archive)** fix a typo - ([1d356e4](https://github.com/abougouffa/minemacs/commit/1d356e40dc3c5aab47929e08a6dcd18b4b075d2a)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(use-package)** better comments about the extra MinEmacs features - ([c953329](https://github.com/abougouffa/minemacs/commit/c95332975542156284b0f67a6632b14ada8a44fb)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add some utilities for unique naming based on a directory - ([24dae81](https://github.com/abougouffa/minemacs/commit/24dae81faf76b91d07e87265972f03f145b8989a)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `nerd-icons-archive` - ([0bec9f3](https://github.com/abougouffa/minemacs/commit/0bec9f3f5f25a0d3df666899d386b95d9ec9ee29)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** cleanup - ([65f82d3](https://github.com/abougouffa/minemacs/commit/65f82d307bb31292e24d13e78e3eda34da05fa3d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(guard-lf)** update the intact major modes - ([7fa7e17](https://github.com/abougouffa/minemacs/commit/7fa7e17f67fbe3429f70e6e42ac1530fd149b016)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-archive)** update config to treat it as global mode - ([65e88a3](https://github.com/abougouffa/minemacs/commit/65e88a31dd88a21df9518b5d02baaeb42c87f481)) - [@abougouffa](https://github.com/abougouffa)
- **(project-tab-groups)** better naming of tabs and tab-groups - ([9a15282](https://github.com/abougouffa/minemacs/commit/9a15282ca9baa9c9e86f01f0eb10f78ddc7df171)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** show the tab bar by default - ([5d803da](https://github.com/abougouffa/minemacs/commit/5d803da9499840521fca6f70e12dcc1439d79997)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** better code generation for `::trigger-commands` - ([2aad9e7](https://github.com/abougouffa/minemacs/commit/2aad9e788e8fd3b701e4fdd7a493d1f720c8f571)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([98f3100](https://github.com/abougouffa/minemacs/commit/98f3100a2ada14e41f1228d8d23cdb80ab76d86a)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([5996c22](https://github.com/abougouffa/minemacs/commit/5996c22bbcc0f6c246d492762ff716b7ea900ba1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cc00643](https://github.com/abougouffa/minemacs/commit/cc00643911fad5e70c1a394e215493c5a3afc5c9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.3.0](https://github.com/abougouffa/minemacs/compare/1fd507f1cf1481ba73144a1ec2175d869c906875..v8.3.0) - 2024-07-04
#### Bug Fixes
- **(consult-notes)** replace the obsolete `denote` function - ([25c7169](https://github.com/abougouffa/minemacs/commit/25c7169ce30915f25fa0a0f300b610ef7bdac4d5)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fix `+super-project-define-commands` - ([dcba2a1](https://github.com/abougouffa/minemacs/commit/dcba2a1b2bb74ad0d8209d74b775cee9ffc65dfd)) - [@abougouffa](https://github.com/abougouffa)
- buggy implementation of running hooks in `minemacs-load-module` - ([85c924d](https://github.com/abougouffa/minemacs/commit/85c924dad759bde9ed5af2ac0bf48a8beada3eaf)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([6aa3298](https://github.com/abougouffa/minemacs/commit/6aa3298333d72acd605c5fd60c54d8ca4be515a8)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(completeion)** initial support for `consult-web` - ([5a94c6b](https://github.com/abougouffa/minemacs/commit/5a94c6b6b653152f3469e14e3d170164ebd45601)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add a helper to define command that act on super-projects - ([9ffa3ec](https://github.com/abougouffa/minemacs/commit/9ffa3ec2297463595b3227e4e03f4aed0b58d83c)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** add `+fzf-super-project` - ([4cf7fbb](https://github.com/abougouffa/minemacs/commit/4cf7fbb0017efc26a4f49288bcdc4fe4fd8b5a6e)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** add support for `consult-denote` (`consult-notes` replacement) - ([adcc1f6](https://github.com/abougouffa/minemacs/commit/adcc1f6b806292890839397e014f9846c3dc3efe)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** make `consult-notes` obsolete - ([e4c0c90](https://github.com/abougouffa/minemacs/commit/e4c0c9043177df015e70982214b5cda9d731ff20)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** restore back the `affe` support - ([5cc1f79](https://github.com/abougouffa/minemacs/commit/5cc1f7937c7ea28226e49a117fac395158755d02)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** enable test on `release-snapshot` (Emacs 30) - ([3ff8331](https://github.com/abougouffa/minemacs/commit/3ff8331e7374d0106fdf693f701beffee7a9b823)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** disable the always demand mode - ([b46b7b7](https://github.com/abougouffa/minemacs/commit/b46b7b7197d0e0368f7d07f533b8aa638d86cb10)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(citre)** simplify finding the project's root marker - ([6c685f1](https://github.com/abougouffa/minemacs/commit/6c685f1123e3a94d715ba1fb151487f103adba93)) - [@abougouffa](https://github.com/abougouffa)
- rename some commands to follow the MinEmacs conventions - ([1fd507f](https://github.com/abougouffa/minemacs/commit/1fd507f1cf1481ba73144a1ec2175d869c906875)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult)** define some super-project commands variants - ([98ffb80](https://github.com/abougouffa/minemacs/commit/98ffb800e9b0465ac2aeef1fc1256e3eb4438d15)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** prefer `Martian Mono` font when available - ([1ea823a](https://github.com/abougouffa/minemacs/commit/1ea823a2f33136501dfae7d094796664930d5f5d)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-pmd)** better defaults - ([f2d3e8a](https://github.com/abougouffa/minemacs/commit/f2d3e8aef84a74de00c8b41a09844e536b72188d)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** make use of the new `+super-project-define-commands` - ([28f24f2](https://github.com/abougouffa/minemacs/commit/28f24f2e6e83c71fa8841edc7a43cb6c4e5bdb81)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** consistent naming, autoload custom commands - ([3fb207d](https://github.com/abougouffa/minemacs/commit/3fb207d47669d4410ddfd0cb17be942d4d85d76e)) - [@abougouffa](https://github.com/abougouffa)
- **(gaurd-lf)** add more modes to ignore - ([f869bbd](https://github.com/abougouffa/minemacs/commit/f869bbdcacbdd2ea8576cae2b62a57925714f7d0)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** enable `+spellcheck-mode` in `git-commit-mode` - ([ebe6f07](https://github.com/abougouffa/minemacs/commit/ebe6f07e95fd8d0a98d35c98058e4126a7f465e8)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** better handling of Python virtual environments in Eglot - ([cf1bc59](https://github.com/abougouffa/minemacs/commit/cf1bc5966662a16faf0abfc8c22a7173802a7a6f)) - [@abougouffa](https://github.com/abougouffa)
- **(rustic)** don't depend on `flycheck` please! - ([afd8b77](https://github.com/abougouffa/minemacs/commit/afd8b77e45970e7c8a12e945be14dfeee74b6e5a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a6ab417](https://github.com/abougouffa/minemacs/commit/a6ab417c34436a07798f0ffcc9bc159c6f8ee1a3)) - [@abougouffa](https://github.com/abougouffa)
- don't pollute messages with logs in `+fn-inhibit-messages!` - ([0a92474](https://github.com/abougouffa/minemacs/commit/0a924743be983b608e77bfab66bfcf0cd36e4bd2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.2.0](https://github.com/abougouffa/minemacs/compare/82b837a84c87666bf9763dc19ca0fce97555a077..v8.2.0) - 2024-07-01
#### Bug Fixes
- **(corfu)** fix the `ispell` issue on Emacs 30 - ([21ae46d](https://github.com/abougouffa/minemacs/commit/21ae46d95df026f85daec52ef92854b7ba049ea7)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** update the handling of client commands for new Eglot API - ([358d7aa](https://github.com/abougouffa/minemacs/commit/358d7aaf5171c42293c161e1bd62aeb49f81e1d6)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** fix Python LSP registration - ([1b3fb5c](https://github.com/abougouffa/minemacs/commit/1b3fb5c08216e9c8d91765325ad9c85ff4566864)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** correctly enable LTeX-LS in (La)TeX modes - ([018ec1e](https://github.com/abougouffa/minemacs/commit/018ec1ea93d131d964652ff362c25b11e0308298)) - [@abougouffa](https://github.com/abougouffa)
- `+eglot-register` failing on Windows - ([b380cc3](https://github.com/abougouffa/minemacs/commit/b380cc31722d7d66b0ead59f65ba8cbb22dedfbe)) - [@abougouffa](https://github.com/abougouffa)
- treat `+with-delayed!` body as one unit - ([21ceb4c](https://github.com/abougouffa/minemacs/commit/21ceb4cf82688a636a4a1cdf836503cccf6c231a)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(builtin)** add a comment - ([f463751](https://github.com/abougouffa/minemacs/commit/f46375116ba565a3331099d712f2b3889e566e6d)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** regenerate the documentation - ([4eb59cb](https://github.com/abougouffa/minemacs/commit/4eb59cb0798c9af766dc69c56b22381b246f9f31)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(docs)** add `markdown-ts-mode` - ([367a9e2](https://github.com/abougouffa/minemacs/commit/367a9e2715c13d39e73c8d03a7b94b8ae0d6a6c8)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add initial support fot Eglot protocol extensions `eglot-x` - ([871efb3](https://github.com/abougouffa/minemacs/commit/871efb31e7b6cdafee1f9df06e3b0856ecff6c55)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** add support for `loccur` - ([1f752ba](https://github.com/abougouffa/minemacs/commit/1f752ba557f083f04942e9e51c1add69d475bc8e)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- minor edit - ([db13ee9](https://github.com/abougouffa/minemacs/commit/db13ee98a1d2b02ef8c6a34d558691783cd1f320)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- refactor some core utils - ([7ef10da](https://github.com/abougouffa/minemacs/commit/7ef10da2a633865ee2946ab5c89049d637611d06)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([775e1da](https://github.com/abougouffa/minemacs/commit/775e1dac383012dc315984624091b5f9344a22a8)) - [@abougouffa](https://github.com/abougouffa)
- don't set directories unless necessary + minor tweaks - ([2689e13](https://github.com/abougouffa/minemacs/commit/2689e13dc08e11409a253fa9519a5efc8e158c91)) - [@abougouffa](https://github.com/abougouffa)
- move leader key related variables to `obsolete/me-evil` - ([914d4d2](https://github.com/abougouffa/minemacs/commit/914d4d2477a60f8b41dc2cf1b6a364c83b8988d2)) - [@abougouffa](https://github.com/abougouffa)
- move `elisp-demos` to `me-emacs-lisp` - ([21cdb25](https://github.com/abougouffa/minemacs/commit/21cdb252c56ff71c8e61f933fb21af39e35991da)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bookmark)** set bookmark by double clicking the left fringe - ([54ad314](https://github.com/abougouffa/minemacs/commit/54ad3146299b1669419bfa1b57b468a82389b0aa)) - [@abougouffa](https://github.com/abougouffa)
- **(breadcrumb)** integrate with `nerd-icons` - ([1f36a5f](https://github.com/abougouffa/minemacs/commit/1f36a5f854c28f333367912b26cb57524bfa662e)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** force using GNU ELPA mirror for installed packages - ([1b4eb14](https://github.com/abougouffa/minemacs/commit/1b4eb14c2fa778643da130d7badbb4c033112203)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** exclude commands irrelevant to the current mode in `M-x` - ([30a96e8](https://github.com/abougouffa/minemacs/commit/30a96e80f23ed9b438e3a2cca57faaa32e2b9a2e)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** don't auto insert the current candidate - ([103b1da](https://github.com/abougouffa/minemacs/commit/103b1da2f4a004672300b6aa9b64d834ceaa4410)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** register `pylyzer` LSP server - ([0b3dc6e](https://github.com/abougouffa/minemacs/commit/0b3dc6e0dbb28244edb4054bc0d55d271f2d9926)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** better implementation of `+eglot-register` - ([6f70f77](https://github.com/abougouffa/minemacs/commit/6f70f77ca55a0ecf37a3b53171c98cf29839cf71)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot-x)** autoload `eglot-x-setup` - ([a242fa2](https://github.com/abougouffa/minemacs/commit/a242fa20a13bd5849a24165bea4450b31436cfc4)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** switch the repo, again! - ([b9956e5](https://github.com/abougouffa/minemacs/commit/b9956e52a03a4ba5f9340fbf69b3a4b73b696cb2)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** switch to my fork - ([3b165f0](https://github.com/abougouffa/minemacs/commit/3b165f09e6a0fc533d9b3f914cb2844be3e2f8b6)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** name tabs after the current project - ([eba0bc4](https://github.com/abougouffa/minemacs/commit/eba0bc4796cc315686e3e46042f77e57049ab80f)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** fix grammars and refactor - ([06cb69d](https://github.com/abougouffa/minemacs/commit/06cb69dc58a52d0b42d57004b5e35bcdc0a68096)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add the missing `casual-lib` dependency - ([de31e49](https://github.com/abougouffa/minemacs/commit/de31e49c67ba46058bda1c2b68c599208d01bd71)) - [@abougouffa](https://github.com/abougouffa)
- **(yasnippet)** suppress some annoying and unimportant messages - ([36b262b](https://github.com/abougouffa/minemacs/commit/36b262b8457d64780966cc8a296a785902df5fc8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d0440b6](https://github.com/abougouffa/minemacs/commit/d0440b62a14232b838d853cb70d4fd79da54dbe5)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([726dc09](https://github.com/abougouffa/minemacs/commit/726dc095a358584e08f3783e752515cc52e9d3dc)) - [@abougouffa](https://github.com/abougouffa)
- bump the `guard-lf` version - ([be1ddb4](https://github.com/abougouffa/minemacs/commit/be1ddb4538b0d03d9ccdf5b9cd0bb9ea0094cfe3)) - [@abougouffa](https://github.com/abougouffa)
- better performance for big `text-mode` files - ([058602e](https://github.com/abougouffa/minemacs/commit/058602ed27c7da3004d185468a568d832519f8c2)) - [@abougouffa](https://github.com/abougouffa)
- better implementation of `minemacs-load-module` - ([abdb445](https://github.com/abougouffa/minemacs/commit/abdb4453d088e8c5b111502d9706c0d501fa27bf)) - [@abougouffa](https://github.com/abougouffa)
- add a command to interactively install modules - ([acf190a](https://github.com/abougouffa/minemacs/commit/acf190a39f1f0b2100fc94d82208764cfdbfff09)) - [@abougouffa](https://github.com/abougouffa)
- don't persist any of `MINEMACS_*` environment variables - ([098a558](https://github.com/abougouffa/minemacs/commit/098a5588070f520717fb41d1e0b9392333bee3a3)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([632d872](https://github.com/abougouffa/minemacs/commit/632d872d269c0ae004672effda00f08c40c8e1da)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([82b837a](https://github.com/abougouffa/minemacs/commit/82b837a84c87666bf9763dc19ca0fce97555a077)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.1.0](https://github.com/abougouffa/minemacs/compare/8da9d400761a46c6d7b21ce1b563f7e8f69fbed6..v8.1.0) - 2024-06-22
#### Documentation
- **(early-init)** update comments - ([2664493](https://github.com/abougouffa/minemacs/commit/2664493d47b89a22ab91dce8fef18b5abb8b1ba6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `minemacs-apply-performance-tweaks` - ([8f7b3cb](https://github.com/abougouffa/minemacs/commit/8f7b3cb366173333a459595043dabbbaa1a4b1ab)) - [@abougouffa](https://github.com/abougouffa)
- **(workspace)** add initial support for `burly` and `bufler` - ([db01fea](https://github.com/abougouffa/minemacs/commit/db01fea43968dc6fed575e25dd8cb6e6cd4d8a33)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `which-key` to `me-builtin`, it is builtin in Emacs 30 - ([5d49030](https://github.com/abougouffa/minemacs/commit/5d490307d9625f05ed565ffc80913553de748cdb)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(guard-lf)** restore the upstream repo (fix merged) - ([8da9d40](https://github.com/abougouffa/minemacs/commit/8da9d400761a46c6d7b21ce1b563f7e8f69fbed6)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** remove unneeded tweaks, better defaults - ([dace43e](https://github.com/abougouffa/minemacs/commit/dace43e72b37c82fe83637b5dbbc2a1a1fa1031c)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** prefer default directories if they fall under `.emacs.d/local` - ([4bf6d03](https://github.com/abougouffa/minemacs/commit/4bf6d03b93045e12b6c980649b842d197cb2e7c5)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** set only the directories when necessary - ([2c978be](https://github.com/abougouffa/minemacs/commit/2c978be6d82292cb97440d7ef1a5ff7ac5791279)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** use `.citre-root` instead of `.citre_root` as a root marker - ([43ef63e](https://github.com/abougouffa/minemacs/commit/43ef63e1090482ab565e38d8f0bcda0e178e985e)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** remove `+toggle-bury-compilation-buffer-if-successful` - ([783c776](https://github.com/abougouffa/minemacs/commit/783c77620d775a33f4f6430015f415d0094a100c)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** the right way to load the package - ([f8c7154](https://github.com/abougouffa/minemacs/commit/f8c7154eaad521f6080ab38405a365a72a858f7b)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** update the don't reply to self config - ([b7b89b1](https://github.com/abougouffa/minemacs/commit/b7b89b1d11f6b1ef761aa0aabdd6872153bd3946)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** pulse line on scroll commands - ([f699764](https://github.com/abougouffa/minemacs/commit/f699764ec8ea2669de699a9be958e9957f68e542)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** suggest a fix to a common issue in `citre` - ([0eab698](https://github.com/abougouffa/minemacs/commit/0eab69874089630e53136a3c9ded6b31a19a3352)) - [@abougouffa](https://github.com/abougouffa)
- ask a better question on quitting an Emacs client - ([15ee45a](https://github.com/abougouffa/minemacs/commit/15ee45af51115955c8686538460108cea1ff9297)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.6..v8.0.0) - 2024-06-21
#### Features
- **(builtin)** disable `so-long` (replaced with `guard-lf`) - ([588ae98](https://github.com/abougouffa/minemacs/commit/588ae98172b5a394274bf608aa9eaacc8efc5e9d)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(builtin)** rearrange comments - ([4c55d9c](https://github.com/abougouffa/minemacs/commit/4c55d9cdab36738c298dd50aa9c4273943911da5)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete)** minor formatting changes - ([0a25976](https://github.com/abougouffa/minemacs/commit/0a2597615edc4605fe5dcc5eee80b452ae11d118)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** cleanup some irrelevant configs - ([b9fdcb9](https://github.com/abougouffa/minemacs/commit/b9fdcb94cf57f2f83bf8f393c52eb30ea822810c)) - [@abougouffa](https://github.com/abougouffa)
- move `blamer`/`+writing-mode` integration to `me-writing-mode` - ([bfb2fd9](https://github.com/abougouffa/minemacs/commit/bfb2fd9768bc7fbd5b99092dcfeffd6dd6c03d54)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(blamer)** don't enable globally - ([231fae7](https://github.com/abougouffa/minemacs/commit/231fae7958de1feed10ef2bc7ca81aed506fd0cd)) - [@abougouffa](https://github.com/abougouffa)
- **(breadcrumb)** enable in some programming modes - ([bb4655d](https://github.com/abougouffa/minemacs/commit/bb4655d7d675efd0596b13e574d75308b73ba8e2)) - [@abougouffa](https://github.com/abougouffa)
- **(code-review)** switch to `doomelpa` fork - ([c4560d3](https://github.com/abougouffa/minemacs/commit/c4560d39a5bee83f50a184a85dc080ada38daf3d)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** use my fork until jcs-elpa/guard-lf#2 gets merged - ([d3f6e4d](https://github.com/abougouffa/minemacs/commit/d3f6e4d94340bf717bb7cfb5169f3b57cd23d38f)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** mark some modes as intact - ([d8dc3d6](https://github.com/abougouffa/minemacs/commit/d8dc3d68af8e78bea7cc67dd0dd23dc2874dc4c4)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** switch to upstream (fixes merged) - ([63d94d3](https://github.com/abougouffa/minemacs/commit/63d94d34058cdfaf0937574f9a16c841e948192d)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** truncate long lines in the Xref references buffer - ([5a21611](https://github.com/abougouffa/minemacs/commit/5a21611a53d5578e1e81737ef14f5bc3039a91c3)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7c7e33a](https://github.com/abougouffa/minemacs/commit/7c7e33a6a2ad6a563f38085e456b1ecd017cda2c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.6](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.3..v8.0.0-alpha.6) - 2024-06-20
#### Documentation
- **(documentation)** regenerate the documentation - ([0de4699](https://github.com/abougouffa/minemacs/commit/0de4699c3ce8dc9f29abd9eff2df1de799ac6c3b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(files)** add initial support for `guard-lf` (better than `so-long`!) - ([1113291](https://github.com/abougouffa/minemacs/commit/111329186cb656789f71c1fb598c29666ad029ea)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** remove `golden-ratio` - ([333758b](https://github.com/abougouffa/minemacs/commit/333758b127f1069c5a763815164e1868bd5beb79)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(guard-lf)** use my fork - ([9acd33d](https://github.com/abougouffa/minemacs/commit/9acd33dec55d1a359a85f7f7cfb29cab5665b1d2)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** more project markers - ([c3fe2f2](https://github.com/abougouffa/minemacs/commit/c3fe2f20a49049edba2d2eb22c58e49b7d3241ec)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([df64559](https://github.com/abougouffa/minemacs/commit/df64559e85e5d39c484277497737bd1172455a6d)) - [@abougouffa](https://github.com/abougouffa)
- load `minemacs-lazy` without printing a message - ([0132028](https://github.com/abougouffa/minemacs/commit/0132028776f7d6d837ee7e15e944b41da5a219ac)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.5](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.3..v8.0.0-alpha.5) - 2024-06-19
#### Bug Fixes
- **(vlf)** correctly load `vlf-setup` - ([1915a43](https://github.com/abougouffa/minemacs/commit/1915a43e84d5e85e0d7c16a4d163dad182f5f1fe)) - [@abougouffa](https://github.com/abougouffa)
- move a `dired` customization to its relevant place - ([eebebdd](https://github.com/abougouffa/minemacs/commit/eebebddf80aaae561736631acd4e0525a995a4cd)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(external-tools)** update the list - ([5ad53cf](https://github.com/abougouffa/minemacs/commit/5ad53cf607560617cc66db282aad97279ee0aea7)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** update the documentation of MinEmacs' synchronization hook - ([985a271](https://github.com/abougouffa/minemacs/commit/985a271bec3186087494bafc2acdd41bcf9634ef)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update CI badges links to include the right branch - ([f68d14b](https://github.com/abougouffa/minemacs/commit/f68d14b538acbc713ac5b1a69b660076a21c40f3)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** mention the change log - ([f15bba7](https://github.com/abougouffa/minemacs/commit/f15bba71fbb83c69e08a87c5dce7aa0e52d952ec)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(embedded)** add `pcap-mode` to display network/sensor capture files - ([cbc99f9](https://github.com/abougouffa/minemacs/commit/cbc99f91ce062096417aaf074dbfeea96f1719dc)) - [@abougouffa](https://github.com/abougouffa)
- **(embedded)** more assembly flavors (fasm, gas, masm and nasm) - ([87de0d3](https://github.com/abougouffa/minemacs/commit/87de0d331c225a6ede4480420657fb9462c09cbb)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** initial support for `jupyter` - ([7cbacc2](https://github.com/abougouffa/minemacs/commit/7cbacc20b1244105b99b8fc7f0d77ead92b02700)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** initial support for `treesit-fold` - ([76d7d9c](https://github.com/abougouffa/minemacs/commit/76d7d9c2403c741087fbc2905782ab43675fa95e)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** add `avy-zap` - ([cde8a6d](https://github.com/abougouffa/minemacs/commit/cde8a6de4d57eed2ac996c0d912d302e1f401c9a)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `keycast` with a hack to display it in `doom-modeline` - ([3e94d67](https://github.com/abougouffa/minemacs/commit/3e94d6730ae2edc15aa7b7ae3b026f1846d40df5)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(cocogitto)** don't auto-push after bumping the version - ([3740e19](https://github.com/abougouffa/minemacs/commit/3740e19cbc6b6c83e4b49cbfcdfa5bf7041644bf)) - [@abougouffa](https://github.com/abougouffa)
- **(version)** v8.0.0-alpha.4 - ([3cf06f2](https://github.com/abougouffa/minemacs/commit/3cf06f237d8a991e5112e13b75ad27064d106594)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(corfu)** minor cleanups - ([f1d0590](https://github.com/abougouffa/minemacs/commit/f1d05907db1af4ce9981b0f824430083c04de12d)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** better comments, little cleanup - ([94c9f9d](https://github.com/abougouffa/minemacs/commit/94c9f9da224356433f6e35735dfd8c5d9b2aefde)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(git-commit)** minor edit - ([59c82fd](https://github.com/abougouffa/minemacs/commit/59c82fd15856c16b935e0d9884e4ba89777f5311)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** load the `jiralib` dependency before `org-jira` - ([12357df](https://github.com/abougouffa/minemacs/commit/12357df41013d7680081599ff97d6b67423cabbe)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** customize `align` - ([b33fa1c](https://github.com/abougouffa/minemacs/commit/b33fa1cc9566671cbfb11aecb962986eb95424fb)) - [@abougouffa](https://github.com/abougouffa)
- **(diff-hl)** stick to defaults - ([dfe6777](https://github.com/abougouffa/minemacs/commit/dfe6777b25873514c74ccb1d15aee4ec049ee447)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** ensure loading `dirvish` before `dired` gets called - ([1659326](https://github.com/abougouffa/minemacs/commit/1659326be2e5157d0cbac087e35e10a38ac52a21)) - [@abougouffa](https://github.com/abougouffa)
- **(magit)** replace project keybindings for `vc` by `magit` - ([2559ca6](https://github.com/abougouffa/minemacs/commit/2559ca62724fdf9b6d23061ec3104bae308a0ca1)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-completion)** enable globally - ([5528884](https://github.com/abougouffa/minemacs/commit/552888452815db42adc03c29e436f820f9428849)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** extra project root markers - ([2ed9498](https://github.com/abougouffa/minemacs/commit/2ed94989cd23ee7ae15dc2f22d53f618a42d6c67)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add `+treesit-create-parser-in-buffer` - ([1d2f793](https://github.com/abougouffa/minemacs/commit/1d2f793f676c58b8da15d707f7410093f4c3cdec)) - [@abougouffa](https://github.com/abougouffa)
- provide `minemacs-lazy` at the end of the lazy packages - ([9970096](https://github.com/abougouffa/minemacs/commit/997009680f7214629a601c5bd8d43ec1a40eef0b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.4](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.3..v8.0.0-alpha.4) - 2024-06-18
#### Bug Fixes
- **(vlf)** correctly load `vlf-setup` - ([1915a43](https://github.com/abougouffa/minemacs/commit/1915a43e84d5e85e0d7c16a4d163dad182f5f1fe)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** mention the change log - ([f15bba7](https://github.com/abougouffa/minemacs/commit/f15bba71fbb83c69e08a87c5dce7aa0e52d952ec)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(math)** initial support for `jupyter` - ([7cbacc2](https://github.com/abougouffa/minemacs/commit/7cbacc20b1244105b99b8fc7f0d77ead92b02700)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** initial support for `treesit-fold` - ([76d7d9c](https://github.com/abougouffa/minemacs/commit/76d7d9c2403c741087fbc2905782ab43675fa95e)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(cocogitto)** don't auto-push after bumping the version - ([3740e19](https://github.com/abougouffa/minemacs/commit/3740e19cbc6b6c83e4b49cbfcdfa5bf7041644bf)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(corfu)** minor cleanups - ([f1d0590](https://github.com/abougouffa/minemacs/commit/f1d05907db1af4ce9981b0f824430083c04de12d)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** better comments, little cleanup - ([94c9f9d](https://github.com/abougouffa/minemacs/commit/94c9f9da224356433f6e35735dfd8c5d9b2aefde)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(git-commit)** minor edit - ([59c82fd](https://github.com/abougouffa/minemacs/commit/59c82fd15856c16b935e0d9884e4ba89777f5311)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** load the `jiralib` dependency before `org-jira` - ([12357df](https://github.com/abougouffa/minemacs/commit/12357df41013d7680081599ff97d6b67423cabbe)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(diff-hl)** stick to defaults - ([dfe6777](https://github.com/abougouffa/minemacs/commit/dfe6777b25873514c74ccb1d15aee4ec049ee447)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** ensure loading `dirvish` before `dired` gets called - ([1659326](https://github.com/abougouffa/minemacs/commit/1659326be2e5157d0cbac087e35e10a38ac52a21)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-completion)** enable globally - ([5528884](https://github.com/abougouffa/minemacs/commit/552888452815db42adc03c29e436f820f9428849)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add `+treesit-create-parser-in-buffer` - ([1d2f793](https://github.com/abougouffa/minemacs/commit/1d2f793f676c58b8da15d707f7410093f4c3cdec)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.3](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.2..v8.0.0-alpha.3) - 2024-06-17
#### Documentation
- **(changelog)** remove duplicate entry - ([271a070](https://github.com/abougouffa/minemacs/commit/271a070a0cd1527dc9a8fe5494d6946bf8eba8b6)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** regenerate the documentation - ([7550704](https://github.com/abougouffa/minemacs/commit/75507047b260ca585e9d100ef518d33fe9d07833)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update the documenation - ([d4e1b09](https://github.com/abougouffa/minemacs/commit/d4e1b099b38e68db65e079b50edb56b4efa47056)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(services)** add support for `webpaste` - ([1f5f8ce](https://github.com/abougouffa/minemacs/commit/1f5f8ce6ddbe4f068c55b8e8fb5962e504d56f95)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(nxml)** auto rename matching tags - ([db6253f](https://github.com/abougouffa/minemacs/commit/db6253ff192f6a274a58e22a1b62bf635e0d4ef0)) - [@abougouffa](https://github.com/abougouffa)
- bump `flymake-collection` to use the right fork - ([c27c1b9](https://github.com/abougouffa/minemacs/commit/c27c1b9f9647075776a89e45c0e27ffbda106ecf)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.2](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.1..v8.0.0-alpha.2) - 2024-06-16
#### Features
- **(ui)** add the `casual` family of transient menus - ([8c77359](https://github.com/abougouffa/minemacs/commit/8c77359386d0f71f3ed4e699cb7172342c25f8a0)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(git)** ignore Yasnippet generated files - ([d5d9f77](https://github.com/abougouffa/minemacs/commit/d5d9f774a9c739da20c3afed1356dd83ef03a922)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- rename a variable - ([f971648](https://github.com/abougouffa/minemacs/commit/f9716480122c22b83ed24323c8442ef1f481db53)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- correct depth values for hooks - ([938e3fe](https://github.com/abougouffa/minemacs/commit/938e3fe584b92beb22c160220a8120b3de009d7b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(flymake-collection)** switch to my fork with additional checkers - ([3942b32](https://github.com/abougouffa/minemacs/commit/3942b322e6eab24dca0c03b27687847230324fec)) - [@abougouffa](https://github.com/abougouffa)
- **(forge)** add the default keybindings - ([06fc6ad](https://github.com/abougouffa/minemacs/commit/06fc6ad5311c2c6000461917b4ff3dbc7ae0e795)) - [@abougouffa](https://github.com/abougouffa)
- **(magit)** automatically refresh Magit after save - ([ff4e5e3](https://github.com/abougouffa/minemacs/commit/ff4e5e31923d968854b7f67c26038ef469bed85d)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `eldoc-box` obsolete - very annoying in some LSP sessions - ([9f3b5d5](https://github.com/abougouffa/minemacs/commit/9f3b5d562fc5583d98b28fdb54420b56ce3ffb85)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight-mode)** use the `isearch` face instead of `region` - ([2c5e431](https://github.com/abougouffa/minemacs/commit/2c5e43125bda79bc76cca3def2b1a9f4155f7f2a)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** add a snippet for Elisp packages - ([24a0b59](https://github.com/abougouffa/minemacs/commit/24a0b590d33b297afb930b76a14e20182bbbf236)) - [@abougouffa](https://github.com/abougouffa)
- bump packages verions - ([c65fd10](https://github.com/abougouffa/minemacs/commit/c65fd107c4d4798bd8c706ba56cfee614f6b4c0d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.1](https://github.com/abougouffa/minemacs/compare/35ea1037c1ea898ba695aa0e664ca8af160f4c7d..v8.0.0-alpha.1) - 2024-06-15
#### Bug Fixes
- **(daemon)** don't launch `elfeed` automatically - ([3176b95](https://github.com/abougouffa/minemacs/commit/3176b95bf38d7aefa8fe42ced98d3366422ee990)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** annoying error related to `god-mode` - ([2497952](https://github.com/abougouffa/minemacs/commit/24979529e2cf14447405f9962650a2922c18f334)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer)** initializing `treesit` grammar in Elisp breaks parinfer - ([a182082](https://github.com/abougouffa/minemacs/commit/a182082e7facb51c767aa4b22f315e4ba8e13794)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** remove reference to obsolete module - ([a6d7453](https://github.com/abougouffa/minemacs/commit/a6d7453f84e4ed1e54896318d92f0c219b1da948)) - [@abougouffa](https://github.com/abougouffa)
- move a `+nvmap` block to `me-evil` - ([dff3fd2](https://github.com/abougouffa/minemacs/commit/dff3fd2ff023b0ae6431f005ca8e339dc1ce3681)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** add information about the NG branch - ([a9432d1](https://github.com/abougouffa/minemacs/commit/a9432d1add81cc228e16b45b5d608db2c2219413)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** add indent size guessing via `dtrt-indent` - ([5f08274](https://github.com/abougouffa/minemacs/commit/5f08274244d170a75b1877aff5e5ed6ea15402be)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add `ws-butler` for better white space cleanup! - ([a94916a](https://github.com/abougouffa/minemacs/commit/a94916a3dfb5f8b6753d603f65c6f6298fedd19a)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `whitespace-cleanup-mode` obsolete - ([880828d](https://github.com/abougouffa/minemacs/commit/880828d947f04f40d66202895f56c0d21d835ce2)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add more searching packages - ([419f297](https://github.com/abougouffa/minemacs/commit/419f2970df00602746255aed382a936022ec8923)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** restore `elisp-demos` - ([da86b46](https://github.com/abougouffa/minemacs/commit/da86b46b1e63a7c2a04dac8514b2151be1de5e78)) - [@abougouffa](https://github.com/abougouffa)
- **(extra)** initial support for `dogears` - ([157d3a8](https://github.com/abougouffa/minemacs/commit/157d3a851962d737fc2fffcb2af741d9720eebc2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** initial support for `xref-union` (not working!) - ([f25d890](https://github.com/abougouffa/minemacs/commit/f25d8901515b2e552246c86bd62b1676ce2badae)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `better-jumper` obsolete (replaced with `dogears`) - ([17328c0](https://github.com/abougouffa/minemacs/commit/17328c0b73f3d5443a4097ae1da9badd7c324b36)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** restore `pulsar`, don't explicitly touch `pulse` - ([9e7f016](https://github.com/abougouffa/minemacs/commit/9e7f0165d7637ed4bc6103536718360accb47fa8)) - [@abougouffa](https://github.com/abougouffa)
- remove unused `svg-lib` - ([b20c18f](https://github.com/abougouffa/minemacs/commit/b20c18f41593123f5d0d8af120f52e014b790202)) - [@abougouffa](https://github.com/abougouffa)
- make `me-nano` obsolete - ([7146164](https://github.com/abougouffa/minemacs/commit/714616471ef92f88ba4b7c5f0be1a550e70bae34)) - [@abougouffa](https://github.com/abougouffa)
- remove `show-marks` and `fm` - ([30ac536](https://github.com/abougouffa/minemacs/commit/30ac536bf73d2eb6704703554cb1ecd0843dfcc0)) - [@abougouffa](https://github.com/abougouffa)
- make `me-evil` obsolete - ([35ea103](https://github.com/abougouffa/minemacs/commit/35ea1037c1ea898ba695aa0e664ca8af160f4c7d)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(cocogitto)** add `minemacs-ng` to branch whitelist - ([2349b12](https://github.com/abougouffa/minemacs/commit/2349b1297997fa9b23475fa218bcfe4db861a413)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- make use of `once-x-call` - ([8b3726b](https://github.com/abougouffa/minemacs/commit/8b3726b464f8915807ab6c29be4895280f3cdad6)) - [@abougouffa](https://github.com/abougouffa)
- remove unused/obsolete functions and macros - ([0df88d6](https://github.com/abougouffa/minemacs/commit/0df88d6a2c0ab885c454669486a073b165f14f02)) - [@abougouffa](https://github.com/abougouffa)
- remove `+after-load!` - ([5236898](https://github.com/abougouffa/minemacs/commit/5236898d27efff17106fbf95728bee951262ca2d)) - [@abougouffa](https://github.com/abougouffa)
- replace `+add-hook!`, `+hook-once!` and `+advice-once!` by `satch` - ([58d6b77](https://github.com/abougouffa/minemacs/commit/58d6b772e4cf735720aa7086338a640c6b78cc75)) - [@abougouffa](https://github.com/abougouffa)
- remove some unused packages - ([63e67a0](https://github.com/abougouffa/minemacs/commit/63e67a0d957fafb3ac278896dc73f48642ea66cd)) - [@abougouffa](https://github.com/abougouffa)
- move `editorconfig` from `me-prog` to `me-editor` - ([5bd5b54](https://github.com/abougouffa/minemacs/commit/5bd5b54403c175ebe858869052fb7255c5ed4316)) - [@abougouffa](https://github.com/abougouffa)
- move search and navigation packages to `me-search` - ([fc5c7e1](https://github.com/abougouffa/minemacs/commit/fc5c7e125eba7f2b5f56f2ed4d9b9d981664a5bd)) - [@abougouffa](https://github.com/abougouffa)
- remove obsolete variables and references to `me-evil` - ([91f9f9f](https://github.com/abougouffa/minemacs/commit/91f9f9f83a2ac1f24e31a1f1ed19305f934e4ec1)) - [@abougouffa](https://github.com/abougouffa)
- move all Evil/General related bindings to `me-evil` - ([ddff771](https://github.com/abougouffa/minemacs/commit/ddff7717fe3b6de1c9e1039e0a21b143b8d6ec75)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- restore testing for Emacs daemon in the CI - ([5133fdc](https://github.com/abougouffa/minemacs/commit/5133fdc8ab045f3df8a004f05bdc3427e2cf83cc)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bootstrap)** pin `once` and `satch` for better stability - ([c3a6883](https://github.com/abougouffa/minemacs/commit/c3a68836b2dc9b43fc49a4435b84bee8f046313b)) - [@abougouffa](https://github.com/abougouffa)
- **(breadcrumb)** don't show the project crumbs - ([b430871](https://github.com/abougouffa/minemacs/commit/b4308714ea5480174fd63c99037507cec067bb53)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** ask before quitting Emacs client session - ([fcef084](https://github.com/abougouffa/minemacs/commit/fcef08482a298bc7b02e132b743702f1d670226f)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** add `+consult-tab` - ([b608f4d](https://github.com/abougouffa/minemacs/commit/b608f4de1c81b58c91fe183660c16edebca19c61)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** don't show documentation unless asked, cleanup old stuff - ([60f0583](https://github.com/abougouffa/minemacs/commit/60f0583bb408e3857fd6eebdfa09bdf70a0b0936)) - [@abougouffa](https://github.com/abougouffa)
- **(dogears)** better integration with the rest of packages - ([603ad0f](https://github.com/abougouffa/minemacs/commit/603ad0ffba01d6dbb7e1346f5adc266f7135dce6)) - [@abougouffa](https://github.com/abougouffa)
- **(dtrt-indent)** be less verbose - ([d868442](https://github.com/abougouffa/minemacs/commit/d868442b644988637ffea2751ad0941614ed20c4)) - [@abougouffa](https://github.com/abougouffa)
- **(isearch)** better defaults - ([98ad75e](https://github.com/abougouffa/minemacs/commit/98ad75eb066b723115b86e0561aa3db527bff4c6)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** better keybindings - ([090b8bc](https://github.com/abougouffa/minemacs/commit/090b8bc8344861e8a18cb83f0b0df670674d1f81)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add `project-shell` to project switch commands - ([44bf89b](https://github.com/abougouffa/minemacs/commit/44bf89b29d839dedf64acdc36a6e399d295a9b61)) - [@abougouffa](https://github.com/abougouffa)
- **(rtags)** better defaults - ([9bc1471](https://github.com/abougouffa/minemacs/commit/9bc1471bf936c104b5037758a26f2a407e492149)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** use a separate buffer when showing references - ([a91553b](https://github.com/abougouffa/minemacs/commit/a91553b16491f3ee47302b640f0063c3edd3720b)) - [@abougouffa](https://github.com/abougouffa)
- **(yasnippet)** enable only on `prog-mode`, `text-mode` and `conf-mode` - ([473a904](https://github.com/abougouffa/minemacs/commit/473a904a53d22d3db0417d80e4a592b093b842ac)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([fc915df](https://github.com/abougouffa/minemacs/commit/fc915df054f05400225d33ca70ce11982c0f8310)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([52d3303](https://github.com/abougouffa/minemacs/commit/52d3303728e9e6d609f710636cc6a7bb6d451759)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bbf4306](https://github.com/abougouffa/minemacs/commit/bbf4306566378a19ba4ba1f073e172c1f7783a03)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.5.0](https://github.com/abougouffa/minemacs/compare/8367da53b996a6b7f8b614300e5276d0025e7d8d..v7.5.0) - 2024-06-11
#### Bug Fixes
- **(god-mode)** defer mode-specific keybindings - ([161c6be](https://github.com/abougouffa/minemacs/commit/161c6be3928ecda39c901afe9ff40a670b7dfe29)) - [@abougouffa](https://github.com/abougouffa)
- correct handling of `:prepend` in `+apply-font-or-script` - ([2a8ebe6](https://github.com/abougouffa/minemacs/commit/2a8ebe62f8137d76f10a18000580e106fccb90f2)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** restore `selection-highlight-mode` - ([a87015b](https://github.com/abougouffa/minemacs/commit/a87015b0d6bd895ee1532dc7b09d399061c900fd)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** replace my hack with `whitespace-cleanup-mode` - ([dcfdae7](https://github.com/abougouffa/minemacs/commit/dcfdae7cf1e7b5378c7b6428b0e6fb674499f561)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** add initial support for `key-chord` - ([6321a04](https://github.com/abougouffa/minemacs/commit/6321a047118b10069091d74463491974794060c2)) - [@abougouffa](https://github.com/abougouffa)
- **(show-marks)** initial import of `show-marks` and `fm` - ([1d98a5b](https://github.com/abougouffa/minemacs/commit/1d98a5bfe9e34ac8d6c8fe35c616990dc11ffd51)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(god-mode)** follow the hook naming conventions - ([72f80fe](https://github.com/abougouffa/minemacs/commit/72f80fe2be02d2cf86644be26ce4c876deab7307)) - [@abougouffa](https://github.com/abougouffa)
- **(which-key)** move Evil specific tweaks to `me-evil` - ([e619c6b](https://github.com/abougouffa/minemacs/commit/e619c6b5089ba4493aa989bd00b9cf92d2a3a114)) - [@abougouffa](https://github.com/abougouffa)
- modernize `show-marks` and `fm` and add some features - ([7ef2c6f](https://github.com/abougouffa/minemacs/commit/7ef2c6f86ec3358731a855255f7cac86907b492d)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- don't apply the early background-color hack - ([f6e826c](https://github.com/abougouffa/minemacs/commit/f6e826cb1af570e40c6365e21a7494c6c0fd9efb)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** set indent for `xmllint` based on `nxml-child-indent` - ([8367da5](https://github.com/abougouffa/minemacs/commit/8367da53b996a6b7f8b614300e5276d0025e7d8d)) - [@abougouffa](https://github.com/abougouffa)
- **(avy)** bind `M-j` to `avy-goto-char-timer` - ([3824b5c](https://github.com/abougouffa/minemacs/commit/3824b5cf5c7ba4e60d993329e3b1ee0a52f73686)) - [@abougouffa](https://github.com/abougouffa)
- **(bootstrap)** remove unused `system-packages` - ([c4ea07b](https://github.com/abougouffa/minemacs/commit/c4ea07bb5b2d6337862facc985ccf5daa400d7b0)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** offer to create parent directories if they don't exist - ([e243589](https://github.com/abougouffa/minemacs/commit/e2435896afd45f9ab4c141aca9c3ae91f17c148e)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** cleanup and fix the keybinding - ([3c101cb](https://github.com/abougouffa/minemacs/commit/3c101cba0ea51071f4ce516bcce6e6950ce25a32)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** use the default ELPA recipe - ([3a060a1](https://github.com/abougouffa/minemacs/commit/3a060a11ad4ff1c4d0f99fe7fd5785364654063c)) - [@abougouffa](https://github.com/abougouffa)
- **(god-mode)** add integration for `which-key` - ([88f6ee2](https://github.com/abougouffa/minemacs/commit/88f6ee2d13fa594c115500f205860f08ad90893c)) - [@abougouffa](https://github.com/abougouffa)
- **(helpful)** bind `helpful-callable` instead of `helpful-function` - ([e6ebe9b](https://github.com/abougouffa/minemacs/commit/e6ebe9bc48324cfdd552312e6fa2881ca32d5032)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** don't try to automatically install the fonts - ([fa62917](https://github.com/abougouffa/minemacs/commit/fa629178c55912e5c490d77318f4b279f4aa0c93)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** bind `C-x p a` to `+project-add-project` - ([8c9db7a](https://github.com/abougouffa/minemacs/commit/8c9db7a80163f2005c8124c51ac73af41a6775b8)) - [@abougouffa](https://github.com/abougouffa)
- **(rg)** add keybindig - ([3553839](https://github.com/abougouffa/minemacs/commit/3553839d279a4bd3bfa7ad155f83e72f086ce319)) - [@abougouffa](https://github.com/abougouffa)
- **(show-marks)** delete unneeded code, several tweaks (still WIP) - ([7847a6f](https://github.com/abougouffa/minemacs/commit/7847a6f3944877f2d8e8196269b9e551dd2bf448)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** bump `bootstrap-version` to 7 - ([c704ab7](https://github.com/abougouffa/minemacs/commit/c704ab7da3e2f00ac4a7a0a252d9527e1a21d9d4)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** don't prompt for `xref-find-references` - ([35cc4e7](https://github.com/abougouffa/minemacs/commit/35cc4e76d072311139c7925daa2d0449c2f1fbfc)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** bind `M-<down-mouse-1>` to `xref-find-references-at-mouse` - ([f1ff172](https://github.com/abougouffa/minemacs/commit/f1ff1728c7a933097f70267e9e1e39eec989ec0e)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** add `+xref-find-references-at-point` - ([ac2718d](https://github.com/abougouffa/minemacs/commit/ac2718d7aeef4908385e959d67f1f1482e89e404)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([12890c0](https://github.com/abougouffa/minemacs/commit/12890c00c6899028ee49ba239f0c3bccbdca8294)) - [@abougouffa](https://github.com/abougouffa)
- smarter kill word commands - ([07065e7](https://github.com/abougouffa/minemacs/commit/07065e74792f5c1aac5c0f45591fceb6bd7cd681)) - [@abougouffa](https://github.com/abougouffa)
- use `+region-or-thing-at-point` in `+consult-insert-thing-at-point` - ([2005a92](https://github.com/abougouffa/minemacs/commit/2005a92f315b13152544fd8f76e9cc1b62f0bfb7)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([3f87d79](https://github.com/abougouffa/minemacs/commit/3f87d79793c4247b507af3c053beaf672376b32e)) - [@abougouffa](https://github.com/abougouffa)
- update modules list - ([0349a1d](https://github.com/abougouffa/minemacs/commit/0349a1de9c8ed3208d8b9640f47beda9a286ef9f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.4.0](https://github.com/abougouffa/minemacs/compare/088db1a5b456efbdcb2851cf2c7ac50621dbad55..v7.4.0) - 2024-06-09
#### Features
- **(elfeed)** obsolete `yt-dl` commands - ([ac9a5b2](https://github.com/abougouffa/minemacs/commit/ac9a5b2b3ec285cf70a69e9567e7e439198158d8)) - [@abougouffa](https://github.com/abougouffa)
- **(fun)** remove `asm-box` - ([088db1a](https://github.com/abougouffa/minemacs/commit/088db1a5b456efbdcb2851cf2c7ac50621dbad55)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `mlscroll` obsolete - ([19f896f](https://github.com/abougouffa/minemacs/commit/19f896f2586adc5b4bea2dd2b97d1205bd90e860)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(citar-embark)** minor edit - ([44f9828](https://github.com/abougouffa/minemacs/commit/44f9828868b372bad8ec4b65c843d3f1b4298086)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** move command dedicated tabs creation to relevant modules - ([a779f72](https://github.com/abougouffa/minemacs/commit/a779f7234a747616eee94440297e29af218e141f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** better default keybindings - ([8a30cff](https://github.com/abougouffa/minemacs/commit/8a30cffc65fd8cd01c1b17f1aa992499ef132f7d)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-themes)** enable visual bell extension - ([a13f510](https://github.com/abougouffa/minemacs/commit/a13f5103d14da83089c3454bd01c90875291facb)) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed)** prevent `db/index` from triggering `minemacs-first-file` - ([ec16f4a](https://github.com/abougouffa/minemacs/commit/ec16f4abecf22d60bbb548ed2b586dc287833ae0)) - [@abougouffa](https://github.com/abougouffa)
- **(lib-extra)** remove unneeded `+eglot-optimization-mode` - ([ff128c1](https://github.com/abougouffa/minemacs/commit/ff128c15c89413f868f74aa0a149b514e3d88701)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** additional keybindings - ([1511f83](https://github.com/abougouffa/minemacs/commit/1511f833ba11a7bd935bbb848d3fa158eb61b463)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** add keybindings, move `meow` stuff to `me-meow` - ([7cfec92](https://github.com/abougouffa/minemacs/commit/7cfec92ba569f12c2b00eabf54c3f39e904c0785)) - [@abougouffa](https://github.com/abougouffa)
- **(yasnippet)** add a snippet for obsolete MinEmacs modules - ([aeaee51](https://github.com/abougouffa/minemacs/commit/aeaee513293e0b8f98ae1e5296c2723cd2211db1)) - [@abougouffa](https://github.com/abougouffa)
- more keybindings - ([8c780f6](https://github.com/abougouffa/minemacs/commit/8c780f6ba138d38622e954b0e5de5459351ecf4d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.3.3](https://github.com/abougouffa/minemacs/compare/31bd912da0c26cbcb4e3475de97d26a9f221bfad..v7.3.3) - 2024-06-09
#### Documentation
- **(readme)** update documentation - ([31bd912](https://github.com/abougouffa/minemacs/commit/31bd912da0c26cbcb4e3475de97d26a9f221bfad)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- add `+server-restart` - ([47e3507](https://github.com/abougouffa/minemacs/commit/47e3507a49726f1a6e1c9d46b51e7844ab0ccf66)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(beardbolt)** switch the the upstream repo, fix merged - ([f5badb7](https://github.com/abougouffa/minemacs/commit/f5badb7f3b158a1898ccf0b5cd60aeaa32ea0b90)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** use `transient` for `+window-adjust-size-transient` - ([2ee13fc](https://github.com/abougouffa/minemacs/commit/2ee13fcdc4b0384bcad5ba9790495e30e6cc0231)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** simpler frame title - ([603a574](https://github.com/abougouffa/minemacs/commit/603a5744021d1444e912a7e25030a6e0251f2526)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.3.2](https://github.com/abougouffa/minemacs/compare/1f605fbad9761740dc397eb07ff3f289a1aab708..v7.3.2) - 2024-06-09
#### Documentation
- **(documentation)** regenerate the documentation - ([d0fbf32](https://github.com/abougouffa/minemacs/commit/d0fbf32f39f2bfcc94cb61179f382b659574b7f2)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update the documentation - ([824ce8e](https://github.com/abougouffa/minemacs/commit/824ce8ee6a0a7b2a073526e92889354a17ab4591)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** fix the `documentation` task - ([44af464](https://github.com/abougouffa/minemacs/commit/44af464f81eb7bf6d2f294ae012046cb76d0664c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(init)** remove `MINEMACS_IGNORE_VERSION_CHECK` - ([d2e6026](https://github.com/abougouffa/minemacs/commit/d2e6026a30386f2ff96acc7c80abfeb29c68f478)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(mixed-pitch)** don't change the cursor (causes the cursor to hide) - ([1f605fb](https://github.com/abougouffa/minemacs/commit/1f605fbad9761740dc397eb07ff3f289a1aab708)) - [@abougouffa](https://github.com/abougouffa)
- **(yasnippet)** add snippets in `~/.minemacs.d/snippets/` - ([fbf8480](https://github.com/abougouffa/minemacs/commit/fbf84806efa7dcd42745e58c44a33df089e877cf)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([de94785](https://github.com/abougouffa/minemacs/commit/de947855daebdd3e61fb1de4bf7e39dfde30c6ec)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.3.1](https://github.com/abougouffa/minemacs/compare/328b50cf41592d6aeb98a6ce72e58a1a05fd360a..v7.3.1) - 2024-06-08
#### Refactoring
- ensure moving all `+map!` & `+map-local!` blocks to `me-evil` - ([95d9cb3](https://github.com/abougouffa/minemacs/commit/95d9cb364205d0e9b06da1bdbf6dde879886a7ea)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(expreg)** use `C-M-SPC` to expand and `S-C-M-SPC` to contract - ([328b50c](https://github.com/abougouffa/minemacs/commit/328b50cf41592d6aeb98a6ce72e58a1a05fd360a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.3.0](https://github.com/abougouffa/minemacs/compare/e67d34e3076bb212f7b2e7024bdba4b377513e7d..v7.3.0) - 2024-06-08
#### Bug Fixes
- **(early-init)** more checks before trying to restore the background color - ([2ff90e7](https://github.com/abougouffa/minemacs/commit/2ff90e790caa17c205259bf7107de74f113bbfa8)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** disable on some problematic commands - ([b1b51bb](https://github.com/abougouffa/minemacs/commit/b1b51bb31bdcf1d2503ed8bb404d809184d417dd)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** update examples - ([e67d34e](https://github.com/abougouffa/minemacs/commit/e67d34e3076bb212f7b2e7024bdba4b377513e7d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** restore `yasnippet` - ([8b4bcba](https://github.com/abougouffa/minemacs/commit/8b4bcbaf76002835e25770361839d5d5c5f4a7b4)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `tempel` obsolete - ([1d74136](https://github.com/abougouffa/minemacs/commit/1d7413655850d79bf52b1190e0df8870f886b7fb)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `spdx` obsolete - ([e696e3b](https://github.com/abougouffa/minemacs/commit/e696e3b550341f4683ae79efabfd661df9d24c9b)) - [@abougouffa](https://github.com/abougouffa)
- **(god)** initial support for `god-mode` - ([70da847](https://github.com/abougouffa/minemacs/commit/70da847aea102f2ddec80ae02ae2bdea19f18fea)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(robot)** extract `rosbag-info-mode` to a separate package - ([8215399](https://github.com/abougouffa/minemacs/commit/8215399d646c44aa1dce76bb7d555478e982b59d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** remove `text-scale-*` bindings to `C-+`, `C-=` & `C--` - ([d34270f](https://github.com/abougouffa/minemacs/commit/d34270f2c39a30c643d3ed2665dd0b9bd5275c92)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** minor edits, set cursor type to `bar` - ([6d48bd8](https://github.com/abougouffa/minemacs/commit/6d48bd812f38b2559b5a37dc9f3b7f3690dda677)) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** add keybindings from Cape's example - ([841eac8](https://github.com/abougouffa/minemacs/commit/841eac832eef1980ff0c83ed60b6ff63663c1847)) - [@abougouffa](https://github.com/abougouffa)
- **(helpful)** bind useless `C-h h` to `helpful-at-point` - ([589ebdd](https://github.com/abougouffa/minemacs/commit/589ebdd2f709b7de6e1bd7fa0d23fa05d8f7053f)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** `q` quits `macrostep-mode` when expanding macros - ([c6c465b](https://github.com/abougouffa/minemacs/commit/c6c465b7f6034d4e1b9dd4efc51880bffc2bb3ea)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([09813f8](https://github.com/abougouffa/minemacs/commit/09813f8a0aa7843a4758049c871a33be5bf347c4)) - [@abougouffa](https://github.com/abougouffa)
- drop Vim-like movement HJKL - ([1283a08](https://github.com/abougouffa/minemacs/commit/1283a08183ecb729c6cef7eb4a64a7fd4de98f16)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.2.0](https://github.com/abougouffa/minemacs/compare/e5bb799fade281f85c7ff239dc14a1a39ccbad30..v7.2.0) - 2024-06-07
#### Bug Fixes
- **(org)** async export bug due to obsolete `minemacs-core-modules` - ([3b28a95](https://github.com/abougouffa/minemacs/commit/3b28a953ef7bd4204a65c5ff38244530e70354c0)) - [@abougouffa](https://github.com/abougouffa)
- use `kill-current-buffer` instead of `kill-this-buffer` - ([8532fed](https://github.com/abougouffa/minemacs/commit/8532fede5f1adfb43011c310e9dbda300da9036d)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(faq)** add an answer for `vterm-module` compilation issue - ([8b7b552](https://github.com/abougouffa/minemacs/commit/8b7b552bd6d7e61e6b7a3ce9fa70d24542b2d4b7)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update code examples in `skel/config.el` - ([c864239](https://github.com/abougouffa/minemacs/commit/c864239b4db9cd104a35b2e89c0c7b56e8f53040)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(docs)** make `edraw` obsolete - ([de857d8](https://github.com/abougouffa/minemacs/commit/de857d8005ebaaf4e93710305f8a93872b866db8)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `logos` obsolete - ([c6905b5](https://github.com/abougouffa/minemacs/commit/c6905b53dbb428531d554c538d07d2d4580559c9)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `sr-speedbar` - ([11179be](https://github.com/abougouffa/minemacs/commit/11179be92a5126f20eba167dee4f49bbe30e6b4b)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** replace `jiralib2` with `jiralib` + port my stuff to it - ([d14d869](https://github.com/abougouffa/minemacs/commit/d14d8698dcd209e731ef2eb5d1dc752db11deddc)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** initial support for `window-purpose` (to be tested) - ([5e20103](https://github.com/abougouffa/minemacs/commit/5e20103d9b22d601d46d60c39af6601ca7961ada)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- save some lines - ([5757000](https://github.com/abougouffa/minemacs/commit/575700038f4c3c9f2ecf3067f255357a854dbeb4)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(biblio)** minor edit - ([e5bb799](https://github.com/abougouffa/minemacs/commit/e5bb799fade281f85c7ff239dc14a1a39ccbad30)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([883f259](https://github.com/abougouffa/minemacs/commit/883f259b6b6da15a6327d9aefc1fe27e7fe3d586)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- don't overwrite the `trailing-whitespace` color - ([adf2c00](https://github.com/abougouffa/minemacs/commit/adf2c00492f4f1df60fea2c3940912722a7129b1)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(jiralib2)** enable inserting ticket summary - ([2e29afa](https://github.com/abougouffa/minemacs/commit/2e29afae63d9c158b5199e2289016440238fbe5b)) - [@abougouffa](https://github.com/abougouffa)
- **(markdown)** enable highlighting syntax & code blocks fontification - ([b3f03cf](https://github.com/abougouffa/minemacs/commit/b3f03cfdbeb1326c45960f883a7deb8f78ee4415)) - [@abougouffa](https://github.com/abougouffa)
- **(tributary)** auto load some commands - ([0056312](https://github.com/abougouffa/minemacs/commit/0056312601e8decc2b49266bb7911ec03453d3ab)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([01cc748](https://github.com/abougouffa/minemacs/commit/01cc7486e914772271cd625e146296d4444e4ff0)) - [@abougouffa](https://github.com/abougouffa)
- avoid flickering UI on initialization - ([d37e4e2](https://github.com/abougouffa/minemacs/commit/d37e4e2aa392230e288236cd62aff639544dd3c8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([b30ddc9](https://github.com/abougouffa/minemacs/commit/b30ddc90bb51c1338cc35c0259835c0241de9ba4)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.1.0](https://github.com/abougouffa/minemacs/compare/5d688f72d4f857a1f5cad0d506bc4b75052c31d2..v7.1.0) - 2024-06-05
#### Bug Fixes
- **(core)** more robust `+dir-locals-open-or-create` - ([3e80a92](https://github.com/abougouffa/minemacs/commit/3e80a92713412b3f2181259268c37c9a9051ff52)) - [@abougouffa](https://github.com/abougouffa)
- **(me-writing-mode)** load after `olivetti` (fix CI failure on Windows) - ([5ee5a28](https://github.com/abougouffa/minemacs/commit/5ee5a282243b74a8100df4e11f88f775b0fbbc8e)) - [@abougouffa](https://github.com/abougouffa)
- **(projection)** restore `projection-*` extensions - ([106ed5a](https://github.com/abougouffa/minemacs/commit/106ed5a47eda5179a3c22135eebab7e9887b4c88)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** force v0.22.0 of C++ grammar (fix syntax highlighting) - ([b10bca5](https://github.com/abougouffa/minemacs/commit/b10bca5bb180fa3b31b07f9d18e5dab5e467b754)) - [@abougouffa](https://github.com/abougouffa)
- remove references to `me-core-ui` - ([ea3397b](https://github.com/abougouffa/minemacs/commit/ea3397b582706e29d4e08dc22bceb717cd3af1b1)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(builtin)** remove customization for `elec-pair` - ([50f09a4](https://github.com/abougouffa/minemacs/commit/50f09a443c170409a76fd771ee3228c190d08e3b)) - [@abougouffa](https://github.com/abougouffa)
- **(debug)** make `realgud` obsolete - ([091a11e](https://github.com/abougouffa/minemacs/commit/091a11e4b9aa33fb2dd7fec4651b661b36e4d812)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** restore `smartparens` - ([02373fc](https://github.com/abougouffa/minemacs/commit/02373fc01c8064ccf5a45ffcef5b61078fef1ad0)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `cargo.el` - ([77fffc8](https://github.com/abougouffa/minemacs/commit/77fffc8bc7963f362402905fd088fb78d9a56fcb)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `rustic` - ([bfb5d53](https://github.com/abougouffa/minemacs/commit/bfb5d5338cb50130f260bab0cc341060b70b941e)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** replace `tmux.el` with `emamux` - ([1f819f2](https://github.com/abougouffa/minemacs/commit/1f819f245114fe0d341837a0ebceb34a46315012)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `mlscroll` - ([3ee9ce3](https://github.com/abougouffa/minemacs/commit/3ee9ce31434c8a7329f765188d400238de14b0ce)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `pulsar` obsolete - ([fa1a38d](https://github.com/abougouffa/minemacs/commit/fa1a38df07181a4ed31f18e8f307f4de5b48ac9d)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** replace `visual-fill-column` with `olivetti` - ([686ccca](https://github.com/abougouffa/minemacs/commit/686cccac4fc77a66beafb4fc376d8005b6c943c2)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** make `gee` and `gerrit` obsolete, only keep `repo-transient` - ([3cde4b1](https://github.com/abougouffa/minemacs/commit/3cde4b1416ce94ef8a7a0d0c13d612d13032b8bf)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** move all `+map!` and `+map-local!` blocks to `me-evil` - ([401bb41](https://github.com/abougouffa/minemacs/commit/401bb41cbf9ae20fa86ace9e06731b3c461a8bf9)) - [@abougouffa](https://github.com/abougouffa)
- move all `SPC` leader keybindings to `me-evil` - ([e2f69db](https://github.com/abougouffa/minemacs/commit/e2f69dbdddb4557400f88a990fb7a9b229fa8605)) - [@abougouffa](https://github.com/abougouffa)
- remove `me-splash` and simplify modules loading section - ([5d688f7](https://github.com/abougouffa/minemacs/commit/5d688f72d4f857a1f5cad0d506bc4b75052c31d2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** accept `:package` & `:module` in `+map` & `+map-local!` - ([7993ed3](https://github.com/abougouffa/minemacs/commit/7993ed3986c1c3031655d224498957a84138d079)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** better integration with `nerd-icons` - ([a9facf2](https://github.com/abougouffa/minemacs/commit/a9facf27a7b0fbb57899eaa28f461c9d84ef5d0f)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** restore the `evil-mc` hack for `smartparens` - ([c3030be](https://github.com/abougouffa/minemacs/commit/c3030bec89a185e89c04bfb6cb25e9ec07c9a1fd)) - [@abougouffa](https://github.com/abougouffa)
- **(logview)** minor tweak for RDK logs sumbode - ([b7a5e21](https://github.com/abougouffa/minemacs/commit/b7a5e211fc27fb58f3b98b111a17c3f3e7c0b8fb)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** bind `expreg-expand` & `expreg-contract` to `v` & `V` - ([aec189a](https://github.com/abougouffa/minemacs/commit/aec189a4f8d60333d37f7008ba9901a7a81ea90f)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** bind `*` to `magit-delete-thing` - ([9890086](https://github.com/abougouffa/minemacs/commit/989008647207a10f15586d6b55635a52f2815783)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** DRY - ([6500f90](https://github.com/abougouffa/minemacs/commit/6500f90a60889b5b68a473fbc4ff362c4b1944f1)) - [@abougouffa](https://github.com/abougouffa)
- merge `me-core-ui` in `me-ui` - ([f5a5729](https://github.com/abougouffa/minemacs/commit/f5a5729a43ff353af1bf2b9efeaf95e6d29e6f67)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([30e5d69](https://github.com/abougouffa/minemacs/commit/30e5d69a1b7eab21372ffbe27c436cda1a72549d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.0.0](https://github.com/abougouffa/minemacs/compare/a98aea952b08a059a4f5433dadd7a360585d9f35..v7.0.0) - 2024-06-03
#### Bug Fixes
- **(beardbolt)** correct macro name, cleaner macro implementation - ([9d9b615](https://github.com/abougouffa/minemacs/commit/9d9b615b5533dfe6deae7030a56b148ea836e3b5)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** do Elisp stuff after loading `elisp-mode` - ([1d21307](https://github.com/abougouffa/minemacs/commit/1d213079f33d2d69d8a46c2b13d013d9887866d0)) - [@abougouffa](https://github.com/abougouffa)
- **(forge)** remove buggy transient binding - ([3d3b70a](https://github.com/abougouffa/minemacs/commit/3d3b70a2d583d3a03b8631aeab82e1a302c8d120)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-selection-mode)** deffer a litter bit more (!) - ([a118f2e](https://github.com/abougouffa/minemacs/commit/a118f2e9fda6dcdf8d034e4b91c503fa3e96b22f)) - [@abougouffa](https://github.com/abougouffa)
- **(iedit)** add the keybindings - ([f24ff3c](https://github.com/abougouffa/minemacs/commit/f24ff3cf4aba3865d3636954a3a7712a22e706b9)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** correctly load the package - ([f102c7d](https://github.com/abougouffa/minemacs/commit/f102c7d4c6a05c82aabaab96a37293c505a6fad6)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-corfu)** load after `corfu` - ([b5b5473](https://github.com/abougouffa/minemacs/commit/b5b547348736a162ec2bba329af6d57cae2b17f3)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** remove `.vscode` from identifiers (conflict with ~/.vscode) - ([9c96067](https://github.com/abougouffa/minemacs/commit/9c960676c198e7ee0d1cc578d7f388e8eea01f25)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** start at the right time, `prog-mode-hook` is too late - ([f05486e](https://github.com/abougouffa/minemacs/commit/f05486e42b38317812d1e0d1f3a12d6d603be0f7)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- update documentation - ([f44cbcb](https://github.com/abougouffa/minemacs/commit/f44cbcb06270f5bb3e4447ef928462bd420b1d5b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(builtin)** enable `repeat-mode` by default - ([0adb6da](https://github.com/abougouffa/minemacs/commit/0adb6daaae97057721b30eeaeb0a172ef9193a40)) - [@abougouffa](https://github.com/abougouffa)
- **(calendar)** initial support for `org-timeblock` - ([0d68027](https://github.com/abougouffa/minemacs/commit/0d68027d888f746dfeff7cdc4bcc6d3a54ec957f)) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** remove `cape-capf-super` unused hacks - ([f1f9182](https://github.com/abougouffa/minemacs/commit/f1f91820b039ad77c6b12f6de0b537d580f6478a)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add the `+mode-alist-add-ts-modes!` macro - ([3430240](https://github.com/abougouffa/minemacs/commit/34302404f99ffdc1d72103d3749ab04ccfb6c462)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `super-save` obsolete - ([aff8cca](https://github.com/abougouffa/minemacs/commit/aff8cca8480139109fe4c10f17751250f01474cc)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `selection-highlight-mode` obsolete - ([7bbeceb](https://github.com/abougouffa/minemacs/commit/7bbecebf743da6bba8dc9c13b7494153c3895f02)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `highlight-indentation-guides` obsolete - ([a0d8edf](https://github.com/abougouffa/minemacs/commit/a0d8edfe7ad641662c71aa6bd94d7cbebeb06a4f)) - [@abougouffa](https://github.com/abougouffa)
- **(formal)** make obsolete - ([0516d30](https://github.com/abougouffa/minemacs/commit/0516d30d181d68b71b2ea5df0b3db7659264d6f3)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** add initial support for Meow modal editing - ([925895d](https://github.com/abougouffa/minemacs/commit/925895dc5e85692d234fd8204f172e641652a196)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-cursors)** add initial support for `multiple-cursors` - ([c3c8565](https://github.com/abougouffa/minemacs/commit/c3c85651ff82dbdcf41b8187ce8fc04687a92345)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** make `ffip` obsolete - ([eea6f5d](https://github.com/abougouffa/minemacs/commit/eea6f5d2c743d0198c72457303f25cc6dfdf7d70)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** initial support for `hurl-mode` (to replace `restclient` someday) - ([5ec1bec](https://github.com/abougouffa/minemacs/commit/5ec1beccc74a45c95197789b0ca769ea2314a651)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** simplify some `use-package` blocks - ([2b2606e](https://github.com/abougouffa/minemacs/commit/2b2606e28d14f635a1ed64c0df3bc269d1cf4506)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** minor edit - ([3f81efc](https://github.com/abougouffa/minemacs/commit/3f81efc3e98cfc035c94b936665e632418aaef46)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** merge `corfu` related customizations - ([31167a6](https://github.com/abougouffa/minemacs/commit/31167a6ef6fca0f04762c37c111302e4edb0aa97)) - [@abougouffa](https://github.com/abougouffa)
- **(embark)** simplify and cleanup duplicate hook - ([ef74d5e](https://github.com/abougouffa/minemacs/commit/ef74d5eef7e0d02a9f0696c1d24bfa49b6443b03)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** move all Evil related configs/packages to `me-evil` - ([5543a6c](https://github.com/abougouffa/minemacs/commit/5543a6c6b2e65ce4018a446224880aee7bdf2f42)) - [@abougouffa](https://github.com/abougouffa)
- **(keybinding)** move `general` to `me-evil` - ([65e7bc0](https://github.com/abougouffa/minemacs/commit/65e7bc058cf263459d2e4525a83ec1490799f468)) - [@abougouffa](https://github.com/abougouffa)
- **(vertico)** use only one `use-package` block - ([6d84749](https://github.com/abougouffa/minemacs/commit/6d84749ee21c2363304f4f1438d26d1fd774bfb5)) - [@abougouffa](https://github.com/abougouffa)
- move `disable-theme` advice to `me-builtin` - ([72be5a8](https://github.com/abougouffa/minemacs/commit/72be5a895bba9323af761e0887e4402ed1b0118e)) - [@abougouffa](https://github.com/abougouffa)
- move optional modules from `core` to `modules` - ([6216b10](https://github.com/abougouffa/minemacs/commit/6216b109d53def042e72ce4ccfafaa859ea90957)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(beardbolt)** use my fork until it get merged upstream - ([77b17d1](https://github.com/abougouffa/minemacs/commit/77b17d169baa2922166a9122e7445367b17bf009)) - [@abougouffa](https://github.com/abougouffa)
- **(better-jumper)** move the Evil related remaps to `me-evil` - ([d32692c](https://github.com/abougouffa/minemacs/commit/d32692c04d4e02aecd46f96f2d6a49cc4f0a4797)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** remap some keybindings for more useful commands - ([6b4262e](https://github.com/abougouffa/minemacs/commit/6b4262e7a6845c08e9edc6c63ac97df91deb7f27)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** unset some annoying commands - ([f3459f2](https://github.com/abougouffa/minemacs/commit/f3459f2d10d139ddebb392af42fb1fa22d41acd0)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** bind ESC to `keyboard-escape-quit` when in Emacs mode - ([4b27a80](https://github.com/abougouffa/minemacs/commit/4b27a80fcae24487437512a166f36eec5ffc383d)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** add keybindings (inspired by the official example) - ([7c6fdd7](https://github.com/abougouffa/minemacs/commit/7c6fdd779c561b5bb6fc61e50e95a1c1e2f16135)) - [@abougouffa](https://github.com/abougouffa)
- **(display-line-numbers)** use absolute numbers instead of relative - ([5cb3fb1](https://github.com/abougouffa/minemacs/commit/5cb3fb1bc2459ffe0a652ffc7289c57805817371)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** use upstream repo (meow related changes merged) - ([63253d7](https://github.com/abougouffa/minemacs/commit/63253d7e7cfa42285890c52644b944eed75432c5)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** use my fork until it gets merged - ([cf7e2a5](https://github.com/abougouffa/minemacs/commit/cf7e2a58752858c3882de9858b864f09da9d82a3)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** minor edit - ([4438997](https://github.com/abougouffa/minemacs/commit/4438997279a61f853967a5427a1db3a0b2f1a255)) - [@abougouffa](https://github.com/abougouffa)
- **(elisp-mode)** better font lock for Elisp shorthands - ([c800d99](https://github.com/abougouffa/minemacs/commit/c800d995406929111449d7e33cc87e4921fefea8)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** remove unused bitmaps - ([998e97e](https://github.com/abougouffa/minemacs/commit/998e97e0ac63872406ff65078bb88b2a91529e43)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** add a command to toggle hiding all blocks - ([82a2016](https://github.com/abougouffa/minemacs/commit/82a2016fc484b90d23c2baa4dd471ef70e7286e2)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** bind `hs-toggle-hiding` to `C-c f` - ([ef414c8](https://github.com/abougouffa/minemacs/commit/ef414c835420a10e117fd13861689f3106209601)) - [@abougouffa](https://github.com/abougouffa)
- **(logview)** add a custom log submode for RDK - ([fc61864](https://github.com/abougouffa/minemacs/commit/fc618648d94c6b1152cb2553dafc584e30967d7d)) - [@abougouffa](https://github.com/abougouffa)
- **(macrostep)** bind `macrostep-expand` to `C-c m` - ([3c83c3f](https://github.com/abougouffa/minemacs/commit/3c83c3f4c89863edece3472e4bea981d4a5bcc56)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** bind `meow-kill-whole-line` to `S` - ([7e29bf5](https://github.com/abougouffa/minemacs/commit/7e29bf5becff7c7a6084eaee4c8fe50aa3177079)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** minor keybindings tweaks, trying to find my comfort! - ([0be36c7](https://github.com/abougouffa/minemacs/commit/0be36c7d6e7423e142232c2f5c4c6325264f792f)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** tinker Meow commands - ([58e4b43](https://github.com/abougouffa/minemacs/commit/58e4b4387d2f48e23c8a787356e538f7cd232208)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** display a warning if Meow and Evil are both enabled - ([724d1f3](https://github.com/abougouffa/minemacs/commit/724d1f329d5c6b75800bc574ac19b109c10ae600)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** quit `corfu` completion when exiting insert state - ([7f6cf6a](https://github.com/abougouffa/minemacs/commit/7f6cf6aae24c07f4958b314f1732262a3c29744d)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** bind more keys - ([1a96d77](https://github.com/abougouffa/minemacs/commit/1a96d7744cbcf492b6538414a27f5762c4bc2804)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** replace obsolete function - ([ee639bd](https://github.com/abougouffa/minemacs/commit/ee639bd9747cdd6081d191d1d746352739d1c1f4)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** move `evil-textobj-tree-sitter` to `me-evil` - ([2ca1f68](https://github.com/abougouffa/minemacs/commit/2ca1f68c3b34e75eea7961489a9e1a7d90da1328)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** add `.projectile` and `.vscode` as project identifiers - ([5b70a55](https://github.com/abougouffa/minemacs/commit/5b70a553cb1f56142a6f1306cce9a3d54be33a46)) - [@abougouffa](https://github.com/abougouffa)
- **(undo-fu-session)** lazy load - ([dc4caa5](https://github.com/abougouffa/minemacs/commit/dc4caa587c77dd0944471a4b12a248a20dfffed2)) - [@abougouffa](https://github.com/abougouffa)
- **(windmove)** add a prefix for moving between windows - ([22ac29d](https://github.com/abougouffa/minemacs/commit/22ac29db88e5077c2ebf763f96077d5e1e22c686)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([eb36378](https://github.com/abougouffa/minemacs/commit/eb36378e7878b78b0c2d7ed768f2f1b374a33811)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([4f2dd83](https://github.com/abougouffa/minemacs/commit/4f2dd833fed2e89cdf8f6bd98aca2d8e52c96a2c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c2ac0c0](https://github.com/abougouffa/minemacs/commit/c2ac0c09fcdabec6ca5ae819ef09564b457d9bf7)) - [@abougouffa](https://github.com/abougouffa)
- make use of `+mode-alist-add-ts-modes!` - ([bc94578](https://github.com/abougouffa/minemacs/commit/bc9457812bf2b5e5a2431b7aa4f24f404ebf2c2e)) - [@abougouffa](https://github.com/abougouffa)
- more info on the message displayed after load - ([a98aea9](https://github.com/abougouffa/minemacs/commit/a98aea952b08a059a4f5433dadd7a360585d9f35)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.8.2](https://github.com/abougouffa/minemacs/compare/9f70fd72f5fd04ed00f2cfce318034bee8330cd4..v6.8.2) - 2024-05-28
#### Documentation
- add `tmux` to the list of external dependencies - ([e53b232](https://github.com/abougouffa/minemacs/commit/e53b232c1e6db2b8d71585cf816b47491c0c59a4)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(jq-mode)** add `+xq-interactively` for XML - ([297e31c](https://github.com/abougouffa/minemacs/commit/297e31c5d07ddbe36dd3fd8d333731be1c6edbb6)) - [@abougouffa](https://github.com/abougouffa)
- **(restclient)** use my fork - ([7c5c378](https://github.com/abougouffa/minemacs/commit/7c5c3786226f2d8de47eaca5ea0a070e97a539a1)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** add initial support for `tributary` (Confluence) - ([b03a091](https://github.com/abougouffa/minemacs/commit/b03a0915b072a5aba4534993c294ca98b16cc35f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(parinfer-rust)** simplify conditions, remove unnecessary (!) hacks - ([9f70fd7](https://github.com/abougouffa/minemacs/commit/9f70fd72f5fd04ed00f2cfce318034bee8330cd4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8b61854](https://github.com/abougouffa/minemacs/commit/8b61854f8e675adeb94833b0564bc6756d76e1ab)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.8.1](https://github.com/abougouffa/minemacs/compare/f73cea17ec10f15ca5c4ea81716f527267306cae..v6.8.1) - 2024-05-28
#### Bug Fixes
- **(tmux)** disable on Windows - ([98d1b9f](https://github.com/abougouffa/minemacs/commit/98d1b9fdfddb2c24a3c265c712f19da0afbc5b04)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(pet)** better activation condition - ([f73cea1](https://github.com/abougouffa/minemacs/commit/f73cea17ec10f15ca5c4ea81716f527267306cae)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2c8a584](https://github.com/abougouffa/minemacs/commit/2c8a58454f201a74f58231a367c87ea5d4fabcd2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.8.0](https://github.com/abougouffa/minemacs/compare/c65e497953c27a627fac4a0b38772de128d6be13..v6.8.0) - 2024-05-27
#### Bug Fixes
- **(eaf)** remove references to `minemacs-fonts` - ([c65e497](https://github.com/abougouffa/minemacs/commit/c65e497953c27a627fac4a0b38772de128d6be13)) - [@abougouffa](https://github.com/abougouffa)
- **(valgrind)** better detection of the project root - ([57fca4b](https://github.com/abougouffa/minemacs/commit/57fca4b0c029358f78aca5934ab17d266b2be202)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** update the modules list - ([ed97985](https://github.com/abougouffa/minemacs/commit/ed9798522d5ae0a1ff99786631e27f90d613e0d9)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(jiralib2)** add `+jira-insert-ticket-link` - ([384fd38](https://github.com/abougouffa/minemacs/commit/384fd3874d1e3819d33384b5e83b7f154b6e4e87)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add initial support for `org-jira` - ([222e9dd](https://github.com/abougouffa/minemacs/commit/222e9ddaf6e8006c6dcd7276025bd8fbead1edf6)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** add a new module for services, move `jiralib2` and `org-jira` - ([8590218](https://github.com/abougouffa/minemacs/commit/85902186a1d9e8983cf53a31730561e5d05a74ea)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `tmux` - ([d6ba7e7](https://github.com/abougouffa/minemacs/commit/d6ba7e7f317ab2b241fcbff0596fc9c85f7b602e)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `ob-restclient` - ([8d99771](https://github.com/abougouffa/minemacs/commit/8d99771ee1b029f3538a4c990f495645604c146c)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `restclient-test` - ([7c20b9c](https://github.com/abougouffa/minemacs/commit/7c20b9cc95438c60a23c086cd07a8b0d738353a5)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `restclient` - ([209bb6b](https://github.com/abougouffa/minemacs/commit/209bb6b96f4a1c3abebae2c214f8083d35de2f0f)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add initial support for `golden-ratio` - ([013af49](https://github.com/abougouffa/minemacs/commit/013af491f6754caa3711eebdc3ee8717d455d0aa)) - [@abougouffa](https://github.com/abougouffa)
- move `jiralib2` from `me-vc` to `me-project` - ([3cebfc3](https://github.com/abougouffa/minemacs/commit/3cebfc374255e4ed02a8aec7070ef01b4c40c599)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(better-jumper)** use instead of `evil-jump-[forward/backward]` - ([c4f0a8c](https://github.com/abougouffa/minemacs/commit/c4f0a8cf1d0077d34c3486f60566306eff46fd95)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** don't pop as initial buff when a file is opened via args - ([cd64605](https://github.com/abougouffa/minemacs/commit/cd646056b6c09b6f558ae537a34e443642a62931)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-snipe)** minor edits - ([cbad2a9](https://github.com/abougouffa/minemacs/commit/cbad2a9623a8e748b0038421cce33abc98e388d5)) - [@abougouffa](https://github.com/abougouffa)
- **(project-tab-groups)** use a better group naming function - ([010f58a](https://github.com/abougouffa/minemacs/commit/010f58aba380aa57f8a0dd8a42f95265a3b4e423)) - [@abougouffa](https://github.com/abougouffa)
- minor tweaks related to profiling MinEmacs - ([4de550d](https://github.com/abougouffa/minemacs/commit/4de550d480e555fd4990fa9108c632ee2d65d935)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.7.4](https://github.com/abougouffa/minemacs/compare/ffaa6c184988ab8a791f16f78e1a225bcf18011b..v6.7.4) - 2024-05-26
#### Tweaks
- **(cocogitto)** better stashing on dirty work directory - ([ffaa6c1](https://github.com/abougouffa/minemacs/commit/ffaa6c184988ab8a791f16f78e1a225bcf18011b)) - [@abougouffa](https://github.com/abougouffa)
- add group for the top level `minemacs` group - ([bb35d3f](https://github.com/abougouffa/minemacs/commit/bb35d3fa46a2af8e0dd7ce553b0df5079da5557f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.7.3](https://github.com/abougouffa/minemacs/compare/7d3be45302a7fdacc82ea510de304f2b9d4f84b9..v6.7.3) - 2024-05-26
#### Refactoring
- use `+package-disabled-p` when needed - ([4dbe652](https://github.com/abougouffa/minemacs/commit/4dbe6529bfd83a47f4caec899c69d76d28c604f1)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(doom-modeline)** hide indent information - ([7d3be45](https://github.com/abougouffa/minemacs/commit/7d3be45302a7fdacc82ea510de304f2b9d4f84b9)) - [@abougouffa](https://github.com/abougouffa)
- prefer lazy loading for almost all packages - ([299e340](https://github.com/abougouffa/minemacs/commit/299e3409e3972446465b4435b9a31a444dbf151c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.7.2](https://github.com/abougouffa/minemacs/compare/4f91a0a8d32733ade1d38ae90d67ab18f6a74e09..v6.7.2) - 2024-05-26
#### Bug Fixes
- **(enlight)** temporary disable on Emacs 28 - ([09e6588](https://github.com/abougouffa/minemacs/commit/09e658811e464680eff697249ae41fb79a45b21b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(doom-modeline)** minor tweaks - ([4f91a0a](https://github.com/abougouffa/minemacs/commit/4f91a0a8d32733ade1d38ae90d67ab18f6a74e09)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9cbe61f](https://github.com/abougouffa/minemacs/commit/9cbe61fe5b0dd6c550a6b354e3f96fb24e306bbc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.7.1](https://github.com/abougouffa/minemacs/compare/41b7f6c16c5ac3e4490d2ad3ea5c28dd5d5cf843..v6.7.1) - 2024-05-26
#### Features
- **(core-ui)** move to `light-dashboard` successor `enlight` [#168] - ([41b7f6c](https://github.com/abougouffa/minemacs/commit/41b7f6c16c5ac3e4490d2ad3ea5c28dd5d5cf843)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.7.0](https://github.com/abougouffa/minemacs/compare/11f30450973c69fc05d878a834afe731be5e2269..v6.7.0) - 2024-05-26
#### Bug Fixes
- **(project-tab-groups)** correctly set tab group naming function - ([977fbab](https://github.com/abougouffa/minemacs/commit/977fbabe69a581ad075311d92ac27a048edea411)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ai)** initial support for `elisa` - ([0fe89d2](https://github.com/abougouffa/minemacs/commit/0fe89d2b6ff406638e66a5dc4eac53e1a14652e1)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** add benchmarking option using `benchmark-init` - ([7ab3307](https://github.com/abougouffa/minemacs/commit/7ab3307d9e668dc48dae930cf91bce6d848ad24c)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add support for `project-x` - ([6e1c164](https://github.com/abougouffa/minemacs/commit/6e1c164d3539b72a41cee16fb206773d0238e399)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add support for `magit-file-icons` - ([efb7c4f](https://github.com/abougouffa/minemacs/commit/efb7c4f04cddb6bceb034331a0df78b0c3a963d6)) - [@abougouffa](https://github.com/abougouffa)
- **(workspaces)** add support for `project-tab-groups` - ([e8e9d50](https://github.com/abougouffa/minemacs/commit/e8e9d507c59343a85e6c068ae60a28470ca71a6e)) - [@abougouffa](https://github.com/abougouffa)
- **(workspaces)** make `tabspaces` obsolete - ([313d2ff](https://github.com/abougouffa/minemacs/commit/313d2ffb8a6c9a5bf2ab205f114b49a34bbdf357)) - [@abougouffa](https://github.com/abougouffa)
- better lazy loading! - ([f2271f6](https://github.com/abougouffa/minemacs/commit/f2271f6e6ee705cbafdf90ab4f5c8a291942716d)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- make use of the builtin `locate-dominating-file` - ([9fcac8f](https://github.com/abougouffa/minemacs/commit/9fcac8fcaa9896b65c0dbe7b39193e27aa24b759)) - [@abougouffa](https://github.com/abougouffa)
- refactor and make use of `cl-callf` when possible - ([1658153](https://github.com/abougouffa/minemacs/commit/165815307395de20bd15391ceb527f5738442d01)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(eros)** minor edit - ([3cdda14](https://github.com/abougouffa/minemacs/commit/3cdda14f33cf537d1e3d6a94c64bdb19aba70518)) - [@abougouffa](https://github.com/abougouffa)
- **(gc)** minor tweaks and cleanup - ([42f93be](https://github.com/abougouffa/minemacs/commit/42f93beec2569558907d324b8a3ca54e8d272799)) - [@abougouffa](https://github.com/abougouffa)
- **(gc)** experimenting with `gc-cons-percentage` without `gcmh` - ([676a3b0](https://github.com/abougouffa/minemacs/commit/676a3b000fabf6af7fdb4c605ed6ec788a52683c)) - [@abougouffa](https://github.com/abougouffa)
- **(llm)** minor refactoring - ([dc4372f](https://github.com/abougouffa/minemacs/commit/dc4372f969e3d078e8dce47c7f48b27e4a047ac5)) - [@abougouffa](https://github.com/abougouffa)
- **(octave)** use `octave-maybe-mode` for `.m` - ([c03e197](https://github.com/abougouffa/minemacs/commit/c03e1978be79dc662c296b4713db6a973750664b)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** minor cleanup - ([3ddb276](https://github.com/abougouffa/minemacs/commit/3ddb2761d259ec6aa09a7bbbba2397e83e3a8f8d)) - [@abougouffa](https://github.com/abougouffa)
- **(org-agenda)** stick to the defaults for some options - ([11f3045](https://github.com/abougouffa/minemacs/commit/11f30450973c69fc05d878a834afe731be5e2269)) - [@abougouffa](https://github.com/abougouffa)
- **(verb)** add keybindings - ([f3d782f](https://github.com/abougouffa/minemacs/commit/f3d782f846ac3646bca242fd0694ac57b79ccddb)) - [@abougouffa](https://github.com/abougouffa)
- more lazy and deferred stuff - ([0c363e0](https://github.com/abougouffa/minemacs/commit/0c363e0a96844563e5173811affe00636d273a15)) - [@abougouffa](https://github.com/abougouffa)
- use `project-prompter` when relevant - ([4ab109a](https://github.com/abougouffa/minemacs/commit/4ab109a376cf084ff2ea45c61101e35cb732131b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d42ce53](https://github.com/abougouffa/minemacs/commit/d42ce53d85eba203fe5c66948afb89ce96596274)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.6.0](https://github.com/abougouffa/minemacs/compare/178a7403c9ef1c50e3c99e41662136b799c6138d..v6.6.0) - 2024-05-23
#### Bug Fixes
- **(keybindings)** bind `auto-insert` to the insert menu - ([178a740](https://github.com/abougouffa/minemacs/commit/178a7403c9ef1c50e3c99e41662136b799c6138d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(debug)** add support for `objdump-disassemble` - ([9887a12](https://github.com/abougouffa/minemacs/commit/9887a1213f106b5c78507a431c2a4a046827a23f)) - [@abougouffa](https://github.com/abougouffa)
- remove `me-binary` module and the `objdump` stuff - ([e3766b0](https://github.com/abougouffa/minemacs/commit/e3766b002da70bd87b488e88fee6f24d5080875a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(spdx)** add some insert keybindings - ([91e9ad2](https://github.com/abougouffa/minemacs/commit/91e9ad2960b3e2fd196f271fa38c26df67404697)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.5.0](https://github.com/abougouffa/minemacs/compare/2e3f35b4faab281ba2dc2a17c359774f64aa7f3a..v6.5.0) - 2024-05-23
#### Features
- **(core-ui)** make `spacious-padding` obsolete - ([078f79d](https://github.com/abougouffa/minemacs/commit/078f79d2c3672f95bd51fa35d2d543ce549b620f)) - [@abougouffa](https://github.com/abougouffa)
- **(gtd)** initial support for `org-gtd` - ([9ce4266](https://github.com/abougouffa/minemacs/commit/9ce4266a51416644e40bcc73d3593e214dc89c3c)) - [@abougouffa](https://github.com/abougouffa)
- **(gtd)** empty module for Getting Things Done workflow - ([5f4ea3f](https://github.com/abougouffa/minemacs/commit/5f4ea3fd7fd558461bc4f40e83762112b7583da0)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `add-node-modules-path` - ([d5f490e](https://github.com/abougouffa/minemacs/commit/d5f490e362359ef75b48ae96c7bfbbf4bb560a59)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** replace unused `ack` with `rg` - ([d0f9d63](https://github.com/abougouffa/minemacs/commit/d0f9d638298ee63720e1555fb98abd53a3f0aa23)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(use-package)** remove unnecessary `t` argument from `:demand` - ([1e91729](https://github.com/abougouffa/minemacs/commit/1e91729144b1d331ef5f35c84edb7a1ea0df7a23)) - [@abougouffa](https://github.com/abougouffa)
- add options file for `ctags` (to be used later) - ([5f8ef60](https://github.com/abougouffa/minemacs/commit/5f8ef60ea020e45c258ddd09fc8ac22f963315e6)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor edit - ([e5de394](https://github.com/abougouffa/minemacs/commit/e5de3945aae6e18b90254b76c4cb6fac252c2bf7)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(parinfer-rust)** remove temporary hack after being fixed upstream - ([8eda977](https://github.com/abougouffa/minemacs/commit/8eda977209fa6674e3ff72f716706bf7d2d11de8)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(c-mode)** use K&R style by default - ([d09be4b](https://github.com/abougouffa/minemacs/commit/d09be4b23a41d8f3ad2188cdf953d857863c0b3f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more ignored environment variables - ([21e13c8](https://github.com/abougouffa/minemacs/commit/21e13c803fc43bfdc0fe210d45d9d73080cf65ec)) - [@abougouffa](https://github.com/abougouffa)
- **(crm)** indicate in the prompt about `completing-read-multiple` - ([02cd1a0](https://github.com/abougouffa/minemacs/commit/02cd1a070e42cef7c494d7c6175cff7f6559ed02)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** take `vhdl-ts-mode` and `verilog-ts-mode` into account - ([2e3f35b](https://github.com/abougouffa/minemacs/commit/2e3f35b4faab281ba2dc2a17c359774f64aa7f3a)) - [@abougouffa](https://github.com/abougouffa)
- **(minibuffer)** don't move cursor to the prompt region - ([786a089](https://github.com/abougouffa/minemacs/commit/786a0897396338a3b0d46b1ab65cdfbfdb31a599)) - [@abougouffa](https://github.com/abougouffa)
- **(org-modern)** stick to the defaults for lists and stars - ([adc38d8](https://github.com/abougouffa/minemacs/commit/adc38d8e0fb0a7c888dd14ee0cc9a86e2e7dfa93)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** hook only to the base mode `lisp-data-mode` - ([ac06034](https://github.com/abougouffa/minemacs/commit/ac0603461ecf51994cc9ee12e3fdb9248d35318d)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** set `use-package` options after tweaking its settings - ([9cf959c](https://github.com/abougouffa/minemacs/commit/9cf959cbed343cdfaf067e07c1db689522e0d2f4)) - [@abougouffa](https://github.com/abougouffa)
- **(wgrep)** move from `me-completion` to `me-editor` - ([212c7a8](https://github.com/abougouffa/minemacs/commit/212c7a8ef5780deb30484a53d96c47a674e33671)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ce049d5](https://github.com/abougouffa/minemacs/commit/ce049d52b047e397d23839e1f1d1d7cb38c1dec9)) - [@abougouffa](https://github.com/abougouffa)
- enable `compilation-shell-minor-mode` in terminals - ([fb0e773](https://github.com/abougouffa/minemacs/commit/fb0e773f857f214ae461e189dcce5b1e731c00bc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.4.1](https://github.com/abougouffa/minemacs/compare/e09a2c2276f6a7901c4ea6ed882d801f81a500f8..v6.4.1) - 2024-05-22
#### Bug Fixes
- **(flymake-guile)** use a mirror on GitHub (Framagit issues) - ([9a3786a](https://github.com/abougouffa/minemacs/commit/9a3786a2bc6fa53f17343f78f4cf48897b9aa76f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core-ui)** enable the lightweight `light-dashboard` - ([f368331](https://github.com/abougouffa/minemacs/commit/f3683310a4dd2867870af658661b6a76cc182e24)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add support for `spdx` - ([46e78aa](https://github.com/abougouffa/minemacs/commit/46e78aac6bcad718c9d6c998b9397c8a71acffd4)) - [@abougouffa](https://github.com/abougouffa)
- **(email)** add initial support for `mu4e-crypto` - ([82a0716](https://github.com/abougouffa/minemacs/commit/82a0716740aeb901865b19dce4eb1771281c5f5e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor edits - ([d4349f2](https://github.com/abougouffa/minemacs/commit/d4349f291bce5604f0af719a720d6c70c634425a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** add an option to disable proxies via environment variable - ([e09a2c2](https://github.com/abougouffa/minemacs/commit/e09a2c2276f6a7901c4ea6ed882d801f81a500f8)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** move Elisp customization to a separate package - ([958462a](https://github.com/abougouffa/minemacs/commit/958462a9380dcefeb2aa4d753a5d11451408c34a)) - [@abougouffa](https://github.com/abougouffa)
- **(verb)** use the default keybinding - ([9c47e37](https://github.com/abougouffa/minemacs/commit/9c47e37154c67b56e1b3b75f62db85024484ba09)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.4.0](https://github.com/abougouffa/minemacs/compare/60d176a92768fceeda7a2f456830d63abaa75a09..v6.4.0) - 2024-05-21
#### Bug Fixes
- `defalias` (hence, `defun`) isn't guaranteed to return the name - ([5d7231b](https://github.com/abougouffa/minemacs/commit/5d7231bb8a75516cdd958029554a1deff42cae31)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- update the list of modules - ([5b06b53](https://github.com/abougouffa/minemacs/commit/5b06b53573c50a9c00e04933a444ce5aa7e13c98)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(bootstrap)** add support for `satch` and `once` - ([d23861e](https://github.com/abougouffa/minemacs/commit/d23861eda559ad254e0bdf21eb48df0a15a8f632)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** restore the `+eglot-help-at-point` command - ([ba2e628](https://github.com/abougouffa/minemacs/commit/ba2e628b6272fc6256e66dfe9f93a10d999cea87)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** extra performance tweaks via `+eglot-optimization-mode` - ([9ebb7b5](https://github.com/abougouffa/minemacs/commit/9ebb7b5579827f45fc37415ade64f6776c4dade7)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add initial support for `find-file-in-project` - ([9a215f3](https://github.com/abougouffa/minemacs/commit/9a215f3c96c1d0eadae31bbcc04b678bbad29d4b)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** add initial support for `ggtags - ([f5cbe58](https://github.com/abougouffa/minemacs/commit/f5cbe588935c5fd8b9622116176b9348596e0bf2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult-notes)** use ripgrep only if available - ([1a3f3c6](https://github.com/abougouffa/minemacs/commit/1a3f3c676f25598462232d34bd1a6cacae15e469)) - [@abougouffa](https://github.com/abougouffa)
- **(dumb-jump)** prefer `xref` interface over obsolete commands - ([60d176a](https://github.com/abougouffa/minemacs/commit/60d176a92768fceeda7a2f456830d63abaa75a09)) - [@abougouffa](https://github.com/abougouffa)
- **(edebug)** decouple from `elisp-mode` and inhibit bindings - ([c7f996f](https://github.com/abougouffa/minemacs/commit/c7f996f3ff525aeb2924282179fd98ef0c17c024)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** better performance by disabling logging to events buffer - ([82cfe06](https://github.com/abougouffa/minemacs/commit/82cfe062e56c9814324acc28ce12c168b64f2bdb)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** make it believe that `projectile` is available to get `fzf-project` - ([78e7d24](https://github.com/abougouffa/minemacs/commit/78e7d242b7984cb3c9b61c5b1575d7e92f3ca295)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** move non-LSP source code tagging packages to `me-tags` - ([4bcaef3](https://github.com/abougouffa/minemacs/commit/4bcaef38268c2578ec8b61d245ae17196c6ddbc1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7887c8c](https://github.com/abougouffa/minemacs/commit/7887c8c5c7fcbde06e799fb8692a1a16a90e224b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.3.0](https://github.com/abougouffa/minemacs/compare/440936fd8f30a299007bdbe3b7bfb0e1259da40d..v6.3.0) - 2024-05-21
#### Bug Fixes
- **(core)** use-package `::trigger-commands` implementation - ([95de9a5](https://github.com/abougouffa/minemacs/commit/95de9a5d272c4aa73d9b039132b7057a364c6f5b)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** fix the provided feature name - ([fb8eb8f](https://github.com/abougouffa/minemacs/commit/fb8eb8f9e853efe615cc19464e6f9d70ed6e5cbc)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** use `:autoload` instead of `:functions` in `use-package` - ([72f7ce1](https://github.com/abougouffa/minemacs/commit/72f7ce1bbbd3cad20a7b243b1fd64cc2e1c41b19)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** disable auto usage of fast strategy on long buffers - ([55432e1](https://github.com/abougouffa/minemacs/commit/55432e1e0ec6d06c0ee3c1ab1d536d21e546c063)) - [@abougouffa](https://github.com/abougouffa)
- **(pet)** hook `pet-mode` in Python - ([440936f](https://github.com/abougouffa/minemacs/commit/440936fd8f30a299007bdbe3b7bfb0e1259da40d)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** autoload `project-remember-projects-under` - ([90c0d95](https://github.com/abougouffa/minemacs/commit/90c0d954515d507ab99f5a45b0e735fc63bbbbf6)) - [@abougouffa](https://github.com/abougouffa)
- **(transient)** auto load some commonly used macros - ([bd6921b](https://github.com/abougouffa/minemacs/commit/bd6921b66791ccd00f520883605b80905249fea0)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** fix grammar recipe for XML - ([95c80b3](https://github.com/abougouffa/minemacs/commit/95c80b3325a21297278490ba1b1979a7ffe6d683)) - [@abougouffa](https://github.com/abougouffa)
- avoid problems on TTY only Emacs (via Termux) - ([98c4d23](https://github.com/abougouffa/minemacs/commit/98c4d23b798ce635455095f6d5f7e16cbb973cd6)) - [@abougouffa](https://github.com/abougouffa)
- better implementation of the first file hooks - ([5c606a0](https://github.com/abougouffa/minemacs/commit/5c606a06f5cc25860983c9c794edaed7dd786cdf)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([3a54d3e](https://github.com/abougouffa/minemacs/commit/3a54d3e4fb5106dfe1470955f132c3578c7ac876)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** don't add the banner to the documentation - ([f65d6bd](https://github.com/abougouffa/minemacs/commit/f65d6bd233e915cad9718828e84f34045f5e6534)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** regenerate the documentation - ([4b9d610](https://github.com/abougouffa/minemacs/commit/4b9d610b740d6b5cbe61fe821c24fc3276a3f539)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** regenerate documentation - ([258ca84](https://github.com/abougouffa/minemacs/commit/258ca849fd505d151451ca86972ee5f2b79aad16)) - [@abougouffa](https://github.com/abougouffa)
- add a comment for some TODOs - ([9e239c6](https://github.com/abougouffa/minemacs/commit/9e239c68fd09e4994311e239b7cbb7b5ae827327)) - [@abougouffa](https://github.com/abougouffa)
- better comments - ([1e5ceb7](https://github.com/abougouffa/minemacs/commit/1e5ceb7662229a936b5029f99490b98695061944)) - [@abougouffa](https://github.com/abougouffa)
- simpler banner in `init.el` - ([b97b2f8](https://github.com/abougouffa/minemacs/commit/b97b2f8c43857752016b8e14c939731b667bb104)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(checkers)** make `flymake-quickdef` and it dependents obsoletes - ([522a351](https://github.com/abougouffa/minemacs/commit/522a351d4b10df6a11604da612fc56b343ccb274)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+advice-once!` - ([63ba2a5](https://github.com/abougouffa/minemacs/commit/63ba2a5c342e23f097ef3d2756e68ee7651e94c6)) - [@abougouffa](https://github.com/abougouffa)
- **(debug)** add support for `beardbolt` - ([7b025e4](https://github.com/abougouffa/minemacs/commit/7b025e46960deaf5e71979e5cc5174f3b4e2bac3)) - [@abougouffa](https://github.com/abougouffa)
- **(debug)** add support for `rmsbolt` - ([276dd20](https://github.com/abougouffa/minemacs/commit/276dd20cadda265f5cc9776ab1bd93f57ca9e18c)) - [@abougouffa](https://github.com/abougouffa)
- **(dired)** add `+dired-here` - ([d91071d](https://github.com/abougouffa/minemacs/commit/d91071d0fee8fc2b17c43a8346877937a2299b3d)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add support for `zones` - ([caed009](https://github.com/abougouffa/minemacs/commit/caed009efc26e13b8adba27bbf9d76141e56181d)) - [@abougouffa](https://github.com/abougouffa)
- **(embedded)** add support for `vhdl-ts-mode` - ([35eb8e3](https://github.com/abougouffa/minemacs/commit/35eb8e313bf78409a7bdf09c795f06a69fb075c9)) - [@abougouffa](https://github.com/abougouffa)
- **(embedded)** add support for `verilog-ts-mode` - ([af14fd0](https://github.com/abougouffa/minemacs/commit/af14fd0b16be50236cf6273dd665d44baca3506f)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add my fork of `neotree` which supports `nerd-icons` - ([43445d8](https://github.com/abougouffa/minemacs/commit/43445d8060f5c05474d00669def633060d9de5a2)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-collection)** add several backends - ([5471c98](https://github.com/abougouffa/minemacs/commit/5471c9849b518245d1615a900e13db6624bafcff)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** add initial Matlab integration - ([8e02bce](https://github.com/abougouffa/minemacs/commit/8e02bcec6a01aa8d648c2bc3c53705378efa507f)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `zig-mode` - ([b4c9f59](https://github.com/abougouffa/minemacs/commit/b4c9f599fee96ed2104a86ad80044d61dabaef28)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `llvm-ts-mode` - ([805251c](https://github.com/abougouffa/minemacs/commit/805251c57caa8cebb93d3d1611f5a0ea8aaec966)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add support for `projection-dape` - ([b6140c3](https://github.com/abougouffa/minemacs/commit/b6140c3f2601752b2a378f3efe83175dbadef912)) - [@abougouffa](https://github.com/abougouffa)
- **(reformatter)** initial support for `reformatter` - ([8767b78](https://github.com/abougouffa/minemacs/commit/8767b78a32f98731916385b338e4e709a1b148b0)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add grammar for Zig - ([31e62f2](https://github.com/abougouffa/minemacs/commit/31e62f274c558536eed7be3d195936ed7d945c9c)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add a grammar for LLVM - ([07c5b50](https://github.com/abougouffa/minemacs/commit/07c5b50f45d0d9e6ad6fe07facc1cab656276e9c)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `lacarte` - ([b044298](https://github.com/abougouffa/minemacs/commit/b04429809a46de1f0a4df083674ecb3a45a40e32)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** add `:trigger-commands` option - ([9e72e96](https://github.com/abougouffa/minemacs/commit/9e72e96ace8c62a988e976ccdf9d3a6e2ff67669)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- provide feature in `me-gdb` - ([7e85327](https://github.com/abougouffa/minemacs/commit/7e85327da2d1d64d413df9cdeddcf2a3442da606)) - [@abougouffa](https://github.com/abougouffa)
- minor edit - ([8494462](https://github.com/abougouffa/minemacs/commit/8494462526f1f0bdf4b448e96c2257babafc806a)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(consult-eglot)** rewrite a condition - ([8502500](https://github.com/abougouffa/minemacs/commit/85025004f1965ee1b0c62dd6a0553b94eb56d6d3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unnecessary condition - ([9fc1774](https://github.com/abougouffa/minemacs/commit/9fc177431325a644a28956c9b0323c4918202dc9)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** better way of adding extra grammar recipes - ([ce0d766](https://github.com/abougouffa/minemacs/commit/ce0d7664d29913679385972188f272de5c7b45b4)) - [@abougouffa](https://github.com/abougouffa)
- move less used stuff from `me-lib` to `me-lib-extra` - ([2dc8d06](https://github.com/abougouffa/minemacs/commit/2dc8d069c4b0565f39fa0d2beade25d27c04b5b0)) - [@abougouffa](https://github.com/abougouffa)
- make some unused macros obsolete - ([4b12fba](https://github.com/abougouffa/minemacs/commit/4b12fbab1e385e8f1b5c08bc745452c8ca214eb7)) - [@abougouffa](https://github.com/abougouffa)
- rearrange customization groups - ([728d965](https://github.com/abougouffa/minemacs/commit/728d96573c77a0215e06fefbd1b589a8969c7d98)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(dired)** don't reinvent the wheel (`dired-jump`) - ([1ab230d](https://github.com/abougouffa/minemacs/commit/1ab230d3ade57564c66feecf1cc30b519bc59b7f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(battery)** defer displaying battery status - ([a5b3446](https://github.com/abougouffa/minemacs/commit/a5b3446a6b0cd29c1f832c74892e8aefd2bf14f3)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** make additional options custom instead of var - ([02f6694](https://github.com/abougouffa/minemacs/commit/02f6694793ee9a5cef625ee1ef714f27ca7add7e)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** remove obsolete variable - ([98538a9](https://github.com/abougouffa/minemacs/commit/98538a9b75950d143428595df2a01a068092b764)) - [@abougouffa](https://github.com/abougouffa)
- **(code-cells)** prefer `ein` only if not disabled - ([74a9f2d](https://github.com/abougouffa/minemacs/commit/74a9f2d314dc21639e85ca55548e6b466994add6)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edit - ([335a886](https://github.com/abougouffa/minemacs/commit/335a8862f291e6df8e6962d76856f64e3b44a2d3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better obsolescence message - ([37253c7](https://github.com/abougouffa/minemacs/commit/37253c7a7927de80349f3ef9b2240a619075d396)) - [@abougouffa](https://github.com/abougouffa)
- **(core-ui)** make `dashboard` obsolete (it slows startup) - ([8b22c5f](https://github.com/abougouffa/minemacs/commit/8b22c5f5cd314abaeb0e0f818a54e1426e31e05f)) - [@abougouffa](https://github.com/abougouffa)
- **(denote)** defer enabling `denote-rename-buffer-mode` - ([308fe71](https://github.com/abougouffa/minemacs/commit/308fe71e817bb61863d5389a20539083bf642ea0)) - [@abougouffa](https://github.com/abougouffa)
- **(dired)** better defaults - ([a4ff541](https://github.com/abougouffa/minemacs/commit/a4ff541adf9436ae44f31786d512d862b7d5883b)) - [@abougouffa](https://github.com/abougouffa)
- **(dired-aux)** ask before creating destination dirs - ([2bf7d2f](https://github.com/abougouffa/minemacs/commit/2bf7d2f53fd70f14ab83865e9486369ad4473d6d)) - [@abougouffa](https://github.com/abougouffa)
- **(dired-aux)** rename VC files with VC - ([db8a3eb](https://github.com/abougouffa/minemacs/commit/db8a3eb85564140a24363fcbc9a3af050ca77b4f)) - [@abougouffa](https://github.com/abougouffa)
- **(dired-x)** better defaults - ([cd05541](https://github.com/abougouffa/minemacs/commit/cd05541d7be0dfc524749bf93163087a075d3bd3)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-nasm)** don't enable by default - ([6829158](https://github.com/abougouffa/minemacs/commit/6829158edaf5276f2d4af297761d1e3e25933475)) - [@abougouffa](https://github.com/abougouffa)
- **(helpful)** remap `describe-*` to `helpful-*` equivalents - ([519da81](https://github.com/abougouffa/minemacs/commit/519da81725e95ae38f1781d60b5ce6a7274d6056)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** minor edits - ([7d62083](https://github.com/abougouffa/minemacs/commit/7d62083ee3cafd91e057eefca15d4e99a316e608)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-todos)** toggle via `SPC m t` - ([5d4c52f](https://github.com/abougouffa/minemacs/commit/5d4c52f330a3fa7760a0aa11a9b169c8ee03638c)) - [@abougouffa](https://github.com/abougouffa)
- **(matlab)** enable only when Matlab is installed on the system - ([634f531](https://github.com/abougouffa/minemacs/commit/634f53172d06555032025ae9b128490af2b66f26)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** load the `spell-fu` only if `jinx` isn't available - ([3a7edcf](https://github.com/abougouffa/minemacs/commit/3a7edcf64f1cfcb04354d9e9a86678ee156e1b9d)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete)** update `yasnippet` / `yasnippet-cape` settings - ([f807b36](https://github.com/abougouffa/minemacs/commit/f807b3680215ff55b26bf5c5bca05c432b2829ca)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** enable on more Lisp modes - ([2450032](https://github.com/abougouffa/minemacs/commit/2450032e492045adf537457bc44ecf3c940183a1)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `treesitter-context` obsolete - ([f34ef7e](https://github.com/abougouffa/minemacs/commit/f34ef7ea25e5189cf12736ca60955d87e79d04e9)) - [@abougouffa](https://github.com/abougouffa)
- **(transient)** don't load immediately - ([b338706](https://github.com/abougouffa/minemacs/commit/b338706ba0228d22e9e583847b6e75010e5e4120)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** minor edit - ([08e6629](https://github.com/abougouffa/minemacs/commit/08e66293806af7c375dc8d0624e2174c628828b8)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** don't add Nix, already added upstream - ([b2f9c1d](https://github.com/abougouffa/minemacs/commit/b2f9c1d04cf380c067de323b26be15429151221a)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `anzu` obsolete - ([5266669](https://github.com/abougouffa/minemacs/commit/52666692ebf1e970497add5604d24037e9f62ffe)) - [@abougouffa](https://github.com/abougouffa)
- **(vlf)** use ELPA instead of MELPA for an updated version - ([1b7ac7f](https://github.com/abougouffa/minemacs/commit/1b7ac7f1410646e7cab7e7aeb54265e6dd3d20a1)) - [@abougouffa](https://github.com/abougouffa)
- **(ztree)** use GNU ELPA mirror - ([e6f071b](https://github.com/abougouffa/minemacs/commit/e6f071b3b53351353ac46184118703cee79ade6d)) - [@abougouffa](https://github.com/abougouffa)
- faster startup by deferring more stuff - ([34f3323](https://github.com/abougouffa/minemacs/commit/34f3323705dafec6c5de1e59517ab02fc4084825)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4542ecb](https://github.com/abougouffa/minemacs/commit/4542ecb9e89e2b56129fe522d299315768c1ed06)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0a68ed8](https://github.com/abougouffa/minemacs/commit/0a68ed80fc09f4fa4416723607f8b5ffc531ff77)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([c8ec252](https://github.com/abougouffa/minemacs/commit/c8ec2526c7025cc481284e9fefc75b77cd896035)) - [@abougouffa](https://github.com/abougouffa)
- faster loading by deferring some stuff - ([d69d00d](https://github.com/abougouffa/minemacs/commit/d69d00d29c623b2c856534f949c44ce5fe9afff2)) - [@abougouffa](https://github.com/abougouffa)
- enhance startup time by correctly deferring some packages - ([3fee224](https://github.com/abougouffa/minemacs/commit/3fee2240bb1b97f0595d88877466d959d70d2169)) - [@abougouffa](https://github.com/abougouffa)
- correctly defer loading some packages + small cleanup - ([dd455b2](https://github.com/abougouffa/minemacs/commit/dd455b24a57dbdb8430d11efcf661e5495347a31)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6fa809e](https://github.com/abougouffa/minemacs/commit/6fa809efea2706bfc00bdbf1f44605c1d4be94dd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.2.2](https://github.com/abougouffa/minemacs/compare/e03e2d15fa668399d953d26c1c6da767a257cd84..v6.2.2) - 2024-05-16
#### Documentation
- **(external-tools)** regenerate - ([5b83f5a](https://github.com/abougouffa/minemacs/commit/5b83f5a5c77b1c765ce6b5b8e1359a6bb190f08a)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update screenshot - ([77a92a5](https://github.com/abougouffa/minemacs/commit/77a92a5da5bf19beca916e3dfa079ccc97702cfd)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** add more ignores - ([4703c6e](https://github.com/abougouffa/minemacs/commit/4703c6e870a6de99859ab9a38f357eec281c4fe5)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(multi-vterm)** move display buffer properties to `me-window` - ([596442a](https://github.com/abougouffa/minemacs/commit/596442a89888994d8887588294ab229609881763)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- add SQLite to the external tools - ([e03e2d1](https://github.com/abougouffa/minemacs/commit/e03e2d15fa668399d953d26c1c6da767a257cd84)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.2.1](https://github.com/abougouffa/minemacs/compare/a0b672b6d241f21b83cf429e44efc7627d5e25b4..v6.2.1) - 2024-05-16
#### Bug Fixes
- don't use `:ensure-system-package` (not available on Emacs 28) - ([a0b672b](https://github.com/abougouffa/minemacs/commit/a0b672b6d241f21b83cf429e44efc7627d5e25b4)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.2.0](https://github.com/abougouffa/minemacs/compare/76cc8b6e611ff3f54f7ecd76e9efe6d4f11406e1..v6.2.0) - 2024-05-16
#### Bug Fixes
- **(core)** fix implementation of `+with-proxies` and `+with-no-proxies` - ([2e5d123](https://github.com/abougouffa/minemacs/commit/2e5d1235072d23b0c0b8020d19e550390bee4e53)) - [@abougouffa](https://github.com/abougouffa)
- use proxies in async update - ([a20a23e](https://github.com/abougouffa/minemacs/commit/a20a23e6900a45d97ac6d32b52150281ef684ff4)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([76cc8b6](https://github.com/abougouffa/minemacs/commit/76cc8b6e611ff3f54f7ecd76e9efe6d4f11406e1)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(bootstrap)** add support for system packages dependencies - ([3494b59](https://github.com/abougouffa/minemacs/commit/3494b59bdaeb789f8b06cde35d9a799b75b328e5)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `pet` (Python Executable Tracker) - ([3492b7f](https://github.com/abougouffa/minemacs/commit/3492b7f867129f11fd65f0f3e1b2de857ab926f4)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(core)** restore old proxy behavior (issues with deferred stuff) - ([51eaafb](https://github.com/abougouffa/minemacs/commit/51eaafb339e37bb7bc3642617e7a3b15c0975822)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** revert to using global proxies - ([b288093](https://github.com/abougouffa/minemacs/commit/b288093574cc86b57053b2960162322bb328a320)) - [@abougouffa](https://github.com/abougouffa)
- **(envrc)** make use of `:ensure-system-package` - ([cbbdc14](https://github.com/abougouffa/minemacs/commit/cbbdc1459d184d03a8f057bdc3e7eabad5a5e080)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** obsolete `pyenv` and `pyvenv` (to be replaced with `pet`) - ([650b10c](https://github.com/abougouffa/minemacs/commit/650b10ce59ec7b99506f19699b94d321cc5a7cd8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([47c8810](https://github.com/abougouffa/minemacs/commit/47c881042056bd5553291bd0186587fb29fd0c98)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.1.1](https://github.com/abougouffa/minemacs/compare/ff4405287b9cf77e8a0e89dfbff193224c740e0d..v6.1.1) - 2024-05-15
#### Tweaks
- **(natural-langs)** make `go-translate` obsolete - ([4a34b9a](https://github.com/abougouffa/minemacs/commit/4a34b9a644eed05d562af0297fd4c401356ec685)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([ff44052](https://github.com/abougouffa/minemacs/commit/ff4405287b9cf77e8a0e89dfbff193224c740e0d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.1.0](https://github.com/abougouffa/minemacs/compare/87e6c5fb66291b3756791b8ead083c8f5a47165a..v6.1.0) - 2024-05-15
#### Bug Fixes
- **(core)** use `+with-proxies` when updating - ([0452e5c](https://github.com/abougouffa/minemacs/commit/0452e5c5284a4e3ac6884c9a48aeed18019ac10b)) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** do Evil stuff after loading Evil - ([b5f55ab](https://github.com/abougouffa/minemacs/commit/b5f55ab0d231c827bee897e2010c6c14ee1b46e1)) - [@abougouffa](https://github.com/abougouffa)
- apply Evil-specific tweaks only when Evil is loaded - ([96d917d](https://github.com/abougouffa/minemacs/commit/96d917dbe88f36ac535a2816dc67aa5d45c4b0be)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** better implementation of proxies - ([401c43b](https://github.com/abougouffa/minemacs/commit/401c43bff3b1f7671524bcd6014978d48af4f11e)) - [@abougouffa](https://github.com/abougouffa)
- **(data)** add `jq-mode` with `yq` integration for YAML - ([f678163](https://github.com/abougouffa/minemacs/commit/f6781634308da44320488070be972844b2552285)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for impostman - ([20bf7f2](https://github.com/abougouffa/minemacs/commit/20bf7f28afd16e7f6f30cd545fe86981d22c8a97)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for verb.el - ([d990039](https://github.com/abougouffa/minemacs/commit/d9900395ef48f8971ef7aabe8ee6a2ed215d4572)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** change the proxy handling mechanism (don't enable globally) - ([67bbfde](https://github.com/abougouffa/minemacs/commit/67bbfdee7b05f2653e59a46c21f3e636a88fb15e)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** regenerate loaddefs - ([87e6c5f](https://github.com/abougouffa/minemacs/commit/87e6c5fb66291b3756791b8ead083c8f5a47165a)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `osm` obsolete - ([da908cb](https://github.com/abougouffa/minemacs/commit/da908cbaac80116192e01b5ddf5fcca84c9c0897)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1801878](https://github.com/abougouffa/minemacs/commit/180187881047f10b713e5f1c43e52aac9eafb8fd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.7](https://github.com/abougouffa/minemacs/compare/26fae161fd85919ca47f7788e0254e32809f4763..v6.0.7) - 2024-05-11
#### Bug Fixes
- bootstrapping error when Emacs lack some builtin features - ([26fae16](https://github.com/abougouffa/minemacs/commit/26fae161fd85919ca47f7788e0254e32809f4763)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([1198fcc](https://github.com/abougouffa/minemacs/commit/1198fcc0544efac23c2f368af0a8e9cfdf8e9003)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.6](https://github.com/abougouffa/minemacs/compare/633137915c6433062f7eed16e5ae4fce01ff3a3a..v6.0.6) - 2024-05-03
#### Bug Fixes
- **(transient)** fix void symbol issue on Emacs 30 - ([624cc7e](https://github.com/abougouffa/minemacs/commit/624cc7eff18080f1799310a1beced28346af84d5)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([6331379](https://github.com/abougouffa/minemacs/commit/633137915c6433062f7eed16e5ae4fce01ff3a3a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(org)** remove unneeded `:after` blocks - ([b540f01](https://github.com/abougouffa/minemacs/commit/b540f01619b24df0107419eb63e2ac7c7cec3d60)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** register conditionally disabled packages - ([87e8b1a](https://github.com/abougouffa/minemacs/commit/87e8b1ab6554ff52da884a2b57a12c6905814132)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8a7fb31](https://github.com/abougouffa/minemacs/commit/8a7fb31eb8ac842820e7da8b6775e3b0face0b08)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.5](https://github.com/abougouffa/minemacs/compare/b88214b3f1dce738b23bc3c58a3e88584545c209..v6.0.5) - 2024-05-01
#### Revert
- **(early-init)** minor simplification (not working on Emacs 28.2) - ([b88214b](https://github.com/abougouffa/minemacs/commit/b88214b3f1dce738b23bc3c58a3e88584545c209)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.4](https://github.com/abougouffa/minemacs/compare/99fb7b1814c1edf34979a15e25f690bbe2ad4022..v6.0.4) - 2024-05-01
#### Documentation
- **(init)** update documentation - ([9450101](https://github.com/abougouffa/minemacs/commit/945010147866c43d103f6c9ab41f39e6411da04e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(early-init)** minor simplification - ([d5e9365](https://github.com/abougouffa/minemacs/commit/d5e9365b5e876e04b91a4fa35a6ce04aac16cf54)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(early-init)** remove duplicate parameters set in `spacious-padding` - ([99fb7b1](https://github.com/abougouffa/minemacs/commit/99fb7b1814c1edf34979a15e25f690bbe2ad4022)) - [@abougouffa](https://github.com/abougouffa)
- **(gerrit)** remove `gerrit-section`, uses obsolete functionalities - ([493a74e](https://github.com/abougouffa/minemacs/commit/493a74ed876163521633d6b60aeea718b38eacd0)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.3](https://github.com/abougouffa/minemacs/compare/557f0a74610d702962fed376bdc0d05be693650a..v6.0.3) - 2024-04-26
#### Bug Fixes
- **(auctex)** revert to the prvious revision (regression on Windows) - ([557f0a7](https://github.com/abougouffa/minemacs/commit/557f0a74610d702962fed376bdc0d05be693650a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(auctex)** pin to an older version due to an issue with Windows - ([747f779](https://github.com/abougouffa/minemacs/commit/747f779ee6018bb4af973cf97bb27a45c4a1d162)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d51e6c7](https://github.com/abougouffa/minemacs/commit/d51e6c7d7169dfe8c3b49e9075b1c883cd635a68)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.2](https://github.com/abougouffa/minemacs/compare/1fa52656297bb4a3ee185c0dfcb1b47205e83685..v6.0.2) - 2024-04-25
#### Tweaks
- **(dirvish)** don't show Git status - ([60c24a1](https://github.com/abougouffa/minemacs/commit/60c24a1b9b2cb0fb1d86b4244d6ae13d04a85323)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** don't show Git messages - ([1fa5265](https://github.com/abougouffa/minemacs/commit/1fa52656297bb4a3ee185c0dfcb1b47205e83685)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3e182d8](https://github.com/abougouffa/minemacs/commit/3e182d80aefabaceef3782540fbf956d7159ded1)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.1](https://github.com/abougouffa/minemacs/compare/eb99c88d5fe072510755be197b9ff412a0c7c6ac..v6.0.1) - 2024-04-22
#### Documentation
- **(documentation)** regenerate the documentation - ([eb99c88](https://github.com/abougouffa/minemacs/commit/eb99c88d5fe072510755be197b9ff412a0c7c6ac)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([67bade8](https://github.com/abougouffa/minemacs/commit/67bade8584af0b220477405984fbc0e2a3332c53)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.0](https://github.com/abougouffa/minemacs/compare/6c80599d2092afd6dd8ac1e8d0c424172b270bd2..v6.0.0) - 2024-04-15
#### Tweaks
- **(skel)** update modules list - ([4b9a0c4](https://github.com/abougouffa/minemacs/commit/4b9a0c4068b8646345ab83335b5232db637efe03)) - [@abougouffa](https://github.com/abougouffa)
- bump `tabspaces` version - ([c43f0e7](https://github.com/abougouffa/minemacs/commit/c43f0e7752c7b60d31a98cca1e6840b0a12b7a8e)) - [@abougouffa](https://github.com/abougouffa)
- make `lsp` + `dap` obsolete in favor of `eglot` + `dape` - ([6c80599](https://github.com/abougouffa/minemacs/commit/6c80599d2092afd6dd8ac1e8d0c424172b270bd2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.2.0](https://github.com/abougouffa/minemacs/compare/3b3ac4820f1cae822ecf0926c46cc2f5e46e483c..v5.2.0) - 2024-04-15
#### Documentation
- regenerate the documentation - ([ee6866c](https://github.com/abougouffa/minemacs/commit/ee6866c1bb0a43f4507c997f9d39525bfd1632f7)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** don't run tests on Emacs 28.1 & 29.1 - ([92a2467](https://github.com/abougouffa/minemacs/commit/92a24679e205b4113a0b59a8c7ed588e7f45ba80)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** cleanup extra stuff to avoid problems - ([f08045e](https://github.com/abougouffa/minemacs/commit/f08045e831ec5a6b51d7c1fdbad093bc581f0313)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(core)** remove an old fix intended for Emacs 29.1 - ([233b4ed](https://github.com/abougouffa/minemacs/commit/233b4edd5070216d5df52c6be217129721337c1d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(biblio)** make `zotxt` obsolete, never really used! - ([cd1ae59](https://github.com/abougouffa/minemacs/commit/cd1ae5961def3070c192b17ddf38cfe0a68677bb)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** minor performance tweaks - ([3b3ac48](https://github.com/abougouffa/minemacs/commit/3b3ac4820f1cae822ecf0926c46cc2f5e46e483c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** don't auto cleanup MinEmacs' directory - ([58cbde9](https://github.com/abougouffa/minemacs/commit/58cbde95f58a41ed15ba1ce7dc7b86fb6be9014b)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unused `+sensitive-data-mode` - ([592b335](https://github.com/abougouffa/minemacs/commit/592b3351226de3026ea361b428d1b663a6f2af43)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unneeded `+package-download-from-urls` - ([0e0c070](https://github.com/abougouffa/minemacs/commit/0e0c070613d9c2cb3338b47efd43b8f27c395a22)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** bind `dirvish-side` - ([8891bf3](https://github.com/abougouffa/minemacs/commit/8891bf3609505b0d17f7390a0fadf59293712e0b)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** make `elisp-demos` obsolete - ([dcd02af](https://github.com/abougouffa/minemacs/commit/dcd02af4095e8e9ebb70e6a3582a671305e29a04)) - [@abougouffa](https://github.com/abougouffa)
- **(envrc)** prevent an annoying error when `direnv` isn't installed - ([1250c6b](https://github.com/abougouffa/minemacs/commit/1250c6b8906fdcd6036bf249ea8bbf36bc5824cc)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `treemacs` obsolete - ([1b9b00c](https://github.com/abougouffa/minemacs/commit/1b9b00c189ba7375643aac47ce0d08e0654d9d12)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** remove unused `sr-speedbar` - ([401d4bc](https://github.com/abougouffa/minemacs/commit/401d4bcd653bf04b4658cc4d04bd0b8f59ac9717)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add a LaTeX class for MPDI template - ([8dd590d](https://github.com/abougouffa/minemacs/commit/8dd590d72f5b8d3ca4b0ef41bdb59a7c560b596b)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `nix-update` & `guix` obsolete - ([6401380](https://github.com/abougouffa/minemacs/commit/6401380334f81776382e3b869561f2359f61912b)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `bitwarden` obsolete - ([9d35669](https://github.com/abougouffa/minemacs/commit/9d35669eaa923f0b3de49c0a90794cc684d81082)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** make `code-review` obsolete - ([868e99f](https://github.com/abougouffa/minemacs/commit/868e99fac9b8615b709eb32edc42b7b5ba2a6129)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([77e928a](https://github.com/abougouffa/minemacs/commit/77e928aec2e6c3cb6c2ee75b6c9907b11951b4f4)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.1.3](https://github.com/abougouffa/minemacs/compare/bc6b6de407e74932d059b9b0ccf57d487b8f3abf..v5.1.3) - 2024-04-04
#### Bug Fixes
- **(protobuf-ts-mode)** use a working repo - ([bc6b6de](https://github.com/abougouffa/minemacs/commit/bc6b6de407e74932d059b9b0ccf57d487b8f3abf)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** add Emacs 29.3 to the CI matrix - ([3caff17](https://github.com/abougouffa/minemacs/commit/3caff17d9c681a865dc9422b6f82ab53b9d2cf25)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(vc)** remove unneeded `with-eval-after-load` - ([937d19d](https://github.com/abougouffa/minemacs/commit/937d19d29704d3a68302e2c69439760fbb81115e)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(selection-highlight)** enable after startup - ([8636072](https://github.com/abougouffa/minemacs/commit/8636072e1238f1bdf18bc22f36f9d638e5d6b03e)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add ts modes to `auto-mode-alist` - ([d6a0227](https://github.com/abougouffa/minemacs/commit/d6a022738ea76e406e2bc2116d32ae1cd429ebda)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([035dbc6](https://github.com/abougouffa/minemacs/commit/035dbc6a2a19376ab9d41b156037f05b8efdb8bf)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([34d5edd](https://github.com/abougouffa/minemacs/commit/34d5edd038adf92432088479bc42c46fc8a9df50)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.1.2](https://github.com/abougouffa/minemacs/compare/v5.1.1..v5.1.2) - 2024-03-13
#### Bug Fixes
- **(combobulate)** temporary disable on Windows - ([9068009](https://github.com/abougouffa/minemacs/commit/9068009439e9a8aa81d80829a021a74686950e6e)) - [@abougouffa](https://github.com/abougouffa)
- **(combobulate)** disable the problematic `html-ts-mode` submodule - ([40994e9](https://github.com/abougouffa/minemacs/commit/40994e9297ac3c5e812349083688bc718378e0e2)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** hide the vertical bar correctly - ([c784e12](https://github.com/abougouffa/minemacs/commit/c784e1277e68a5a35c2aeed447e49c3546f200a9)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** add support for searching the current project via `project.el` - ([273f0db](https://github.com/abougouffa/minemacs/commit/273f0db83fc996e2e438c555ad7b9b2b761681d7)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** fix FAQ link - ([551c9b2](https://github.com/abougouffa/minemacs/commit/551c9b23bdb18ae28d89a0bac238a27cd0e71623)) - Ezequiel Birman
- **(selection-highlight)** delay enabling it by 2.0 to avoid issues - ([d135281](https://github.com/abougouffa/minemacs/commit/d13528142605576c4a9a9e0ecc8bb0f7592e9808)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** document `MINEMACS_LOAD_ALL_MODULES` - ([74331b0](https://github.com/abougouffa/minemacs/commit/74331b0678918f9341e0848cb4ebec074430925a)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ui)** add `logos` - ([9095fcc](https://github.com/abougouffa/minemacs/commit/9095fcc1658e5feb1e1537c16ff1f8c01922a26d)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** initial support for `spacious-padding` - ([ad8a44d](https://github.com/abougouffa/minemacs/commit/ad8a44d7c0143f8657cf4b3058c4ea2735d76533)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `solaire-mode` obsolete - ([b5c958f](https://github.com/abougouffa/minemacs/commit/b5c958f195d5736a394b55e26c1e696cef56dc5b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** remove scratch project from `project-prefix-map` - ([32fbfa8](https://github.com/abougouffa/minemacs/commit/32fbfa8863f0a6760f172ae8982a990e0be5926b)) - [@abougouffa](https://github.com/abougouffa)
- **(denote)** remove obsolete configs - ([a912ab8](https://github.com/abougouffa/minemacs/commit/a912ab812f8452c9b4d1197016567dff9f7f38bd)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** bind `find-project` to `SPC /` (analogue to `SPC :`) - ([fdb62bf](https://github.com/abougouffa/minemacs/commit/fdb62bf65e0f42a10ceadc82b11a0a7f165cb50d)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-vterm)** remap `multi-vterm-project` to `project-shell` - ([b00936a](https://github.com/abougouffa/minemacs/commit/b00936a995eca53aa4efd462d279bb71fd8b4678)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** declare `treesit` as pseudo package when TS is available - ([97f679c](https://github.com/abougouffa/minemacs/commit/97f679c7f7b6aea62b51bd5422861ffe70cf3443)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto+html-ts-mode)** pin HTML grammar to v0.20.1 - ([fd4994d](https://github.com/abougouffa/minemacs/commit/fd4994ddba3f70260286248f377dca8881e39e37)) - [@abougouffa](https://github.com/abougouffa)
- **(treesitter-context)** suppress `treesitter-context-fold-mode` - ([6905f23](https://github.com/abougouffa/minemacs/commit/6905f2394fd076123760b7b3689b0d7b18e1957b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.1.1](https://github.com/abougouffa/minemacs/compare/v5.1.0..v5.1.1) - 2024-03-03
#### Bug Fixes
- **(denote)** remove obsolete - ([553097a](https://github.com/abougouffa/minemacs/commit/553097af1047ad8166ab1518e1c40319689c43b3)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** add `cc-isearch-menu` - ([3be4ef2](https://github.com/abougouffa/minemacs/commit/3be4ef2c74b6b1eb70d16be274a0cc7e2d6ed61f)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** use `treesitter-context-fold` for code folding - ([caabcf8](https://github.com/abougouffa/minemacs/commit/caabcf86864283415fb41213dcd95a9f561085d9)) - [@abougouffa](https://github.com/abougouffa)
- make the code folding spaghetti code obsolete - ([aa8dbb2](https://github.com/abougouffa/minemacs/commit/aa8dbb26970adbb57ae954519c3b5049dd8309f7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(rust-mode)** enable deriving from `treesit` when available - ([34a0cae](https://github.com/abougouffa/minemacs/commit/34a0caed7961106a015fb6fe9613adb109136712)) - [@abougouffa](https://github.com/abougouffa)
- **(treesitter-context)** remove old hack (merged upstream) - ([a0c8b3d](https://github.com/abougouffa/minemacs/commit/a0c8b3dfc6e448088cc27e9080a9ad6902fc8a0f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7bbcb13](https://github.com/abougouffa/minemacs/commit/7bbcb1391c97b4e2200f429466226c3ac32770d5)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.1.0](https://github.com/abougouffa/minemacs/compare/v5.0.1..v5.1.0) - 2024-02-27
#### Bug Fixes
- **(eldoc-box)** do not enable in terminal mode - ([10f660c](https://github.com/abougouffa/minemacs/commit/10f660cfbb47e7057945ea88bc6cf88b48988ca7)) - [@abougouffa](https://github.com/abougouffa)
- **(pdfgrep)** autoload commands + require `pdf-isearch` - ([70ce45c](https://github.com/abougouffa/minemacs/commit/70ce45c2b9648078e0b4c9889bd90ad00ddc0744)) - [@abougouffa](https://github.com/abougouffa)
- **(treesitter-context)** correctly set colors variables - ([9fc6b46](https://github.com/abougouffa/minemacs/commit/9fc6b467b62eaa45551e2f981273ce4236056647)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(external-tools)** regenerate the list - ([eab6060](https://github.com/abougouffa/minemacs/commit/eab6060e317677f53ccfdf660125d8aeb1e8e376)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** regenerate the list - ([a394f8e](https://github.com/abougouffa/minemacs/commit/a394f8eac1f4735a2346ac10cef890dcd9513c0c)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(tools)** add `fzf` support - ([fcc6358](https://github.com/abougouffa/minemacs/commit/fcc63583d46a196580a438229fe215971ae657ae)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** setup tooltip font in `+setup-fonts` - ([d4b608d](https://github.com/abougouffa/minemacs/commit/d4b608da55772296007b2ce5665e9a90e8da1977)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** add `fzf` - ([d3d7d7c](https://github.com/abougouffa/minemacs/commit/d3d7d7cf69ea6d3036149a453f4b3e8a0588b353)) - [@abougouffa](https://github.com/abougouffa)
- **(nix-update)** install despite `nix` availability - ([1454e75](https://github.com/abougouffa/minemacs/commit/1454e75338f5a1dca5cef9952aad0e4e8856838d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f69310b](https://github.com/abougouffa/minemacs/commit/f69310b051abe6be4f0a178d15f91871b45314ff)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.0.1](https://github.com/abougouffa/minemacs/compare/v5.0.0..v5.0.1) - 2024-02-25
#### Bug Fixes
- **(email)** temporary disable `org-msg` until fixed for `mu` 1.12.0 - ([e317cfa](https://github.com/abougouffa/minemacs/commit/e317cfaaafbdab35b2edbacb000dc000d57dd43a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** make msgs of `minemacs-enable-proxy` logs instead of infos - ([68a821b](https://github.com/abougouffa/minemacs/commit/68a821b10af85cc50ba184bc93f322058ed951ba)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** use a less intrusive face for threads - ([52d8768](https://github.com/abougouffa/minemacs/commit/52d8768465046328c3a32293c43bca90648ed5cc)) - [@abougouffa](https://github.com/abougouffa)
- **(sr-speedbar)** disable icons - ([41f0f31](https://github.com/abougouffa/minemacs/commit/41f0f31ca3642b72e7d230fb2cb9ddd3dcabd8d9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.0.0](https://github.com/abougouffa/minemacs/compare/v4.16.4..v5.0.0) - 2024-02-23
#### Bug Fixes
- **(compile-multi)** must be installed before `projection` - ([001428d](https://github.com/abougouffa/minemacs/commit/001428d3a4b3b9efe26a5993c190704d6d714977)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** do not redefine the segments - ([76603aa](https://github.com/abougouffa/minemacs/commit/76603aab1be29aff12cf60ff2ceffaf6214ea1b3)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** run bump and upgrade asynchronously - ([445ba4b](https://github.com/abougouffa/minemacs/commit/445ba4b90304319017b01131a07d905fe26f1414)) - [@abougouffa](https://github.com/abougouffa)
- **(docs)** add `pdfgrep` - ([12872d2](https://github.com/abougouffa/minemacs/commit/12872d2a71bb8351b8ce6b9c030976f3429da748)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `makefile-executor` obsolete - ([f42dc43](https://github.com/abougouffa/minemacs/commit/f42dc43005997e8d003ce0e35af88b0925ccdf20)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add `projection` for better out-of-the-box experience - ([35855b2](https://github.com/abougouffa/minemacs/commit/35855b2d1d99aecfc04de0d49623a47c7a476389)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** make `project-cmake` obsolete - ([c3630f7](https://github.com/abougouffa/minemacs/commit/c3630f7cb30a68c8f7cb2d14e8fce8fa72e57079)) - [@abougouffa](https://github.com/abougouffa)
- replace `ibuffer-project` with `projection-ibuffer` - ([baaa638](https://github.com/abougouffa/minemacs/commit/baaa6389a7295826bb764bba5aa39f83aacb058a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(prog)** remove the unused `posframe-plus` project - ([bb8e43e](https://github.com/abougouffa/minemacs/commit/bb8e43eb535ed3222ea339ce8f2cbf8ec2fa0ab4)) - [@abougouffa](https://github.com/abougouffa)
- remove obsolete commands - ([79ec179](https://github.com/abougouffa/minemacs/commit/79ec179e42c3e54f25130903dd307deccf3fff9d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([393ce5b](https://github.com/abougouffa/minemacs/commit/393ce5baaf50fe83d095af4fba38ab7b4352c59c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([420c43e](https://github.com/abougouffa/minemacs/commit/420c43ee53f9e48c7ee3135fb9c98c2ae374f446)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.16.4](https://github.com/abougouffa/minemacs/compare/v4.16.3..v4.16.4) - 2024-02-20
#### Features
- **(prog)** add an experimental support for Clink - ([0db4963](https://github.com/abougouffa/minemacs/commit/0db49635fdd76cf4a42126e0e25afbdaf8abf98f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(clink)** auto enable when available - ([cc19a02](https://github.com/abougouffa/minemacs/commit/cc19a029f61224dfc4323103dd63e1d43af4ec29)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** bump versions of `cpptools` and `code-debug` - ([39c2465](https://github.com/abougouffa/minemacs/commit/39c2465fc37e0e72842142d0e0374fbb5dada43b)) - [@abougouffa](https://github.com/abougouffa)
- **(gdb)** minor edits in the GDB integration - ([32f1afc](https://github.com/abougouffa/minemacs/commit/32f1afccceae48e697360f1b03e6ba0bdad791d2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bdc8039](https://github.com/abougouffa/minemacs/commit/bdc803906cfad73ca7fa0aabd178af335cd87659)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([06f7d26](https://github.com/abougouffa/minemacs/commit/06f7d26b24f5a47088658d7ccc27eb5fe0a1d239)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.16.3](https://github.com/abougouffa/minemacs/compare/v4.16.2..v4.16.3) - 2024-02-16
#### Bug Fixes
- **(backup)** remove dead code and avoid errors when viewing backups - ([d87b73d](https://github.com/abougouffa/minemacs/commit/d87b73d6cf48601cb1e4a6444fc57c8489fbe6db)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(real-backup)** make `real-backup` as a separate package - ([f892016](https://github.com/abougouffa/minemacs/commit/f892016b74a7dc0b16dfad1f727fbbb2bf9604bc)) - [@abougouffa](https://github.com/abougouffa)
- **(real-backup)** bump package, new version with new features - ([bf02dc2](https://github.com/abougouffa/minemacs/commit/bf02dc2c9b52ec8e74031ae35727cf81d54ce920)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.16.2](https://github.com/abougouffa/minemacs/compare/v4.16.1..v4.16.2) - 2024-02-15
#### Tweaks
- **(backup)** rename buffer when viewing a backup file - ([4a8568a](https://github.com/abougouffa/minemacs/commit/4a8568ad0ff31623a593675c87c0e3dfb61f069f)) - [@abougouffa](https://github.com/abougouffa)
- **(backup)** autoload - ([151b527](https://github.com/abougouffa/minemacs/commit/151b527f3cb5cf1501d662d06a304e035dcee17f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8f24e85](https://github.com/abougouffa/minemacs/commit/8f24e857b7a1fb9a9626a302817e005f72a17613)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([a14238a](https://github.com/abougouffa/minemacs/commit/a14238ad1ae70af2ad7de4d3a557cba3e4406bdd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.16.1](https://github.com/abougouffa/minemacs/compare/v4.16.0..v4.16.1) - 2024-02-15
#### Bug Fixes
- **(treesit-auto)** do not create parsers for non-installed grammars - ([a5be915](https://github.com/abougouffa/minemacs/commit/a5be9159e56b19adda72c5bf69b6e4147cecfaa6)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(backup)** bump version, update docs - ([8ec19bc](https://github.com/abougouffa/minemacs/commit/8ec19bcc5948258879d20c791b70116d0dd1457b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(backup)** signal an error if in non-visiting buffer - ([a7c845c](https://github.com/abougouffa/minemacs/commit/a7c845ca83dbfb507b43372ddb3f50f9b3f629d2)) - [@abougouffa](https://github.com/abougouffa)
- **(backup)** more tweaks and features for `backup-each-save` - ([b5a6ffb](https://github.com/abougouffa/minemacs/commit/b5a6ffb6ff61874368e680344287fbec28da1080)) - [@abougouffa](https://github.com/abougouffa)
- **(backup-each-save)** add backup cleanup support - ([65656f4](https://github.com/abougouffa/minemacs/commit/65656f440b3a66c3066f1fd084bd44716156b532)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.16.0](https://github.com/abougouffa/minemacs/compare/v4.15.0..v4.16.0) - 2024-02-14
#### Tweaks
- **(treesit-auto)** enable treesit parsers even in non-treesit modes - ([8af4ad8](https://github.com/abougouffa/minemacs/commit/8af4ad8fc0e7dee5fa6b8e7212cf4a18cdf1efcc)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** install `elisp` grammar - ([2422c85](https://github.com/abougouffa/minemacs/commit/2422c854be84c13478dcaa895018834c92b1ed9d)) - [@abougouffa](https://github.com/abougouffa)
- **(ts-movement)** better detection of treesit enabled modes - ([09b365d](https://github.com/abougouffa/minemacs/commit/09b365d210e43a1fbf0bb0dc089dfd54a5e355b6)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.15.0](https://github.com/abougouffa/minemacs/compare/v4.14.0..v4.15.0) - 2024-02-14
#### Bug Fixes
- **(ecryptfs)** `epa` needed if `ecryptfs-mount-private` is invoked early - ([d4a52d1](https://github.com/abougouffa/minemacs/commit/d4a52d1a6ba4ae674b819fb4a37ef6a826d221f7)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+sensitive-data-mode` - ([fec482a](https://github.com/abougouffa/minemacs/commit/fec482ae957b0894fa2533e4aa19519e127a3405)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `devdocs` - ([94c0654](https://github.com/abougouffa/minemacs/commit/94c0654952a6dfac0a1af18996263a8f94a0e753)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add Protocol Buffers support - ([12e8494](https://github.com/abougouffa/minemacs/commit/12e8494e53386b22ee92008b90e9c9bf7f2597e8)) - [@abougouffa](https://github.com/abougouffa)
- **(rfc-mode)** add a mode to download and display RFCs - ([707d80c](https://github.com/abougouffa/minemacs/commit/707d80ce3c37d4b510ae93212db2a8f53a06c966)) - [@abougouffa](https://github.com/abougouffa)
- integrate a tweaked version of `backup-each-save` - ([bd0a364](https://github.com/abougouffa/minemacs/commit/bd0a364bcffbd922b2fd2566907f8d1bdaa4ec11)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(dockerfile)** add more tools (WIP) - ([e78a7a5](https://github.com/abougouffa/minemacs/commit/e78a7a5bd7f17983fe68e90a8f620e7c479cab30)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** fix the `locked` rule - ([23c1a74](https://github.com/abougouffa/minemacs/commit/23c1a74f01db69b98f216a3940b6e624479a6376)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** update all packages on `make update` - ([c583a41](https://github.com/abougouffa/minemacs/commit/c583a41c39ba410923e9407194be8c607a93ca66)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(citre)** load after the first C/C++ file - ([ebc4431](https://github.com/abougouffa/minemacs/commit/ebc4431e386b4c1b3ed490535d3efd02ab654cd8)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** ignore `.repo` when generating files list - ([7cc4608](https://github.com/abougouffa/minemacs/commit/7cc4608f988c2fce5321f101b3b503a632ad9e95)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** add a command to generate `gtags.files` - ([ed5bbee](https://github.com/abougouffa/minemacs/commit/ed5bbeef1430248808593cf7a8702ac80118b046)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** decrease default font size - ([a8ce083](https://github.com/abougouffa/minemacs/commit/a8ce083b24079f89fb2422af1f8d093bc34d11a7)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** no need for version check, fixed upstream - ([a2e2d82](https://github.com/abougouffa/minemacs/commit/a2e2d826a2f79e7fb65e5e7aad031f3bfdaf0ace)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-todos)** do not enable (too slow on big code bases) - ([e845bcb](https://github.com/abougouffa/minemacs/commit/e845bcb0bd383296f689c20ac26dfe64827e105d)) - [@abougouffa](https://github.com/abougouffa)
- **(vertico)** enable `vertico-mouse-mode` - ([48b10dd](https://github.com/abougouffa/minemacs/commit/48b10dd6d059934ec4b6b7bed4a910b91a1be83e)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** change `frame-title-format` - ([0ea96fb](https://github.com/abougouffa/minemacs/commit/0ea96fb43abaef7610219b36825fd85e61493371)) - [@abougouffa](https://github.com/abougouffa)
- update packages versions - ([b6963a8](https://github.com/abougouffa/minemacs/commit/b6963a85f96a88bfa387ae19cc6517b05261d1ce)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9738ac4](https://github.com/abougouffa/minemacs/commit/9738ac4e1c36b1f0bfa3b438993be5e34258098d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ee40277](https://github.com/abougouffa/minemacs/commit/ee40277b86243118460277cef006860263be916f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.14.0](https://github.com/abougouffa/minemacs/compare/v4.13.3..v4.14.0) - 2024-02-08
#### Bug Fixes
- **(dape)** disable on Emacs 28 (requires new `jsonrpc`) - ([a2c8f98](https://github.com/abougouffa/minemacs/commit/a2c8f98724dd0d34761f31e24d628edb0ee34b8b)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([e78ef9e](https://github.com/abougouffa/minemacs/commit/e78ef9e907373bb427a968641d33a52ef688a995)) - [@abougouffa](https://github.com/abougouffa)
- regenerate documentation - ([b1d3779](https://github.com/abougouffa/minemacs/commit/b1d37791a8f5cbae8e713e8bbd2b69860bedc383)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add proxy setup - ([06fc5ec](https://github.com/abougouffa/minemacs/commit/06fc5ec32ab0c0efff7c3c97bbee0bc806a74ec9)) - [@abougouffa](https://github.com/abougouffa)
- **(embedded)** add `+serial-run-command-on-host` - ([c200b53](https://github.com/abougouffa/minemacs/commit/c200b537d9d36d322c7c3e51b98aae12493b3166)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `treesitter-context` - ([a22fd0b](https://github.com/abougouffa/minemacs/commit/a22fd0bc0d57cb84dcd188f4776180f9884eed85)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `breadcrumb` - ([5788292](https://github.com/abougouffa/minemacs/commit/5788292636b1d8175deae07e451efb784b7d3103)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** automatically enable proxies when set - ([bf1f0be](https://github.com/abougouffa/minemacs/commit/bf1f0be3a69aad06ae026fff4181198f4a3adf45)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** move `+serial-*` to `me-lib`, several enhancements - ([20a41ee](https://github.com/abougouffa/minemacs/commit/20a41eee1c648ecbd0a04f354b32b818b9d2e236)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-quickdef)** include docstring in the generated function - ([e9db03d](https://github.com/abougouffa/minemacs/commit/e9db03d630f7cf0bf7b8a9623a848d321155cb7f)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-quickdef)** better args handling in `flymake-clang-tidy` - ([ff6c407](https://github.com/abougouffa/minemacs/commit/ff6c407fbbd7cab063cd04ff86c915bab113fce6)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([664a3b2](https://github.com/abougouffa/minemacs/commit/664a3b2e28f7dfc15cff125673706ad9b6fe4cbc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.13.3](https://github.com/abougouffa/minemacs/compare/v4.13.2..v4.13.3) - 2024-02-04
#### Bug Fixes
- **(core)** buggy `+package-disabled-p` - ([1da5b30](https://github.com/abougouffa/minemacs/commit/1da5b306a6fbfb9fdb149d8fe1a4e7c1cb8cff2e)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(external-tools)** regenerate - ([5db1bb2](https://github.com/abougouffa/minemacs/commit/5db1bb2b234a71135a2124a0d1d0933cedbf38fa)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult-eglot)** make use of `+package-disabled-p` - ([d2c45d1](https://github.com/abougouffa/minemacs/commit/d2c45d1b7d94f17bce82dd1813d59105a465bcb1)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add an option to check for modules in `+package-disabled-p` - ([c348bbb](https://github.com/abougouffa/minemacs/commit/c348bbb0089e3b7954d2c8ec3c29a0e689555cb9)) - [@abougouffa](https://github.com/abougouffa)
- **(ellama)** auto load installed models - ([0a6f71d](https://github.com/abougouffa/minemacs/commit/0a6f71d848de19033c108203da0b430b69cbb63e)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** make use of `+package-disabled-p` to control installing evil packages - ([3d69084](https://github.com/abougouffa/minemacs/commit/3d690840039bc9a3471fb33b3f3a4ea08475e0d4)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** update the list - ([eef78cc](https://github.com/abougouffa/minemacs/commit/eef78cc4544e15a2e9e78fbbc227420d1c8be703)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-quickdef)** more accurate regexp in `flymake-clang-tidy` - ([c674dd2](https://github.com/abougouffa/minemacs/commit/c674dd2e2e814f44d155c02862a7e1983794ad20)) - [@abougouffa](https://github.com/abougouffa)
- **(xcscope)** disable on Windows - ([ee7db68](https://github.com/abougouffa/minemacs/commit/ee7db68a7f0e607d33750c9986c36c052b9692cf)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.13.2](https://github.com/abougouffa/minemacs/compare/v4.13.1..v4.13.2) - 2024-01-31
#### Documentation
- **(external-tools)** regenerate the list - ([f18f495](https://github.com/abougouffa/minemacs/commit/f18f4953d96b9e2ba116c05ba03d217d4914a126)) - [@abougouffa](https://github.com/abougouffa)
- regenerate documentation - ([ee5fe28](https://github.com/abougouffa/minemacs/commit/ee5fe287c2e219da10a064c81686bf068c5fd41c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(doom-themes)** defer loading the Org extension - ([4e782c8](https://github.com/abougouffa/minemacs/commit/4e782c88909a36b39bb32e34cfbe7b57c00f3ec1)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([647b0f2](https://github.com/abougouffa/minemacs/commit/647b0f2f2d0b55a5cbd91929c493bb0fffd1c238)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.13.1](https://github.com/abougouffa/minemacs/compare/v4.13.0..v4.13.1) - 2024-01-31
#### Miscellaneous Chores
- **(ci)** add Emacs 29.2 to the CI matrix - ([ec7b395](https://github.com/abougouffa/minemacs/commit/ec7b395511660f18f178d620b78c8d4dc1e6c864)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(external-tools)** prefer GitHub links when available, update the list - ([68126dd](https://github.com/abougouffa/minemacs/commit/68126ddf8ad39c07398849ca650a6514dd29dd81)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.13.0](https://github.com/abougouffa/minemacs/compare/v4.12.1..v4.13.0) - 2024-01-31
#### Bug Fixes
- **(flymake)** fix finding the `.clang-tidy` file - ([fd128f6](https://github.com/abougouffa/minemacs/commit/fd128f608d7db99b9a61944b3f50a238aa9fd580)) - [@abougouffa](https://github.com/abougouffa)
- **(ts-movement)** install only when Emacs has tree-sitter support - ([90ef5da](https://github.com/abougouffa/minemacs/commit/90ef5da71674931e87c14706455ded057ffd5fd2)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(external-tools)** update the list of external tools - ([7687639](https://github.com/abougouffa/minemacs/commit/76876399b82f660407cde4892f75e664b0b06373)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** regenerate the list - ([4f7e4c1](https://github.com/abougouffa/minemacs/commit/4f7e4c1a4e0f0e6c3eb4bf392d95153d2c3bfd3f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add an option to load all modules - ([4cbdf1e](https://github.com/abougouffa/minemacs/commit/4cbdf1e707493ab38236ce5694030fbb31c00c1f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+directory-root-containing-file` - ([3ee9727](https://github.com/abougouffa/minemacs/commit/3ee9727780aa700f8933239b895b4d41190c4b90)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add a `flymake` backend for `clang-tidy` - ([f652ab1](https://github.com/abougouffa/minemacs/commit/f652ab1b766998db2e376cc84d2b56ff7a48dee9)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `ts-movement` - ([a0e5a3d](https://github.com/abougouffa/minemacs/commit/a0e5a3df2c4b798d9ad04a4ed0e6933112e4ba4d)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** make use of the new `MINEMACS_LOAD_ALL_MODULES` envvar - ([2e24998](https://github.com/abougouffa/minemacs/commit/2e2499839aa9cb4defaed4cb4bcc6387a621e656)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** add an option for `+xmllint-indent` to set indentation - ([d8af747](https://github.com/abougouffa/minemacs/commit/d8af747b08a30c43d5f7569af576a52359247213)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** make use of `+directory-root-containing-file` - ([d411a14](https://github.com/abougouffa/minemacs/commit/d411a1498aee92da51ffa5d028e586e1183ca8e6)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** add `yq` - ([dc0da27](https://github.com/abougouffa/minemacs/commit/dc0da27d569b6b5001089de8a546b1208d8ec2db)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** install XML grammar - ([a853a72](https://github.com/abougouffa/minemacs/commit/a853a72bfeb58b04f85b50707964d94b0095d70c)) - [@abougouffa](https://github.com/abougouffa)
- **(ts-movement)** move `+ts-movement-maybe` to `:init` - ([554bee5](https://github.com/abougouffa/minemacs/commit/554bee56ee31a00c80bb1fdb4eb782688cfef48c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0700633](https://github.com/abougouffa/minemacs/commit/0700633de628124997df3d233b3043fd18dfe63f)) - [@abougouffa](https://github.com/abougouffa)
- declare more external tools - ([ad6de25](https://github.com/abougouffa/minemacs/commit/ad6de25150195d31e4f2c25cf222c23e35d0a987)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.12.1](https://github.com/abougouffa/minemacs/compare/v4.12.0..v4.12.1) - 2024-01-28
#### Documentation
- **(external-tools)** regenerate the list - ([13dc390](https://github.com/abougouffa/minemacs/commit/13dc3905a0b8cb06b481f62bf9b373d44dd84be3)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** update the list - ([f8a8262](https://github.com/abougouffa/minemacs/commit/f8a82621c0dcf7497585f68c37b41a140870ca95)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** declare external dependencies - ([82088d2](https://github.com/abougouffa/minemacs/commit/82088d2e0e641858af185af1d752d679fbf23a6b)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example in `init-tweaks.el` - ([2beb3d7](https://github.com/abougouffa/minemacs/commit/2beb3d7ac48a91ebc7a43efe4a87a46ba4a60895)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.12.0](https://github.com/abougouffa/minemacs/compare/v4.11.0..v4.12.0) - 2024-01-28
#### Bug Fixes
- **(dirvish)** ignore previewing `*.po` files - ([79dabfb](https://github.com/abougouffa/minemacs/commit/79dabfb586e6fc5a61a4da19276d4e955852cda1)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(external-tools)** update the list - ([45405e2](https://github.com/abougouffa/minemacs/commit/45405e20f3635cf542b60805f12918ea64dc92a5)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update links - ([a6e5785](https://github.com/abougouffa/minemacs/commit/a6e57851aa89b8c485ea538efbe95ef9a9838dfc)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** show pictures - ([27e7e7e](https://github.com/abougouffa/minemacs/commit/27e7e7ecc93d70eab766c783faa0a81049c871a3)) - [@abougouffa](https://github.com/abougouffa)
- regenerate documentation - ([f4211e3](https://github.com/abougouffa/minemacs/commit/f4211e3ab8fce5ecc0d6907bffb1d89578192c59)) - [@abougouffa](https://github.com/abougouffa)
- add a list of external tools (WIP) - ([f31b94b](https://github.com/abougouffa/minemacs/commit/f31b94bad762fbf35a52b055b7031c673b708480)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+binary-file-p` - ([2cc53c6](https://github.com/abougouffa/minemacs/commit/2cc53c6b1ca3c2e8d476f4ef96fcf3f40bed95f2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `rscope` - ([431d3b2](https://github.com/abougouffa/minemacs/commit/431d3b2ad69bb55bbf25d0496396ac2d4c8994a7)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `eopengrok` support - ([6dde0da](https://github.com/abougouffa/minemacs/commit/6dde0da0f5fe717f1997be5a3ee67484854efb0b)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `rtags` support - ([f311840](https://github.com/abougouffa/minemacs/commit/f3118407cdcf1df7245a7e5bdd49fea66a14619a)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `eopengrok` support - ([2efe395](https://github.com/abougouffa/minemacs/commit/2efe395db7bd596e8d91c14bdf46e549ae94fa17)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(docs)** move documentation to a separate directory - ([897d5ab](https://github.com/abougouffa/minemacs/commit/897d5ab41780ef6b25d90191be1a1efa96acd2d8)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** update `DOCS.md` path - ([6f1de1c](https://github.com/abougouffa/minemacs/commit/6f1de1cc2df56ffcc6ec96e2878e203dedcd91f1)) - [@abougouffa](https://github.com/abougouffa)
- **(version)** v4.11.0 - ([0d9da59](https://github.com/abougouffa/minemacs/commit/0d9da59eb35340b46f1127b68cf7ba9e6578d520)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(rtags)** defer until explicitly enabled - ([41395b7](https://github.com/abougouffa/minemacs/commit/41395b79627a39250205e7f2d230ce44303f2610)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.11.0](https://github.com/abougouffa/minemacs/compare/v4.10.0..v4.11.0) - 2024-01-28
#### Bug Fixes
- **(builtin)** fix loading the `+whitespace-auto-cleanup-mode` hook - ([2245c31](https://github.com/abougouffa/minemacs/commit/2245c317b5b388cffb58937ca2f3e20451195d1d)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** ignore previewing `*.po` files - ([6bd406d](https://github.com/abougouffa/minemacs/commit/6bd406dc65965dbfb24c6b938b870e075bb2a4b9)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(prog)** add a comment - ([4da1401](https://github.com/abougouffa/minemacs/commit/4da1401d4282cfe2f34617620b96e5f48097fb1e)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+binary-file-p` - ([e9fee97](https://github.com/abougouffa/minemacs/commit/e9fee97b1b79630e0d081f4db72ac07d258fefde)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `rtags` support - ([5b11206](https://github.com/abougouffa/minemacs/commit/5b11206ec6d7d4b6b86a52cf6bbeed240dc1ba2f)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `eopengrok` support - ([8a876f5](https://github.com/abougouffa/minemacs/commit/8a876f52b0504a698f0508fbbf206f43192f4a42)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `xcscope` - ([111c68f](https://github.com/abougouffa/minemacs/commit/111c68fab088dc024d0a4b2611dd73b128c8588e)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `semantic-refactor` for use with non-LSP workspaces - ([d0aec15](https://github.com/abougouffa/minemacs/commit/d0aec15dcff703f1a1f5ce1e18edc408e0bb8ac0)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `ack` - ([98279ac](https://github.com/abougouffa/minemacs/commit/98279ac30f9df964644bf7bebf1cc062c512302c)) - [@abougouffa](https://github.com/abougouffa)
- add `me-ai` module - ([6e293e3](https://github.com/abougouffa/minemacs/commit/6e293e3d15eb9b36f2c64ffb63c3468f817354c8)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** map `SPC t c` to toggle `+whitespace-auto-cleanup-mode` - ([7db8582](https://github.com/abougouffa/minemacs/commit/7db858281b3a8de8b0f8c4d1efe63897107c3d29)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** change default TAB behavior - ([262a43f](https://github.com/abougouffa/minemacs/commit/262a43f8db0958e0254fd2a3f40604e630b6de17)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** make the default tab width 4 - ([3ae0f65](https://github.com/abougouffa/minemacs/commit/3ae0f65820d1628615ea5fc60a9b4bcecd1af420)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** better color for displaying trailing white space - ([bcf5089](https://github.com/abougouffa/minemacs/commit/bcf5089f8c4e9aa95f177b433779d17d3dd51691)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** add `+whitespace-auto-cleanup-mode` - ([f7804eb](https://github.com/abougouffa/minemacs/commit/f7804eb64d7e57a77a47ccd8926ecb24d2645051)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** map `gl` and `gh` to jump forward and backward - ([cdd2c9d](https://github.com/abougouffa/minemacs/commit/cdd2c9d8103f1f7f45a6b061c61faefb577e384f)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add `me-nano` in the examples - ([a2c58cc](https://github.com/abougouffa/minemacs/commit/a2c58cc933ce67b582ac9e71008ff4e9ce1974a2)) - [@abougouffa](https://github.com/abougouffa)
- open `clang-[format|tidy]` in YAML mode - ([0f1e448](https://github.com/abougouffa/minemacs/commit/0f1e4484db7c0aa5a9e76a580053ae1b7c8e77bc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.10.0](https://github.com/abougouffa/minemacs/compare/v4.9.0..v4.10.0) - 2024-01-24
#### Bug Fixes
- **(core)** `+scratch-open-...` conflict with `project-cmake` - ([b9e4cbb](https://github.com/abougouffa/minemacs/commit/b9e4cbbb9e252da8311f1f6885dd7f970baf1751)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** refactor and fix persistent buffer implementation - ([99ea57d](https://github.com/abougouffa/minemacs/commit/99ea57d4630800d6deaa1e8d859f80f18feff9ae)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** include the full list of obsolete modules - ([d33e0fa](https://github.com/abougouffa/minemacs/commit/d33e0faae903c076b577d3a7d8de9b5be3b0d721)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** reintegrate `cmake-mode` and `cmake-font-lock` - ([03e9bf1](https://github.com/abougouffa/minemacs/commit/03e9bf1eea28f2f962624006b836e7dae7ddea70)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `eglot-booster` as a package - ([f9e77d5](https://github.com/abougouffa/minemacs/commit/f9e77d5c648cf6252facb1ab8c9b421aa3ab4cb5)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(docker)** add a docker image with utils included (WIP) - ([5baf8b0](https://github.com/abougouffa/minemacs/commit/5baf8b0da32726c3aeda932783af10fd93ac7290)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** move a variable to `me-vars` - ([e12ee0c](https://github.com/abougouffa/minemacs/commit/e12ee0c8400e508036cec8136f85d8c87f42eba0)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** minor tweak and documentation change - ([ea9ab4c](https://github.com/abougouffa/minemacs/commit/ea9ab4c4662b1572d0e2b1e31c23c38c24d323fc)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better implementation of persistent scratch buffers - ([99645cf](https://github.com/abougouffa/minemacs/commit/99645cf12625dcf4b156172a9e5cd217e6bd876a)) - [@abougouffa](https://github.com/abougouffa)
- **(treemacs)** smaller font size for root nodes - ([d35f55c](https://github.com/abougouffa/minemacs/commit/d35f55c3bb8929ca9e388715648652b2e6be2312)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9ff3223](https://github.com/abougouffa/minemacs/commit/9ff3223ba6134091734383dd683d958e6a845011)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([e1d6b6f](https://github.com/abougouffa/minemacs/commit/e1d6b6f167c3b3105a98f29f6ea3a4ad55dbcd0c)) - [@abougouffa](https://github.com/abougouffa)
- make NetExtender integration obsolete, not used any more - ([ca2c631](https://github.com/abougouffa/minemacs/commit/ca2c631b49069096af77e208f9d0ab4c00fb25f5)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.9.0](https://github.com/abougouffa/minemacs/compare/v4.8.1..v4.9.0) - 2024-01-21
#### Features
- **(prog)** add `citre` for Universal Tags (`ctags`) support - ([6bf0f8b](https://github.com/abougouffa/minemacs/commit/6bf0f8b6be99e9386233fc54aea84db07cde0871)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(citre)** use the detected project root by default - ([c723218](https://github.com/abougouffa/minemacs/commit/c723218583b2484cb155e68093565bea329e6e5a)) - [@abougouffa](https://github.com/abougouffa)
- **(whitespace)** smartly auto cleanup trailing white space on save - ([f892550](https://github.com/abougouffa/minemacs/commit/f8925505a5eb30aab465afe9f278386421d4e4eb)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2f04c2a](https://github.com/abougouffa/minemacs/commit/2f04c2a6b9646879217bd50954775a7779631589)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.8.1](https://github.com/abougouffa/minemacs/compare/v4.8.0..v4.8.1) - 2024-01-20
#### Bug Fixes
- **(evil-vimish-fold)** enable the mode globally - ([64e1cbf](https://github.com/abougouffa/minemacs/commit/64e1cbf6cf0d62454390daddc4c81e70c6e34810)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(code-folding)** cleanup dead code - ([9b27509](https://github.com/abougouffa/minemacs/commit/9b27509166c660cee87d8c6e30cab9df3c4f3615)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** use `add-hook` instead of `+add-hook!` in simple cases - ([e1bc8a0](https://github.com/abougouffa/minemacs/commit/e1bc8a01e9403ac4c9acf6656ab144b4b1b63e16)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** better hook documentation in `+make-first-file-hook!` - ([a32eb79](https://github.com/abougouffa/minemacs/commit/a32eb79b76048a83f403913d5dbafeadffe45cc7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.8.0](https://github.com/abougouffa/minemacs/compare/v4.7.0..v4.8.0) - 2024-01-20
#### Bug Fixes
- **(core)** fix `+make-first-file-hook!` behavior in daemon mode - ([6dbfae6](https://github.com/abougouffa/minemacs/commit/6dbfae6848d47bcfd620090e7e040c4562207a2e)) - [@abougouffa](https://github.com/abougouffa)
- **(pyenv)** missing argument in a `+log!` statement - ([ace25b8](https://github.com/abougouffa/minemacs/commit/ace25b86df0df0eec8a36603cb3b8c23919ffed0)) - [@abougouffa](https://github.com/abougouffa)
- **(pyenv)** ensure before enabling it globally (daemon hanging issue) - ([5984552](https://github.com/abougouffa/minemacs/commit/59845528a33fff8991e66055b6be2ade53d127b6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(code-folding)** better code folding experience (from Doom Emacs) - ([f76a571](https://github.com/abougouffa/minemacs/commit/f76a5715f140d6870f066dec293b0df9ccc34d06)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add `vimish-fold` and `evil-vimish-fold` - ([13d1107](https://github.com/abougouffa/minemacs/commit/13d11074fa46d9f8822f682cb1a878f98571e256)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add `highlight-indent-guides`, but as an opt-in (slow!) - ([0792ea4](https://github.com/abougouffa/minemacs/commit/0792ea4af3bfd001557691095304d26233f2c359)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** make `xmllint` the default for `nxml-mode` - ([eeac59e](https://github.com/abougouffa/minemacs/commit/eeac59e8c02ef022e3a75803b090ea407f33dd76)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** additional modes' rules - ([641dd76](https://github.com/abougouffa/minemacs/commit/641dd7620799cba383da97a8cd6081fad9da3005)) - [@abougouffa](https://github.com/abougouffa)
- **(repo)** restore the default repository, fix merged - ([a37537e](https://github.com/abougouffa/minemacs/commit/a37537e868bf987555e4b97e30fd3c72ff7147bb)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([20e3e5d](https://github.com/abougouffa/minemacs/commit/20e3e5db24b1eeb69d67e93b3bd3c2c113b8ba7d)) - [@abougouffa](https://github.com/abougouffa)
- don't disable some packages based on `executable-find` - ([c466611](https://github.com/abougouffa/minemacs/commit/c466611a555c457aef3bf3a2e310694d3629ea5b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.7.0](https://github.com/abougouffa/minemacs/compare/v4.6.3..v4.7.0) - 2024-01-17
#### Bug Fixes
- **(repo)** use my fork until it gets merged upstream - ([fb9363b](https://github.com/abougouffa/minemacs/commit/fb9363bfc62eaa86fe9b29ebf2276a5d38e43b17)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(init)** more first file special hooks - ([55e6bb3](https://github.com/abougouffa/minemacs/commit/55e6bb3afd81e17b9709560d9e1a1a6564f3da0e)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `pyenv` via `pyenv.el` - ([9bb5b84](https://github.com/abougouffa/minemacs/commit/9bb5b8451dc7ddcb9e5c7cba7fb0983ce5dcc698)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add more Gerrit/repo utilities from ChromeOS's `dev-util` - ([6f25cf3](https://github.com/abougouffa/minemacs/commit/6f25cf36ad02dbf4935d57e1ae703839b9b06bc9)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(treesit-fold)** still buggy, crashing in C/C++ files - ([fec0f2f](https://github.com/abougouffa/minemacs/commit/fec0f2f2bd5684ccca8c31f60cb68af76ea85ab0)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** enable `hs-minor-mode` in `nxml-mode` - ([e8338bc](https://github.com/abougouffa/minemacs/commit/e8338bcfc3eaf55638b806f1a71d84635f4860b6)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** prioritize `minemacs-first-file` over `minemacs-first-X-file` - ([c846a0f](https://github.com/abougouffa/minemacs/commit/c846a0f45545e4e31a05774ad9a50c57fe8260b2)) - [@abougouffa](https://github.com/abougouffa)
- **(org-contrib)** ensure using the right branch - ([0c7d38d](https://github.com/abougouffa/minemacs/commit/0c7d38dbcc8c5e34b88fdf05ec0ec0bbd9583c33)) - [@abougouffa](https://github.com/abougouffa)
- **(pyvenv)** remove unnecessary tweaks - ([2436e29](https://github.com/abougouffa/minemacs/commit/2436e29a35cfc7dd5283c6a9a25b65ffac7c736b)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** exclude the `VIRTUAL_ENV` from saved env vars - ([7f8c0da](https://github.com/abougouffa/minemacs/commit/7f8c0da1d64e91ea8d4d559948b26050023823f3)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([29a7467](https://github.com/abougouffa/minemacs/commit/29a74670526c0c1348d4cbb7c2d59662ae940152)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.6.3](https://github.com/abougouffa/minemacs/compare/v4.6.2..v4.6.3) - 2024-01-14
#### Bug Fixes
- **(core)** ignore case when matching regexps in `+make-first-file-hook!` - ([6241629](https://github.com/abougouffa/minemacs/commit/624162920477929775abaad623e9a1c8681b7f44)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(skel)** add an example of `init-tweaks.el` - ([57d796f](https://github.com/abougouffa/minemacs/commit/57d796f6d52ee054f09e119bc59d849aa65cc557)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- obfuscate email addresses with `rot13` - ([3c3c709](https://github.com/abougouffa/minemacs/commit/3c3c709d2f22f8cefb03feb7ad95748b91483a66)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(evil-iedit-state)** use `:after iedit` instead - ([0521268](https://github.com/abougouffa/minemacs/commit/052126838e74ee0fe0549241134ef67eceba43ec)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.6.2](https://github.com/abougouffa/minemacs/compare/v4.6.1..v4.6.2) - 2024-01-14
#### Refactoring
- **(obsolete/flycheck)** minor edits - ([f34a85b](https://github.com/abougouffa/minemacs/commit/f34a85bd3ba973ce336010a41a5e8e753d1f37c2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(multi-cursors)** remove unused `multiple-cursors` - ([8226319](https://github.com/abougouffa/minemacs/commit/82263197b56ec55e1709218070a8b2694b056127)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-cursors)** better packages loading - ([f81a573](https://github.com/abougouffa/minemacs/commit/f81a573ba1dabdf3c467c4c915dfaa2513daa136)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([92eb5ef](https://github.com/abougouffa/minemacs/commit/92eb5ef154a5975f41f2eda0f855c4e47dcceacb)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.6.1](https://github.com/abougouffa/minemacs/compare/v4.6.0..v4.6.1) - 2024-01-13
#### Bug Fixes
- **(core)** better implementation of `objdump-disassemble-mode` - ([b3f4149](https://github.com/abougouffa/minemacs/commit/b3f4149460f42b9166b7151ba4bf566d53da0e28)) - [@abougouffa](https://github.com/abougouffa)
- **(org-contrib)** use my mirror repo (sr.ht) isn't stable - ([056bfe8](https://github.com/abougouffa/minemacs/commit/056bfe87d39354600e89a77fbe6e40dddf2392df)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- update copyright year - ([45d5132](https://github.com/abougouffa/minemacs/commit/45d5132a38c7c1e6b6c9703678a2e1d3217db60c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(early-init)** move `LSP_USE_PLISTS` to `me-lsp` - ([d1aa86a](https://github.com/abougouffa/minemacs/commit/d1aa86a511c7b84cbea79d38e05ba62f92cb5ecd)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(binary)** don't disassemble by default - ([18a24e9](https://github.com/abougouffa/minemacs/commit/18a24e99f168e679981a1999c7dad6eb26917578)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.6.0](https://github.com/abougouffa/minemacs/compare/v4.5.4..v4.6.0) - 2024-01-13
#### Documentation
- **(core)** add documentation for `+font--get-valid-args` - ([8c3cb2c](https://github.com/abougouffa/minemacs/commit/8c3cb2c3eb0976cc40e6dd6500f3389b97cd2585)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documenatation - ([b738333](https://github.com/abougouffa/minemacs/commit/b7383330dc244daef39c4774347fa18d80603144)) - [@abougouffa](https://github.com/abougouffa)
- minor documentation tweaks - ([3e9cfb2](https://github.com/abougouffa/minemacs/commit/3e9cfb234e1f5074a54f5e1facf186a7b2d29b72)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([649d3d5](https://github.com/abougouffa/minemacs/commit/649d3d5b738fcd444ef4764e6afbf0695a9b4d9b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** add support for `evil-textobj-tree-sitter-get-textobj` - ([a8325b0](https://github.com/abougouffa/minemacs/commit/a8325b0bddc2f855b7d4fb5ccbb86023078df9a6)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-fold)** restore support after fixes (still WIP) - ([874fa85](https://github.com/abougouffa/minemacs/commit/874fa8513af7d977231fdf210994e25fb2a932e8)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(git)** add `user-config` to `.gitignore` - ([f22165b](https://github.com/abougouffa/minemacs/commit/f22165b10bf2129c79f872f6521f7aeb058cbd2b)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(org)** simplify the code and add documentation - ([874a87b](https://github.com/abougouffa/minemacs/commit/874a87b6a38cc87d40a03f8438260be4c3c5a3b8)) - [@abougouffa](https://github.com/abougouffa)
- better convention for advice functions naming - ([569f10b](https://github.com/abougouffa/minemacs/commit/569f10b160ca7e544d216aea0994f4d333604f4e)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** use `file-name-concat` in `+load` - ([08cc205](https://github.com/abougouffa/minemacs/commit/08cc20541b4fe07aaa730372007f5a92979bc83f)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-gdb)** directly use my recipe - ([92719c7](https://github.com/abougouffa/minemacs/commit/92719c702fe4a0752be93ff29c8449b45f6b7efe)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile-executor)** add keybindings - ([8871df4](https://github.com/abougouffa/minemacs/commit/8871df469df11ed7e4c690ae3879d5e0c4dc6f31)) - [@abougouffa](https://github.com/abougouffa)
- **(org-contrib)** make it lazy - ([1b0777c](https://github.com/abougouffa/minemacs/commit/1b0777c8cd03df5d90ed818ab9eabb3002741d87)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.5.4](https://github.com/abougouffa/minemacs/compare/v4.5.3..v4.5.4) - 2024-01-10
#### Bug Fixes
- **(core)** better inference of filename in `+package-download-from-urls` - ([b7e0739](https://github.com/abougouffa/minemacs/commit/b7e0739f3112b7da015a872126057737e4e83301)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add Nix to the list of languages [#140] - ([63ff477](https://github.com/abougouffa/minemacs/commit/63ff4776dbf67051ee080cd3541a12cae58f4314)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(cape)** restore `cape-dict` (it in fact useful) [#150] - ([5caddc4](https://github.com/abougouffa/minemacs/commit/5caddc402bf9dc71325b29114d69b92ce36ec181)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** cache the downloaded `loaddefs-gen.el` file - ([0d26f30](https://github.com/abougouffa/minemacs/commit/0d26f30c79ef25ea4fa36bd27392c70b5077c212)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** make use of `+ignore-root` to exclude `x-win` sessions - ([b677fd6](https://github.com/abougouffa/minemacs/commit/b677fd6b6d5d27cef9fcbe47230749911ea54ce3)) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** disable unused `cape-dict` [#150] - ([ed2bae5](https://github.com/abougouffa/minemacs/commit/ed2bae5b9f5c68de65de6cf106170bbe406ad5b3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better exclusion of `recentf` files in `+ignore-roots` - ([4e619ae](https://github.com/abougouffa/minemacs/commit/4e619ae702ca8f93d8bd972a82651ff344561e68)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.5.3](https://github.com/abougouffa/minemacs/compare/v4.5.2..v4.5.3) - 2024-01-08
#### Bug Fixes
- **(scratch)** always replace the default scratch with the persistent one - ([e4a0ed9](https://github.com/abougouffa/minemacs/commit/e4a0ed9b84db64e4422de7d9c1c0806dbba280a6)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([3ee82a9](https://github.com/abougouffa/minemacs/commit/3ee82a935286ca37709fd20dcadb6314b7593745)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.5.2](https://github.com/abougouffa/minemacs/compare/v4.5.1..v4.5.2) - 2024-01-08
#### Bug Fixes
- **(core)** avoid issues when evaluating buffer name variables [#150] - ([dd653fe](https://github.com/abougouffa/minemacs/commit/dd653fe55e4124bae27ad2ed49974d2d44538cc3)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** proper convention to forget remote zombie projects - ([aca7981](https://github.com/abougouffa/minemacs/commit/aca7981c0ff5fa8bbc41d3eaf19fd19deb999d41)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** fix the issue of upgrading Tramp on Emacs 29.1 - ([4a8a0a2](https://github.com/abougouffa/minemacs/commit/4a8a0a25f4ef61d2235eb79c3deb7b8338b0d650)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([c2dbbd9](https://github.com/abougouffa/minemacs/commit/c2dbbd943e059cefad6cb9a672a9e17c88364413)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(recentf)** better exclusion of remote files - ([f6f1a10](https://github.com/abougouffa/minemacs/commit/f6f1a1064876dcf145931d01397acbd4d8938778)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** use up-to-date Tramp, remove old Magit related fix - ([984c5da](https://github.com/abougouffa/minemacs/commit/984c5da9142b4c2a2f1526d0cb8ab0da688c410c)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** take the MacOS case into account in `stty` workaround - ([3267853](https://github.com/abougouffa/minemacs/commit/3267853f645de850321bb71c3dc98bb42356e58b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.5.1](https://github.com/abougouffa/minemacs/compare/v4.5.0..v4.5.1) - 2024-01-07
#### Bug Fixes
- **(jinx)** don't show compile buffer on failure in `+jinx-load-module` - ([008dbaa](https://github.com/abougouffa/minemacs/commit/008dbaaccd27702d1822c449486da9523fb87913)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-vterm)** change work dir correctly on remote dedicated terminal - ([aed3220](https://github.com/abougouffa/minemacs/commit/aed3220589226a4928f2911ca6bd4d528837a396)) - [@abougouffa](https://github.com/abougouffa)
- **(python)** correct the condition for tweaking `pyenv` integration - ([f79dfe7](https://github.com/abougouffa/minemacs/commit/f79dfe7f6b95923b9bda7617a0c28a7c2f926e5b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(eglot)** add experimental `eglot-booster`! - ([6b2a8d4](https://github.com/abougouffa/minemacs/commit/6b2a8d45aa35e91d6cffd97097762c359b535a16)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** initial support for `pyvenv` (WIP) - ([8a34f24](https://github.com/abougouffa/minemacs/commit/8a34f243e79dd5f968dbc8ff0a0903e965781a32)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** format remote files using local formatters - ([74c113b](https://github.com/abougouffa/minemacs/commit/74c113b9f349a85d8c6811862883d2b0963d4ff4)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot-booster)** rename file - ([762da5f](https://github.com/abougouffa/minemacs/commit/762da5fa354b73e7b0aa02a036ad185f74a2b3ef)) - [@abougouffa](https://github.com/abougouffa)
- **(jinx)** log the error message in `+jinx-load-module` - ([72db594](https://github.com/abougouffa/minemacs/commit/72db594a309dd372727fb3675dc40a2d966d4eaa)) - [@abougouffa](https://github.com/abougouffa)
- **(python)** better `pyenv` integration (WIP) - ([685bdcf](https://github.com/abougouffa/minemacs/commit/685bdcf676f077e99b636933d278438fc811c0de)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([85008d7](https://github.com/abougouffa/minemacs/commit/85008d70c3e1467e79c6039e83d5051c36724401)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.5.0](https://github.com/abougouffa/minemacs/compare/v4.4.0..v4.5.0) - 2024-01-04
#### Features
- **(embedded)** add support for DTS via `dts-mode` & `virtual-dts-mode` - ([2c2d7e7](https://github.com/abougouffa/minemacs/commit/2c2d7e7569b3bbf9e1157f7940a59201ba5016d3)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(natural-langs)** restore jinx - ([6014efa](https://github.com/abougouffa/minemacs/commit/6014efad5775cf9e3fedd447d4ef0798698cc529)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** restore `jinx` and `spell-fu` examples - ([9d0f9f5](https://github.com/abougouffa/minemacs/commit/9d0f9f5dda8c5c701276b16415070c2194c8f118)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** smaller internal border - ([3eddc3b](https://github.com/abougouffa/minemacs/commit/3eddc3b273a60d7c91a3e81b80aeb977bb2b60be)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-numbers)** define the right number format for `dts-mode` - ([9f48b02](https://github.com/abougouffa/minemacs/commit/9f48b0273db4620aad9bccb47a5900fcfddf8566)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** use `jinx` if available or fallback to `spell-fu` - ([153cbd0](https://github.com/abougouffa/minemacs/commit/153cbd0f5d1532d2016acba94dfe412a628c28df)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** remove obsolete examples - ([e99daea](https://github.com/abougouffa/minemacs/commit/e99daea826ff2a075b500492375a1f7313a1edf6)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.4.0](https://github.com/abougouffa/minemacs/compare/v4.3.3..v4.4.0) - 2024-01-03
#### Features
- **(core)** add function to get/set the standard values - ([540ad79](https://github.com/abougouffa/minemacs/commit/540ad79ab3f4b4784ff288087e8b3ddb17dc124e)) - [@abougouffa](https://github.com/abougouffa)
- **(fun)** add `wordel` - ([d37b0d1](https://github.com/abougouffa/minemacs/commit/d37b0d1b6f21c17d608dc119a415842a323192ec)) - [@abougouffa](https://github.com/abougouffa)
- **(nano)** add initial N Λ N O Emacs UI (WIP) - ([761278f](https://github.com/abougouffa/minemacs/commit/761278f2d830ca98717cffaed7d48c7da528335c)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** make jinx obsolete, add `flyspell-correct` - ([1d10383](https://github.com/abougouffa/minemacs/commit/1d1038379c24c73853aca02797ee6b95fd9951a3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** add an internal border of 15px - ([539e42c](https://github.com/abougouffa/minemacs/commit/539e42ced9b7e491b6e4b060b273f3f847c30485)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** be more intelligent when trying to load the theme - ([4e23071](https://github.com/abougouffa/minemacs/commit/4e23071ff51f13b0e65e93b9a52d58ec4a4647b6)) - [@abougouffa](https://github.com/abougouffa)
- **(editorconfig)** trigger on the first file, exclude compressed files - ([c4a703c](https://github.com/abougouffa/minemacs/commit/c4a703cd90423b0954f8e64a59cc94a3df331ef9)) - [@abougouffa](https://github.com/abougouffa)
- **(envrc)** ensure loading envrc for babel source blocks - ([9cc51e9](https://github.com/abougouffa/minemacs/commit/9cc51e9c501753455e7eca596eb4df0b79f5d60a)) - [@abougouffa](https://github.com/abougouffa)
- **(envrc)** disable on Windows - ([cf348a7](https://github.com/abougouffa/minemacs/commit/cf348a79825cdf465edcfcfd212ee1b081a36669)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** `treesit-auto-langs` set incorrectly - ([3c5d6e9](https://github.com/abougouffa/minemacs/commit/3c5d6e9cd237d1206688b410b27263c9bcf3c350)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** add `minemacs-obsolete-modules-dir` - ([fb1a605](https://github.com/abougouffa/minemacs/commit/fb1a605c3cfb8dc6f542ad56c091f8ea82fd5be2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8f8b8f8](https://github.com/abougouffa/minemacs/commit/8f8b8f8b4658bf6df2947f1213f5d397275b4b77)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.3.3](https://github.com/abougouffa/minemacs/compare/v4.3.2..v4.3.3) - 2023-12-29
#### Miscellaneous Chores
- **(makefile)** add `locked` rule - ([2bbb4ee](https://github.com/abougouffa/minemacs/commit/2bbb4ee61a3360a49d12e270fccb2757fd52d8a4)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** fix the `update` rule - ([038ec19](https://github.com/abougouffa/minemacs/commit/038ec19108919cd5a619ff1f13ba984655339dbb)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(core)** minor refactor - ([d4db4cf](https://github.com/abougouffa/minemacs/commit/d4db4cfb641a67267f2965531361fc978d205f9e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(fonts)** move font & script settings to `me-lib` - ([aa4de70](https://github.com/abougouffa/minemacs/commit/aa4de707d412ef635ad8a1f37ff8c48c9b8e4d8d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(init)** cleanup irrelevant comments/logs - ([f7c841d](https://github.com/abougouffa/minemacs/commit/f7c841d4064b39dbb58c8e24771b8992e359e2b7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f460d8f](https://github.com/abougouffa/minemacs/commit/f460d8fd4952fb43265a368b8d03338428d58740)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.3.2](https://github.com/abougouffa/minemacs/compare/v4.3.1..v4.3.2) - 2023-12-29
#### Refactoring
- **(core)** change the signature of `+github-latest-release` - ([47087b8](https://github.com/abougouffa/minemacs/commit/47087b8e580cd672586bd5457dca5d87cb1e7a19)) - [@abougouffa](https://github.com/abougouffa)
- move `+github-latest-release` to `me-lib` - ([bca42ac](https://github.com/abougouffa/minemacs/commit/bca42ac181a956c11abbeb6ae93b5b76aee52f88)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(jinx)** hook if the compilation is easy (Unix or Win+MSYS) [#147] - ([f09dd85](https://github.com/abougouffa/minemacs/commit/f09dd8583cd211e8fb8d60830430afd0535a9d6c)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example of how to force loading an obsolete module - ([293008d](https://github.com/abougouffa/minemacs/commit/293008dc239dd5d978f85efd1cf03b45c50b553b)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example of `jinx-languages` - ([6976d38](https://github.com/abougouffa/minemacs/commit/6976d3884ad0713abbc1617595c22d4228c24765)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.3.1](https://github.com/abougouffa/minemacs/compare/v4.3.0..v4.3.1) - 2023-12-28
#### Documentation
- regenerate documentation - ([524a903](https://github.com/abougouffa/minemacs/commit/524a9039fedca51e750321c3b1df6b039a098dcd)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(compile-multi)** enable integration for `consult` and `embark` - ([47bcf99](https://github.com/abougouffa/minemacs/commit/47bcf99f69935e916976ced9acf463ecdb06765a)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** regenerate loaddefs - ([b37ed85](https://github.com/abougouffa/minemacs/commit/b37ed85b22f97f8d2d28c5bf86db1271609d8f8a)) - [@abougouffa](https://github.com/abougouffa)
- **(spell-fu)** move dict registration macro to the obsolete module - ([116aa8e](https://github.com/abougouffa/minemacs/commit/116aa8ef71536f0c53ab17bd1ecdca61995129c6)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.3.0](https://github.com/abougouffa/minemacs/compare/v4.2.4..v4.3.0) - 2023-12-28
#### Bug Fixes
- **(natural-langs)** fallback to `spell-fu` on Windows [#147] - ([9e91a1e](https://github.com/abougouffa/minemacs/commit/9e91a1ee6df25c3f5f2d16ea2e9840faaabdb888)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** remove obsolete `spell-fu` config example [#146] - ([8e6bc6d](https://github.com/abougouffa/minemacs/commit/8e6bc6dd2356b41bf91c3758e96eed4de9cafd34)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate documentation - ([e0891b2](https://github.com/abougouffa/minemacs/commit/e0891b29f17531dbe1216d724b6bec514d54c4bf)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- remove unnecessary straight recipe names - ([28b5aaf](https://github.com/abougouffa/minemacs/commit/28b5aafc76196d21b68b11168cd8152d0a9d7b76)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(compile-multi)** install the Embark and Consult extensions - ([1bac5a4](https://github.com/abougouffa/minemacs/commit/1bac5a47d756ca031542ecc1c834b4ac72d6ee0f)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-collection)** remove the `corfu` hack, merged upstream - ([8947f29](https://github.com/abougouffa/minemacs/commit/8947f29a9e32388864e1cbaead8fa173e9179e23)) - [@abougouffa](https://github.com/abougouffa)
- **(jinx)** add `jinx--load-module` to `minemacs-build-functions` - ([3357935](https://github.com/abougouffa/minemacs/commit/33579352255ba74d8aa225486970294ddb11e6a3)) - [@abougouffa](https://github.com/abougouffa)
- **(vterm)** move `C-l/h/k/j` to directions - ([58198b8](https://github.com/abougouffa/minemacs/commit/58198b871e9a5d343f6b31ff76d3b895df46b7ed)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([44cb788](https://github.com/abougouffa/minemacs/commit/44cb78835db38fe9c91c6e3a23f34ec9b9c57e36)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.2.4](https://github.com/abougouffa/minemacs/compare/v4.2.3..v4.2.4) - 2023-12-25
#### Features
- **(files)** add support for `ztree` - ([ef01c78](https://github.com/abougouffa/minemacs/commit/ef01c7846b7d3ddc3688d2a3ca649af1c8f05358)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** use `jinx` for spell checking instead of `spell-fu` - ([b2b62bb](https://github.com/abougouffa/minemacs/commit/b2b62bbfd3c148ed47f5d8cd8cc128a3224e7f74)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `osm` - ([9e8ae0a](https://github.com/abougouffa/minemacs/commit/9e8ae0ac702c262c086c2186b5f0077b62072657)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(jinx)** enable only when Emacs is built with modules support - ([c603e55](https://github.com/abougouffa/minemacs/commit/c603e5585fe32c8808c3b58e15fea7076df04282)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** make `spell-fu` obsolete - ([79aba80](https://github.com/abougouffa/minemacs/commit/79aba805c78d1a5c7f39f7c0509133556e9aef18)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.2.3](https://github.com/abougouffa/minemacs/compare/v4.2.2..v4.2.3) - 2023-12-25
#### Bug Fixes
- **(evil-collection)** fix `corfu--setup` signature - ([feeae81](https://github.com/abougouffa/minemacs/commit/feeae81cf08f533084eecc015d4353f96fbfd040)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(cog)** add contributors, remove the long v0.1.0 changelog entry - ([2368c5b](https://github.com/abougouffa/minemacs/commit/2368c5b5041472db016083a77d892cf994a5ca55)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(app-launcher)** update recipe - ([22f34d1](https://github.com/abougouffa/minemacs/commit/22f34d1673a758e7ff987c06593f2a1a8b9df0b4)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([4cd8ed5](https://github.com/abougouffa/minemacs/commit/4cd8ed5fa56bddd6ff76d656ad4f937f4df8ec51)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.2.2](https://github.com/abougouffa/minemacs/compare/v4.2.1..v4.2.2) - 2023-12-24
#### Bug Fixes
- **(cape)** enable `cape-elisp-block` in `org-mode` only - ([8331f8e](https://github.com/abougouffa/minemacs/commit/8331f8ea65d7c3fac1977498758077c48d133fac)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** pin to a working commit - ([9dee456](https://github.com/abougouffa/minemacs/commit/9dee456719ea03c3c3d25283f6e279509351f143)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.2.1](https://github.com/abougouffa/minemacs/compare/v4.2.0..v4.2.1) - 2023-12-24
#### Bug Fixes
- **(core)** better management of first files hooks [#142] - ([6d20b61](https://github.com/abougouffa/minemacs/commit/6d20b61cff9311c867976af362a2edcf5111a60b)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** better implementation for `+eglot-auto-enable` [#142] - ([26f6784](https://github.com/abougouffa/minemacs/commit/26f678455a65ee6d238908d7d1499f3ba0eaaebf)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot+lsp)** remove unneeded loop in auto-enable LSP/Eglot - ([4ad574a](https://github.com/abougouffa/minemacs/commit/4ad574a66a00744f1061f3639e2e85056204b068)) - [@abougouffa](https://github.com/abougouffa)
- **(elec-pair)** disable auto-pairing of "<" in `org-mode` - ([ecb3675](https://github.com/abougouffa/minemacs/commit/ecb367584ca97883d257be1d02694ad1cafe573c)) - [@Hmanhng](https://github.com/Hmanhng)
- **(recentf)** load early to work correctly on non-daemon Emacs [#142] - ([01f6f30](https://github.com/abougouffa/minemacs/commit/01f6f30d95fe8065e47a0156bea122962289ab0f)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate documentation - ([69b20ed](https://github.com/abougouffa/minemacs/commit/69b20ed251f3c3390e0a09acc9aa44cb7c79c407)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(elisp-mode)** make use of `+setq-hook!` - ([8748e8b](https://github.com/abougouffa/minemacs/commit/8748e8b6dfad9b515ab2bfbcbbe44e66d9e4dcd7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(lsp)** update to the new auto-enable convention - ([6856b09](https://github.com/abougouffa/minemacs/commit/6856b098224a1ca68a9acb43599b3ae72e3059de)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f5a74cc](https://github.com/abougouffa/minemacs/commit/f5a74cc2e931c1499e53abb5f2f59e876c032446)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.2.0](https://github.com/abougouffa/minemacs/compare/v4.1.3..v4.2.0) - 2023-12-19
#### Bug Fixes
- **(cape)** rename obsolete `cape-symbol` to `cape-elisp-symbol` - ([6313c3d](https://github.com/abougouffa/minemacs/commit/6313c3d0b9d36c01b90d4f0dc71ebfb80be7f4e3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** don't alias `loaddefs-generate` to `make-directory-autoloads` - ([00b241f](https://github.com/abougouffa/minemacs/commit/00b241ff4bfd1310421991ced70488e783683255)) - [@abougouffa](https://github.com/abougouffa)
- **(saveplace)** enable at init to work with files passed as args - ([b91c3cc](https://github.com/abougouffa/minemacs/commit/b91c3cc74fb6a1373c6883480738adb06586c9ed)) - [@abougouffa](https://github.com/abougouffa)
- **(saveplace)** enable before opening the first file [#142] - ([69aedcc](https://github.com/abougouffa/minemacs/commit/69aedcc0c286ba7ce1dca2b6ca6293091a000b8d)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** buggy detection for non-installed grammars [#140] - ([c770acb](https://github.com/abougouffa/minemacs/commit/c770acb7b8d1e24ece65404b2a2270ad0a5cc168)) - [@abougouffa](https://github.com/abougouffa)
- **(undo-fu-session)** load early - ([c57c147](https://github.com/abougouffa/minemacs/commit/c57c147e0721f617c143dfbe05e534b9867a3d86)) - [@abougouffa](https://github.com/abougouffa)
- **(undo-fu-session)** fix renamed global mode - ([8452ff5](https://github.com/abougouffa/minemacs/commit/8452ff579c026b573d40d3b742090794e72ffba8)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move constants to `me-vars` - ([1d7aeb7](https://github.com/abougouffa/minemacs/commit/1d7aeb7aceb8a2a63066e622972ee7baa7e703a7)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- restore the original `minemacs-directory-arg-p` constant - ([9a1e46d](https://github.com/abougouffa/minemacs/commit/9a1e46d86778211a5e9128059e3d226fc3cf1515)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(window)** wider help windows - ([c8c4d10](https://github.com/abougouffa/minemacs/commit/c8c4d10fdf8be03ff37d04eb5b06cf2572bcfa9f)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([396efdb](https://github.com/abougouffa/minemacs/commit/396efdb05c97608750ea539a60b1be4b92f31bd0)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([10a02e0](https://github.com/abougouffa/minemacs/commit/10a02e0ca226d366316bf95937a2c032808b9fb7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.1.3](https://github.com/abougouffa/minemacs/compare/v4.1.2..v4.1.3) - 2023-12-16
#### Bug Fixes
- **(super-save)** correct a renamed customization variable - ([98ab319](https://github.com/abougouffa/minemacs/commit/98ab319882063bdd00fa513da42b6689ced5f4b0)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate DOCS.md - ([c11dbc4](https://github.com/abougouffa/minemacs/commit/c11dbc406e815083ec3f1ab79aa8ef3c01312482)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** minor edit - ([9325ba5](https://github.com/abougouffa/minemacs/commit/9325ba5c746438befc298dcd64040747c7443131)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** allow updating packages downloaded from URLs - ([7cff195](https://github.com/abougouffa/minemacs/commit/7cff19560d9d665ad0eaa1e09862806a40c44f5e)) - [@abougouffa](https://github.com/abougouffa)
- **(diffview)** add keybindings - ([d7b1998](https://github.com/abougouffa/minemacs/commit/d7b1998f90af7531eee879b913de80570318cea3)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib2)** make open status customizable - ([fbca453](https://github.com/abougouffa/minemacs/commit/fbca4539837c1d82a042ac243d187d58f8d664b5)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a50fdba](https://github.com/abougouffa/minemacs/commit/a50fdba70c03b8bb2593925bae6d1adaf21c3537)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.1.2](https://github.com/abougouffa/minemacs/compare/v4.1.1..v4.1.2) - 2023-12-14
#### Bug Fixes
- **(bitbake)** define keybindings consistently - ([f424e76](https://github.com/abougouffa/minemacs/commit/f424e764f09b34721ef8ca1ec40c0dea2ec02dc7)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(bootstrap)** add docstring to `+straight-prune-build-cache` - ([ab1707e](https://github.com/abougouffa/minemacs/commit/ab1707e438169bbbe096e3165a3823ffea6f76e7)) - [@abougouffa](https://github.com/abougouffa)
- generate the documentation and mention it in README - ([6e5408d](https://github.com/abougouffa/minemacs/commit/6e5408ddf707cc2de88bda253e87a2f274e8c802)) - [@abougouffa](https://github.com/abougouffa)
- minor edits and formatting - ([3daabd4](https://github.com/abougouffa/minemacs/commit/3daabd435f0bcb8376acd8428115587d4ea098d1)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(data)** replace `+csv-rainbow` with `rainbow-csv` - ([00a1cb0](https://github.com/abougouffa/minemacs/commit/00a1cb0efc5bf7efac617e9b6ab761484eed01a4)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add `gee` for Gerrit support in Emacs (useful for Yocto) - ([316099a](https://github.com/abougouffa/minemacs/commit/316099acddfac6902e3ac01fad6840c98b4b9ee1)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add initial support for `diffview` - ([dff169e](https://github.com/abougouffa/minemacs/commit/dff169eecba834fe6a50212be327052261f0fdb2)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** enable documentation generation using - ([309c71f](https://github.com/abougouffa/minemacs/commit/309c71f4ef5d1ba688651b3139d828adc02f4b0f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bitbake)** tailor bitbake modes and add keybindings - ([f0e38c7](https://github.com/abougouffa/minemacs/commit/f0e38c7e69495df7358a57406831ca4c8a2756b3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** demote errors when loading modules unless in debug mode - ([6b3b4cc](https://github.com/abougouffa/minemacs/commit/6b3b4cc0092fb7033680e212c9f695f47efcec6d)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** minor edit - ([2e3c4ed](https://github.com/abougouffa/minemacs/commit/2e3c4edb7d32462c60063546bb9cc4b65c006a49)) - [@abougouffa](https://github.com/abougouffa)
- **(super-save)** add more trigger commands - ([49601c9](https://github.com/abougouffa/minemacs/commit/49601c95f05dcdd9f9516ace83e1638596a0709a)) - [@abougouffa](https://github.com/abougouffa)
- **(super-save)** use the default idle duration (5s) - ([d6e030d](https://github.com/abougouffa/minemacs/commit/d6e030d25812c62bee7cd7c1a305ce62d06ee0ac)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c2624e2](https://github.com/abougouffa/minemacs/commit/c2624e2c3c92e645ae7f595f89694f727aeb6600)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.1.1](https://github.com/abougouffa/minemacs/compare/v4.1.0..v4.1.1) - 2023-12-10
#### Documentation
- **(readme)** add a note on `general-describe-keybindings` - ([376266a](https://github.com/abougouffa/minemacs/commit/376266a82246fd99adbda65b9132f2f7af30b913)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update documentation - ([d8b62ce](https://github.com/abougouffa/minemacs/commit/d8b62ceadb9dc360fd11a92b97a47310e71a1625)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(latex)** add `latex-preview-pane` - ([525c936](https://github.com/abougouffa/minemacs/commit/525c9365c9c3c5ca0cf6056ba5f513679151d6c5)) - [@abougouffa](https://github.com/abougouffa)
- **(pdf-tools)** save/restore position in PDFs using `pdf-view-restore` - ([1e392e1](https://github.com/abougouffa/minemacs/commit/1e392e1f8c4e70241919a22020140e7d410cdc74)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(readme)** prettify - ([39677df](https://github.com/abougouffa/minemacs/commit/39677df506a1871a34987630c510a2156f6d0f36)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** make use of `minemacs-assets-dir` - ([5106970](https://github.com/abougouffa/minemacs/commit/5106970f144a484fc80b823ad479aff40b8262e1)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.1.0](https://github.com/abougouffa/minemacs/compare/v4.0.3..v4.1.0) - 2023-12-09
#### Bug Fixes
- **(core)** ensure `minemacs-extra-packages-dir` exists - ([8bd5f50](https://github.com/abougouffa/minemacs/commit/8bd5f505b65547a0824e7ee3374338c10646f3cf)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** mark only non-installed grammar for install - ([fd0477f](https://github.com/abougouffa/minemacs/commit/fd0477f202b1a0621b88a45381a6bc04d389b3bf)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+download-package-from-urls` to pkgs from non-VC URLs - ([4300caf](https://github.com/abougouffa/minemacs/commit/4300cafb94c6d472f9ba70eb7c9434e3e170d06d)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add support for `selection-highlight-mode` - ([e08c0c5](https://github.com/abougouffa/minemacs/commit/e08c0c5616f672939562b3f1304bf14139b903c9)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add support for `sr-speedbar` - ([dfc4c32](https://github.com/abougouffa/minemacs/commit/dfc4c323e42f24cfa41b6d63db3e0db326863908)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** add `julia-repl` - ([f5aaca2](https://github.com/abougouffa/minemacs/commit/f5aaca22ce8ed4dcde5f087570dd594621355da7)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** initial support for `quickrun` - ([ce60604](https://github.com/abougouffa/minemacs/commit/ce6060471394fdd742eb36e6189d82091320511e)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `html-ts-mode` - ([483224c](https://github.com/abougouffa/minemacs/commit/483224cdb8d1dada2b3460701ba5f84649ed0dc3)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for just files - ([42e336d](https://github.com/abougouffa/minemacs/commit/42e336d036d9b549cd80d7633c6fa515d88f80cc)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** 29.2 is not yet available - ([375ead4](https://github.com/abougouffa/minemacs/commit/375ead4db938f5990ff193cb3c87dfcad86db8f5)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** don't fail on Emacs snapshot - ([ad5a28c](https://github.com/abougouffa/minemacs/commit/ad5a28c9b818018955a0adcc17726199c235055c)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** add Emacs 28.1 & 29.2 to the matrix - ([5f09582](https://github.com/abougouffa/minemacs/commit/5f09582420d45712e7360b986b092d3db6bf15c1)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- correct a typo in a commit - ([86de1df](https://github.com/abougouffa/minemacs/commit/86de1dfd5b8efa905e197cc6bdc93294ee4d8a16)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** use a dashed line as a display fill column indicator - ([68736c1](https://github.com/abougouffa/minemacs/commit/68736c1c51a875a56bac1989f7bd7386e79a6ecb)) - [@abougouffa](https://github.com/abougouffa)
- **(combobulate)** use `M-S-<up/down/left/right>` to avoid conflict - ([30ee5ee](https://github.com/abougouffa/minemacs/commit/30ee5eef8377264662f7b153f7e172026de5c375)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** use canonical function naming - ([84f653f](https://github.com/abougouffa/minemacs/commit/84f653f24ac1be14898f28634eca7d9c75624ee0)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** make use of `rename-visited-file` when available - ([ccefd5c](https://github.com/abougouffa/minemacs/commit/ccefd5c5387e5831379845670ffc542a743f7370)) - [@abougouffa](https://github.com/abougouffa)
- **(core-ui)** don't make line numbers small, doesn't integrate well - ([9527fe1](https://github.com/abougouffa/minemacs/commit/9527fe1a7ac4bc3c803494b7239fe6f19af0c0da)) - [@abougouffa](https://github.com/abougouffa)
- **(drag-stuff)** more intuitive keybindings - ([b3a925f](https://github.com/abougouffa/minemacs/commit/b3a925f9ba336a6de4c5b5d1fcfb8740d8868337)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight-mode)** use a different face than region - ([0991708](https://github.com/abougouffa/minemacs/commit/099170891a7f51ba6a1d025d002f923dad79fbef)) - [@abougouffa](https://github.com/abougouffa)
- **(sr-speedbar)** remove unnecessary require - ([b49c064](https://github.com/abougouffa/minemacs/commit/b49c064fd0ff77e12a71244f2ce16f0637376054)) - [@abougouffa](https://github.com/abougouffa)
- **(with-editor)** don't load on `julia-repl` - ([393d297](https://github.com/abougouffa/minemacs/commit/393d297e865b4f4fd29c15b0a4222ef03513b196)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.0.3](https://github.com/abougouffa/minemacs/compare/v4.0.2..v4.0.3) - 2023-12-08
#### Features
- **(ui)** add `anzu` to show number of matches in modeline - ([2f3b9e8](https://github.com/abougouffa/minemacs/commit/2f3b9e8f1db60e117cfa01f5d2a420770ed1be27)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** automatically detected all available modules - ([8456f69](https://github.com/abougouffa/minemacs/commit/8456f69e23c032b3232db686063c24b487d9ac75)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** don't force loading all packages in normal mode - ([baf864b](https://github.com/abougouffa/minemacs/commit/baf864bed9f01188e6834b1e0bf2ffb82ca0918a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(super-save)** remove the hack (merged upstream) - ([3bbe563](https://github.com/abougouffa/minemacs/commit/3bbe5639186189e891c7b6a5585a71f4e91db14e)) - [@abougouffa](https://github.com/abougouffa)
- **(super-save)** temporary support for `super-save-all-buffers` - ([bf671e2](https://github.com/abougouffa/minemacs/commit/bf671e2941de0362cb6ee92081bd40f8c216900d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([192503a](https://github.com/abougouffa/minemacs/commit/192503a5beaea566c453aa539f37d2bfcf078ad2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.0.2](https://github.com/abougouffa/minemacs/compare/v4.0.1..v4.0.2) - 2023-12-07
#### Features
- **(editor)** use the new `super-save` instead of `auto-save` - ([86dc4ec](https://github.com/abougouffa/minemacs/commit/86dc4ecfce64f1c3fa2628963ea4202b85cea08d)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** simplify and remove dead code - ([6c3a45e](https://github.com/abougouffa/minemacs/commit/6c3a45e66d054a78ebab426cbb5abf1139f6e0a5)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.0.1](https://github.com/abougouffa/minemacs/compare/v4.0.0..v4.0.1) - 2023-12-07
#### Bug Fixes
- **(core)** avoid `thing-at-point` errors - ([0a0dac7](https://github.com/abougouffa/minemacs/commit/0a0dac71aa8972d3feccef78759f9bfe2ca870be)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** don't reload theme on frame creation [#136] - ([defe63f](https://github.com/abougouffa/minemacs/commit/defe63f334769c43bf570ae705158a83633ae68c)) - [@abougouffa](https://github.com/abougouffa)
- move accidentally created `me-smartparens.el` to `obsolete` - ([a8c77a7](https://github.com/abougouffa/minemacs/commit/a8c77a7c628f33f3b6fd88297b668ee1d50324f0)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** add support for `auto-save` - ([ef19196](https://github.com/abougouffa/minemacs/commit/ef19196cf27b261a305be9f4f60ae8aa9f6b2a11)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add `org-re-reveal`, `org-re-reveal-citeproc` & `oer-reveal` - ([3b1e01c](https://github.com/abougouffa/minemacs/commit/3b1e01c1b67eaaf1576c0bb920368edfe228c0cd)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `guix` - ([920dee5](https://github.com/abougouffa/minemacs/commit/920dee518d154bbe1bbe7f0d346cff8756611ac7)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(core)** insert lines to separate sections in `me-lib` - ([e1cf22a](https://github.com/abougouffa/minemacs/commit/e1cf22a151d8098e458dd1a30d29a6dae9e8494a)) - [@abougouffa](https://github.com/abougouffa)
- comment - ([1df87a0](https://github.com/abougouffa/minemacs/commit/1df87a0fdb9b2f63253f52af4ec65daa4ad743a7)) - [@abougouffa](https://github.com/abougouffa)
- code formatting - ([7b4fe80](https://github.com/abougouffa/minemacs/commit/7b4fe80e9fc855681ea342152edd6465d074b301)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** remove unneeded checks in persistent scratch - ([5bccc99](https://github.com/abougouffa/minemacs/commit/5bccc995627e72a003ca3087a1c883f0652d986f)) - [@abougouffa](https://github.com/abougouffa)
- **(splash)** minor edit - ([5297c42](https://github.com/abougouffa/minemacs/commit/5297c42330ab94b6ea5da139f26ede03d60a8d78)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(ui)** restore the smaller lines numbers tweak - ([88a9282](https://github.com/abougouffa/minemacs/commit/88a9282bb1a8524c790e4bf90024803bfc9efd7d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(backports)** avoid problems on Emacs 28 - ([9ce07ee](https://github.com/abougouffa/minemacs/commit/9ce07eeea704d5532bb0329d0cfa1b2e7dc2057a)) - [@abougouffa](https://github.com/abougouffa)
- **(backports)** better compatibility with Emacs 28 - ([c09eca1](https://github.com/abougouffa/minemacs/commit/c09eca114b1bd75d6fd3f83fd15a2be04c7395fb)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** better conditions for `mu4e` and `elfeed` - ([bc51ebf](https://github.com/abougouffa/minemacs/commit/bc51ebfa28dbac3727edb716bca5d33e7a190713)) - [@abougouffa](https://github.com/abougouffa)
- **(elec-pair)** don't complete / in Org (annoying when writing paths) - ([931f584](https://github.com/abougouffa/minemacs/commit/931f58494b084a47ba62249ae826038ea29730e8)) - [@abougouffa](https://github.com/abougouffa)
- **(lib)** make `+single-file` a command - ([926262b](https://github.com/abougouffa/minemacs/commit/926262b908dae731c72303fd92fec6e90e0fb7bf)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** use filled numbers for hints - ([a76e2c0](https://github.com/abougouffa/minemacs/commit/a76e2c01f7b5c552193f2095851e9ea6f5aa9f68)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** better defaults + use of nerd-fonts for the close button - ([b6bb265](https://github.com/abougouffa/minemacs/commit/b6bb26513ea01d298b9c2e3f20151b6d4a0c8c04)) - [@abougouffa](https://github.com/abougouffa)
- **(tldr)** minor tweaks - ([bc26fd9](https://github.com/abougouffa/minemacs/commit/bc26fd98d6f45975882cc26f1e17356746a20f56)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** conditionally install/configure packages - ([0446e34](https://github.com/abougouffa/minemacs/commit/0446e34acef51a7bc81ad1cc881a632f716ac053)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([051eb74](https://github.com/abougouffa/minemacs/commit/051eb742159c7e551d81ca8cf03ac82c8cf0f0d0)) - [@abougouffa](https://github.com/abougouffa)
- multiple minor tweaks and edits - ([229dd56](https://github.com/abougouffa/minemacs/commit/229dd566e56b74a0d420b523a6a060e82f6f715e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.0.0](https://github.com/abougouffa/minemacs/compare/v3.11.0..v4.0.0) - 2023-11-30
#### Bug Fixes
- **(core)** undefined variable on Emacs 28 - ([6749e49](https://github.com/abougouffa/minemacs/commit/6749e49cb9092081404706422afe56ce0cded2f3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** loading `me-lib` fails on Emacs 28 - ([b199ffc](https://github.com/abougouffa/minemacs/commit/b199ffcf8d3f5536d06bee18ddbd1e54252bc6bf)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** load `early-config` instead of `early-init` - ([295cf57](https://github.com/abougouffa/minemacs/commit/295cf571fc2d48512b4cfe6b873daaa6ee9e4324)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(core)** minor documentation edits - ([0160a85](https://github.com/abougouffa/minemacs/commit/0160a858ec548b692305cba73997138d8030a184)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better documentation and comments - ([a21d66f](https://github.com/abougouffa/minemacs/commit/a21d66fa22d6e32fbc19c735953913bbb9ca9707)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more documentation for core functions - ([312485f](https://github.com/abougouffa/minemacs/commit/312485fe66fc27dd1328e88ab33412ea3f77c142)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(files)** add `dired-rsync` - ([0fc0c9b](https://github.com/abougouffa/minemacs/commit/0fc0c9b4b60e1747d32f10e149fd0649cd2ef094)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(skel)** correct some typos - ([1ade54f](https://github.com/abougouffa/minemacs/commit/1ade54f7b56b3411559ce172244e111fb52dcb12)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** move persistent scratch routines to `me-lib` - ([fa44fa3](https://github.com/abougouffa/minemacs/commit/fa44fa3735d75a096f897e6d3d1ef8cb10c997b0)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** move all core tweaks to `me-lib` - ([aca3d52](https://github.com/abougouffa/minemacs/commit/aca3d5277284d66af837b906c843bb782bbc36a6)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(fonts)** remove obsolete aliases - ([f127b7f](https://github.com/abougouffa/minemacs/commit/f127b7f6c98a9660258bcfaad3b5dc9ffaba5f87)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([58e5105](https://github.com/abougouffa/minemacs/commit/58e5105fb735b5b8a651bce42d6e3adf9bfc581f)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([8c2cfae](https://github.com/abougouffa/minemacs/commit/8c2cfae248552d5f2e59827cc348a1aebd601c95)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.8](https://github.com/abougouffa/minemacs/compare/v3.10.7..v3.10.8) - 2023-11-26
#### Bug Fixes
- **(org)** stick Org to the built-in (stable) version - ([7c8635b](https://github.com/abougouffa/minemacs/commit/7c8635b89738ee29de835bf63faaf24125402f58)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(tools)** add `nix-ts-mode` and tweak Eglot servers to run for it - ([25b98eb](https://github.com/abougouffa/minemacs/commit/25b98ebc1eb058ae51a12225f45f4f0c7788d4ec)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(treesit-auto)** add Nix, use the `treesit-auto-langs` variable - ([600691e](https://github.com/abougouffa/minemacs/commit/600691ed2595a90be9b083fb9fa7f5753741e5c8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.7](https://github.com/abougouffa/minemacs/compare/v3.10.6..v3.10.7) - 2023-11-26
#### Features
- **(checkers)** add `flymake-pyre` - ([184f4b2](https://github.com/abougouffa/minemacs/commit/184f4b2e3b6cc449e54ed2534fb06bc635fa9c2f)) - [@abougouffa](https://github.com/abougouffa)
- **(completion)** add `wgrep` (integrates with `consult` & `embark`) - ([4b66448](https://github.com/abougouffa/minemacs/commit/4b6644814603113486c7e924eb87d86ab166e43e)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `direnv` - ([aefdfa1](https://github.com/abougouffa/minemacs/commit/aefdfa1a3be7a77601229e5a27b532cc8792a202)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for Nix - ([0868ba9](https://github.com/abougouffa/minemacs/commit/0868ba9b7ec2570c062b87b4b918b2045f97e384)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(builtin)** disable `auto-save-visited-mode` - ([ca6725a](https://github.com/abougouffa/minemacs/commit/ca6725ab192f31a4629968f66eb3e12e8f15c9f0)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** let straight decide where to get builtin packages - ([0f7300a](https://github.com/abougouffa/minemacs/commit/0f7300aa03b05b5ad0b2a047c3298e152b602e40)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** bind `grep`/`file` alongside with `rg`/`fd` - ([845bfb8](https://github.com/abougouffa/minemacs/commit/845bfb824341d5ceb485642a5355a5086202f926)) - [@abougouffa](https://github.com/abougouffa)
- **(org-modern)** disable rendering checkboxes as unicode chars - ([ee8f74a](https://github.com/abougouffa/minemacs/commit/ee8f74a636e81d723bc071d5b6a9e09bf669820c)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** add Nix-REPL to the REPLs display rule - ([ca2923c](https://github.com/abougouffa/minemacs/commit/ca2923c4db6babb8b1ecfa041750e41b0f15a4d8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1136d0a](https://github.com/abougouffa/minemacs/commit/1136d0accf0f3044619b0c30b5b6e72ecfbf416a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([72086e9](https://github.com/abougouffa/minemacs/commit/72086e95ee23e597747996890e5cb15f6f017595)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.6](https://github.com/abougouffa/minemacs/compare/v3.10.5..v3.10.6) - 2023-11-25
#### Nitpicks, changes with no side effect
- minor refactors - ([d65b966](https://github.com/abougouffa/minemacs/commit/d65b9660d48d463dd99c84efa9ac5f253621b028)) - [@abougouffa](https://github.com/abougouffa)
- correct typos - ([de4184f](https://github.com/abougouffa/minemacs/commit/de4184f64e01341eafa3335750f17f5d41dfe52c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** remove `pgformatter` as it is merged upstream - ([3328cea](https://github.com/abougouffa/minemacs/commit/3328cea4f15f7c0ae1e438ff8f3a4f0c9f771078)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** enable `auto-save-visited-mode` - ([81c0441](https://github.com/abougouffa/minemacs/commit/81c0441597de9c47cca20f3f5b14ce59dfb2e86d)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-notes)** use only for Denote - ([d0bb198](https://github.com/abougouffa/minemacs/commit/d0bb1987f7a92c68e076ebef8d400a659ff47abc)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove the never used `me-meow` module - ([ab863c9](https://github.com/abougouffa/minemacs/commit/ab863c9065bd25378d64fc0989e2a59221f5bf19)) - [@abougouffa](https://github.com/abougouffa)
- **(keybinding)** bind `other-window-prefix` to `SPC O` - ([aafcb2d](https://github.com/abougouffa/minemacs/commit/aafcb2d78899e03eccbb5e33b41fe24b4231961e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([181815a](https://github.com/abougouffa/minemacs/commit/181815aed87e4723f9c14ec01ed438ec585534fc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.5](https://github.com/abougouffa/minemacs/compare/v3.10.4..v3.10.5) - 2023-11-23
#### Features
- **(checkers)** add `flymake-nasm` - ([25eb2a2](https://github.com/abougouffa/minemacs/commit/25eb2a24ed43d4d1edf1e3cac677be861657f5a4)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** add `flymake-pmd` - ([79b09b0](https://github.com/abougouffa/minemacs/commit/79b09b09a7ead1be16f3c26b8ab96a9b45fcdab4)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** add `flymake-relint` to check regexps in Elisp - ([8639ad4](https://github.com/abougouffa/minemacs/commit/8639ad49268bc6804699a073516d36959749c836)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `sideline-blame` - ([0527103](https://github.com/abougouffa/minemacs/commit/0527103013a2c0ce1a878532bdbceee91c7893e6)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** make `blamer` obsolete - ([625d1d4](https://github.com/abougouffa/minemacs/commit/625d1d424bed31696acd98ccf3979191274fdcf3)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(checkers)** remove dead code - ([b52cdc0](https://github.com/abougouffa/minemacs/commit/b52cdc0fba5baf832f508f32de1ca55467259028)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(checkers)** code formatting - ([762c1c4](https://github.com/abougouffa/minemacs/commit/762c1c48e6fcb3469f16d22abc6365f68facd3fc)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(ui)** remove `sideline-blame` - ([189ce9c](https://github.com/abougouffa/minemacs/commit/189ce9c020602c14a2125921eb4a16cced64d540)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(elec-pair)** don't pair * in Org mode - ([c466e01](https://github.com/abougouffa/minemacs/commit/c466e01223ac09463cfd87fb03e8f0313946ec01)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** activate more checkers for Python - ([36dac31](https://github.com/abougouffa/minemacs/commit/36dac319332da92422c44f257edec0eb5a2318dd)) - [@abougouffa](https://github.com/abougouffa)
- **(sideline)** change date format for `sideline-blame` - ([557af19](https://github.com/abougouffa/minemacs/commit/557af1927e89f44284637c40c92c703522cdbdd5)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** add more rules for displaying help buffers - ([a45e026](https://github.com/abougouffa/minemacs/commit/a45e026b22f5f51707fd9a74ebbc0f2bc6243417)) - [@abougouffa](https://github.com/abougouffa)
- bump package versions - ([efdf896](https://github.com/abougouffa/minemacs/commit/efdf896f57ee4b46ff6e99501d123ff0707c4f43)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.4](https://github.com/abougouffa/minemacs/compare/v3.10.3..v3.10.4) - 2023-11-21
#### Bug Fixes
- **(parinfer)** ensure that the directory exits (#129) - ([620cd92](https://github.com/abougouffa/minemacs/commit/620cd92aa98f46870b346312daa0667cbd8eccda)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** disable the buggy `treesit-fold` - ([94b31d6](https://github.com/abougouffa/minemacs/commit/94b31d6fb1915ff7c90966064fe7c6d28c862643)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** add a caveat to `config.el` - ([a9dd90f](https://github.com/abougouffa/minemacs/commit/a9dd90fa47240fc59789e094d96accbdd7149b83)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(builtin)** use `electric-pair-mode` for the moment - ([8301aee](https://github.com/abougouffa/minemacs/commit/8301aeeee8cffc2591d9b3903e56a1247a9e2dfd)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** initial support for `project-cmake` - ([92d4b89](https://github.com/abougouffa/minemacs/commit/92d4b8912d66cd7fb8c95c5d2ed750d91b3d96be)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `solaire-mode` - ([ed68c64](https://github.com/abougouffa/minemacs/commit/ed68c644bccec956f906acf5c8fff0e07bd3eb8d)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(core-ui)** remove `lin` - ([fb76722](https://github.com/abougouffa/minemacs/commit/fb76722ea4cb3aba3f8a14c459b1bf27dd1e1998)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(editor)** make `smartparens` obsolete - ([64a3601](https://github.com/abougouffa/minemacs/commit/64a360172eec553c53f8ce168acf0e49531b51ad)) - [@abougouffa](https://github.com/abougouffa)
- **(electric-pair)** more rules for Org-mode and Markdown - ([3cd3a7d](https://github.com/abougouffa/minemacs/commit/3cd3a7d7cce74fadc6746f5d75252f1f80c4c5ab)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** remove duplicate settings - ([0712396](https://github.com/abougouffa/minemacs/commit/07123965b32c85bba8cc574ff894d5009a8c883f)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** show `man` and `woman` on a dedicated side window - ([95f2637](https://github.com/abougouffa/minemacs/commit/95f26370ead16b755952568157aac4842065d137)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([76756f5](https://github.com/abougouffa/minemacs/commit/76756f5b50b85e456ed924ffd705f673608f7bb0)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.3](https://github.com/abougouffa/minemacs/compare/v3.10.2..v3.10.3) - 2023-11-20
#### Bug Fixes
- **(transient)** install from Elpa on Emacs 28 - ([978a698](https://github.com/abougouffa/minemacs/commit/978a698545925bd6888f5c25adf4ccb94e0dab80)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core-ui)** add `lin` - ([d3e90a3](https://github.com/abougouffa/minemacs/commit/d3e90a37c1bf936b2809c06cd43839e28c8b369c)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add `+project-forget-duplicate-projects` helper - ([87345a3](https://github.com/abougouffa/minemacs/commit/87345a304b4768dce2ce82a10ba8aa94952dfaab)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** minor tweak - ([fcb11fe](https://github.com/abougouffa/minemacs/commit/fcb11fe53ebe2374631bf06502f6a927f5d554e7)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** define menu with `transient` instead of `hydra` - ([e615b37](https://github.com/abougouffa/minemacs/commit/e615b37a18c20fba00e812fdf5464a86633e843e)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** centralize global keybindings for builtin stuff - ([166233b](https://github.com/abougouffa/minemacs/commit/166233b9ffff3eeeeadbdb0cb32642c84bb2c29f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dape)** add keybindings - ([28dea66](https://github.com/abougouffa/minemacs/commit/28dea66c4b1227b939e9e9490667cca55cb101b5)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** now the batteries are already included! - ([6308572](https://github.com/abougouffa/minemacs/commit/6308572ba879e94b4a9cba7d2300e0e273dc8cb4)) - [@abougouffa](https://github.com/abougouffa)
- **(denote)** better integration with Emacs' builtin packages - ([029d78b](https://github.com/abougouffa/minemacs/commit/029d78b62542109388ef003950b4a10f0a6bf09c)) - [@abougouffa](https://github.com/abougouffa)
- **(denote)** autoload command aliases and tweak keybindings - ([383fe1a](https://github.com/abougouffa/minemacs/commit/383fe1a23579a0759107f626b8201a840bc4a014)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** add debug section - ([af3566f](https://github.com/abougouffa/minemacs/commit/af3566f6d9fc47be248c1c0ca8bb51c872999d26)) - [@abougouffa](https://github.com/abougouffa)
- **(ligature)** make it obsolete, buggy when the font have no ligatures - ([2392012](https://github.com/abougouffa/minemacs/commit/2392012c54062a1730d1057d31323893e6cef5c7)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** better cleanup of duplicate projects - ([086eff9](https://github.com/abougouffa/minemacs/commit/086eff9c73f30f88d6e00ad18e6926f23ffc0940)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ea121ee](https://github.com/abougouffa/minemacs/commit/ea121ee4439b45677ad269c16bd4f5bdf96196e1)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([02da9d2](https://github.com/abougouffa/minemacs/commit/02da9d2584c32c9f58e9de4aeec2fdc73139f481)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.2](https://github.com/abougouffa/minemacs/compare/v3.10.1..v3.10.2) - 2023-11-19
#### Documentation
- **(readme)** update screenshot - ([c36712c](https://github.com/abougouffa/minemacs/commit/c36712cf744e324a3487f625672c3499183e8b65)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(flymake)** add `flymenu-flymake` - ([8d9cf1c](https://github.com/abougouffa/minemacs/commit/8d9cf1c35da07bf85000cdcc6327aaa65c3786dd)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(spell-fu)** minor changes - ([37e3bfd](https://github.com/abougouffa/minemacs/commit/37e3bfda8302e0342cd7001bdfb18c79c72fd26f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.1](https://github.com/abougouffa/minemacs/compare/v3.10.0..v3.10.1) - 2023-11-19
#### Bug Fixes
- **(window)** restore current buffer on title bar - ([b715232](https://github.com/abougouffa/minemacs/commit/b7152323a8407b65eb29b65fd4c5e5220bc1d787)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(flymake)** add `flymake-guile` - ([1dcdd21](https://github.com/abougouffa/minemacs/commit/1dcdd2187062b4391a34ce05ddb75273050fec0f)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** add `flymake-cppcheck` - ([9cd9ede](https://github.com/abougouffa/minemacs/commit/9cd9ede25f20d4fd99c73e6584ff2abfd8b8ffc5)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** add `julia-ts-mode` - ([1956bee](https://github.com/abougouffa/minemacs/commit/1956bee919af7c222c596b2f43502ae9a57dee9e)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** restore Maxima configuration - ([9a2b356](https://github.com/abougouffa/minemacs/commit/9a2b35695b206f6f1dfe5f98469f72f638fb1292)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(builtin)** correct typos in comments - ([4719f17](https://github.com/abougouffa/minemacs/commit/4719f17d5661e37d95f51c2aef95eccfbfe652c0)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(window)** better window placement for REPL buffers - ([7e66878](https://github.com/abougouffa/minemacs/commit/7e66878c1cabd069160f0009170827f53d0da175)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** show help/helpful/info buffers in a dedicated window - ([16f7d57](https://github.com/abougouffa/minemacs/commit/16f7d574b52964fab4325008d8d4c58dcbe77e79)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** smaller help window (40%) - ([4416062](https://github.com/abougouffa/minemacs/commit/4416062780338f63b4ee0d99dd424b6bf7e662c8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.0](https://github.com/abougouffa/minemacs/compare/v3.9.1..v3.10.0) - 2023-11-19
#### Bug Fixes
- **(plantuml)** buggy `use-package` block - ([66b23c6](https://github.com/abougouffa/minemacs/commit/66b23c61186f702e4fd6fa37f1c97af5f9f52af6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(checkers)** add `flymake-quickdef` - ([8db462e](https://github.com/abougouffa/minemacs/commit/8db462e42caf9c5de689ed2dae78762933222e74)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** remove old `flymake-easy` - ([1e8c126](https://github.com/abougouffa/minemacs/commit/1e8c12623fddce3d250633e2f865eac5e86e675f)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** add `flymake-shellcheck` - ([895a328](https://github.com/abougouffa/minemacs/commit/895a3282831cfbe5f215ad4e957c5a6d15779cfc)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** add `flymake-ruff` - ([cf838c7](https://github.com/abougouffa/minemacs/commit/cf838c76cbbe6c83b3ba7ebdac825de8a89d1e65)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** add `flymake-collection` - ([a7ae42c](https://github.com/abougouffa/minemacs/commit/a7ae42cbfe7271b5b34440b43cde9a73379ec0da)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** add a backend for Codespell - ([b15b6cb](https://github.com/abougouffa/minemacs/commit/b15b6cb7c8245f356b64bfb65ab0e48c97f8eaa6)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** add `flymake-plantuml` - ([b17b039](https://github.com/abougouffa/minemacs/commit/b17b039eaf945803737e35983c6058fbebfcd628)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** add Bandit backend for Python - ([c326db8](https://github.com/abougouffa/minemacs/commit/c326db8aec9083803aade6d9287bf0423c7aa832)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `web-mode` - ([efee175](https://github.com/abougouffa/minemacs/commit/efee17562608ab1ace1d80dc1b0315141c12a447)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(checkers)** remove `flymake-ruff`, present in `flymake-collection` - ([33c0fc3](https://github.com/abougouffa/minemacs/commit/33c0fc39477e952328c3f44619966c5eab2d2069)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** `flymake-shellcheck` included in `flymake-collection` - ([8b38ff5](https://github.com/abougouffa/minemacs/commit/8b38ff53389dfd949ca1823aaea325d2c235c0ac)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** add formatters for SQL - ([c6ceef0](https://github.com/abougouffa/minemacs/commit/c6ceef01d9e15d7e3cf9e8acfd03b7aad1c87c1d)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** auto enable when relevant - ([e21d176](https://github.com/abougouffa/minemacs/commit/e21d17689a6973e70f9c6e28c23e76f42148bdaa)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-quickdef)** advice to auto generate `+flymake-*-load` func - ([e12c8a7](https://github.com/abougouffa/minemacs/commit/e12c8a70cd1e717c06b435379f42bc3d70771b63)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** add a template for shell scripts in Org mode - ([fd1b3ae](https://github.com/abougouffa/minemacs/commit/fd1b3aee979dc4316390c7aeeee5193681e0806f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.9.1](https://github.com/abougouffa/minemacs/compare/v3.9.0..v3.9.1) - 2023-11-18
#### Bug Fixes
- **(vars)** buggy variable set - ([8ca2aba](https://github.com/abougouffa/minemacs/commit/8ca2abab98ba72e078ed366f8f881f9ad8a7f4bf)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.9.0](https://github.com/abougouffa/minemacs/compare/v3.8.1..v3.9.0) - 2023-11-18
#### Bug Fixes
- **(treesit-fold)** wrong `:after` block - ([2745d3b](https://github.com/abougouffa/minemacs/commit/2745d3be9dd4934979af98d25f6a4ede993f06d6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(formal)** add some modes for formal verification/proof tools - ([05d8c02](https://github.com/abougouffa/minemacs/commit/05d8c02b167719122ae42d323f43574dc2b2c4ef)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- minor code formatting - ([34c10e2](https://github.com/abougouffa/minemacs/commit/34c10e276b19b274730f8ea5fd51e54138301ccd)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(auctex-latexmk)** remove unnecessary hook - ([db88d81](https://github.com/abougouffa/minemacs/commit/db88d81e6e94fcae2a960c6c21adeab53c4dd650)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** rename some variables - ([f552bd6](https://github.com/abougouffa/minemacs/commit/f552bd656bf1f5c3d814a19c619d420fb4a65a2a)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** minor changes - ([24a19a7](https://github.com/abougouffa/minemacs/commit/24a19a755e692a2b4a4f2fa890115ee10ae2de6c)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(io)** `crux-open-with` provides this functionality - ([0fba2b7](https://github.com/abougouffa/minemacs/commit/0fba2b795206b101f29c9a356052bda02ce53379)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** remove unnecessary formatters (included in upstream) - ([5acf050](https://github.com/abougouffa/minemacs/commit/5acf050a960d2d4aa436bed6bd29bb87766f3229)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** hide time icon - ([728d803](https://github.com/abougouffa/minemacs/commit/728d8033878795d9d512adaadf6048b36788d80a)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-multiedit)** minor keybinding changes - ([79cd9c8](https://github.com/abougouffa/minemacs/commit/79cd9c855231ea89750420f8bf11a72ec6c49b23)) - [@abougouffa](https://github.com/abougouffa)
- **(fonts)** rename function - ([3cb67a1](https://github.com/abougouffa/minemacs/commit/3cb67a1be14f248b1ad63bd473d40d593f9fda15)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** better check for `parinfer-rust` compatibility - ([636d673](https://github.com/abougouffa/minemacs/commit/636d6733cac18a6022f77c2d685eb1c1333a739c)) - [@abougouffa](https://github.com/abougouffa)
- **(vterm)** minor keybinding changes - ([7bfc6dd](https://github.com/abougouffa/minemacs/commit/7bfc6dd40683fea1b16f0731ec80b106df534d61)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** cleanup irrelevant code - ([b39dfd2](https://github.com/abougouffa/minemacs/commit/b39dfd29d3c7c0a5159b76a76b6b4dd06b7230b1)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** more accurate display buffer rules - ([28f5f4f](https://github.com/abougouffa/minemacs/commit/28f5f4fdec8fed849f4ec08ebc2963568122a442)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([c688dac](https://github.com/abougouffa/minemacs/commit/c688dac7a196230d9715dbc51e759434a4b1518b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.8.1](https://github.com/abougouffa/minemacs/compare/v3.8.0..v3.8.1) - 2023-11-17
#### Bug Fixes
- **(treesit-fold)** load only when `treesit` is available - ([c033b65](https://github.com/abougouffa/minemacs/commit/c033b65d992cd2298d0a4827aa4071899ed2f530)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(multi-cursors)** add `multiple-cursors` (dep of `combobulate`) - ([879200e](https://github.com/abougouffa/minemacs/commit/879200ee580d68c6cf47fad9efd6863eafbe8df5)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** exit minibuffer from anywhere using `S-ESC` - ([070a9bd](https://github.com/abougouffa/minemacs/commit/070a9bd8bd62b1e691852b220e7be4a2e983b99b)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** additional adapters for GO and JS - ([664c17b](https://github.com/abougouffa/minemacs/commit/664c17ba73f23de96ddd4553b2a03c76dc64ac9a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.8.0](https://github.com/abougouffa/minemacs/compare/v3.7.0..v3.8.0) - 2023-11-17
#### Bug Fixes
- **(binary)** don't objdump remote files - ([0fb752f](https://github.com/abougouffa/minemacs/commit/0fb752f0fcb7496953aa5896b30d6d6dd525e1a2)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better environement variables management - ([818131c](https://github.com/abougouffa/minemacs/commit/818131c897afd38544231d46d1eab28d27413253)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** load the right local configuration files - ([eae5274](https://github.com/abougouffa/minemacs/commit/eae5274f1736f07c2b162299781dde8b0c9c031e)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** load immediately if a directory is passed to Emacs as arg - ([5340fd8](https://github.com/abougouffa/minemacs/commit/5340fd804937cac7bf0104ff547a526dfa41e9f2)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** remove unused `ox-pandoc` - ([a544398](https://github.com/abougouffa/minemacs/commit/a544398b60d50b4ffe5478468f434f054623c8f8)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** correctly check if project root is a directory - ([7b31d73](https://github.com/abougouffa/minemacs/commit/7b31d738117a63a2838f55e48f3ee402d1ac4608)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(core)** update init file documentation - ([b65ee69](https://github.com/abougouffa/minemacs/commit/b65ee69e2f208746341f60ab4c0f9daf500c45c5)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update documentation - ([32ed27e](https://github.com/abougouffa/minemacs/commit/32ed27e76a806437d08b6bc435b34f886eea0949)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** enable local (machine-specific) configurations - ([fb5c52c](https://github.com/abougouffa/minemacs/commit/fb5c52c4d4268232a1644dc25c6cd4478e8f64e7)) - [@abougouffa](https://github.com/abougouffa)
- **(debug)** initial support for `dape` - ([125d68c](https://github.com/abougouffa/minemacs/commit/125d68cba520c06656ef706d81f4a6f7a8eff8e8)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** use my `treesit-fold` fork of `ts-fold` - ([0fe2482](https://github.com/abougouffa/minemacs/commit/0fe24829dc8ae942b71ab54f26b6156d2ccd47c0)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** add `+open-with-default-app` - ([b84e73e](https://github.com/abougouffa/minemacs/commit/b84e73eb8693370abdca199daea1fb7fc4472e34)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make Chezmoi obsolete (migrated the simpler GNU Stow) - ([609781a](https://github.com/abougouffa/minemacs/commit/609781a4bf63c5abf718ea6a6265cc84bbfe94b1)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `app-launcher` - ([9881bbc](https://github.com/abougouffa/minemacs/commit/9881bbc0bf28c546aa57e3cf58dcc803ed0256e0)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** minor edit - ([2885f64](https://github.com/abougouffa/minemacs/commit/2885f64a55914ff1b8edfeed57be1fdd24ae6167)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** setup `gud` parameters separately - ([5c568d8](https://github.com/abougouffa/minemacs/commit/5c568d8d39903dd9251470e10e4b4d2323b44af3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simplify user config loading and ignoring - ([77d6cf2](https://github.com/abougouffa/minemacs/commit/77d6cf2a5550d395b03166f5a59e4bacd94d221d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(binary)** move the remote file check to `+binary-objdump-p` - ([f4a21da](https://github.com/abougouffa/minemacs/commit/f4a21da6d2e7e35b81ef77630a59b2de4f0ff558)) - [@abougouffa](https://github.com/abougouffa)
- **(blamer)** store avatars in MinEmacs' cache - ([6b6ef59](https://github.com/abougouffa/minemacs/commit/6b6ef59908c20c3623b2271810191e6dc3f61c11)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** don't exit minibuffer on mouse click - ([396d9ee](https://github.com/abougouffa/minemacs/commit/396d9eeeb6a458c4dcf50ac3071a611eea837a75)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add an option to disable all user config files - ([5c6739e](https://github.com/abougouffa/minemacs/commit/5c6739ef8dbd7c0f0d2f331d6a0b2a045563693b)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** initial configuration for adapters - ([9b73891](https://github.com/abougouffa/minemacs/commit/9b7389184bd0bf8ce70f6f731bb39e9832636688)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** decrease height form 35 to 28 - ([4b3fa3d](https://github.com/abougouffa/minemacs/commit/4b3fa3dafd16d6e15e1ef38acd9b15743c4f0b59)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** make `evil-escape` obsolete - ([b107371](https://github.com/abougouffa/minemacs/commit/b1073716fc2ac5957d892f8acc841e631a2b5fb2)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** bind `+open-with-default-app` to `SPC o SPC` - ([d65c6a0](https://github.com/abougouffa/minemacs/commit/d65c6a08e7379a6379d35ed805703976db9ea09d)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `ts-fold` obsolete - ([ee7208a](https://github.com/abougouffa/minemacs/commit/ee7208ae413b11c9a2c63d54eedc68ddb6567fd2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2651ac6](https://github.com/abougouffa/minemacs/commit/2651ac6873c925e7a9439cbe3fa14e2fb1fc45b1)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([1018a12](https://github.com/abougouffa/minemacs/commit/1018a1295d8899b36742a8f67cb2640ad85af2ad)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.7.0](https://github.com/abougouffa/minemacs/compare/v3.6.1..v3.7.0) - 2023-11-12
#### Bug Fixes
- **(consult-dir)** load after `vertico` - ([7c27876](https://github.com/abougouffa/minemacs/commit/7c278769dc20ff059e67152a417a13667aff10c8)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** don't try to add inexistant directories - ([50b8448](https://github.com/abougouffa/minemacs/commit/50b8448bc2fe93934370b92f3138f77abec4d118)) - [@abougouffa](https://github.com/abougouffa)
- **(sudo-edit)** no extra overhead - ([c82ac39](https://github.com/abougouffa/minemacs/commit/c82ac3916ece50c5c0318166e5b6ac7b7a311edd)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(faq)** add a Tramp related question - ([dce96a0](https://github.com/abougouffa/minemacs/commit/dce96a049a9c9e0ad4811646930da8d9ce08884e)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** comment commands that are overwritten elsewhere - ([8c6e4d1](https://github.com/abougouffa/minemacs/commit/8c6e4d13b9f4ba3c7eb99e1b17c1cb8c97c285dc)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ace-window)** explicitly include Ace + add keybindings - ([2a7b4b5](https://github.com/abougouffa/minemacs/commit/2a7b4b5b13d4bbb9abf5447f0a68158fb53d8568)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `sudo-edit` - ([faa1299](https://github.com/abougouffa/minemacs/commit/faa12993c6dd55597e8fbd82164bd2c6a3133902)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** explicitly add `avy` - ([005f1eb](https://github.com/abougouffa/minemacs/commit/005f1eb27d4c86c976915a9484ba8e35721faf8a)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `with-editor` - ([8ae2ed8](https://github.com/abougouffa/minemacs/commit/8ae2ed820e5bb4c10916c4318699f6843e1823f4)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** minor Makefile edit - ([8ca3831](https://github.com/abougouffa/minemacs/commit/8ca38315cb6cb421aaba080f62d1ecfe965fee9e)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** minor tweaks - ([1b3f32c](https://github.com/abougouffa/minemacs/commit/1b3f32cb496b8bee230415ffbd369fafd5c8e93a)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** test MinEmacs in daemon mode - ([4efaf20](https://github.com/abougouffa/minemacs/commit/4efaf204abd027db5fad9f9cba0b65a9ca1c5094)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** run actions on workflow changes - ([a48d307](https://github.com/abougouffa/minemacs/commit/a48d307fad626b3000e473d791f9014d22cfb65f)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(jiralib2)** avoid confusing variable names - ([8d227b8](https://github.com/abougouffa/minemacs/commit/8d227b855e15cd1a67dc978a966ca9eee11d99ff)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(completion)** correctly load `vertico` and `corfu` extensions - ([0159597](https://github.com/abougouffa/minemacs/commit/01595977acceebf46ce026cc81a88b0edd752230)) - [@abougouffa](https://github.com/abougouffa)
- **(completion)** some code formatting - ([3fd8e40](https://github.com/abougouffa/minemacs/commit/3fd8e4012e63fa7ef8b3f5bc1814a39e0f7e2d16)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simpler `+sudo-save-buffer` - ([cccbb9a](https://github.com/abougouffa/minemacs/commit/cccbb9af3c29e7ac05064da7e9c2a1901fe4569b)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** don't reinvent the wheel - ([d746085](https://github.com/abougouffa/minemacs/commit/d746085ae53c0c27f6e176e4034833f75edb542a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(autoinsert)** disable `auto-insert-mode`, bind to `SPC f i` - ([a07f438](https://github.com/abougouffa/minemacs/commit/a07f438fa7f414289636921558eb2f2c149cb55b)) - [@abougouffa](https://github.com/abougouffa)
- **(blamer)** a little smaller font size - ([890f6bb](https://github.com/abougouffa/minemacs/commit/890f6bba5a028fa6bf139bae05fbe97b1b924816)) - [@abougouffa](https://github.com/abougouffa)
- **(chezmoi)** correctly load extensions - ([376bcfc](https://github.com/abougouffa/minemacs/commit/376bcfc0e5883a5b1bf9dd4cb526645155f0074f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** cleanup accidentally added `elpa` directories - ([aae6016](https://github.com/abougouffa/minemacs/commit/aae6016203fe2d7324cd733682f26c11c54a352f)) - [@abougouffa](https://github.com/abougouffa)
- **(embark)** better keybindings - ([da58755](https://github.com/abougouffa/minemacs/commit/da5875595be6c9c37b9bd445127806beb3e0f228)) - [@abougouffa](https://github.com/abougouffa)
- **(embark)** bind `embark-act` to `C-²` (for French AZERTY keyboards) - ([14385db](https://github.com/abougouffa/minemacs/commit/14385dbb558a6d3064fcd36c45e0ebe115a29346)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** accept `html` and `htm` extensions when converting to PDF - ([113515e](https://github.com/abougouffa/minemacs/commit/113515eefbde1d1a2f5d3c84703496f2ff4de908)) - [@abougouffa](https://github.com/abougouffa)
- **(treemacs)** no rename for keybinding - ([88b7e3e](https://github.com/abougouffa/minemacs/commit/88b7e3e1ef148ebcff1d485cfbbfe1696194f185)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.6.1](https://github.com/abougouffa/minemacs/compare/v3.6.0..v3.6.1) - 2023-11-10
#### Bug Fixes
- **(daemon)** empty font list if called too early - ([e99e989](https://github.com/abougouffa/minemacs/commit/e99e989815f85dad03d9931b14f7cc193e45a88d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(completion)** add `consult-dir` - ([877e677](https://github.com/abougouffa/minemacs/commit/877e6775130eb5c425e0b225637ddefe9504660b)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- use `use-package`'s `:bind` to bind keys - ([3637597](https://github.com/abougouffa/minemacs/commit/3637597e54761a9f4c0a34f04676b28459c714fa)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult)** add keybinding for `consult-yank-pop` - ([8ff02d2](https://github.com/abougouffa/minemacs/commit/8ff02d2ca83b488057a6ce88357ebef8cd117f9d)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** replace `org-roam` example with `denote`'s one - ([174f78a](https://github.com/abougouffa/minemacs/commit/174f78ab436a46486924821182e13ed7a0480c21)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** remove `affe`, buggy and stops randomly - ([bf54bcd](https://github.com/abougouffa/minemacs/commit/bf54bcdaeb5ed4351af5cfb2f21a67157d5643ff)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cbad02d](https://github.com/abougouffa/minemacs/commit/cbad02d3d64dd025a024a795d78ba840bd1fb514)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.6.0](https://github.com/abougouffa/minemacs/compare/v3.5.0..v3.6.0) - 2023-11-09
#### Bug Fixes
- **(builtin)** use `emacs` pseudo-package instead of `x-win` - ([da0e57d](https://github.com/abougouffa/minemacs/commit/da0e57ded6bfb60081da4fdc634509cbcee0f8c7)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** load `me-compat` before `me-builtin` - ([c5f39db](https://github.com/abougouffa/minemacs/commit/c5f39db85d2bd7cb831ab41df20de30c879ad2c3)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(consult)** fill initial query using `consult-customize` - ([600d22b](https://github.com/abougouffa/minemacs/commit/600d22b0c9c20930143e99a473b9d63544d0921c)) - [@abougouffa](https://github.com/abougouffa)
- move `transient` to the end of `me-builtin` - ([e7d7d07](https://github.com/abougouffa/minemacs/commit/e7d7d070e5f286f8ab6889b25ac322e33043bd46)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cocogitto)** display error message when not in VC directory - ([e2b85e6](https://github.com/abougouffa/minemacs/commit/e2b85e6a6d30d747e17c432a56006ef1937a9b2a)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** bind `consult-isearch-history` to `SPC s i` - ([542e5f3](https://github.com/abougouffa/minemacs/commit/542e5f38f3c441187b2171825971c4d2773e8476)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** add more useful keybindings - ([1fb5994](https://github.com/abougouffa/minemacs/commit/1fb599415a631b704d3329c9263cc0eaafc7ac5c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** update recipe repositories on update - ([9240670](https://github.com/abougouffa/minemacs/commit/92406704cc8bfc539d950109dfeb34abfc6f9c97)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** use `isearch` instead of `evil-search` - ([1182d73](https://github.com/abougouffa/minemacs/commit/1182d73ee06062ebe731f720f2681c368e78e856)) - [@abougouffa](https://github.com/abougouffa)
- **(isearch)** enable ring scrolling using `UP`/`DOWN` & `C-j`/`C-k` - ([04e985f](https://github.com/abougouffa/minemacs/commit/04e985fd4d3283c90b45007af0a18b99da9d58ba)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** add keybinding for `keep-lines` - ([6228628](https://github.com/abougouffa/minemacs/commit/622862840692369deedfe028d9d5828ae72b52cd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.5.0](https://github.com/abougouffa/minemacs/compare/v3.4.2..v3.5.0) - 2023-11-08
#### Bug Fixes
- **(core)** first file stuff loaded immediately when in daemon mode - ([5711ac8](https://github.com/abougouffa/minemacs/commit/5711ac833fd5f3c5287ebd619c5433ce98b3b565)) - [@abougouffa](https://github.com/abougouffa)
- **(treemacs)** remove `treemacs-evil` [#123] - ([96d6936](https://github.com/abougouffa/minemacs/commit/96d6936be0c4a8aa8c7bcf209d638af1916bcba2)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** move `me-defaults` to `me-builtin` - ([7a645ae](https://github.com/abougouffa/minemacs/commit/7a645ae5248acfcb259a2ec8d7e44e56a004e4bd)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** move generic daemon tweaks to `me-builtin` - ([c38c671](https://github.com/abougouffa/minemacs/commit/c38c6712194ec190bb432d21845c65d006d7910e)) - [@abougouffa](https://github.com/abougouffa)
- minor refactor, regenerate loaddefs - ([5807929](https://github.com/abougouffa/minemacs/commit/5807929102b94bec8cfac1ab93e3c5f33fb45c77)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.4.2](https://github.com/abougouffa/minemacs/compare/v3.4.1..v3.4.2) - 2023-11-07
#### Documentation
- **(readme)** minor edit - ([d3a494c](https://github.com/abougouffa/minemacs/commit/d3a494cd4e2326e85d51a0c7fe14b5b75095055b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(completion)** use `nerd-icons-corfu` instead of `kind-icons` - ([98c284f](https://github.com/abougouffa/minemacs/commit/98c284f5fe4e4112f3b21a4a69f3469825117b5b)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** show buffer size in mode line - ([b31d0ef](https://github.com/abougouffa/minemacs/commit/b31d0ef991ee42213f6578eaf751fc58e547004a)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** trigger github pages action only on Markdown changes - ([df2e586](https://github.com/abougouffa/minemacs/commit/df2e58628acfb63d25fde8d5693c26ffe620b72e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor edit - ([21e790d](https://github.com/abougouffa/minemacs/commit/21e790d87ffccbb4215f10db7cfbe64f4eee18e2)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edits - ([4168a1f](https://github.com/abougouffa/minemacs/commit/4168a1fe934530d96bc2bab1c393d0b745bbcc01)) - [@abougouffa](https://github.com/abougouffa)
- **(fonts)** minor change - ([68dc71b](https://github.com/abougouffa/minemacs/commit/68dc71b334c45a59a2a843c96b87601336679012)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.4.1](https://github.com/abougouffa/minemacs/compare/v3.4.0..v3.4.1) - 2023-11-05
#### Bug Fixes
- **(electric)** fix sh/bash keywords extraction from grammar - ([9341311](https://github.com/abougouffa/minemacs/commit/93413115c8eb5e64bbd50968ac65e7327692e192)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ui)** add support for `pulsar` - ([a526580](https://github.com/abougouffa/minemacs/commit/a5265800b78761e310b3992505fd11a4b89bd80c)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** fine tune when CI get invoked - ([f05b572](https://github.com/abougouffa/minemacs/commit/f05b57280f1d64338be3f1380e30374b833297ed)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(init)** better contrast for ASCII banner - ([889ae07](https://github.com/abougouffa/minemacs/commit/889ae072a618b28be28d69ff528a9e3c440cfcd6)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** cleanup useless `minemacs-theme` set - ([679693a](https://github.com/abougouffa/minemacs/commit/679693a9181138672558bcc3376a4ee95c47caa8)) - [@abougouffa](https://github.com/abougouffa)
- code cleanup and minor rewrites - ([4b28273](https://github.com/abougouffa/minemacs/commit/4b28273a82cdd466849699ede09a0b35f04c8de0)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.4.0](https://github.com/abougouffa/minemacs/compare/v3.3.3..v3.4.0) - 2023-11-04
#### Bug Fixes
- **(init)** correct way to handle loading `init.el` in Org async export - ([d8fe648](https://github.com/abougouffa/minemacs/commit/d8fe64889fb9fa628ffa321b7f0357cecb056ca8)) - [@abougouffa](https://github.com/abougouffa)
- remove problematic `polymode` configuration (makes markdown unusable) - ([3754ecd](https://github.com/abougouffa/minemacs/commit/3754ecd9a3e49d9a822a9d031b24e8967057c2e8)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(init)** document load and hooks order - ([e3c7bb5](https://github.com/abougouffa/minemacs/commit/e3c7bb5c1b91222681464c21cc27838dea6bd433)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** remove redundant information - ([a68f3aa](https://github.com/abougouffa/minemacs/commit/a68f3aa816a1b7ed9158af3fb1acb56709e49c32)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** add hooks/files load order - ([48efe7c](https://github.com/abougouffa/minemacs/commit/48efe7c6869e7961a30d63c4e4b3269544299ba6)) - [@abougouffa](https://github.com/abougouffa)
- minor fix in hooks order - ([f581114](https://github.com/abougouffa/minemacs/commit/f58111408ddb4879cd41b7d46fd911a65aec8800)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(electric)** electric indent on keywords - ([cf52c65](https://github.com/abougouffa/minemacs/commit/cf52c651afc6cc567ec2e40bef7a73b66fabd5bd)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(init)** add banner - ([3f5c207](https://github.com/abougouffa/minemacs/commit/3f5c2070760dda7bf262e7563dcfb67d321e5cea)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** kill splash screen at the last time - ([244b883](https://github.com/abougouffa/minemacs/commit/244b8834985e710a6df728f4549cb2ee7859cdf3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more robust `minemacs-first-*-file-hook` - ([6fb7628](https://github.com/abougouffa/minemacs/commit/6fb7628e7db6e0e5e443ffa488df74fd7c5d5ad4)) - [@abougouffa](https://github.com/abougouffa)
- **(splash)** add a banner to the splash screen - ([b35987b](https://github.com/abougouffa/minemacs/commit/b35987bb15b91b9cc08406b44f7e4ae0d950aa58)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.3.3](https://github.com/abougouffa/minemacs/compare/v3.3.2..v3.3.3) - 2023-11-03
#### Bug Fixes
- **(tramp)** set persistency file correctly - ([da81579](https://github.com/abougouffa/minemacs/commit/da815792017970e4038195dc51ee08a1b74aceab)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+delete-this-file-and-buffer` - ([46de381](https://github.com/abougouffa/minemacs/commit/46de38127b5e1f8943e16c332d03fd71165e05c4)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(init)** minor rewrite - ([d545d04](https://github.com/abougouffa/minemacs/commit/d545d049541d58641d35ade777a2a8c55792948a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(keybindings)** use my commands for delete/sudo instead of `crux`'s - ([f98c353](https://github.com/abougouffa/minemacs/commit/f98c353581ce1f4e188375741c43ac6bc8843cc6)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5cc06b6](https://github.com/abougouffa/minemacs/commit/5cc06b616c6368068cc4e67b6aad95b64e6e6d83)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.3.2](https://github.com/abougouffa/minemacs/compare/v3.3.1..v3.3.2) - 2023-11-02
#### Refactoring
- **(elisp)** move Elisp tweaks to `me-builtin` - ([76dbbfa](https://github.com/abougouffa/minemacs/commit/76dbbfa5534b0d476bcdf3a96a4955ac94d58bf5)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(keybindings)** add keybindings for zooming text - ([1470550](https://github.com/abougouffa/minemacs/commit/14705506269efa8b503a8f1221a0730db28a5eed)) - [@abougouffa](https://github.com/abougouffa)
- **(polymode)** enable in Markdown/GFM - ([4ff674a](https://github.com/abougouffa/minemacs/commit/4ff674adcaca4ec25b3a927378a9660ee971b89f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2d17f9a](https://github.com/abougouffa/minemacs/commit/2d17f9ab1b3ed2facbcd96672fad54372f7a60c6)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([1386571](https://github.com/abougouffa/minemacs/commit/138657183fcfade89f36153606f9d104989f76c8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.3.1](https://github.com/abougouffa/minemacs/compare/v3.3.0..v3.3.1) - 2023-11-01
#### Bug Fixes
- **(latex)** remove buggy dead code - ([8537095](https://github.com/abougouffa/minemacs/commit/85370959a6cf32a9398d95e15b3706030ec891b1)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.3.0](https://github.com/abougouffa/minemacs/compare/v3.2.1..v3.3.0) - 2023-11-01
#### Bug Fixes
- **(corfu)** correct function name in the hook - ([333ad2e](https://github.com/abougouffa/minemacs/commit/333ad2e3dfea4fa45af236377f74b06d633b4ce9)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-mc)** move and fix `evil-escape` integration - ([8cc883d](https://github.com/abougouffa/minemacs/commit/8cc883db352c9a2906d76802192e373e970a6a39)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** minor tweaks - ([eaf4581](https://github.com/abougouffa/minemacs/commit/eaf45811d5555d908be0e025ef84ffa6b217a7b2)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(corfu)** canonize hook function name - ([cbf53a7](https://github.com/abougouffa/minemacs/commit/cbf53a72460ee884f19184ac43927484351757e0)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** move fontification tweaks to `me-latex` - ([22fe929](https://github.com/abougouffa/minemacs/commit/22fe929108e4fbfbe954bd6e03fc3502fe9d6410)) - [@abougouffa](https://github.com/abougouffa)
- **(vertico)** better way to extract extensions directory - ([559abe9](https://github.com/abougouffa/minemacs/commit/559abe9e93bc58601ea2568ee671aacc2687cfa7)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(vertico)** better way to extract extensions directory - ([56ac0ae](https://github.com/abougouffa/minemacs/commit/56ac0ae172057f67af7f7fd9f004f868863a7180)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cape)** move `cape-capf-super` stuff to `me-completion` - ([dbfc114](https://github.com/abougouffa/minemacs/commit/dbfc11412ff443c0a7be4d78ff7ba4a0cbe8a1cf)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** add a command to do corfu completions in minibuffer - ([c85ba97](https://github.com/abougouffa/minemacs/commit/c85ba9799216db3d19321c63c771cad2e426bc87)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** use TAB and S-TAB for next/previous - ([cb3f35d](https://github.com/abougouffa/minemacs/commit/cb3f35d64f7d2832e807b6c1bcdf2b867eed2d6f)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([de449e3](https://github.com/abougouffa/minemacs/commit/de449e3b550479f26745114159c4875af32abacb)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.2.1](https://github.com/abougouffa/minemacs/compare/v3.2.0..v3.2.1) - 2023-10-31
#### Bug Fixes
- **(fonts)** autoload `plistp` (fatal on Emacs 28) - ([3d51c8a](https://github.com/abougouffa/minemacs/commit/3d51c8aae9657bcf90ee0be1d4abfeaf5d9347e1)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** correctly manage the different `MINEMACS_IGNORE_*` vars - ([d68e20d](https://github.com/abougouffa/minemacs/commit/d68e20dbd7a6a5fd621c7ef1cc4aee829e255e40)) - [@abougouffa](https://github.com/abougouffa)
- don't apply fonts too early - ([c393d1c](https://github.com/abougouffa/minemacs/commit/c393d1c2e12be9292c7c745da405167540675606)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** add `obsolete/me-lexic` to the list - ([621bf07](https://github.com/abougouffa/minemacs/commit/621bf07b7e959b871b9ed5879849da728a631edd)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(math)** obsolete Maxima configuration as I'm not using it - ([2bb4192](https://github.com/abougouffa/minemacs/commit/2bb41920718f5f08a1a565e763447a2c4cc82739)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.2.0](https://github.com/abougouffa/minemacs/compare/v3.1.3..v3.2.0) - 2023-10-31
#### Bug Fixes
- **(backports)** correct load path for back ports - ([3627f8c](https://github.com/abougouffa/minemacs/commit/3627f8c84ae5da8c9781bbe339868bd25b05f1be)) - [@abougouffa](https://github.com/abougouffa)
- treat `tree-sitter` as pseudo package only when built with treesit - ([62f5988](https://github.com/abougouffa/minemacs/commit/62f5988827273d16785df33e0cfc102617d4adbe)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(changelog)** minor fix - ([b873dd4](https://github.com/abougouffa/minemacs/commit/b873dd4f6e82c9c239678acb1f3575609ddd0872)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update the documentation - ([74d5dd1](https://github.com/abougouffa/minemacs/commit/74d5dd1478335d3583eff8a2339d41907620fce8)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(undo)** use builtin `undo-redo` instead of `undo-fu` - ([e9f1f4f](https://github.com/abougouffa/minemacs/commit/e9f1f4f2ab607f52991485cb726a875b4a3ac792)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** add an option to always demand packages - ([49cdf0d](https://github.com/abougouffa/minemacs/commit/49cdf0d4709e9a09d26da9d13fddfe952e81ec73)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** enable Emacs 28, 29 and snapshot on Linux and MacOS - ([5869e1b](https://github.com/abougouffa/minemacs/commit/5869e1ba1a3b5b3361bfcb3454270a76bcae574b)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** run a step of MinEmacs in always demand mode - ([6d430dc](https://github.com/abougouffa/minemacs/commit/6d430dce293b8ac626058958a1f911a8256704bb)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(ci)** minor refactor of init script - ([ca2bad4](https://github.com/abougouffa/minemacs/commit/ca2bad45bb0e85af5211a80010a938bbc6e005fb)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-numbers)** rewrite regexp with `rx` - ([d46593a](https://github.com/abougouffa/minemacs/commit/d46593a5f3d0888fd78f5350d0f6e51fc673937f)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make use of `:unless` - ([b04adb1](https://github.com/abougouffa/minemacs/commit/b04adb13c6d4e5dd4ff9f391b30ed23db37334e0)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(ci)** simplify `minemacs-root-dir` deduction - ([988463d](https://github.com/abougouffa/minemacs/commit/988463d421607907055d61e11c046b1dded28ccf)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** load fonts early - ([2c2d1ce](https://github.com/abougouffa/minemacs/commit/2c2d1ced5c074d0115231c273f4393189399f16c)) - [@abougouffa](https://github.com/abougouffa)
- **(lexic)** make lexic obsolete - ([9e15401](https://github.com/abougouffa/minemacs/commit/9e154013a89a5df202fe79555f126508327256a3)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete)** sync keybindings for `expand-region` to match `expreg` - ([87fc42e](https://github.com/abougouffa/minemacs/commit/87fc42e8c1f0c047d42cb0ddefee48740213145c)) - [@abougouffa](https://github.com/abougouffa)
- **(systemd)** use the Company backend as Capf - ([aea6966](https://github.com/abougouffa/minemacs/commit/aea6966b832bd9f7c163494e6cba15aec5ea620e)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** nom need to add AWK support, merged upstream - ([c16c71d](https://github.com/abougouffa/minemacs/commit/c16c71d73d51ec4e9d8a7a5466c2aeec29c6ecda)) - [@abougouffa](https://github.com/abougouffa)
- **(x86-lookup)** auto download the PDF if not available - ([f677cc2](https://github.com/abougouffa/minemacs/commit/f677cc24ce559c4c97af944d31c7a66ba958f9f4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cede9ee](https://github.com/abougouffa/minemacs/commit/cede9ee793ff49c292f04efce4704736bda08efc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.1.3](https://github.com/abougouffa/minemacs/compare/v3.1.2..v3.1.3) - 2023-10-29
#### Bug Fixes
- **(fonts)** correctly set fonts with `custom-theme-set-faces` - ([c5d39f9](https://github.com/abougouffa/minemacs/commit/c5d39f92f6eeba6be69ca7f77ea479e0b7c64fb7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** mark `seq` as builtin - ([d72f9ad](https://github.com/abougouffa/minemacs/commit/d72f9ad418c2fd2fb9df7e7049c8e54f4312d60c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** move backports to a separate directory - ([1f24de5](https://github.com/abougouffa/minemacs/commit/1f24de5158dbd2bab2ced1b0893e48309030adc3)) - [@abougouffa](https://github.com/abougouffa)
- **(fonts)** inherit `default` in `fixed-pitch` and `fixed-pitch-serif` - ([01c0dea](https://github.com/abougouffa/minemacs/commit/01c0deadd66e68395dfaa77577ac9943d000e1a8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([99a5ae8](https://github.com/abougouffa/minemacs/commit/99a5ae85b701bd30deea3aca75d1c0334eee8506)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.1.2](https://github.com/abougouffa/minemacs/compare/v3.1.1..v3.1.2) - 2023-10-28
#### Bug Fixes
- **(vars)** fix environment variable for disabling `early-config.el` - ([7e22ce2](https://github.com/abougouffa/minemacs/commit/7e22ce27f8b2d2389b9a14cd7ec1519ea3c513ae)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** minor update - ([35878bd](https://github.com/abougouffa/minemacs/commit/35878bda704b59d33eddae25b789a724193c9131)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- remove the disclaimer on Windows - ([6c545b6](https://github.com/abougouffa/minemacs/commit/6c545b6e91dd339461e1233e960cddb58116cf1d)) - [@abougouffa](https://github.com/abougouffa)
- simplify the code - ([d6ba81b](https://github.com/abougouffa/minemacs/commit/d6ba81b8ae537efc4d72a66135b9604ff9c8bc57)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(eaf)** use `:when` to condition `use-package` - ([a182961](https://github.com/abougouffa/minemacs/commit/a1829615a0ad7bd1ab445355d090bcc86b691f8a)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** better titlebar on MacOS! - ([f7efd93](https://github.com/abougouffa/minemacs/commit/f7efd934826ad8502d3d9752e9e917d7c3cb136d)) - [@abougouffa](https://github.com/abougouffa)
- **(markdown)** add some local keybindings - ([3c8fce8](https://github.com/abougouffa/minemacs/commit/3c8fce8e2372b7ae9ffc5eadb5a6dd9e1c06121f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.1.1](https://github.com/abougouffa/minemacs/compare/v3.1.0..v3.1.1) - 2023-10-28
#### Tweaks
- **(cocogitto)** display a message after finishing - ([92bd36b](https://github.com/abougouffa/minemacs/commit/92bd36bcaba18810553c41293f24cb66418421df)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.1.0](https://github.com/abougouffa/minemacs/compare/v3.0.3..v3.1.0) - 2023-10-28
#### Bug Fixes
- **(window)** fix warning window position - ([b2e48f0](https://github.com/abougouffa/minemacs/commit/b2e48f0572357adad949154341b5bf1045b4e823)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(calfw)** document a command - ([19f2c37](https://github.com/abougouffa/minemacs/commit/19f2c37d1d583ff668a89dec000099469e577f51)) - [@abougouffa](https://github.com/abougouffa)
- **(faq)** convert to markdown - ([c75c88f](https://github.com/abougouffa/minemacs/commit/c75c88fdbf9543564a057b4d4cfc1ebdf419861d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** add support for editing Gitlab CI YAML files - ([089ded9](https://github.com/abougouffa/minemacs/commit/089ded9e4675ff5e945180332c8f0a5f47c07062)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(modules)** remove the obsolete `me-lisp` module - ([20c6280](https://github.com/abougouffa/minemacs/commit/20c62809d7ddbf6e74aac629b669f9f565ec2b39)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult-eglot)** better check for LSP - ([fe4316a](https://github.com/abougouffa/minemacs/commit/fe4316a8167cefcf91c60fd9f5282fd64bcc1676)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-notes)** minor refactor - ([a05792a](https://github.com/abougouffa/minemacs/commit/a05792a27bdeaea6811f710a4ef0e3f4672eaa91)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** sort modules, enable `me-workspaces`, disable `me-binary` - ([c1b3ec0](https://github.com/abougouffa/minemacs/commit/c1b3ec011b43bb864cfa404ce675424ecf2b9d22)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** minor refactor - ([172a803](https://github.com/abougouffa/minemacs/commit/172a803f95ddcf99cbd47fe08831b4ce2c371d20)) - [@abougouffa](https://github.com/abougouffa)
- **(fonts)** add more Iosevka fonts - ([44083e4](https://github.com/abougouffa/minemacs/commit/44083e49f3b53bd0589f1af6c61f02a9dab4c97b)) - [@abougouffa](https://github.com/abougouffa)
- **(nov)** remove unneeded UI tweaks - ([4e3fd06](https://github.com/abougouffa/minemacs/commit/4e3fd0690b1554dd3a3897874f9b6faf9a5f80f8)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** make `org-present` obsolete - ([c6445db](https://github.com/abougouffa/minemacs/commit/c6445db84c28217abb2b2e165def1b3eff6b79fe)) - [@abougouffa](https://github.com/abougouffa)
- **(ros)** autoload some commands - ([5e6a410](https://github.com/abougouffa/minemacs/commit/5e6a4109949ac277e4c8a387aa6f43b81a1120e3)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** minor `config.el` refactor - ([3a01518](https://github.com/abougouffa/minemacs/commit/3a01518044b5b3c6d2964ed6a85906bbbb6338f5)) - [@abougouffa](https://github.com/abougouffa)
- **(spell-fu)** remove obsolete alias - ([6a03238](https://github.com/abougouffa/minemacs/commit/6a03238dc37a6edf21f021517a5fa7cbc01fcf21)) - [@abougouffa](https://github.com/abougouffa)
- remove unneeded `:mode` blocks - ([05ca6d2](https://github.com/abougouffa/minemacs/commit/05ca6d21dfd1b0e0a0a64c19debfecd557bcb943)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.0.3](https://github.com/abougouffa/minemacs/compare/v3.0.2..v3.0.3) - 2023-10-28
#### Bug Fixes
- **(fonts)** buggy check for installed fonts - ([31c1029](https://github.com/abougouffa/minemacs/commit/31c1029782eeac8f5b752721fbbacfaee5f9065e)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(fonts)** add some documentation - ([c458053](https://github.com/abougouffa/minemacs/commit/c4580533b1e63779fbbb57c9a9ea8f77765242cc)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- add Github pages deployment - ([10aa065](https://github.com/abougouffa/minemacs/commit/10aa06585c1da29b8b8f643067e10ab26e5f235a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(fonts)** more default fonts settings - ([fbf6429](https://github.com/abougouffa/minemacs/commit/fbf6429499775e5dc5e0b08a1ffe1888183f0c32)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** more font setting examples in `skel/config.el` - ([129d917](https://github.com/abougouffa/minemacs/commit/129d917bbf271a4f1933f88eb8dc2c50d7f50ca6)) - [@abougouffa](https://github.com/abougouffa)
- better modules loading - ([de54ff2](https://github.com/abougouffa/minemacs/commit/de54ff20f3c5d1da6ee85eb1f6c2fb36fd45375a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.0.2](https://github.com/abougouffa/minemacs/compare/v3.0.1..v3.0.2) - 2023-10-27
#### Tweaks
- **(fonts)** code cleanup - ([dbaccf0](https://github.com/abougouffa/minemacs/commit/dbaccf0698791d85b74c572bad2b7a1194f0187f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.0.1](https://github.com/abougouffa/minemacs/compare/v3.0.0..v3.0.1) - 2023-10-27
#### Documentation
- **(core)** add a description to `minemacs-fonts-plist` - ([53ce66b](https://github.com/abougouffa/minemacs/commit/53ce66b9e79b85cbd8bb71b384fcdf30aa421da1)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** minor edits - ([61f806e](https://github.com/abougouffa/minemacs/commit/61f806e043578c513f589add7cd91a6785dd8c53)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** more font setting examples - ([f627965](https://github.com/abougouffa/minemacs/commit/f6279650cbc4e0f2d101111140559e61037ae048)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(fonts)** accept full `font-spec/set-face-attribute` arguments (#120) - ([6ec46f1](https://github.com/abougouffa/minemacs/commit/6ec46f1525dba2a48d0e5fc7d1d84c1c024dd526)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** update loaddefs - ([a533aea](https://github.com/abougouffa/minemacs/commit/a533aeaf2eae5a5bcf9265ba555ca56dc844cf72)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.0.0](https://github.com/abougouffa/minemacs/compare/v2.0.2..v3.0.0) - 2023-10-26
#### Documentation
- **(readme)** convert to Markdown - ([817db39](https://github.com/abougouffa/minemacs/commit/817db3922e2180f57065b7aeec3397bca0874527)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** fix link - ([7c26e8a](https://github.com/abougouffa/minemacs/commit/7c26e8ae2d99fcaa0b06bb7d0157f0d612145c36)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example to how to fix `vterm` compilation - ([19a5213](https://github.com/abougouffa/minemacs/commit/19a52137bc8ead5bd81ac47b941b9b4a94c95663)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** change font handling - ([7c4a8b8](https://github.com/abougouffa/minemacs/commit/7c4a8b8dfeb9f1e7b56c3c6444d82c76818ac525)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add helper function `+font-installed-p` - ([0200750](https://github.com/abougouffa/minemacs/commit/0200750c91f77f2e1ce3e1cb7c547341b649b04f)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** add an option to keep the "check if disabled" advice - ([f08c370](https://github.com/abougouffa/minemacs/commit/f08c370ed352992d8f8d8851a648d6c08b45ed44)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** minor tweak - ([c44d135](https://github.com/abougouffa/minemacs/commit/c44d135d0b1fe7bd75daa4db8bcde6a0527052eb)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(project)** use `use-package` `:hook` block - ([96b6d8b](https://github.com/abougouffa/minemacs/commit/96b6d8b392b5f76c305f69354cd3ec7156be3708)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(nerd-icons)** auto-install fonts when they aren't installed - ([6615a59](https://github.com/abougouffa/minemacs/commit/6615a59d033f5cceab190140e04a659d11a53621)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** include examples of the new font setting - ([35dad97](https://github.com/abougouffa/minemacs/commit/35dad970b97dd19e7ae155edcc0310e3aee871fe)) - [@abougouffa](https://github.com/abougouffa)
- **(vterm)** don't install if Emacs don't support modules - ([c86aa7c](https://github.com/abougouffa/minemacs/commit/c86aa7c660f85a96b4f713e17c1768782178fdc0)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v2.0.2](https://github.com/abougouffa/minemacs/compare/v2.0.1..v2.0.2) - 2023-10-25
#### Bug Fixes
- **(cocogitto)** fix the `+cocogitto-bump` command - ([45a4e0a](https://github.com/abougouffa/minemacs/commit/45a4e0aa0acee52f715e3be752860af936a51ab1)) - [@abougouffa](https://github.com/abougouffa)
- **(cocogitto)** fix change log generation - ([91abc8a](https://github.com/abougouffa/minemacs/commit/91abc8a51fdc5d36a156821f4b70e782d209ae07)) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** adapt to the new `use-package` disabled packages check (#119) - ([a1c959b](https://github.com/abougouffa/minemacs/commit/a1c959be9bc5b53ed5ebaaceb002533ba5be7b4e)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** fix the real cause of #119 - ([00345fe](https://github.com/abougouffa/minemacs/commit/00345feb8b7051a3d97b46321e49472a07fc3a31)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(use-package)** add comment - ([1adbfeb](https://github.com/abougouffa/minemacs/commit/1adbfeb2bbf4140fa797e40c3dbdebf3de10a4ab)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add helper function `+varplist-get` - ([f5f8474](https://github.com/abougouffa/minemacs/commit/f5f847428e251fbff773a1207d50f55b4f2471ba)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** minor refactoring - ([3df04e7](https://github.com/abougouffa/minemacs/commit/3df04e784af1a11cc5be275a5feeda76c525c453)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(dashboard)** the bug was caused by the `use-package` advice - ([1f62efd](https://github.com/abougouffa/minemacs/commit/1f62efd04b95cbdd94a278a711630fc5a47d01f9)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dashboard)** use `:unless` instead of `:when (not ...)` - ([729f656](https://github.com/abougouffa/minemacs/commit/729f65660a57cb34bf23505a11cf467c28d4387c)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** don't fail when trying to load inexistant file in `+load` - ([18a47bd](https://github.com/abougouffa/minemacs/commit/18a47bdf4a5572ac8ff393a520fd163d554d855f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v2.0.1](https://github.com/abougouffa/minemacs/compare/v2.0.0..v2.0.1) - 2023-10-22
#### Bug Fixes
- **(evil)** evil repeat error - ([69b2258](https://github.com/abougouffa/minemacs/commit/69b22581e7a7e1391141042de5402438186f1450)) - [@donneyluck](https://github.com/donneyluck)
- **(ts-fold)** ensure enabling on `yaml-ts-mode` - ([d3565cc](https://github.com/abougouffa/minemacs/commit/d3565ccb096c97c0a284a4a827016fea2c8ab1e0)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** fix CI badge - ([30cc7a2](https://github.com/abougouffa/minemacs/commit/30cc7a233929c37ee7165c9a38e5da3115461043)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** cleanup MacOS M1 test (paid) - ([6f014c8](https://github.com/abougouffa/minemacs/commit/6f014c873698a6ad49ef196d6e9b3c33eadf753f)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** fix failure detection - ([9b63d42](https://github.com/abougouffa/minemacs/commit/9b63d42c552e80babbc4b38889c963322cb9b5a2)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** try to enable testing on Apple M1 - ([a4f95c8](https://github.com/abougouffa/minemacs/commit/a4f95c8a4eba2be2cd301f4506b8477f67cd7c8d)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** split CI jobs by OS to separate workflows - ([15c1c17](https://github.com/abougouffa/minemacs/commit/15c1c17dced4551bb1eb1dd71456fc0091145c88)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better failure detection - ([665681d](https://github.com/abougouffa/minemacs/commit/665681db13080d80ccdf403aaebcfac1304e33e3)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** fix error extraction regexp - ([6e739da](https://github.com/abougouffa/minemacs/commit/6e739da0c0479c3a11628ebaca82a1adaf486174)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** use an independent initialization script for CI - ([a6c7c7e](https://github.com/abougouffa/minemacs/commit/a6c7c7e9faf522a85599ba83ca7be5e4505f51e5)) - [@abougouffa](https://github.com/abougouffa)
- **(make)** fix CI rule - ([c76ae0c](https://github.com/abougouffa/minemacs/commit/c76ae0c488b448e0e50f5f1ed839f289f1c221a2)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- remove CI specific code - ([e1286b3](https://github.com/abougouffa/minemacs/commit/e1286b3dd882a0c4e7e79e1adc52d5dff0747a2a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(eglot)** register LSP server for AWK - ([5587589](https://github.com/abougouffa/minemacs/commit/5587589d6e67914d0bb973fe95b12af358180d74)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v2.0.0](https://github.com/abougouffa/minemacs/compare/v1.7.1..v2.0.0) - 2023-10-21
#### Bug Fixes
- **(vterm)** disable on Windows - ([e082919](https://github.com/abougouffa/minemacs/commit/e082919664fa61958b26460625e7041b2c48b916)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** mention the CI actions - ([ef86e67](https://github.com/abougouffa/minemacs/commit/ef86e671917f22d34ceef58f37bdd75939de9c94)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** rename `build.yaml` to `ci.yaml` - ([5087247](https://github.com/abougouffa/minemacs/commit/5087247844659bb42d25d5f3846b748acfe53485)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** refactor and enable MacOS and Windows - ([ee6eb96](https://github.com/abougouffa/minemacs/commit/ee6eb9618e9958371d51f5c0fd5c527749558d3c)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** test on MacOS - ([f3d160f](https://github.com/abougouffa/minemacs/commit/f3d160fd8a2df06f614d1d0f468da9f808df41c6)) - [@abougouffa](https://github.com/abougouffa)
- **(cocogitto)** merge push and pull request checks - ([4a5c562](https://github.com/abougouffa/minemacs/commit/4a5c5628b6ad6b5d23e199ccb7f7096854763c34)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(keybindings)** bind ecryptfs/netextender only when relevant - ([5724330](https://github.com/abougouffa/minemacs/commit/5724330d978549e72e36692f9a933298e23c49d1)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.7.1](https://github.com/abougouffa/minemacs/compare/v1.7.0..v1.7.1) - 2023-10-21
#### Bug Fixes
- **(use-package)** better checking for disabled packages - ([71b2ad6](https://github.com/abougouffa/minemacs/commit/71b2ad66e189434f8c459ad67025c3014df6b0f5)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** better reporting - ([db7606a](https://github.com/abougouffa/minemacs/commit/db7606a317ebfb9729de7652c7625c86fb4a8c85)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** fix failure checking - ([3eec9f1](https://github.com/abougouffa/minemacs/commit/3eec9f1ceac2c4efe006b7d4f8fded2993acfcf3)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** fix Emacs version extraction regexp - ([df45156](https://github.com/abougouffa/minemacs/commit/df45156b7aabd2c8f2640416ea774e93d327fa6e)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** disable `fail-fast` strategy - ([fe90ae9](https://github.com/abougouffa/minemacs/commit/fe90ae940e9a0c7f1627c377c26c5bd7a10c3946)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better reporting - ([64b6f26](https://github.com/abougouffa/minemacs/commit/64b6f264da3587ca6a0c424ba3a9ea363d6ff10f)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** enable Emacs 28 & 29 + better reporting - ([8e53daf](https://github.com/abougouffa/minemacs/commit/8e53dafde1a0a88fb802e19927890c96d60aa145)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better report generation - ([516ec07](https://github.com/abougouffa/minemacs/commit/516ec07e729ad8aaffcf03e9081d85f05e7c39eb)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better report generation - ([fc610e5](https://github.com/abougouffa/minemacs/commit/fc610e58fa93fd640ff11c28981562d35e70ad8a)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- better conditional loading - ([b27a088](https://github.com/abougouffa/minemacs/commit/b27a088b57ec22909555d95cb4467266a6ad1a87)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(plantuml)** remove unneeded exec mode tweak - ([9e549f9](https://github.com/abougouffa/minemacs/commit/9e549f9bbbb870a36f9bc6e180b9e93baf1a3cdb)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.7.0](https://github.com/abougouffa/minemacs/compare/v1.6.1..v1.7.0) - 2023-10-21
#### Bug Fixes
- **(blamer)** edge case when launching Emacs from tty - ([95b52ae](https://github.com/abougouffa/minemacs/commit/95b52ae287c20ad259be152db2645ef842b0c3bd)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** check for `fringe` before using `set-fringe-style` - ([9a34479](https://github.com/abougouffa/minemacs/commit/9a34479a47fa022e28f68ed9c74c165f1d14efe4)) - [@abougouffa](https://github.com/abougouffa)
- **(dockerfile-mode)** disable when Emacs have builtin tree-sitter - ([9919244](https://github.com/abougouffa/minemacs/commit/991924470e63e90d04c1a65e32d8d0b52a9c27c1)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** correctly call `+delete-file-or-directory` - ([ebb8ed5](https://github.com/abougouffa/minemacs/commit/ebb8ed54ff95796299cf73d0c192050f8cacc522)) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** pin to a stable commit - ([bbc8bce](https://github.com/abougouffa/minemacs/commit/bbc8bce5a478d4113670e4149605f72c4ceab52a)) - [@abougouffa](https://github.com/abougouffa)
- fix several deferred packages issues - ([60145fd](https://github.com/abougouffa/minemacs/commit/60145fd061fc22516f874b75aff97bb444792825)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** add workflow status badge - ([792a5f5](https://github.com/abougouffa/minemacs/commit/792a5f50ba4982ebaf42d46314ef73ca5fff4823)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** add `awk-ts-mode` - ([66657f7](https://github.com/abougouffa/minemacs/commit/66657f7fb73d54b574d970dcff87b33a16c9d8cc)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** minor tweaks for CI - ([c62a1cf](https://github.com/abougouffa/minemacs/commit/c62a1cf724ee668746432e9847761d54b75c0116)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better management of output data - ([58456af](https://github.com/abougouffa/minemacs/commit/58456afbb96e6108bfb73e124a9f3c46c6bc1ad2)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better CI run - ([10594e0](https://github.com/abougouffa/minemacs/commit/10594e0a4404350ff097e508f2ec3edb58fd38ea)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** run MinEmacs directly in CI - ([f2bc02a](https://github.com/abougouffa/minemacs/commit/f2bc02a9a7ae2d1597ea0069da09d38a1e937188)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** run MinEmacs in CI mode, with all modules - ([40109b3](https://github.com/abougouffa/minemacs/commit/40109b3e15b33c92edb5db2f0c4a29b5575fb9de)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** WIP - ([ec6d024](https://github.com/abougouffa/minemacs/commit/ec6d024cd32f02e711e5fe6f057c1e6af6f99684)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** WIP - ([8bab9ea](https://github.com/abougouffa/minemacs/commit/8bab9ea3257fc22cec71cfea2151f46dc561b0e5)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** WIP build CI - ([35ea7da](https://github.com/abougouffa/minemacs/commit/35ea7da3ebe8093d0dad800265f3812683b3d5a9)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- edit the `non-executable-script.sh` example - ([a1be1b5](https://github.com/abougouffa/minemacs/commit/a1be1b5a35e361d4e24c4be9cedded0a112976f0)) - [@abougouffa](https://github.com/abougouffa)
- add an example for `non-executable-script.sh` - ([e316607](https://github.com/abougouffa/minemacs/commit/e3166079b9cded34ee59ab5d0981d12d8ae273e0)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move advice from `me-builtin` to `me-defaults` - ([d285a56](https://github.com/abougouffa/minemacs/commit/d285a5654be4fe4f9cba47c30c3c0bc2080eaad5)) - [@abougouffa](https://github.com/abougouffa)
- extract reusable `+delete-file-or-directory` function, refactor code - ([597df98](https://github.com/abougouffa/minemacs/commit/597df98bdaa09192d616481c90b28fc0a02527fe)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** load all packages when MinEmacs is invoked in CI context - ([09bc0be](https://github.com/abougouffa/minemacs/commit/09bc0be1a096c79d7d24d8c14e637291cde4fb5f)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** auto-cleanup MinEmacs directory (`eln-cache`, ...) - ([a8abb44](https://github.com/abougouffa/minemacs/commit/a8abb443c44ca1790d250141ec3b7e2c3a96bf9b)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** cleanup zombie projects on exit - ([fd07d8b](https://github.com/abougouffa/minemacs/commit/fd07d8b956e7bcebbaac7fdc086db728fe9b9e2a)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** refactor and rename straight cache cleaning function - ([f06b06a](https://github.com/abougouffa/minemacs/commit/f06b06aaddb6a53d70925db4c5345dfa436eea02)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** cleanup old byte compiled Elisp - ([733c89b](https://github.com/abougouffa/minemacs/commit/733c89be2daa1f4e76ce37ada50774b9084bf609)) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** remove the explicit pin as upstream gets fixed - ([b423753](https://github.com/abougouffa/minemacs/commit/b4237531e0b6de99a0528059bafc5c2519399c54)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add support for AWK - ([3ad602e](https://github.com/abougouffa/minemacs/commit/3ad602e37b563dd2bd4ac64c4a22d52990934f5c)) - [@abougouffa](https://github.com/abougouffa)
- make EAF obsolete - ([8c4ff6f](https://github.com/abougouffa/minemacs/commit/8c4ff6fa743c76febc5bf9bfd50c73d283c1910d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5971a8a](https://github.com/abougouffa/minemacs/commit/5971a8ab87ad8862ffdc0ce9ee9da4502e7ebc30)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([183ebd9](https://github.com/abougouffa/minemacs/commit/183ebd90bf890c644e26c33797f41c4075391f9e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.6.1](https://github.com/abougouffa/minemacs/compare/v1.6.0..v1.6.1) - 2023-10-16
#### Bug Fixes
- **(+writing-mode)** fix enable/disable hooks - ([7f21ba9](https://github.com/abougouffa/minemacs/commit/7f21ba979410f570240265637e95794f8799197c)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** revert `minemacs-first-org-file`, it causes Org export to fail - ([2466391](https://github.com/abougouffa/minemacs/commit/246639197c43cad75d9bfd7db647837d7d51b399)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(blamer)** disable when in `+writing-mode` - ([478d4f6](https://github.com/abougouffa/minemacs/commit/478d4f66da70dfa56f2e11773d1f35fffe17add2)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more generic `+env-save` - ([46fda3c](https://github.com/abougouffa/minemacs/commit/46fda3cc2409fab8fdfec3d0c07671e8c1a31f5b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.6.0](https://github.com/abougouffa/minemacs/compare/v1.5.0..v1.6.0) - 2023-10-15
#### Features
- **(core)** add `+make-first-file-hook!` - ([5bc1777](https://github.com/abougouffa/minemacs/commit/5bc1777bc19ffc72e61c41624c08ec342ce1b055)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** define some first file hooks - ([580a089](https://github.com/abougouffa/minemacs/commit/580a0899a98c6c144a0b8c2c26c951e2b5fc99a6)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(extras)** simplify `+cocogitto-bump` - ([702c268](https://github.com/abougouffa/minemacs/commit/702c268d585625e86a8c13ea7a8b5ed20a876c16)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** conditionally disable `dockerfile-mode` MinEmacs way - ([8785b5a](https://github.com/abougouffa/minemacs/commit/8785b5afdd35da43480ad933b41f70cea55a7f6c)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(treesit-auto)** remove protobuf temporary hack, fixed upstream - ([cc7d74e](https://github.com/abougouffa/minemacs/commit/cc7d74e030d1735d32076a8d16b6f336a2b338a7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(elisp)** make use of minemacs-first-elisp-file - ([1a3c3a5](https://github.com/abougouffa/minemacs/commit/1a3c3a5dc54b4dd41767e05f8db940b38cf22ce9)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib2)** add a helper command to get active ticket IDs - ([58f1eca](https://github.com/abougouffa/minemacs/commit/58f1eca7054035adbeb2f4212e72357b9067887c)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** make use of first org file hook to ensure deferring Org - ([dc4807b](https://github.com/abougouffa/minemacs/commit/dc4807b6075704fa8ab5d73e41064f796b132db6)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add a configuration example for `jiralib2` - ([773aadd](https://github.com/abougouffa/minemacs/commit/773aadd11291268629181c3597f4ecbeb33e702c)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([b05eb06](https://github.com/abougouffa/minemacs/commit/b05eb060ae5da664a62d0ab8b2edecd80c92d79f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.5.0](https://github.com/abougouffa/minemacs/compare/v1.4.10..v1.5.0) - 2023-10-14
#### Features
- **(core)** add `+first-line-empty-p` helper - ([d83aa23](https://github.com/abougouffa/minemacs/commit/d83aa23f5ed593e90560e89bcb5c780a77a79682)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** migrate from `org-roam` to `denote` - ([ef9fbb6](https://github.com/abougouffa/minemacs/commit/ef9fbb6c1c5e9cc09373336cbbb863c957605d44)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add support for `jiralib2` WIP - ([6c562ac](https://github.com/abougouffa/minemacs/commit/6c562ac8f8ac4f74eecda4589b5047110fe83329)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(version)** v1.4.11 - ([7256aba](https://github.com/abougouffa/minemacs/commit/7256abae8b300da1acd25838180f1a4732e95151)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** conditionally enable `treesit` stuff the MinEmacs way - ([b50775d](https://github.com/abougouffa/minemacs/commit/b50775df21ee81080509d759fda3332203fbbe35)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(denote)** unbind `consult-notes-org-roam` autoloads - ([ff3c86f](https://github.com/abougouffa/minemacs/commit/ff3c86f0a9a4a6abdadfefac684be0bfe6f1cab7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([eeb9e1d](https://github.com/abougouffa/minemacs/commit/eeb9e1d9d3be91b2f2133958755c11a124ab50ed)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.10](https://github.com/abougouffa/minemacs/compare/v1.4.9..v1.4.10) - 2023-10-10
#### Bug Fixes
- **(recentf)** more explicit exclusion regexps - ([79e93fd](https://github.com/abougouffa/minemacs/commit/79e93fdd2e9e22c0dcf3d41447a87a3f924388a4)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** fix language name to exclude Protobuf - ([0a2d2de](https://github.com/abougouffa/minemacs/commit/0a2d2def8807fc663aeb20b200cf85390d2fcd45)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(vc)** add `blamer` support - ([e44184f](https://github.com/abougouffa/minemacs/commit/e44184f605c5729a8f82e3daf4d63641320307b2)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor changes in `minemacs-update-restore-locked` - ([7ac449f](https://github.com/abougouffa/minemacs/commit/7ac449fa010971b64eab65c370ce45ac8374513c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(blamer)** make font size 10% smaller than default - ([fcdf95e](https://github.com/abougouffa/minemacs/commit/fcdf95e87ac6cc32000dee2aff32c9c95228bc74)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** rename command + unselect region after kill - ([775f350](https://github.com/abougouffa/minemacs/commit/775f350701704fd743e1e10afa022358084c0951)) - [@abougouffa](https://github.com/abougouffa)
- **(hydra)** hydra is behaving strangely, it gets loaded immediately - ([4d352ce](https://github.com/abougouffa/minemacs/commit/4d352ce0346e6cf1caa12e5b1d66a4ca01cf5d5c)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** remove `evil-textobj-tree-sitter` - ([d4ac03d](https://github.com/abougouffa/minemacs/commit/d4ac03da70f0e68ce9ab471a24396387332e09fc)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([27da8da](https://github.com/abougouffa/minemacs/commit/27da8da177c7f1f944d164f616bcfa490e553432)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([de76af5](https://github.com/abougouffa/minemacs/commit/de76af5148025f99ca7b6cb12b9c37e6655a23c7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.9](https://github.com/abougouffa/minemacs/compare/v1.4.8..v1.4.9) - 2023-10-09
#### Bug Fixes
- **(cocogitto)** buggy command on pre-release - ([e2d8a7a](https://github.com/abougouffa/minemacs/commit/e2d8a7ac7b4eece8e62f17690f078b3183593269)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(changelog)** add historical v0.1.0 + links to old commits - ([23ca7fc](https://github.com/abougouffa/minemacs/commit/23ca7fc7e247fb6ea6fad5f4c56365454ed1d716)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(cocogitto)** bump `actions/checkout` and `cocogitto/cocogitto-action` - ([ea4b575](https://github.com/abougouffa/minemacs/commit/ea4b575093e00fc4a6eae47510b26a0718f481b3)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** do not save/load environment variables in Windows - ([9f8ec81](https://github.com/abougouffa/minemacs/commit/9f8ec819c7793cd94d8aa3b461daab647f0f4387)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** enable restoring lockfile from backup - ([44c0b35](https://github.com/abougouffa/minemacs/commit/44c0b350d866268a212f3283abfe36482b86c7cb)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.8](https://github.com/abougouffa/minemacs/compare/v1.4.7..v1.4.8) - 2023-10-08
#### Documentation
- **(skel)** update modules documentation and examples - ([020af86](https://github.com/abougouffa/minemacs/commit/020af8627859e7f73b48375ea5c1eb82e2795cbf)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add some helper commands for text cleanup - ([396993a](https://github.com/abougouffa/minemacs/commit/396993adc2970ca84c8cd183d7617e86ded0e08b)) - [@abougouffa](https://github.com/abougouffa)
- **(extra)** add a convenience command for Cocogitto - ([24f73eb](https://github.com/abougouffa/minemacs/commit/24f73ebdb35886626940c1925ed0f0d5a4c76fe2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** regenerate loaddefs - ([307dbef](https://github.com/abougouffa/minemacs/commit/307dbefad9a372b776c5cc8da0d1755298e57644)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** bind a helper command - ([6e2d8a4](https://github.com/abougouffa/minemacs/commit/6e2d8a417374463ee85d9e3338e94bdbaaeda57e)) - [@abougouffa](https://github.com/abougouffa)
- **(markdown)** better integration with Pandoc - ([ae73311](https://github.com/abougouffa/minemacs/commit/ae73311ddc9fa42f29957296b0aa200bb1f79862)) - [@abougouffa](https://github.com/abougouffa)
- **(tapspaces)** ensure reading package list at startup - ([b512fce](https://github.com/abougouffa/minemacs/commit/b512fce5a951dd04741a160a84ca5bc2e84ad277)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([698fca2](https://github.com/abougouffa/minemacs/commit/698fca23a98001419cfe50c32b19bf7d69d29c28)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.7](https://github.com/abougouffa/minemacs/compare/v1.4.6..v1.4.7) - 2023-10-07
#### Bug Fixes
- **(core)** autoload error - ([6c53635](https://github.com/abougouffa/minemacs/commit/6c53635c265c879973823c8d3ce8d82771823032)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.6](https://github.com/abougouffa/minemacs/compare/v1.4.5..v1.4.6) - 2023-10-07
#### Refactoring
- **(latex)** move some vars from builtin to latex - ([65190dd](https://github.com/abougouffa/minemacs/commit/65190dd0e6b9e77a68f407b27f0666f6ff78b829)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** autoload some `vc-git` commands - ([17149e0](https://github.com/abougouffa/minemacs/commit/17149e084aef2be928b699357da3a670d4a798ea)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** additional tweaks - ([92c28a9](https://github.com/abougouffa/minemacs/commit/92c28a9216b583ce6a2a986cd358ac98d4de6b6f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.5](https://github.com/abougouffa/minemacs/compare/v1.4.4..v1.4.5) - 2023-10-07
#### Bug Fixes
- **(+writing-mode)** autoload `+writing-global-mode` - ([f4291ce](https://github.com/abougouffa/minemacs/commit/f4291ce9107b997565e96c81592b3fe7df072f07)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(+writing-mode)** disassociate major modes from derived major modes - ([d5f6104](https://github.com/abougouffa/minemacs/commit/d5f6104de86877753e829143bd6e56b7fd69c9a5)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** regenerate loaddefs - ([180b550](https://github.com/abougouffa/minemacs/commit/180b550235b820b7484d5b957e4d26a87c0212b7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.4](https://github.com/abougouffa/minemacs/compare/v1.4.3..v1.4.4) - 2023-10-07
#### Bug Fixes
- **(+writing-mode)** enable globally for a set of modes - ([38d5339](https://github.com/abougouffa/minemacs/commit/38d5339251052bf35a97a25e79622f0a0e0c17e8)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(ui)** add keybinding for `+writing-global-mode` - ([5ea190d](https://github.com/abougouffa/minemacs/commit/5ea190d9bd3a86a91c6e8989e4af96c349edbfb7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.3](https://github.com/abougouffa/minemacs/compare/v1.4.2..v1.4.3) - 2023-10-07
#### Bug Fixes
- **(treesit-auto)** minor bug when deleting `protobuf` - ([28cd503](https://github.com/abougouffa/minemacs/commit/28cd503a035d2f42eb34880fffe2e45556857762)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- add some examples and file templates - ([f3c1df2](https://github.com/abougouffa/minemacs/commit/f3c1df285e0fc89f19f27b06274315470424fb87)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.2](https://github.com/abougouffa/minemacs/compare/v1.4.1..v1.4.2) - 2023-10-07
#### Bug Fixes
- **(core)** fix a comment to make `parinfer-rust-mode` happy - ([9551edc](https://github.com/abougouffa/minemacs/commit/9551edc3c40a1789c2104faeb131191242ef62b6)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(builtin)** fix a typo - ([1a5b6d1](https://github.com/abougouffa/minemacs/commit/1a5b6d1c06153d390daefbc6bdb7eb8e17ba92cb)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(+writing-mode)** add `+writing-global-mode` (fix #117) - ([2cabf08](https://github.com/abougouffa/minemacs/commit/2cabf0840326cbc26a38f35ab28597801d133c56)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `minemacs-update-restore-locked` - ([b0276fa](https://github.com/abougouffa/minemacs/commit/b0276fa81ec0793743960a154e6e9035383c0f53)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** default `speedbar` config from Crafted Emacs - ([b0fd9f4](https://github.com/abougouffa/minemacs/commit/b0fd9f41a241111f835de2c9b770a4ab14c6e0d6)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better error handling in `minemacs-update-restore-locked` - ([ffe08fa](https://github.com/abougouffa/minemacs/commit/ffe08faa94d798f7e3f83d55abbca70ddd65cc02)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** add pulse line - ([7d54316](https://github.com/abougouffa/minemacs/commit/7d54316323794e588944f6c95f232a35b6413ed5)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** temporary disable `protobuf` (see #114) - ([af0f970](https://github.com/abougouffa/minemacs/commit/af0f970f00e9158b309556931270b1d8ba225cb7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([51ac487](https://github.com/abougouffa/minemacs/commit/51ac4875dfbdb6c653c95470680cb29d88c5f15f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.1](https://github.com/abougouffa/minemacs/compare/v1.4.0..v1.4.1) - 2023-10-05
#### Features
- **(natural-langs)** add support for `reverso` - ([da98f62](https://github.com/abougouffa/minemacs/commit/da98f62ae69605aea12f63e2bb592010094eef87)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.0](https://github.com/abougouffa/minemacs/compare/v1.3.0..v1.4.0) - 2023-10-05
#### Bug Fixes
- **(tramp)** fix Tramp bug (thanks Phundrak) - ([d40fb21](https://github.com/abougouffa/minemacs/commit/d40fb21fbd2db5810a2e08bec10c4813ecae2597)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(defaults)** enable `context-menu-mode` - ([505c301](https://github.com/abougouffa/minemacs/commit/505c30146ff7a7469cd4b17d0a5cf968ad959714)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `makefile-executor` - ([e65bb1a](https://github.com/abougouffa/minemacs/commit/e65bb1a67fbe04075f9b1856b4b6eb269913a729)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(hl-todo)** add the `FIX` keyword - ([5dd383e](https://github.com/abougouffa/minemacs/commit/5dd383ea82b830acef908a1e8e7c71660faa4798)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete)** add `ts-fold` to `tree-sitter` - ([8e48553](https://github.com/abougouffa/minemacs/commit/8e48553f5423a59eed343f61830884e84fd1f4c0)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add `forge` config example to `config.el` - ([c2c900a](https://github.com/abougouffa/minemacs/commit/c2c900a4e74ebf54d9871fc6d7b1db46341c79fc)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c990ed3](https://github.com/abougouffa/minemacs/commit/c990ed352f14bcca6fa4c04f2a756ab9d7e4cb2a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.3.0](https://github.com/abougouffa/minemacs/compare/v1.2.0..v1.3.0) - 2023-09-17
#### Bug Fixes
- **(core)** correctly handle edge cases in `+env-save` - ([6047d92](https://github.com/abougouffa/minemacs/commit/6047d92ca0372a8365ae73511809414123e8e674)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** respect `visual-line-mode` - ([672f31c](https://github.com/abougouffa/minemacs/commit/672f31c1b95c8d63f1f83e506e60599f543f0555)) - [@abougouffa](https://github.com/abougouffa)
- **(regexp)** fix keybindings - ([49d7ff7](https://github.com/abougouffa/minemacs/commit/49d7ff75b87b1ce202356ea6f33fcf2eb3109827)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(emacs-lisp)** add a comment - ([0c4b0a4](https://github.com/abougouffa/minemacs/commit/0c4b0a4d4f40537fd3ebf6c262ba4fd3c851e4a5)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update copyright year - ([ae42456](https://github.com/abougouffa/minemacs/commit/ae42456b7595f4cfd9a4404cbca37586088807d3)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** add support for FreeBasic via `fb-mode` - ([510c299](https://github.com/abougouffa/minemacs/commit/510c29913b83e5b24e80b1885a87148e91d44230)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- cleanup changelog - ([2ea7834](https://github.com/abougouffa/minemacs/commit/2ea783408f7dc431327bcaf28db668a84d5fd913)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- clean file header - ([d2de9a2](https://github.com/abougouffa/minemacs/commit/d2de9a2dcf68aacb07344e3b7881ff598f73cc5b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** disable auto desktop saving - ([fcdb25e](https://github.com/abougouffa/minemacs/commit/fcdb25eee68906f69542ce4cae90e62e05232434)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** bump `codelldb` and `cpptools` versions - ([25c78f0](https://github.com/abougouffa/minemacs/commit/25c78f07debeb37e83269fb79a233547df0128d7)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** bump `cpptools` version - ([8092276](https://github.com/abougouffa/minemacs/commit/809227660cd0b028952830c79c73ec77d91b38eb)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** replace `expand-region` with `expreg` - ([ea64eaa](https://github.com/abougouffa/minemacs/commit/ea64eaad6a1c0f5bcfd9c4129d0050aaa646ee49)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** restore `evil-snipe` - ([a4f2f05](https://github.com/abougouffa/minemacs/commit/a4f2f059eb69803b51129bd7d8ee067bbca5bc21)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** use π as prefix for `project-` - ([efef895](https://github.com/abougouffa/minemacs/commit/efef8953e3d8364649fe09fa30c019da02f4f95a)) - [@abougouffa](https://github.com/abougouffa)
- **(lisp)** drop support for `me-lisp` - ([a616cb8](https://github.com/abougouffa/minemacs/commit/a616cb8825e4d2e315692325f2be5028fa4c0521)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete)** minor edit in the `unicode-fonts` package - ([7f96d85](https://github.com/abougouffa/minemacs/commit/7f96d855c983de67984ae00fd9059044ab1b8128)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** remove useless class - ([31c5960](https://github.com/abougouffa/minemacs/commit/31c59609e5ba350211e9fa45d5e3dff1cbb63b63)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** revert FACILE format to PDF/A-1b - ([62aa675](https://github.com/abougouffa/minemacs/commit/62aa67552d8f18fa8a1ba9a0f867267ff972dffb)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** additional latex classes - ([74e6914](https://github.com/abougouffa/minemacs/commit/74e69141ee3569a29ed813434a93c5e4944382d5)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** enable `magit-todos` - ([5244743](https://github.com/abougouffa/minemacs/commit/524474338d5438f0b555934c998af6e3e5936c67)) - [@abougouffa](https://github.com/abougouffa)
- **(xml)** auto detect `*.xmpi` files - ([ee119c7](https://github.com/abougouffa/minemacs/commit/ee119c79b76ff27a7ade8ab5f2bb8f6a3b220d2a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([700c92b](https://github.com/abougouffa/minemacs/commit/700c92b972e9a7629ca5ab345fbe4c283361e778)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d12d29b](https://github.com/abougouffa/minemacs/commit/d12d29b6a97023ef2f9f44a324ae352910cc3ba2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6da0a15](https://github.com/abougouffa/minemacs/commit/6da0a1579ca313525af4db135efc548257b481e6)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([aaf5857](https://github.com/abougouffa/minemacs/commit/aaf58574f2ca38a46034e819be59166ab733fbf2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0d4bb85](https://github.com/abougouffa/minemacs/commit/0d4bb85ad19cac30c35b9cd9fb3f17830c3ed359)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([917b968](https://github.com/abougouffa/minemacs/commit/917b968beb23c386e7e38b33f8624ec50a85248f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d40764a](https://github.com/abougouffa/minemacs/commit/d40764a6f5c2514bfff0072b1d4ee757cbd7e616)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.2.0](https://github.com/abougouffa/minemacs/compare/v1.1.0..v1.2.0) - 2023-08-18
#### Bug Fixes
- **(core)** fix deprecated `pcase` statement - ([5d8ed44](https://github.com/abougouffa/minemacs/commit/5d8ed4492ee9c02d72191fc4dc369b754112d9f1)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** ignore stderr when saving system envvars - ([2b638db](https://github.com/abougouffa/minemacs/commit/2b638db5fb4e6f59b0d6d9c49115837e4110e0df)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** temporary disable `evil-snipe` - ([ca6ff07](https://github.com/abougouffa/minemacs/commit/ca6ff07ce269fa3dc5621e28b6aed6544247a4c2)) - [@abougouffa](https://github.com/abougouffa)
- **(magit)** temporary disable `magit-todos` - ([2812e8d](https://github.com/abougouffa/minemacs/commit/2812e8d6f3f092acc6b567508679a75d2b2e31d0)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** fix keybindings when mu4e is not auto-started - ([f4c41db](https://github.com/abougouffa/minemacs/commit/f4c41db5b8d784d36b7b5272d74e43213c183a5d)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** typo (#106) - ([cf899af](https://github.com/abougouffa/minemacs/commit/cf899af2c85d5eae56b8ed892b1c9c954f162533)) - [@abougouffa](https://github.com/abougouffa)
- **(smerge)** autoload the hydra menu - ([767792d](https://github.com/abougouffa/minemacs/commit/767792d20a7e72c8f2cc9a8578ab924658dcea00)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(core)** minor documentation edit - ([154f945](https://github.com/abougouffa/minemacs/commit/154f94595f1e812f039cc20ea0c4688f21cf2039)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** add documentation for some constants - ([33207e0](https://github.com/abougouffa/minemacs/commit/33207e00f1cdbe0527e0bf6d37401e9c84c6773d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(data)** add support for `gnuplot` - ([62fb2ad](https://github.com/abougouffa/minemacs/commit/62fb2ad9f54cda1213066c047bbb5b4631fa9a7a)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** enable evil-textobj-tree-sitter (WIP) - ([c1b9cc4](https://github.com/abougouffa/minemacs/commit/c1b9cc49512fa12862f7043aa0f9a7f06a6d7b65)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** enable `ox-odt` export backend - ([f533a45](https://github.com/abougouffa/minemacs/commit/f533a4569617a965030906fec8082180a561b345)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** enable `ox-koma-letter` export backend - ([ebfdd71](https://github.com/abougouffa/minemacs/commit/ebfdd71d4239113aa336e10e654cdd648a0de7fb)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** additional templates - ([ef5d4f2](https://github.com/abougouffa/minemacs/commit/ef5d4f2c30d12f0b5d4f2bc6448cb69b56d39cec)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- organize templates - ([d4e73e9](https://github.com/abougouffa/minemacs/commit/d4e73e9d784fd21cb4f56c787d2b5b60591ca965)) - [@abougouffa](https://github.com/abougouffa)
- update the bug report template - ([5a21e6c](https://github.com/abougouffa/minemacs/commit/5a21e6c6f3d94b2a3c22da7218c401f30c639d0e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(daemon)** make use of `+add-hook!` - ([918256a](https://github.com/abougouffa/minemacs/commit/918256a22bf4336fe93015c7c80465588d45b84b)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** rename hook functions - ([73a0a06](https://github.com/abougouffa/minemacs/commit/73a0a0646b924dd480f386bc33c20d35c3c1dcf9)) - [@abougouffa](https://github.com/abougouffa)
- move `smerge` to `me-builtin` - ([c6dca7e](https://github.com/abougouffa/minemacs/commit/c6dca7e9769fba808f4295315ffabc85e1215796)) - [@abougouffa](https://github.com/abougouffa)
- group `use-package` related hacks - ([bc25440](https://github.com/abougouffa/minemacs/commit/bc25440f987424eb5fc4ce7e4c627a34e9cc2db3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bootstrap)** expand `use-package` minimally unless in debug mode - ([fc883b6](https://github.com/abougouffa/minemacs/commit/fc883b62bd4139417fb9584e0b07a0038cf7fd89)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove `+fill-scratch-buffer` - ([1f31061](https://github.com/abougouffa/minemacs/commit/1f31061dab8b7acf3f9391ed60b302a35e4d67f3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove `+unix.el` - ([2dd871e](https://github.com/abougouffa/minemacs/commit/2dd871eb023b8c69708eb58ef150b926b53595c5)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** regenerate loaddefs - ([64bb338](https://github.com/abougouffa/minemacs/commit/64bb3388bac06781844244232fedd6090389e8f0)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove some unused macros - ([0e10572](https://github.com/abougouffa/minemacs/commit/0e10572d4b374188d55a32b6bc13a13c2c28cc08)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** update `me-loaddefs` - ([f5c6fcd](https://github.com/abougouffa/minemacs/commit/f5c6fcdf135887cd3c8eabcbc095993b0117ba75)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unused `+debug!` macro - ([e401223](https://github.com/abougouffa/minemacs/commit/e401223854516f01840818f3e0277139dca66229)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** provide `+lazy-delay` - ([1ab081d](https://github.com/abougouffa/minemacs/commit/1ab081d54715081d0d2f2a67d7b6a2c97c0989cb)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simplify `+env-save` - ([0da8387](https://github.com/abougouffa/minemacs/commit/0da838748ee2e1d1b802fa36672a820b00911edc)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better envvars management - ([7180101](https://github.com/abougouffa/minemacs/commit/7180101befdc39694b631688443295650453be77)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edit in `+env-save` - ([9f2c175](https://github.com/abougouffa/minemacs/commit/9f2c175f78651d34fc74811b831f30b350af3c56)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** bump `cpptools` version - ([5de872d](https://github.com/abougouffa/minemacs/commit/5de872ddc058e90776a2a886d9385457c6a92b91)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** bump default `cpptools` version - ([048c1d6](https://github.com/abougouffa/minemacs/commit/048c1d6355b9604cf36d7a04eb673e9c64126b18)) - [@abougouffa](https://github.com/abougouffa)
- **(dap-cpptools)** bump version - ([1634dfd](https://github.com/abougouffa/minemacs/commit/1634dfd2147cce4b83654606f0ae50d6e82c4d64)) - [@abougouffa](https://github.com/abougouffa)
- **(dap-python)** use `debugpy` instead of `ptvsd` - ([4f88b64](https://github.com/abougouffa/minemacs/commit/4f88b6480cf380155bcafd84ce2d0108e1f7c3d1)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** ignore case in completion - ([36523e6](https://github.com/abougouffa/minemacs/commit/36523e6872a654c2dcc2f2dd22ba32dd7fd49dcd)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** set `apropos-do-all` - ([0decadd](https://github.com/abougouffa/minemacs/commit/0decaddea09d25fd4f2066965e1a0c4bd3a880df)) - [@abougouffa](https://github.com/abougouffa)
- **(edraw)** customize files - ([029e722](https://github.com/abougouffa/minemacs/commit/029e722050a78b1b11a04ce029f94828522ebccd)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** move a custom function - ([d61510c](https://github.com/abougouffa/minemacs/commit/d61510c6778c8f3223db84d25d17220b42b5b460)) - [@abougouffa](https://github.com/abougouffa)
- **(git-commit)** simplify the config - ([2445798](https://github.com/abougouffa/minemacs/commit/2445798612b8020ce6d5c09a29cf9b3573d43b81)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** butter bindings for `sudo*` - ([79fe849](https://github.com/abougouffa/minemacs/commit/79fe849887e187849935bf092ad4c18f493a6d33)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** replace :commands with :functions - ([bd327ba](https://github.com/abougouffa/minemacs/commit/bd327ba4be0cc43b6ba4bcc2967d369bcf7e38b6)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** explicitly enable `ox-beamer` - ([6dff7a2](https://github.com/abougouffa/minemacs/commit/6dff7a276b8c9df8f74791db7a76af0253cfd027)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** load `ox-*` after `ox` - ([b758dae](https://github.com/abougouffa/minemacs/commit/b758dae2662c1784edfaceadd2117b7c70caba7b)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add optional arg in `+project-scan-for-projects` - ([23a617d](https://github.com/abougouffa/minemacs/commit/23a617d73ce79307c6957d36c9de073bd9547b50)) - [@abougouffa](https://github.com/abougouffa)
- **(smartparens)** enable globally - ([b2cec8b](https://github.com/abougouffa/minemacs/commit/b2cec8b206a97cc4fe57ecbde42a47ce2d2d4b4c)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** additional `org-mode` templates - ([b54e45d](https://github.com/abougouffa/minemacs/commit/b54e45d4b6ca1f708799f100d55f8c0fef9a7c38)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** move additional templates to `assets/` - ([fddcedd](https://github.com/abougouffa/minemacs/commit/fddcedd343e085a0fc356dbfac9208ab4ffacbea)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** minor edits in `+env-deny-vars` - ([d566710](https://github.com/abougouffa/minemacs/commit/d566710e6bf2fba6b4d41736a4d92933c08de69b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3a3fa2b](https://github.com/abougouffa/minemacs/commit/3a3fa2b9ac4aef4406aefdc41a7be8df1edf4398)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d6b01b6](https://github.com/abougouffa/minemacs/commit/d6b01b682696c047e381621d402d4ab045b4d75c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a68a092](https://github.com/abougouffa/minemacs/commit/a68a0927e94e7272e0b13ccf3c1795dd732a4ce0)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([30fee3b](https://github.com/abougouffa/minemacs/commit/30fee3ba89d2d7a572c922dffb0023f033a11ee6)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([c9398a0](https://github.com/abougouffa/minemacs/commit/c9398a07011ac5f5c40796bd23d19c1b52f72442)) - [@abougouffa](https://github.com/abougouffa)
- split `me-lisp` into `me-emacs-lisp`, `me-common-lisp`, `me-scheme` & `me-clojure` - ([d06712e](https://github.com/abougouffa/minemacs/commit/d06712e0e38fde75805fa4de0022fc89829d715b)) - [@abougouffa](https://github.com/abougouffa)
- update `+html2pdf` templates directory - ([54bf131](https://github.com/abougouffa/minemacs/commit/54bf131a734f51ccbfca4bff8d44baff01ab776b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4869483](https://github.com/abougouffa/minemacs/commit/4869483a137fee361df219086be4c49f302da2e9)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ab9084e](https://github.com/abougouffa/minemacs/commit/ab9084efe27191fd0ab5f94eee5502766fce16c1)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.1.0](https://github.com/abougouffa/minemacs/compare/v1.0.3..v1.1.0) - 2023-07-18
#### Bug Fixes
- **(+writing-mode)** don't suppose Org to be loaded (#103) - ([6bedf30](https://github.com/abougouffa/minemacs/commit/6bedf30c78859a5f3402c63564dd4aaedefd6cf6)) - [@abougouffa](https://github.com/abougouffa)
- **(bootstratp)** disable `exec-path-from-shell` in Windows (#99) - ([59b4864](https://github.com/abougouffa/minemacs/commit/59b486445ef1d2cdd1e9f57de222d45b8b8582e2)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** use-package error - ([285f46c](https://github.com/abougouffa/minemacs/commit/285f46ca0de3a81a27e9e112d4b27abd8246ecd7)) - [@abougouffa](https://github.com/abougouffa)
- **(dumb-jump)** fixes find reference in C/C++ - ([02a7f39](https://github.com/abougouffa/minemacs/commit/02a7f3902ea99e6d0834860cdab2eb978aba846d)) - [@abougouffa](https://github.com/abougouffa)
- **(elisp)** autoload a function - ([61a5862](https://github.com/abougouffa/minemacs/commit/61a58628c7010df9cce9097f8b4c46a6ad46739a)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** don't load environment vars on Win (#104) - ([5a38524](https://github.com/abougouffa/minemacs/commit/5a385249b816306805f1f44a21d63c0cc6eae2ab)) - [@abougouffa](https://github.com/abougouffa)
- **(netextender)** use the right command function - ([541931f](https://github.com/abougouffa/minemacs/commit/541931f23fa30d2420278bd62a4d67f48b51e525)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** warn on Windows - ([279f58e](https://github.com/abougouffa/minemacs/commit/279f58eaba45cdf91aead5c838902dfb19c5cd71)) - [@abougouffa](https://github.com/abougouffa)
- fix some byte compiler errors - ([7a46079](https://github.com/abougouffa/minemacs/commit/7a4607942acb949d9c15344f30e086ea1de16e16)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- update modules documentation - ([5828984](https://github.com/abougouffa/minemacs/commit/58289843c3fd4d6561f14ce6a5803c37b4094954)) - [@abougouffa](https://github.com/abougouffa)
- update the screenshot - ([805f96b](https://github.com/abougouffa/minemacs/commit/805f96bb7fe21293639b3df0d9573ee9ac1372b6)) - [@abougouffa](https://github.com/abougouffa)
- update screenshot - ([b84b2e0](https://github.com/abougouffa/minemacs/commit/b84b2e0ed5d16e6feba440fb8dacb50c913422a3)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(cape)** helpers to make use of `cape-super-capf` - ([ac587e0](https://github.com/abougouffa/minemacs/commit/ac587e0800d1dea4026a2a3bf92e0ab9fa5f021f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better management of environment variables - ([87e498e](https://github.com/abougouffa/minemacs/commit/87e498e6460f0d0f0b86a6587f7337bce5ca23b3)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `cov` obsolete - ([4516c9b](https://github.com/abougouffa/minemacs/commit/4516c9b62ce3f9aa47f1bc4791f2a54ecbb26fd6)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `chezmoi` support - ([c5386d3](https://github.com/abougouffa/minemacs/commit/c5386d3ae493a82dfdc0fce3f71c712d461714ff)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- update issue template - ([66054d4](https://github.com/abougouffa/minemacs/commit/66054d46be3d0a35427a47224ed3ee4daafca574)) - [@abougouffa](https://github.com/abougouffa)
- add bug report template - ([10e3411](https://github.com/abougouffa/minemacs/commit/10e3411cdb030fbd4b92e5b74f153870885b1ac8)) - [@abougouffa](https://github.com/abougouffa)
- minor Makefile edit - ([a5d2e65](https://github.com/abougouffa/minemacs/commit/a5d2e65657c5bf292675312ebb04d746a53da616)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(elisp)** minor refactoring - ([dd32645](https://github.com/abougouffa/minemacs/commit/dd326459d1765426ab2f5dd9554b7a8d7bbf585d)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** minor edits - ([5bcb8d2](https://github.com/abougouffa/minemacs/commit/5bcb8d26ef779ad3e06007691545b08e1ea435f8)) - [@abougouffa](https://github.com/abougouffa)
- **(which-key)** use explicit function symbol - ([01d4a5a](https://github.com/abougouffa/minemacs/commit/01d4a5ab0ad0206b7c9bbf527cae04af3c72e657)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core-ui)** make use of `+add-hook!` - ([3a35113](https://github.com/abougouffa/minemacs/commit/3a35113182d25db58ec3981e97bdd96b3e122c80)) - [@abougouffa](https://github.com/abougouffa)
- move more settings to `me-builtin` - ([3f89b75](https://github.com/abougouffa/minemacs/commit/3f89b751783ca7ed4eb87ab47f3fd7c93149fb43)) - [@abougouffa](https://github.com/abougouffa)
- move more settings to `me-builtin` - ([ac827ff](https://github.com/abougouffa/minemacs/commit/ac827ffcfdc36b52303b3f100c2f754cf0730f33)) - [@abougouffa](https://github.com/abougouffa)
- move more settings from `me-defaults` to `me-builtin` - ([f0da499](https://github.com/abougouffa/minemacs/commit/f0da499a79cfc00ff2bfd8838132a62055e16083)) - [@abougouffa](https://github.com/abougouffa)
- move more customization to `me-builtin` - ([24e6d3b](https://github.com/abougouffa/minemacs/commit/24e6d3bec762b6cc9a0153c7bb7be112b618c165)) - [@abougouffa](https://github.com/abougouffa)
- move more settings to `me-builtin` - ([dcf61b7](https://github.com/abougouffa/minemacs/commit/dcf61b7caac1dd9be7cb873c6af9c46bbf680d11)) - [@abougouffa](https://github.com/abougouffa)
- minor refactoring - ([90defba](https://github.com/abougouffa/minemacs/commit/90defba213c389be0e554a680a8507fd1603403d)) - [@abougouffa](https://github.com/abougouffa)
- move all built-in packages configs to `me-builtin` - ([f3aeaa2](https://github.com/abougouffa/minemacs/commit/f3aeaa215f084df98e9c14956a7ee071dd4e0675)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(core)** remove `exec-path-from-shell` - ([8dfb0d9](https://github.com/abougouffa/minemacs/commit/8dfb0d9c96d22cba28f6bc23f1e56f4796c7d215)) - [@abougouffa](https://github.com/abougouffa)
- **(header2)** remove it (use autoinsert instead) - ([b50c299](https://github.com/abougouffa/minemacs/commit/b50c299df7d585ea75e9e4998fae0e6769bf63d3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(biblio)** install `citar-org-roam` only when needed - ([04590b6](https://github.com/abougouffa/minemacs/commit/04590b6977eafc413ec49714f082bc5e7af55a5e)) - [@abougouffa](https://github.com/abougouffa)
- **(calfw)** change keybinding - ([29dc09d](https://github.com/abougouffa/minemacs/commit/29dc09d391c95e77118977adde2858fcfd31d653)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** make auto bury customizable - ([9469b40](https://github.com/abougouffa/minemacs/commit/9469b40fc7282a9ca336c44e15e194b1a05f85e0)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edit - ([b474c52](https://github.com/abougouffa/minemacs/commit/b474c524779c30efe7fa9ef386b1c6b4638ffe1d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** ignore more envvars - ([7a49948](https://github.com/abougouffa/minemacs/commit/7a49948a9e42766de7d51c9a4a08a131efd06b52)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better management of environment variables - ([fc202b6](https://github.com/abougouffa/minemacs/commit/fc202b68c90fbdc7c547564af82e4c44198abb53)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** double-check for elfeed - ([55f3eae](https://github.com/abougouffa/minemacs/commit/55f3eaebb0a7b6831da44b005880ff0ce715c22d)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `unicode-fonts` obsolete - ([9e1af9a](https://github.com/abougouffa/minemacs/commit/9e1af9a624966babbd85e4b958beafc7654b2470)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** remove `goggles` - ([c06f811](https://github.com/abougouffa/minemacs/commit/c06f8112a4fc4deb5fc2e1f1ebbe77f11ba873bb)) - [@abougouffa](https://github.com/abougouffa)
- **(header2)** cleaner default header - ([8d5cb21](https://github.com/abougouffa/minemacs/commit/8d5cb212f99f7adcee2ee9b9caf757700a04fa02)) - [@abougouffa](https://github.com/abougouffa)
- **(hideif)** do not enable by default - ([ec7fe35](https://github.com/abougouffa/minemacs/commit/ec7fe35e24571d86a88cfc705d53fa608b022e7a)) - [@abougouffa](https://github.com/abougouffa)
- **(json)** enable `json-mode` commands in `json-ts-mode` - ([72f5f90](https://github.com/abougouffa/minemacs/commit/72f5f9049a77f8886baa102bffc29a0e35904f32)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** use original command names - ([5f608b2](https://github.com/abougouffa/minemacs/commit/5f608b24c0bd1873942143e002708c483579e2b0)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** clean unused modes - ([b0a6c9f](https://github.com/abougouffa/minemacs/commit/b0a6c9f4b06ddcec84d4e03c990874273d8d4b16)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add `use-package` example - ([9905990](https://github.com/abougouffa/minemacs/commit/9905990086334e43dbbb6c17d76f5384f5149615)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** enable user-defined snippets - ([59d8b1f](https://github.com/abougouffa/minemacs/commit/59d8b1fd79e32ef21fb0d497c7245894ca88ef78)) - [@abougouffa](https://github.com/abougouffa)
- **(undo-fu)** set `evil-undo-system` with `setopt` - ([7841b94](https://github.com/abougouffa/minemacs/commit/7841b94692009558c8ed3fd3b3b86916bf183c32)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** better OS checking - ([5f194cd](https://github.com/abougouffa/minemacs/commit/5f194cde9e84a6b164023f3af2a27c192295a275)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** add a disclaimer when Windows is detected - ([7763e04](https://github.com/abougouffa/minemacs/commit/7763e04c082a8db7a4302d07967e2b28d02a57f4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5c18fc3](https://github.com/abougouffa/minemacs/commit/5c18fc3c940ebbc09cd312c690c5c1decb139e1b)) - [@abougouffa](https://github.com/abougouffa)
- remove empty `me-gnus` module - ([730c411](https://github.com/abougouffa/minemacs/commit/730c4118c4b515c32228fb1f5af5c1322a60b60c)) - [@abougouffa](https://github.com/abougouffa)
- update packages versions - ([52262dd](https://github.com/abougouffa/minemacs/commit/52262dd8833dfc226518e6ade52b2470fdc5d748)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ad02cb3](https://github.com/abougouffa/minemacs/commit/ad02cb35f19a0c74b13657b20bd59f44457cb4c0)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9ba6301](https://github.com/abougouffa/minemacs/commit/9ba63014cfc0080fd675289f8910f15d610014f9)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0921dbf](https://github.com/abougouffa/minemacs/commit/0921dbfc6beed2daa3ee0541b1af8f9c2cfc4f7a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.0.3](https://github.com/abougouffa/minemacs/compare/v1.0.2..v1.0.3) - 2023-06-26
#### Bug Fixes
- **(daemon)** defer checking `+mu4e-available-p` (#79) - ([52d72de](https://github.com/abougouffa/minemacs/commit/52d72de5fa3a12e2bd4a25340c7e1b72b8115272)) - [@abougouffa](https://github.com/abougouffa)
- **(drag-stuff)** avoid keybinding conflict - ([54343a4](https://github.com/abougouffa/minemacs/commit/54343a485fc6414cd6a32389d8bde2c78693fc79)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** add `rust-ts-mode` to auto enable modes - ([9edfb5c](https://github.com/abougouffa/minemacs/commit/9edfb5c5330037da7286ce07d06765e2d185358f)) - [@abougouffa](https://github.com/abougouffa)
- **(keybinding)** fix a typo, bind new command - ([c7f1fe3](https://github.com/abougouffa/minemacs/commit/c7f1fe3cd1f9ea1bfda0c87c2a946ae85dad259a)) - [@donneyluck](https://github.com/donneyluck)
- **(keybinding)** handle `SPC u SPC u ...` - ([77a4948](https://github.com/abougouffa/minemacs/commit/77a4948afd44e210fa65262920eef5c66144f9d3)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** update obsolete functions - ([7041206](https://github.com/abougouffa/minemacs/commit/704120643fc9c49b742052b656ac336072fd5aac)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** accept list grouped disabled packages - ([e86a19a](https://github.com/abougouffa/minemacs/commit/e86a19ae49e213f3c58ce1161b21eb7303efff70)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add support for `drag-stuff` - ([b42e987](https://github.com/abougouffa/minemacs/commit/b42e9877c236257ceaf677e95ac5233af3ce5080)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** basic locking functions - ([75a28ba](https://github.com/abougouffa/minemacs/commit/75a28ba6978b67730c0d240fb76e38e9917dc421)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `ts-fold` for `treesit` - ([16db93a](https://github.com/abougouffa/minemacs/commit/16db93aa9da21c946d83a530fff24219a67ad2ef)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** restore `affe` - ([693de26](https://github.com/abougouffa/minemacs/commit/693de26cc7bfb54f28f46767453db920d182aa1d)) - [@abougouffa](https://github.com/abougouffa)
- add `me-calendar` WIP - ([665f39f](https://github.com/abougouffa/minemacs/commit/665f39f80637924a31ba292ce212e243632da93c)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- minor tweak in Cocogitto conf - ([f6e6baa](https://github.com/abougouffa/minemacs/commit/f6e6baa85408570a9b8b6c862986679a2e8b1572)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(init)** minor edit - ([819c3cd](https://github.com/abougouffa/minemacs/commit/819c3cd5f8650cd1070f444829578d46ed4012de)) - [@abougouffa](https://github.com/abougouffa)
- **(ligature)** enable only on supported systems - ([87d640a](https://github.com/abougouffa/minemacs/commit/87d640abb9cf871b09d2745c9873c0b51fdaeea9)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(prog)** minor fixes and tweaks - ([ddb7267](https://github.com/abougouffa/minemacs/commit/ddb7267b9cf555e4ecdf1bcfb903d4346aa7dc1b)) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** better check - ([985b1f3](https://github.com/abougouffa/minemacs/commit/985b1f366441e723474f2271561d6b294e38d440)) - [@abougouffa](https://github.com/abougouffa)
- simplify `use-package`'s `:hook` forms - ([cd75b1b](https://github.com/abougouffa/minemacs/commit/cd75b1b6c2b43a9ab8aabecde611beb31159a156)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(init)** fixes an error on Emacs 28 - ([32a461d](https://github.com/abougouffa/minemacs/commit/32a461d669d94f8cb1263498a79c2f369ebf44be)) - [@abougouffa](https://github.com/abougouffa)
- simplify `use-package`'s `:hook` forms (fix E28) - ([cd83f88](https://github.com/abougouffa/minemacs/commit/cd83f88d423a459e0b9c68d065246919253c04a3)) - [@abougouffa](https://github.com/abougouffa)
- rewrite conditional package installs - ([0643de4](https://github.com/abougouffa/minemacs/commit/0643de4c3e7c5998e5c29abbcc13c650f481afce)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bootstrap)** set `use-package-verbose` accordingly - ([7e1dc87](https://github.com/abougouffa/minemacs/commit/7e1dc871624378e1cc2a43e6c155c4919cff5722)) - [@abougouffa](https://github.com/abougouffa)
- **(cmake)** use built-in `cmake-ts-mode` - ([e75ba00](https://github.com/abougouffa/minemacs/commit/e75ba00f5eefff7e7a9ff6b4cf4c453ae36d1710)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** minor tweaks - ([4af1675](https://github.com/abougouffa/minemacs/commit/4af16758cff9444f0446aeacecffff247fc9b992)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** make `minemacs-build-functions` a special hook - ([459981b](https://github.com/abougouffa/minemacs/commit/459981bb3697fd3840e06b1c1d489707442cde3c)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** simplify a condition - ([3bd1f76](https://github.com/abougouffa/minemacs/commit/3bd1f761ec4e5a8dad3cade65845cbace9816790)) - [@abougouffa](https://github.com/abougouffa)
- **(epa)** ask for passphrase in Emacs minibuffer - ([601972b](https://github.com/abougouffa/minemacs/commit/601972b7fbadaf1c8e09a173e3c5888a6726555b)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-easy)** autoload `flymake-easy-load` - ([a21b7cc](https://github.com/abougouffa/minemacs/commit/a21b7ccb9ba4c4df116c9453a5bf639020415a31)) - [@abougouffa](https://github.com/abougouffa)
- **(forge)** demand after `magit` - ([709bf10](https://github.com/abougouffa/minemacs/commit/709bf10fdced6107b2d7d4c0ca8c50dd4e394040)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** minor edits in `+html2pdf` - ([c274a6c](https://github.com/abougouffa/minemacs/commit/c274a6c87083190b3e6eee1d00875f19dfc95813)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** do not use by default for LaTeX modes - ([5056403](https://github.com/abougouffa/minemacs/commit/50564039a46d0a69cc50ab9c27eeea325c682f40)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** set mu4e-modeline icons after `nerd-icons` - ([cb5686a](https://github.com/abougouffa/minemacs/commit/cb5686a4ddbf8a396f384bb031c3b24185fe331f)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** add locking mechanism - ([079f070](https://github.com/abougouffa/minemacs/commit/079f070ae57ac35e7250de3f852e224b3bd75cfb)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** better icons for `mu4e-modeline` - ([71cb363](https://github.com/abougouffa/minemacs/commit/71cb363bb0c25bfcbbb2399745b94e7c7119cee7)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** do not show message - ([a653069](https://github.com/abougouffa/minemacs/commit/a653069a8fc7cf2701d6e5604200656b6ecbb1c1)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** take account of multiple emails in auto BCC - ([5587715](https://github.com/abougouffa/minemacs/commit/5587715c1d0a1879ce64be81cc57da632d86036b)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add some preconfigured LaTeX classes - ([198c56f](https://github.com/abougouffa/minemacs/commit/198c56f948b85dad5de388507db6aabbc66d8766)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add babel tangle bindings - ([6e5af80](https://github.com/abougouffa/minemacs/commit/6e5af80a240ba47c45d5332aa8cf64196945b99c)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** use booktabs by default - ([718c187](https://github.com/abougouffa/minemacs/commit/718c187264e7f7f3557fad83b9c851d0de68781f)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** bind `project-execute-extended-command` - ([06a8eb6](https://github.com/abougouffa/minemacs/commit/06a8eb6b93407a5bc8e757fc770d03af16b4ad0a)) - [@abougouffa](https://github.com/abougouffa)
- **(ros)** better dependency management - ([f64ff69](https://github.com/abougouffa/minemacs/commit/f64ff69fd01514798b580e896cc7a087b58aa6b9)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update examples - ([abc3c91](https://github.com/abougouffa/minemacs/commit/abc3c91e6ebbeb1508c09246eed8434043aeee7e)) - [@abougouffa](https://github.com/abougouffa)
- **(smartparens)** disable expensive navigation features - ([77ca837](https://github.com/abougouffa/minemacs/commit/77ca837c11bba22f5c0bf0fb278899544605eba3)) - [@abougouffa](https://github.com/abougouffa)
- **(ts-fold)** use my fork - ([ed087a5](https://github.com/abougouffa/minemacs/commit/ed087a5fe548941380df236ec1ced02c5da15229)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** do not deffer - ([a75d587](https://github.com/abougouffa/minemacs/commit/a75d58773c1b6bbb7cf2f89233e38e61723912e9)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6940f9f](https://github.com/abougouffa/minemacs/commit/6940f9f2571a1c12afe75bee58e9938fdeb2a68d)) - [@abougouffa](https://github.com/abougouffa)
- prefer built-in `cmake-ts-mode` - ([3839539](https://github.com/abougouffa/minemacs/commit/383953954abdc5e9b2ff21d9ba3b7205c18670fc)) - [@abougouffa](https://github.com/abougouffa)
- rewrite conditional package installs - ([da579e7](https://github.com/abougouffa/minemacs/commit/da579e75763a614dcf0df8eac555541fc9f4b3d5)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([b3f3474](https://github.com/abougouffa/minemacs/commit/b3f3474a473269703772ea3dc240f35b81d24a3c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.0.2](https://github.com/abougouffa/minemacs/compare/v1.0.1..v1.0.2) - 2023-06-17
#### Bug Fixes
- **(bootstrap)** do store disabled packages with configured ones - ([04650b1](https://github.com/abougouffa/minemacs/commit/04650b1c84c10240e4b29cc7d0a7ed7cd51df69f)) - [@abougouffa](https://github.com/abougouffa)
- **(buffer)** trim extra spaces in `+fill-scratch-buffer` - ([4887558](https://github.com/abougouffa/minemacs/commit/48875582780655541b6629c63cfa6e0a6257ca3c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fix persistent scratch hooks/functions - ([309e5a5](https://github.com/abougouffa/minemacs/commit/309e5a51f4c0e667b4ffd06ad6a5a779d65d636d)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs)** fix the `+def-dedicated-tab!` macro (#76) - ([054940b](https://github.com/abougouffa/minemacs/commit/054940bd6f1e7dbfda3bee659790411459a00c22)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** repeat last command "." fix - ([1cca61c](https://github.com/abougouffa/minemacs/commit/1cca61c5f05566966bdc9d0baab99285f5d7c62c)) - [@abougouffa](https://github.com/abougouffa)
- **(evil+parinfer)** disable parinfer on some commands - ([a46a79f](https://github.com/abougouffa/minemacs/commit/a46a79f039d215d84f6fad17ba52159406ab0feb)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** remove duplicate paragraph (#75) - ([77f05eb](https://github.com/abougouffa/minemacs/commit/77f05eb6442f74871323dd4514f6b96c74d6aebe)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** use persistent scratch buffers by default - ([936dae0](https://github.com/abougouffa/minemacs/commit/936dae026ad39cd1e4e656735b9383c94fc6d015)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+define-dedicated-workspace!` - ([cae91b2](https://github.com/abougouffa/minemacs/commit/cae91b2da24252d1cb620feb5593e6738d4e982a)) - [@abougouffa](https://github.com/abougouffa)
- define dedicated tab for `vterm` - ([109d902](https://github.com/abougouffa/minemacs/commit/109d90230c8fa21a395f235e8b30ac460fd9f78d)) - [@abougouffa](https://github.com/abougouffa)
- add persistent scratch hacks from Doom - ([17b421d](https://github.com/abougouffa/minemacs/commit/17b421de4d4c03125b4adc57e80de4d8b77ae789)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- update cocogitto configuration file - ([e97faa3](https://github.com/abougouffa/minemacs/commit/e97faa366e3e92173e3891d0d5eac2f299acca68)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(emacs)** rename a macro argument - ([833b03d](https://github.com/abougouffa/minemacs/commit/833b03dce2a16413e75cbfc07f211a827922ef8b)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([2f69a75](https://github.com/abougouffa/minemacs/commit/2f69a75db744956621d3c9f54236018f1711106e)) - [@abougouffa](https://github.com/abougouffa)
- add optional quotes - ([eeaf3b8](https://github.com/abougouffa/minemacs/commit/eeaf3b88bfe78b05c7c3bf5a5020a5a03db5c2c4)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor enhancement - ([5ccce03](https://github.com/abougouffa/minemacs/commit/5ccce03f68a95236a9451db7eb75de70a96378e5)) - [@abougouffa](https://github.com/abougouffa)
- **(eaf)** use a new strategy to disable - ([c2a11c8](https://github.com/abougouffa/minemacs/commit/c2a11c8002b2f4b7d4f8a611287fa9b8a249646c)) - [@abougouffa](https://github.com/abougouffa)
- **(kind-icon)** more explicit setup - ([5728453](https://github.com/abougouffa/minemacs/commit/57284530fbf3efa48ad5c5951b57ff3745ca772b)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** make use of `+define-dedicated-workspace!` - ([2ecc219](https://github.com/abougouffa/minemacs/commit/2ecc219dc24c40a4b9d0f47bec182d428ea505dc)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** minor edits in persistant scratch buffer - ([8258734](https://github.com/abougouffa/minemacs/commit/8258734fd067589006f8fa308d711f8c7c598224)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** accept optional args in `+hook-once!` - ([9db34ce](https://github.com/abougouffa/minemacs/commit/9db34cec007b2ac1c94471769a92733c40cb1835)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** accept functions with args in `+define-dedicated-workspace!` - ([7ca7b66](https://github.com/abougouffa/minemacs/commit/7ca7b6672f670c40045e11b7f952dd8381abc269)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** remove unneeded hack - ([8b5d2df](https://github.com/abougouffa/minemacs/commit/8b5d2df717eef69f040c49d355995dd2052227b0)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** don't ask about new files, better buffer naming - ([a5ce718](https://github.com/abougouffa/minemacs/commit/a5ce718297b9a1a9d20859bbd7ee225ac2a728d6)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** add some default directories - ([662fdf0](https://github.com/abougouffa/minemacs/commit/662fdf0cacfc5f0f53e04ea34b19a8059f8d321a)) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed)** run in dedicated workspace - ([0a0c29c](https://github.com/abougouffa/minemacs/commit/0a0c29cf1fe2a09b35f8975642a9ee6381fe61da)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** trash icalendar mails after reply - ([66caba7](https://github.com/abougouffa/minemacs/commit/66caba747da491c2e04c78c77d0d39e0c877bf18)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** minor edits - ([0f16835](https://github.com/abougouffa/minemacs/commit/0f168352346bd84865fb4478b1235cda19445e73)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add a book class that passes the FACILE test - ([571bf73](https://github.com/abougouffa/minemacs/commit/571bf73c2d29db06bdda4f74f48717e9077f63e7)) - [@abougouffa](https://github.com/abougouffa)
- **(org-msg)** remember last directory when adding attachements - ([499d731](https://github.com/abougouffa/minemacs/commit/499d7319040646e1dcb4adad65393d64647019e2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([53c88d7](https://github.com/abougouffa/minemacs/commit/53c88d73c5a7a82cb9c3ae31e090b99026347962)) - [@abougouffa](https://github.com/abougouffa)
- rename to `+def-dedicated-tab!`, return function symbol - ([8e3c27b](https://github.com/abougouffa/minemacs/commit/8e3c27b30bc411cbd52fb9842671b706ebd058ea)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([e2ad2ea](https://github.com/abougouffa/minemacs/commit/e2ad2ea99098df2c09eaf53675186395c1ae9440)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([de85b88](https://github.com/abougouffa/minemacs/commit/de85b88474ef212c625966a4152e76b53547b2b6)) - [@abougouffa](https://github.com/abougouffa)

- - -

## v1.0.1 - 2023-06-13
#### Tweaks
- **(default)** hide `tab-bar` tabs - ([023c0f4](https://github.com/abougouffa/minemacs/commit/023c0f4)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** make use of `+add-hook!` - ([d731c77](https://github.com/abougouffa/minemacs/commit/d731c77)) - [@abougouffa](https://github.com/abougouffa)

- - -

## v1.0.0 - 2023-06-12
#### Bug Fixes
- **(binary)** fix `objdump-disassemble-mode` - ([3f77e8d](https://github.com/abougouffa/minemacs/commit/3f77e8d)) - [@abougouffa](https://github.com/abougouffa)
- **(binary)** better management of objdump - ([41e20ec](https://github.com/abougouffa/minemacs/commit/41e20ec)) - [@abougouffa](https://github.com/abougouffa)
- **(binary)** temporary disable auto `hexl-mode` (#67) - ([ad08679](https://github.com/abougouffa/minemacs/commit/ad08679)) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** hook capfs the right way - ([31c733d](https://github.com/abougouffa/minemacs/commit/31c733d)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** check if `mu4e` is available - ([0950451](https://github.com/abougouffa/minemacs/commit/0950451)) - [@abougouffa](https://github.com/abougouffa)
- **(eaf)** remove undefined function - ([21548b7](https://github.com/abougouffa/minemacs/commit/21548b7)) - [@abougouffa](https://github.com/abougouffa)
- **(epa-file)** ensure enabling `epa-file` (#67) - ([568bb5d](https://github.com/abougouffa/minemacs/commit/568bb5d)) - [@abougouffa](https://github.com/abougouffa)
- **(keybinding)** remove duplicate binding for workspace - ([d391634](https://github.com/abougouffa/minemacs/commit/d391634)) - [@abougouffa](https://github.com/abougouffa)
- **(media)** problematic executable check (#65) - ([08cf1ef](https://github.com/abougouffa/minemacs/commit/08cf1ef)) - [@abougouffa](https://github.com/abougouffa)
- **(org-roam)** autosync - ([d62475b](https://github.com/abougouffa/minemacs/commit/d62475b)) - [@donneyluck](https://github.com/donneyluck)
- **(org-roam)** autosync database (#68) - ([bd4cd61](https://github.com/abougouffa/minemacs/commit/bd4cd61)) - [@abougouffa](https://github.com/abougouffa)
- **(org-roam-ui)** use another keybinding (#68) - ([5ba3042](https://github.com/abougouffa/minemacs/commit/5ba3042)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** do not overwrite Capf - ([d7ba8b7](https://github.com/abougouffa/minemacs/commit/d7ba8b7)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(mu4e-alert)** function documentation en comments - ([e9004e0](https://github.com/abougouffa/minemacs/commit/e9004e0)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example - ([f1bc80b](https://github.com/abougouffa/minemacs/commit/f1bc80b)) - [@abougouffa](https://github.com/abougouffa)
- update README - ([0dbf11c](https://github.com/abougouffa/minemacs/commit/0dbf11c)) - [@abougouffa](https://github.com/abougouffa)
- tiny fix in README - ([c292c96](https://github.com/abougouffa/minemacs/commit/c292c96)) - [@abougouffa](https://github.com/abougouffa)
- update README - ([392fdd8](https://github.com/abougouffa/minemacs/commit/392fdd8)) - [@abougouffa](https://github.com/abougouffa)
- update README to include the new variable - ([444f2c5](https://github.com/abougouffa/minemacs/commit/444f2c5)) - [@abougouffa](https://github.com/abougouffa)
- include the new environment vars in README - ([2077726](https://github.com/abougouffa/minemacs/commit/2077726)) - [@abougouffa](https://github.com/abougouffa)
- minor updates - ([868bb15](https://github.com/abougouffa/minemacs/commit/868bb15)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(code-cells)** initial support - ([64b041f](https://github.com/abougouffa/minemacs/commit/64b041f)) - [@abougouffa](https://github.com/abougouffa)
- **(combobulate)** initial support - ([fea6426](https://github.com/abougouffa/minemacs/commit/fea6426)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+setq-hook!` & `+unsetq-hook!` from Doom - ([5cdab26](https://github.com/abougouffa/minemacs/commit/5cdab26)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+add-hook!` & `+remove-hook!` from Doom - ([61bb207](https://github.com/abougouffa/minemacs/commit/61bb207)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** disable individual modules packages - ([4b91dc0](https://github.com/abougouffa/minemacs/commit/4b91dc0)) - [@abougouffa](https://github.com/abougouffa)
- **(docs)** add `pandoc-mode` - ([569f328](https://github.com/abougouffa/minemacs/commit/569f328)) - [@abougouffa](https://github.com/abougouffa)
- **(email)** add `org-mime` - ([bb7a610](https://github.com/abougouffa/minemacs/commit/bb7a610)) - [@abougouffa](https://github.com/abougouffa)
- **(ibuffer-project)** group buffers by projects in ibuffer - ([67c8d2d](https://github.com/abougouffa/minemacs/commit/67c8d2d)) - [@abougouffa](https://github.com/abougouffa)
- **(lisp)** additional Common Lisp packages - ([128cd50](https://github.com/abougouffa/minemacs/commit/128cd50)) - [@abougouffa](https://github.com/abougouffa)
- **(ox-pandoc)** initial support - ([c7c882f](https://github.com/abougouffa/minemacs/commit/c7c882f)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(keybindings)** remove extra space - ([f7d7969](https://github.com/abougouffa/minemacs/commit/f7d7969)) - [@abougouffa](https://github.com/abougouffa)
- **(mixed-pitch)** sort list elements - ([9d210bb](https://github.com/abougouffa/minemacs/commit/9d210bb)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** the `mu4e` command is already autoloaded - ([8fd5850](https://github.com/abougouffa/minemacs/commit/8fd5850)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** minor formatting - ([6d7ed85](https://github.com/abougouffa/minemacs/commit/6d7ed85)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(flymake)** small simplification - ([6fa0fe3](https://github.com/abougouffa/minemacs/commit/6fa0fe3)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** minor UI edits - ([117a9f4](https://github.com/abougouffa/minemacs/commit/117a9f4)) - [@abougouffa](https://github.com/abougouffa)
- **(netextender)** better error management - ([f1c2442](https://github.com/abougouffa/minemacs/commit/f1c2442)) - [@abougouffa](https://github.com/abougouffa)
- **(netextender)** better way to manage the custom command - ([852a7d8](https://github.com/abougouffa/minemacs/commit/852a7d8)) - [@abougouffa](https://github.com/abougouffa)
- move `+eglot-auto-enable` and `+lsp-auto-enable` - ([1017235](https://github.com/abougouffa/minemacs/commit/1017235)) - [@abougouffa](https://github.com/abougouffa)
- use `keymap[-global]-set` instead of `define-key` - ([1e36c9a](https://github.com/abougouffa/minemacs/commit/1e36c9a)) - [@abougouffa](https://github.com/abougouffa)
- use `use-package`'s `:hook` as much as possible - ([e31ca12](https://github.com/abougouffa/minemacs/commit/e31ca12)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(binary)** temporary disable auto `hexl-mode` (#67) - ([71309a2](https://github.com/abougouffa/minemacs/commit/71309a2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(auctex)** better defaults - ([a4152ef](https://github.com/abougouffa/minemacs/commit/a4152ef)) - [@abougouffa](https://github.com/abougouffa)
- **(binary)** better deferring - ([761337c](https://github.com/abougouffa/minemacs/commit/761337c)) - [@abougouffa](https://github.com/abougouffa)
- **(binary)** simplify condition - ([0b85122](https://github.com/abougouffa/minemacs/commit/0b85122)) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** better integration with `pcomplete` - ([0cad15f](https://github.com/abougouffa/minemacs/commit/0cad15f)) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** tweak the cape backends - ([7540b43](https://github.com/abougouffa/minemacs/commit/7540b43)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-eglot)** better check for `consult-lsp` - ([811b74b](https://github.com/abougouffa/minemacs/commit/811b74b)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** rename `+quoted` to `+quoted-p` - ([99834d5](https://github.com/abougouffa/minemacs/commit/99834d5)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `minemacs-run-build-functions` - ([04041bb](https://github.com/abougouffa/minemacs/commit/04041bb)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** save a list of packages configured by MinEmacs - ([bdd3898](https://github.com/abougouffa/minemacs/commit/bdd3898)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `minemacs-after-loading-modules-hook` - ([0e4c53d](https://github.com/abougouffa/minemacs/commit/0e4c53d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** centralize `minemacs-ignore-user-config` - ([fd4a585](https://github.com/abougouffa/minemacs/commit/fd4a585)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add new env vars to disable user config - ([4e229dd](https://github.com/abougouffa/minemacs/commit/4e229dd)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** better integration with `eshell` - ([174e3d8](https://github.com/abougouffa/minemacs/commit/174e3d8)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** update cpptools & codelldb default versions - ([b9f673c](https://github.com/abougouffa/minemacs/commit/b9f673c)) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** simplify condition - ([56fc4a2](https://github.com/abougouffa/minemacs/commit/56fc4a2)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** better TAB behavior, dired, scripts, ... - ([0ea4bd7](https://github.com/abougouffa/minemacs/commit/0ea4bd7)) - [@abougouffa](https://github.com/abougouffa)
- **(dired)** enable adding mail attachements from `dired` - ([6a3fd7d](https://github.com/abougouffa/minemacs/commit/6a3fd7d)) - [@abougouffa](https://github.com/abougouffa)
- **(doc-view)** enable continuous mode - ([9af3adc](https://github.com/abougouffa/minemacs/commit/9af3adc)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** enable word count - ([921ee7f](https://github.com/abougouffa/minemacs/commit/921ee7f)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** show time icon - ([19c78b5](https://github.com/abougouffa/minemacs/commit/19c78b5)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-themes)** apply org tweaks - ([3b4847d](https://github.com/abougouffa/minemacs/commit/3b4847d)) - [@abougouffa](https://github.com/abougouffa)
- **(ecryptfs)** make passphrase file customizable - ([f35fa77](https://github.com/abougouffa/minemacs/commit/f35fa77)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** add a helper function - ([56f430d](https://github.com/abougouffa/minemacs/commit/56f430d)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs)** ask for output file in `+screenshot-svg` - ([77a89a7](https://github.com/abougouffa/minemacs/commit/77a89a7)) - [@abougouffa](https://github.com/abougouffa)
- **(embark)** bind to `SPC a` instead of `SPC .` - ([3d7537c](https://github.com/abougouffa/minemacs/commit/3d7537c)) - [@abougouffa](https://github.com/abougouffa)
- **(embark-consult)** activate on `embark-collect` - ([3020308](https://github.com/abougouffa/minemacs/commit/3020308)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** minor edits - ([5aeb051](https://github.com/abougouffa/minemacs/commit/5aeb051)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** add hydra menu - ([4407230](https://github.com/abougouffa/minemacs/commit/4407230)) - [@abougouffa](https://github.com/abougouffa)
- **(forge)** remove obsolete var, add merge keybinding - ([63a235d](https://github.com/abougouffa/minemacs/commit/63a235d)) - [@abougouffa](https://github.com/abougouffa)
- **(hideif)** more intelligent integration - ([34d6a31](https://github.com/abougouffa/minemacs/commit/34d6a31)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** use `file-truename` in `+load` - ([01353db](https://github.com/abougouffa/minemacs/commit/01353db)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** add `pandoc` as backend for `+html2pdf` - ([74bf4c4](https://github.com/abougouffa/minemacs/commit/74bf4c4)) - [@abougouffa](https://github.com/abougouffa)
- **(keybinding)** bind `bury-buffer` - ([e71564a](https://github.com/abougouffa/minemacs/commit/e71564a)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** check for `latexmk` before activation - ([179ed3d](https://github.com/abougouffa/minemacs/commit/179ed3d)) - [@abougouffa](https://github.com/abougouffa)
- **(lisp)** limit geiser scheme implementations - ([b28be92](https://github.com/abougouffa/minemacs/commit/b28be92)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** make `eglot-ltex-language` local-safe - ([a0e2ed2](https://github.com/abougouffa/minemacs/commit/a0e2ed2)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-imerge)** add keybinding - ([f18e907](https://github.com/abougouffa/minemacs/commit/f18e907)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** open in a dedicated workspace/tab - ([5a04ae8](https://github.com/abougouffa/minemacs/commit/5a04ae8)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** add variable to control auto-start in daemon - ([936dc80](https://github.com/abougouffa/minemacs/commit/936dc80)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** auto save google accounts on registration - ([f099f67](https://github.com/abougouffa/minemacs/commit/f099f67)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** don't reply to self, copy the header instead - ([d52f58e](https://github.com/abougouffa/minemacs/commit/d52f58e)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e-alert)** use icon only if it exists - ([f531cda](https://github.com/abougouffa/minemacs/commit/f531cda)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** register `nerd-icons-install-fonts` - ([c83ef08](https://github.com/abougouffa/minemacs/commit/c83ef08)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** minor edit - ([bd108a4](https://github.com/abougouffa/minemacs/commit/bd108a4)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** set custom TODO keywords - ([62865a5](https://github.com/abougouffa/minemacs/commit/62865a5)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** restore `pcomplete` - ([20785e2](https://github.com/abougouffa/minemacs/commit/20785e2)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** print the right file name when exporting - ([e55d3d6](https://github.com/abougouffa/minemacs/commit/e55d3d6)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** disable annoying completion - ([c427db1](https://github.com/abougouffa/minemacs/commit/c427db1)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** better set latex classes and default packages (#69) - ([2bcfb17](https://github.com/abougouffa/minemacs/commit/2bcfb17)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** consider language when exporting to PDF (#69) - ([dc9517f](https://github.com/abougouffa/minemacs/commit/dc9517f)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add a way to disable lower case keywords - ([314f1eb](https://github.com/abougouffa/minemacs/commit/314f1eb)) - [@abougouffa](https://github.com/abougouffa)
- **(org-msg)** additional keybinding - ([a48150f](https://github.com/abougouffa/minemacs/commit/a48150f)) - [@abougouffa](https://github.com/abougouffa)
- **(org-roam)** bigger space for tags in completion - ([7a68af9](https://github.com/abougouffa/minemacs/commit/7a68af9)) - [@abougouffa](https://github.com/abougouffa)
- **(org-roam)** show tags in `vertico` + autosync - ([4033d75](https://github.com/abougouffa/minemacs/commit/4033d75)) - [@abougouffa](https://github.com/abougouffa)
- **(realgud)** use realgud:gdb for GDB supported languages - ([3a4be4f](https://github.com/abougouffa/minemacs/commit/3a4be4f)) - [@abougouffa](https://github.com/abougouffa)
- **(scheme)** use guile by default - ([4f01f23](https://github.com/abougouffa/minemacs/commit/4f01f23)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example in `early-config.el` - ([16b197d](https://github.com/abougouffa/minemacs/commit/16b197d)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add package disabling example - ([51375d8](https://github.com/abougouffa/minemacs/commit/51375d8)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update `org-roam` config example - ([1e9d244](https://github.com/abougouffa/minemacs/commit/1e9d244)) - [@abougouffa](https://github.com/abougouffa)
- **(spell-fu)** correctly check CamelCase words - ([4dc0a79](https://github.com/abougouffa/minemacs/commit/4dc0a79)) - [@abougouffa](https://github.com/abougouffa)
- **(spell-fu)** update macro name to follow the convention - ([867f362](https://github.com/abougouffa/minemacs/commit/867f362)) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** auto switch to scratch on create - ([7d1447a](https://github.com/abougouffa/minemacs/commit/7d1447a)) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** auto rename the first tab to default - ([f7b42c6](https://github.com/abougouffa/minemacs/commit/f7b42c6)) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** minor edit - ([ade261d](https://github.com/abougouffa/minemacs/commit/ade261d)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** minor tweak - ([ed4b7cf](https://github.com/abougouffa/minemacs/commit/ed4b7cf)) - [@abougouffa](https://github.com/abougouffa)
- **(tldr)** register `tldr-update-docs` as build fn - ([856e474](https://github.com/abougouffa/minemacs/commit/856e474)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1e999eb](https://github.com/abougouffa/minemacs/commit/1e999eb)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loadddefs - ([926c9c9](https://github.com/abougouffa/minemacs/commit/926c9c9)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7576d14](https://github.com/abougouffa/minemacs/commit/7576d14)) - [@abougouffa](https://github.com/abougouffa)
- defer `forge` & `code-review` - ([4afad72](https://github.com/abougouffa/minemacs/commit/4afad72)) - [@abougouffa](https://github.com/abougouffa)
- provide file names - ([4db86af](https://github.com/abougouffa/minemacs/commit/4db86af)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([875cd46](https://github.com/abougouffa/minemacs/commit/875cd46)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e5fa45f](https://github.com/abougouffa/minemacs/commit/e5fa45f)) - [@abougouffa](https://github.com/abougouffa)
- beautify hydra menus - ([d77704a](https://github.com/abougouffa/minemacs/commit/d77704a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([b55c182](https://github.com/abougouffa/minemacs/commit/b55c182)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([b29eda4](https://github.com/abougouffa/minemacs/commit/b29eda4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3cd0eac](https://github.com/abougouffa/minemacs/commit/3cd0eac)) - [@abougouffa](https://github.com/abougouffa)
- better use of `executable-find` - ([a976cab](https://github.com/abougouffa/minemacs/commit/a976cab)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([01ddbfc](https://github.com/abougouffa/minemacs/commit/01ddbfc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## v0.4.0 - 2023-05-27
#### Bug Fixes
- **(citar)** avoid using `all-the-icons` until it is loaded - ([960d978](https://github.com/abougouffa/minemacs/commit/960d978)) - [@abougouffa](https://github.com/abougouffa)
- **(code-review)** use fixed fork, unpin closql & forge - ([79423c5](https://github.com/abougouffa/minemacs/commit/79423c5)) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed)** correct key for +elfeed-download-image - ([4b69a74](https://github.com/abougouffa/minemacs/commit/4b69a74)) - [@DarkBuffalo](https://github.com/DarkBuffalo)
- **(flymake)** use custom icons only when suitable - ([1e84c48](https://github.com/abougouffa/minemacs/commit/1e84c48)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** use ltex-ls as TCP server, add helpers - ([fe290c2](https://github.com/abougouffa/minemacs/commit/fe290c2)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(backports)** update function documentation - ([c5ab9fe](https://github.com/abougouffa/minemacs/commit/c5ab9fe)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update comment - ([125d82a](https://github.com/abougouffa/minemacs/commit/125d82a)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** add header2 support - ([0017976](https://github.com/abougouffa/minemacs/commit/0017976)) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed)** add yt-dlp support - ([8b8e611](https://github.com/abougouffa/minemacs/commit/8b8e611)) - DarkBuffalo
#### Nitpicks, changes with no side effect
- **(defaults)** add an optional argument - ([a02e5cc](https://github.com/abougouffa/minemacs/commit/a02e5cc)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** minor edit - ([6fc983d](https://github.com/abougouffa/minemacs/commit/6fc983d)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-collection)** cleanup previous fix - ([21ddd4d](https://github.com/abougouffa/minemacs/commit/21ddd4d)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** code formatting - ([3827863](https://github.com/abougouffa/minemacs/commit/3827863)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(backports)** precise condition - ([22a09fa](https://github.com/abougouffa/minemacs/commit/22a09fa)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edit - ([a0861ff](https://github.com/abougouffa/minemacs/commit/a0861ff)) - [@abougouffa](https://github.com/abougouffa)
- **(ecryptfs)** partial rewrite - ([e1e4034](https://github.com/abougouffa/minemacs/commit/e1e4034)) - [@abougouffa](https://github.com/abougouffa)
- move pcache directory customization to me-defaults - ([f10b3cc](https://github.com/abougouffa/minemacs/commit/f10b3cc)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(ltex)** remove server commands - ([f02b0a7](https://github.com/abougouffa/minemacs/commit/f02b0a7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bootstrap)** call build functions interactively - ([63b92d4](https://github.com/abougouffa/minemacs/commit/63b92d4)) - [@abougouffa](https://github.com/abougouffa)
- **(bootstrap)** minor refactor - ([cefcdac](https://github.com/abougouffa/minemacs/commit/cefcdac)) - [@abougouffa](https://github.com/abougouffa)
- **(bootstrap)** do not ask when running build functions - ([3d390ea](https://github.com/abougouffa/minemacs/commit/3d390ea)) - [@abougouffa](https://github.com/abougouffa)
- **(bootstrap)** run build functions on update - ([940fdaa](https://github.com/abougouffa/minemacs/commit/940fdaa)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** add ts modes - ([e317bbf](https://github.com/abougouffa/minemacs/commit/e317bbf)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** move compilation options to `me-prog` - ([f949b46](https://github.com/abougouffa/minemacs/commit/f949b46)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** move package updating routines - ([5bbfe68](https://github.com/abougouffa/minemacs/commit/5bbfe68)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** update loaddefs - ([71eb596](https://github.com/abougouffa/minemacs/commit/71eb596)) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** add an option to disable it - ([ee30640](https://github.com/abougouffa/minemacs/commit/ee30640)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** set variables with `setopt` - ([75fbb43](https://github.com/abougouffa/minemacs/commit/75fbb43)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** better scrolling settings - ([f1b4639](https://github.com/abougouffa/minemacs/commit/f1b4639)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** more UI customization - ([d7e7f98](https://github.com/abougouffa/minemacs/commit/d7e7f98)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** move `password` and `auth-source` to `me-builtin` - ([dc3a0da](https://github.com/abougouffa/minemacs/commit/dc3a0da)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** correctly set `show-trailing-whitespace` - ([997f6b2](https://github.com/abougouffa/minemacs/commit/997f6b2)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** set additional paths - ([9e8c7b9](https://github.com/abougouffa/minemacs/commit/9e8c7b9)) - [@abougouffa](https://github.com/abougouffa)
- **(eaf)** disable pdf-viewer - ([c35356b](https://github.com/abougouffa/minemacs/commit/c35356b)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** prefer loading newer Elisp files - ([9bdf851](https://github.com/abougouffa/minemacs/commit/9bdf851)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** do not assume servers to be executable - ([51dd7b8](https://github.com/abougouffa/minemacs/commit/51dd7b8)) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed)** Enhance customizability - ([d2f124a](https://github.com/abougouffa/minemacs/commit/d2f124a)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-numbers)** enable for `conf-mode`, tweak regexp - ([849dba3](https://github.com/abougouffa/minemacs/commit/849dba3)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** use `lisp-interaction-mode` for scratch - ([5f755e4](https://github.com/abougouffa/minemacs/commit/5f755e4)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** code refactoring - ([c5ef698](https://github.com/abougouffa/minemacs/commit/c5ef698)) - [@abougouffa](https://github.com/abougouffa)
- **(lexic)** fix local keybindigs - ([ab56fb4](https://github.com/abougouffa/minemacs/commit/ab56fb4)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** add `tex-mode` - ([0e344a8](https://github.com/abougouffa/minemacs/commit/0e344a8)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** minor edits, add documentation - ([8e0421c](https://github.com/abougouffa/minemacs/commit/8e0421c)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** use Eglot's server/port syntax - ([faec1c5](https://github.com/abougouffa/minemacs/commit/faec1c5)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex-ls)** additional languages - ([ea19fa6](https://github.com/abougouffa/minemacs/commit/ea19fa6)) - [@abougouffa](https://github.com/abougouffa)
- **(macrostep)** remove unneeded hack (fixed upstream) - ([0f0b5ef](https://github.com/abougouffa/minemacs/commit/0f0b5ef)) - [@abougouffa](https://github.com/abougouffa)
- **(macrostep)** use upstream repo, apply a hack - ([c048d3c](https://github.com/abougouffa/minemacs/commit/c048d3c)) - [@abougouffa](https://github.com/abougouffa)
- **(media)** automatically open Youtube links in MPV - ([1fcc323](https://github.com/abougouffa/minemacs/commit/1fcc323)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** use `nerd-icons` - ([5407ab5](https://github.com/abougouffa/minemacs/commit/5407ab5)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** set an icon for matlab/octave files - ([972d3a4](https://github.com/abougouffa/minemacs/commit/972d3a4)) - [@abougouffa](https://github.com/abougouffa)
- **(realgud)** add local binding for treesit modes - ([51d8078](https://github.com/abougouffa/minemacs/commit/51d8078)) - [@abougouffa](https://github.com/abougouffa)
- **(realgud)** minor tweaks and fixes - ([8365877](https://github.com/abougouffa/minemacs/commit/8365877)) - [@abougouffa](https://github.com/abougouffa)
- **(realgud)** define commands - ([920ab8d](https://github.com/abougouffa/minemacs/commit/920ab8d)) - [@abougouffa](https://github.com/abougouffa)
- **(realgud-lldb)** remove unneeded autoload - ([689a1da](https://github.com/abougouffa/minemacs/commit/689a1da)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update the skeleton's modules list - ([1e86106](https://github.com/abougouffa/minemacs/commit/1e86106)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** bind TAB and S-TAB to next/previous - ([dff6996](https://github.com/abougouffa/minemacs/commit/dff6996)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** remove unused themes - ([d8f4e3b](https://github.com/abougouffa/minemacs/commit/d8f4e3b)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `nerd-icons` explicitly - ([33e3ffe](https://github.com/abougouffa/minemacs/commit/33e3ffe)) - [@abougouffa](https://github.com/abougouffa)
- **(xclip)** remove useless `+xclip--enable-in-tty-h` - ([0481b8e](https://github.com/abougouffa/minemacs/commit/0481b8e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([69e9235](https://github.com/abougouffa/minemacs/commit/69e9235)) - [@abougouffa](https://github.com/abougouffa)
- use `nerd-icons` instead of `all-the-icons` - ([403a3a1](https://github.com/abougouffa/minemacs/commit/403a3a1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6525e40](https://github.com/abougouffa/minemacs/commit/6525e40)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8b90843](https://github.com/abougouffa/minemacs/commit/8b90843)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([272bb17](https://github.com/abougouffa/minemacs/commit/272bb17)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bffed94](https://github.com/abougouffa/minemacs/commit/bffed94)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([53caf4d](https://github.com/abougouffa/minemacs/commit/53caf4d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c89d5cc](https://github.com/abougouffa/minemacs/commit/c89d5cc)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([693efa0](https://github.com/abougouffa/minemacs/commit/693efa0)) - [@abougouffa](https://github.com/abougouffa)
- register package-specific build functions - ([ef635c4](https://github.com/abougouffa/minemacs/commit/ef635c4)) - [@abougouffa](https://github.com/abougouffa)

- - -

## v0.3.0 - 2023-05-03
#### Bug Fixes
- **(aphelia)** adapt to the new upstream changes - ([06f7776](https://github.com/abougouffa/minemacs/commit/06f7776)) - [@abougouffa](https://github.com/abougouffa)
- **(backports)** fix a bug causing straight to fail (#51) - ([ded8596](https://github.com/abougouffa/minemacs/commit/ded8596)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** avoid running `mu4e` repeatedly - ([c784d05](https://github.com/abougouffa/minemacs/commit/c784d05)) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** do not show when Emacs started with a file - ([ebe1a9a](https://github.com/abougouffa/minemacs/commit/ebe1a9a)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** fix +html2pdf output file name - ([a8d4435](https://github.com/abougouffa/minemacs/commit/a8d4435)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** add safety guards to avoid conflict with evil - ([db20803](https://github.com/abougouffa/minemacs/commit/db20803)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** clone the full packages repos - ([4f51035](https://github.com/abougouffa/minemacs/commit/4f51035)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** use master on Emacs 28, develop on newer versions - ([7f7f33d](https://github.com/abougouffa/minemacs/commit/7f7f33d)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** wrong parenthesis disabling corfu in other modes - ([1ad3b33](https://github.com/abougouffa/minemacs/commit/1ad3b33)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** `:pin-ref` problem on Emacs28 (#49) - ([e37b984](https://github.com/abougouffa/minemacs/commit/e37b984)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** pin problematic packages to working versions - ([424fd54](https://github.com/abougouffa/minemacs/commit/424fd54)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** fix broken link - ([87247a5](https://github.com/abougouffa/minemacs/commit/87247a5)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `minemacs-update` command - ([5692b8d](https://github.com/abougouffa/minemacs/commit/5692b8d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add the `+hook-once!` macro - ([1eaa535](https://github.com/abougouffa/minemacs/commit/1eaa535)) - [@abougouffa](https://github.com/abougouffa)
- **(docs)** initial support for `poly-markdown` - ([2460e18](https://github.com/abougouffa/minemacs/commit/2460e18)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** add a helper to save URLs to HTML snapshots - ([24e03db](https://github.com/abougouffa/minemacs/commit/24e03db)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** WIP optional evil replacement - ([954a549](https://github.com/abougouffa/minemacs/commit/954a549)) - [@abougouffa](https://github.com/abougouffa)
- **(modeling)** add `medelica-mode` - ([86e8c09](https://github.com/abougouffa/minemacs/commit/86e8c09)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `hy-mode` - ([031b5ba](https://github.com/abougouffa/minemacs/commit/031b5ba)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add all-the-icons for ibuffer - ([40d8bb8](https://github.com/abougouffa/minemacs/commit/40d8bb8)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** add the `:pin-ref` keyword to use `straight-x` - ([cc4f11b](https://github.com/abougouffa/minemacs/commit/cc4f11b)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(git)** track straight's default pins - ([52390b7](https://github.com/abougouffa/minemacs/commit/52390b7)) - [@abougouffa](https://github.com/abougouffa)
- provide `make update` - ([4d6af7d](https://github.com/abougouffa/minemacs/commit/4d6af7d)) - [@abougouffa](https://github.com/abougouffa)
- save straight's versions when cleaning - ([8795bbf](https://github.com/abougouffa/minemacs/commit/8795bbf)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(biblio)** minor edit - ([cc742e7](https://github.com/abougouffa/minemacs/commit/cc742e7)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simplify a condition - ([4a82a91](https://github.com/abougouffa/minemacs/commit/4a82a91)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** rename a parameter - ([4dd3285](https://github.com/abougouffa/minemacs/commit/4dd3285)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** remove extra spaces - ([b8d9d91](https://github.com/abougouffa/minemacs/commit/b8d9d91)) - [@abougouffa](https://github.com/abougouffa)
- add files headers and footers - ([298543e](https://github.com/abougouffa/minemacs/commit/298543e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(mu4e)** remove commented code - ([a766768](https://github.com/abougouffa/minemacs/commit/a766768)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** small cleanup - ([0c3dc30](https://github.com/abougouffa/minemacs/commit/0c3dc30)) - [@abougouffa](https://github.com/abougouffa)
- make use of `+hook-once!` - ([43b12bb](https://github.com/abougouffa/minemacs/commit/43b12bb)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(corfu)** restore in `(org/markdown)-mode` - ([be458ac](https://github.com/abougouffa/minemacs/commit/be458ac)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bootstrap)** revert to straight's develop branch - ([797e115](https://github.com/abougouffa/minemacs/commit/797e115)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** make sure `+emacs-features-p` returns a boolean - ([2c194de](https://github.com/abougouffa/minemacs/commit/2c194de)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** accept hook symbol in `+hook-once!` - ([4a08072](https://github.com/abougouffa/minemacs/commit/4a08072)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fallback to a builtin theme if `minemacs-theme` fails - ([48b0b62](https://github.com/abougouffa/minemacs/commit/48b0b62)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** disable in `org-mode` and `markdown-mode` - ([9360b69](https://github.com/abougouffa/minemacs/commit/9360b69)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** ensure keeping `mu4e` alive in background - ([2358e3d](https://github.com/abougouffa/minemacs/commit/2358e3d)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** set straight branch to develop - ([d9688e1](https://github.com/abougouffa/minemacs/commit/d9688e1)) - [@abougouffa](https://github.com/abougouffa)
- **(ecryptfs)** better support - ([7034976](https://github.com/abougouffa/minemacs/commit/7034976)) - [@abougouffa](https://github.com/abougouffa)
- **(eldoc-box)** remove special case, fixed upstream - ([f38adf1](https://github.com/abougouffa/minemacs/commit/f38adf1)) - [@abougouffa](https://github.com/abougouffa)
- **(email)** refine attachement detection regexp - ([7b72d76](https://github.com/abougouffa/minemacs/commit/7b72d76)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-collection)** restore `mu4e` - ([3c60a72](https://github.com/abougouffa/minemacs/commit/3c60a72)) - [@abougouffa](https://github.com/abougouffa)
- **(gts-translate)** add an option to choose translation langs - ([8e4d74f](https://github.com/abougouffa/minemacs/commit/8e4d74f)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** enable `hs-minor-mode` - ([29d22a4](https://github.com/abougouffa/minemacs/commit/29d22a4)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** better fontification - ([9d31c01](https://github.com/abougouffa/minemacs/commit/9d31c01)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** additional tweaks - ([ad5adae](https://github.com/abougouffa/minemacs/commit/ad5adae)) - [@abougouffa](https://github.com/abougouffa)
- **(logview)** set custom files paths - ([f817a8f](https://github.com/abougouffa/minemacs/commit/f817a8f)) - [@abougouffa](https://github.com/abougouffa)
- **(maxima)** use locally installed packages - ([024a05e](https://github.com/abougouffa/minemacs/commit/024a05e)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** don't ask for the alias when there is only one - ([6bda132](https://github.com/abougouffa/minemacs/commit/6bda132)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** minor edits - ([94a8c1f](https://github.com/abougouffa/minemacs/commit/94a8c1f)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** better org-roam protocol handling - ([9a9748c](https://github.com/abougouffa/minemacs/commit/9a9748c)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** prefer using `latexmk` or `tectonic` when found - ([4e1267d](https://github.com/abougouffa/minemacs/commit/4e1267d)) - [@abougouffa](https://github.com/abougouffa)
- **(pcache)** create the cache in the cache directory - ([28f2c04](https://github.com/abougouffa/minemacs/commit/28f2c04)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** update packages - ([1dd3044](https://github.com/abougouffa/minemacs/commit/1dd3044)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** add pin file - ([03e13d2](https://github.com/abougouffa/minemacs/commit/03e13d2)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** restrict `org/markdown` to tempel capf - ([439b6aa](https://github.com/abougouffa/minemacs/commit/439b6aa)) - [@abougouffa](https://github.com/abougouffa)
- **(treemacs)** enable `evil` support - ([2176941](https://github.com/abougouffa/minemacs/commit/2176941)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6a94af1](https://github.com/abougouffa/minemacs/commit/6a94af1)) - [@abougouffa](https://github.com/abougouffa)
- bump package versions - ([8372b89](https://github.com/abougouffa/minemacs/commit/8372b89)) - [@abougouffa](https://github.com/abougouffa)
- bump package versions - ([0f6a2fd](https://github.com/abougouffa/minemacs/commit/0f6a2fd)) - [@abougouffa](https://github.com/abougouffa)
- update loaddefs - ([09a9cea](https://github.com/abougouffa/minemacs/commit/09a9cea)) - [@abougouffa](https://github.com/abougouffa)

- - -

## v0.2.0 - 2023-04-01
#### Bug Fixes
- **(auctex)** require `tex` - ([104a41e](https://github.com/abougouffa/minemacs/commit/104a41e)) - [@abougouffa](https://github.com/abougouffa)
- **(backports)** add `scratch-buffer` (#41) - ([f01f80b](https://github.com/abougouffa/minemacs/commit/f01f80b)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** remove accidentally added quote - ([69bd4b3](https://github.com/abougouffa/minemacs/commit/69bd4b3)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** no initial fill when in `minibuffer` #37 - ([5c30bcd](https://github.com/abougouffa/minemacs/commit/5c30bcd)) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** load after `evil-collection` (#42) - ([d3b0976](https://github.com/abougouffa/minemacs/commit/d3b0976)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** setup default hooks early - ([720da8c](https://github.com/abougouffa/minemacs/commit/720da8c)) - [@abougouffa](https://github.com/abougouffa)
- **(docker)** better handling of `Dockerfile`s - ([69544e2](https://github.com/abougouffa/minemacs/commit/69544e2)) - [@abougouffa](https://github.com/abougouffa)
- **(ebnf-mode)** fix a typo - ([f63014f](https://github.com/abougouffa/minemacs/commit/f63014f)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** temporary disable `unicode-fonts` - ([a116b7b](https://github.com/abougouffa/minemacs/commit/a116b7b)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot-box)** better integration with `tab-bar` and `tool-bar` - ([f43d7ff](https://github.com/abougouffa/minemacs/commit/f43d7ff)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** temporary disable `evil-escape` - ([22f9a6d](https://github.com/abougouffa/minemacs/commit/22f9a6d)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** the right way to use `evil-search` - ([a5c61ab](https://github.com/abougouffa/minemacs/commit/a5c61ab)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-mc)** avoid inserting the first `evil-escape` char - ([99559a8](https://github.com/abougouffa/minemacs/commit/99559a8)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** fixes related to evil-collection - ([56533ad](https://github.com/abougouffa/minemacs/commit/56533ad)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** mu 1.10 UI and evil fixes - ([2398bd0](https://github.com/abougouffa/minemacs/commit/2398bd0)) - [@abougouffa](https://github.com/abougouffa)
- **(pdf-tools)** make sure to use it to show PDFs - ([d8bc950](https://github.com/abougouffa/minemacs/commit/d8bc950)) - [@abougouffa](https://github.com/abougouffa)
- disable packages causing problems on the last build - ([6b2c3ae](https://github.com/abougouffa/minemacs/commit/6b2c3ae)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** more use cases of `early-config.el` - ([b6c2104](https://github.com/abougouffa/minemacs/commit/b6c2104)) - [@abougouffa](https://github.com/abougouffa)
- add a header image in README - ([2130b3c](https://github.com/abougouffa/minemacs/commit/2130b3c)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(dashboard)** add dashboard - ([f0d5a10](https://github.com/abougouffa/minemacs/commit/f0d5a10)) - [@abougouffa](https://github.com/abougouffa)
- **(doc-view)** use SVG when available - ([b9a7715](https://github.com/abougouffa/minemacs/commit/b9a7715)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-multiedit)** initial support - ([535f2ba](https://github.com/abougouffa/minemacs/commit/535f2ba)) - [@abougouffa](https://github.com/abougouffa)
- **(lisp)** add support for Clojure (via cider) - ([7546f08](https://github.com/abougouffa/minemacs/commit/7546f08)) - [@abougouffa](https://github.com/abougouffa)
- **(lisp)** add more geiser backends - ([2f95ff1](https://github.com/abougouffa/minemacs/commit/2f95ff1)) - [@abougouffa](https://github.com/abougouffa)
- **(lsp-bridge)** initial support (WIP) - ([5820016](https://github.com/abougouffa/minemacs/commit/5820016)) - [@abougouffa](https://github.com/abougouffa)
- **(mermaid)** initial support - ([5718db8](https://github.com/abougouffa/minemacs/commit/5718db8)) - [@abougouffa](https://github.com/abougouffa)
- add the missing template for `+html2pdf` - ([c3c6fd2](https://github.com/abougouffa/minemacs/commit/c3c6fd2)) - [@abougouffa](https://github.com/abougouffa)
- add more backends to `+html2pdf` - ([8b025d0](https://github.com/abougouffa/minemacs/commit/8b025d0)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(changlog)** remove - ([8d54ac2](https://github.com/abougouffa/minemacs/commit/8d54ac2)) - [@abougouffa](https://github.com/abougouffa)
- move templates to assets - ([4042455](https://github.com/abougouffa/minemacs/commit/4042455)) - [@abougouffa](https://github.com/abougouffa)
- move pictures to `assets` - ([2f9809a](https://github.com/abougouffa/minemacs/commit/2f9809a)) - [@abougouffa](https://github.com/abougouffa)
- add names for the workflows - ([1d41152](https://github.com/abougouffa/minemacs/commit/1d41152)) - [@abougouffa](https://github.com/abougouffa)
- add `clean_pcache` target in Makefile - ([a7e02d1](https://github.com/abougouffa/minemacs/commit/a7e02d1)) - [@abougouffa](https://github.com/abougouffa)
- add the "v" prefix in cocogitto - ([0a70fbc](https://github.com/abougouffa/minemacs/commit/0a70fbc)) - [@abougouffa](https://github.com/abougouffa)
- bump cocogitto version to 3.4 - ([a1c5ab7](https://github.com/abougouffa/minemacs/commit/a1c5ab7)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(elisp)** minor edits - ([bf3a4e0](https://github.com/abougouffa/minemacs/commit/bf3a4e0)) - [@abougouffa](https://github.com/abougouffa)
- **(macrostep)** update recipe - ([9054280](https://github.com/abougouffa/minemacs/commit/9054280)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** format recipes - ([284c970](https://github.com/abougouffa/minemacs/commit/284c970)) - [@abougouffa](https://github.com/abougouffa)
- **(pdf-tools)** simplify - ([427c0c1](https://github.com/abougouffa/minemacs/commit/427c0c1)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** remove unneeded package - ([6baf7b0](https://github.com/abougouffa/minemacs/commit/6baf7b0)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([81dc6a6](https://github.com/abougouffa/minemacs/commit/81dc6a6)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(robot)** make ROS commands customizable - ([c041258](https://github.com/abougouffa/minemacs/commit/c041258)) - [@abougouffa](https://github.com/abougouffa)
- **(writeroom-mode)** hook via `use-package` - ([7c0832e](https://github.com/abougouffa/minemacs/commit/7c0832e)) - [@abougouffa](https://github.com/abougouffa)
- move `transient` to `me-builtin` - ([0761b3d](https://github.com/abougouffa/minemacs/commit/0761b3d)) - [@abougouffa](https://github.com/abougouffa)
- define MinEmacs sub-groups - ([a8f563c](https://github.com/abougouffa/minemacs/commit/a8f563c)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- restore disabled packages, using emacs@6bf441ff11540 - ([6e00f68](https://github.com/abougouffa/minemacs/commit/6e00f68)) - [@abougouffa](https://github.com/abougouffa)
- replace `writeroom-mode` with simpler config - ([4c2255d](https://github.com/abougouffa/minemacs/commit/4c2255d)) - [@abougouffa](https://github.com/abougouffa)
- replace `yasnippet` with `tempel` - ([b1edd7e](https://github.com/abougouffa/minemacs/commit/b1edd7e)) - [@abougouffa](https://github.com/abougouffa)
- remove `lsp-bridge` - ([34ce221](https://github.com/abougouffa/minemacs/commit/34ce221)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(+writing-mode)** increase text scale - ([ddfb23a](https://github.com/abougouffa/minemacs/commit/ddfb23a)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** pin `map` and `let-alist` - ([6eff30b](https://github.com/abougouffa/minemacs/commit/6eff30b)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** enable ANSI colors, restore savehist integration - ([c701113](https://github.com/abougouffa/minemacs/commit/c701113)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** add a message on toggle burying buffer - ([6c7c28d](https://github.com/abougouffa/minemacs/commit/6c7c28d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove obsolete keybinding aliases - ([48c53de](https://github.com/abougouffa/minemacs/commit/48c53de)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** define a group for MinEmacs' custom variables - ([56c37a8](https://github.com/abougouffa/minemacs/commit/56c37a8)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** set `custom-theme-directory` - ([fa621b6](https://github.com/abougouffa/minemacs/commit/fa621b6)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** enable drag and drop of regions - ([fb1d75d](https://github.com/abougouffa/minemacs/commit/fb1d75d)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** do not use system tooltips - ([3adf802](https://github.com/abougouffa/minemacs/commit/3adf802)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** update the main modeline layout - ([f8c367e](https://github.com/abougouffa/minemacs/commit/f8c367e)) - [@abougouffa](https://github.com/abougouffa)
- **(eaf)** minor edits, start in emacs state - ([d818ab6](https://github.com/abougouffa/minemacs/commit/d818ab6)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** disable annoying reporting in echo area - ([23af99e](https://github.com/abougouffa/minemacs/commit/23af99e)) - [@abougouffa](https://github.com/abougouffa)
- **(ein)** load org-babel the right way - ([2a68535](https://github.com/abougouffa/minemacs/commit/2a68535)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** simplify the `file-name-handler-alist` hack - ([9dd1345](https://github.com/abougouffa/minemacs/commit/9dd1345)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** edit keybindings - ([f6fbe96](https://github.com/abougouffa/minemacs/commit/f6fbe96)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** disable the new `mu4e-modeline-mode` - ([e7af964](https://github.com/abougouffa/minemacs/commit/e7af964)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** small UI tweak - ([b3588e9](https://github.com/abougouffa/minemacs/commit/b3588e9)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e-alert)** better filtering of spams - ([0c54543](https://github.com/abougouffa/minemacs/commit/0c54543)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** update keybindings - ([7675c7a](https://github.com/abougouffa/minemacs/commit/7675c7a)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** dynamically set latex fragments scale - ([26f1fd9](https://github.com/abougouffa/minemacs/commit/26f1fd9)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** add repo hash to the build directory - ([4ad4c3a](https://github.com/abougouffa/minemacs/commit/4ad4c3a)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** decrease default fonts size - ([ba61d68](https://github.com/abougouffa/minemacs/commit/ba61d68)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** wider window for `lexic` - ([6d2550c](https://github.com/abougouffa/minemacs/commit/6d2550c)) - [@abougouffa](https://github.com/abougouffa)
- **(writeroom)** minor edits - ([a1bc1a8](https://github.com/abougouffa/minemacs/commit/a1bc1a8)) - [@abougouffa](https://github.com/abougouffa)
- make `yasnippet` conf obsolete - ([e8025e9](https://github.com/abougouffa/minemacs/commit/e8025e9)) - [@abougouffa](https://github.com/abougouffa)

- - -- - -

## v0.1.0 - 2023-03-21

- Initial release

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).
