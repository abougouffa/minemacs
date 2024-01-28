# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

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
