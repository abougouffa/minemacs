# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

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
- **(keybinding)** fix a typo, bind new command - ([c7f1fe3](https://github.com/abougouffa/minemacs/commit/c7f1fe3cd1f9ea1bfda0c87c2a946ae85dad259a)) - donneyluck
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
- **(default)** hide `tab-bar` tabs - (023c0f4) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** make use of `+add-hook!` - (d731c77) - [@abougouffa](https://github.com/abougouffa)

- - -

## v1.0.0 - 2023-06-12
#### Bug Fixes
- **(binary)** fix `objdump-disassemble-mode` - (3f77e8d) - [@abougouffa](https://github.com/abougouffa)
- **(binary)** better management of objdump - (41e20ec) - [@abougouffa](https://github.com/abougouffa)
- **(binary)** temporary disable auto `hexl-mode` (#67) - (ad08679) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** hook capfs the right way - (31c733d) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** check if `mu4e` is available - (0950451) - [@abougouffa](https://github.com/abougouffa)
- **(eaf)** remove undefined function - (21548b7) - [@abougouffa](https://github.com/abougouffa)
- **(epa-file)** ensure enabling `epa-file` (#67) - (568bb5d) - [@abougouffa](https://github.com/abougouffa)
- **(keybinding)** remove duplicate binding for workspace - (d391634) - [@abougouffa](https://github.com/abougouffa)
- **(media)** problematic executable check (#65) - (08cf1ef) - [@abougouffa](https://github.com/abougouffa)
- **(org-roam)** autosync - (d62475b) - donneyluck
- **(org-roam)** autosync database (#68) - (bd4cd61) - [@abougouffa](https://github.com/abougouffa)
- **(org-roam-ui)** use another keybinding (#68) - (5ba3042) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** do not overwrite Capf - (d7ba8b7) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(mu4e-alert)** function documentation en comments - (e9004e0) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example - (f1bc80b) - [@abougouffa](https://github.com/abougouffa)
- update README - (0dbf11c) - [@abougouffa](https://github.com/abougouffa)
- tiny fix in README - (c292c96) - [@abougouffa](https://github.com/abougouffa)
- update README - (392fdd8) - [@abougouffa](https://github.com/abougouffa)
- update README to include the new variable - (444f2c5) - [@abougouffa](https://github.com/abougouffa)
- include the new environment vars in README - (2077726) - [@abougouffa](https://github.com/abougouffa)
- minor updates - (868bb15) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(code-cells)** initial support - (64b041f) - [@abougouffa](https://github.com/abougouffa)
- **(combobulate)** initial support - (fea6426) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+setq-hook!` & `+unsetq-hook!` from Doom - (5cdab26) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+add-hook!` & `+remove-hook!` from Doom - (61bb207) - [@abougouffa](https://github.com/abougouffa)
- **(core)** disable individual modules packages - (4b91dc0) - [@abougouffa](https://github.com/abougouffa)
- **(docs)** add `pandoc-mode` - (569f328) - [@abougouffa](https://github.com/abougouffa)
- **(email)** add `org-mime` - (bb7a610) - [@abougouffa](https://github.com/abougouffa)
- **(ibuffer-project)** group buffers by projects in ibuffer - (67c8d2d) - [@abougouffa](https://github.com/abougouffa)
- **(lisp)** additional Common Lisp packages - (128cd50) - [@abougouffa](https://github.com/abougouffa)
- **(ox-pandoc)** initial support - (c7c882f) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(keybindings)** remove extra space - (f7d7969) - [@abougouffa](https://github.com/abougouffa)
- **(mixed-pitch)** sort list elements - (9d210bb) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** the `mu4e` command is already autoloaded - (8fd5850) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** minor formatting - (6d7ed85) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(flymake)** small simplification - (6fa0fe3) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** minor UI edits - (117a9f4) - [@abougouffa](https://github.com/abougouffa)
- **(netextender)** better error management - (f1c2442) - [@abougouffa](https://github.com/abougouffa)
- **(netextender)** better way to manage the custom command - (852a7d8) - [@abougouffa](https://github.com/abougouffa)
- move `+eglot-auto-enable` and `+lsp-auto-enable` - (1017235) - [@abougouffa](https://github.com/abougouffa)
- use `keymap[-global]-set` instead of `define-key` - (1e36c9a) - [@abougouffa](https://github.com/abougouffa)
- use `use-package`'s `:hook` as much as possible - (e31ca12) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(binary)** temporary disable auto `hexl-mode` (#67) - (71309a2) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(auctex)** better defaults - (a4152ef) - [@abougouffa](https://github.com/abougouffa)
- **(binary)** better deferring - (761337c) - [@abougouffa](https://github.com/abougouffa)
- **(binary)** simplify condition - (0b85122) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** better integration with `pcomplete` - (0cad15f) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** tweak the cape backends - (7540b43) - [@abougouffa](https://github.com/abougouffa)
- **(consult-eglot)** better check for `consult-lsp` - (811b74b) - [@abougouffa](https://github.com/abougouffa)
- **(core)** rename `+quoted` to `+quoted-p` - (99834d5) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `minemacs-run-build-functions` - (04041bb) - [@abougouffa](https://github.com/abougouffa)
- **(core)** save a list of packages configured by MinEmacs - (bdd3898) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `minemacs-after-loading-modules-hook` - (0e4c53d) - [@abougouffa](https://github.com/abougouffa)
- **(core)** centralize `minemacs-ignore-user-config` - (fd4a585) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add new env vars to disable user config - (4e229dd) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** better integration with `eshell` - (174e3d8) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** update cpptools & codelldb default versions - (b9f673c) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** simplify condition - (56fc4a2) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** better TAB behavior, dired, scripts, ... - (0ea4bd7) - [@abougouffa](https://github.com/abougouffa)
- **(dired)** enable adding mail attachements from `dired` - (6a3fd7d) - [@abougouffa](https://github.com/abougouffa)
- **(doc-view)** enable continuous mode - (9af3adc) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** enable word count - (921ee7f) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** show time icon - (19c78b5) - [@abougouffa](https://github.com/abougouffa)
- **(doom-themes)** apply org tweaks - (3b4847d) - [@abougouffa](https://github.com/abougouffa)
- **(ecryptfs)** make passphrase file customizable - (f35fa77) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** add a helper function - (56f430d) - [@abougouffa](https://github.com/abougouffa)
- **(emacs)** ask for output file in `+screenshot-svg` - (77a89a7) - [@abougouffa](https://github.com/abougouffa)
- **(embark)** bind to `SPC a` instead of `SPC .` - (3d7537c) - [@abougouffa](https://github.com/abougouffa)
- **(embark-consult)** activate on `embark-collect` - (3020308) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** minor edits - (5aeb051) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** add hydra menu - (4407230) - [@abougouffa](https://github.com/abougouffa)
- **(forge)** remove obsolete var, add merge keybinding - (63a235d) - [@abougouffa](https://github.com/abougouffa)
- **(hideif)** more intelligent integration - (34d6a31) - [@abougouffa](https://github.com/abougouffa)
- **(init)** use `file-truename` in `+load` - (01353db) - [@abougouffa](https://github.com/abougouffa)
- **(io)** add `pandoc` as backend for `+html2pdf` - (74bf4c4) - [@abougouffa](https://github.com/abougouffa)
- **(keybinding)** bind `bury-buffer` - (e71564a) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** check for `latexmk` before activation - (179ed3d) - [@abougouffa](https://github.com/abougouffa)
- **(lisp)** limit geiser scheme implementations - (b28be92) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** make `eglot-ltex-language` local-safe - (a0e2ed2) - [@abougouffa](https://github.com/abougouffa)
- **(magit-imerge)** add keybinding - (f18e907) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** open in a dedicated workspace/tab - (5a04ae8) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** add variable to control auto-start in daemon - (936dc80) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** auto save google accounts on registration - (f099f67) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** don't reply to self, copy the header instead - (d52f58e) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e-alert)** use icon only if it exists - (f531cda) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** register `nerd-icons-install-fonts` - (c83ef08) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** minor edit - (bd108a4) - [@abougouffa](https://github.com/abougouffa)
- **(org)** set custom TODO keywords - (62865a5) - [@abougouffa](https://github.com/abougouffa)
- **(org)** restore `pcomplete` - (20785e2) - [@abougouffa](https://github.com/abougouffa)
- **(org)** print the right file name when exporting - (e55d3d6) - [@abougouffa](https://github.com/abougouffa)
- **(org)** disable annoying completion - (c427db1) - [@abougouffa](https://github.com/abougouffa)
- **(org)** better set latex classes and default packages (#69) - (2bcfb17) - [@abougouffa](https://github.com/abougouffa)
- **(org)** consider language when exporting to PDF (#69) - (dc9517f) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add a way to disable lower case keywords - (314f1eb) - [@abougouffa](https://github.com/abougouffa)
- **(org-msg)** additional keybinding - (a48150f) - [@abougouffa](https://github.com/abougouffa)
- **(org-roam)** bigger space for tags in completion - (7a68af9) - [@abougouffa](https://github.com/abougouffa)
- **(org-roam)** show tags in `vertico` + autosync - (4033d75) - [@abougouffa](https://github.com/abougouffa)
- **(realgud)** use realgud:gdb for GDB supported languages - (3a4be4f) - [@abougouffa](https://github.com/abougouffa)
- **(scheme)** use guile by default - (4f01f23) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example in `early-config.el` - (16b197d) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add package disabling example - (51375d8) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update `org-roam` config example - (1e9d244) - [@abougouffa](https://github.com/abougouffa)
- **(spell-fu)** correctly check CamelCase words - (4dc0a79) - [@abougouffa](https://github.com/abougouffa)
- **(spell-fu)** update macro name to follow the convention - (867f362) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** auto switch to scratch on create - (7d1447a) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** auto rename the first tab to default - (f7b42c6) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** minor edit - (ade261d) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** minor tweak - (ed4b7cf) - [@abougouffa](https://github.com/abougouffa)
- **(tldr)** register `tldr-update-docs` as build fn - (856e474) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (1e999eb) - [@abougouffa](https://github.com/abougouffa)
- regenerate loadddefs - (926c9c9) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (7576d14) - [@abougouffa](https://github.com/abougouffa)
- defer `forge` & `code-review` - (4afad72) - [@abougouffa](https://github.com/abougouffa)
- provide file names - (4db86af) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (875cd46) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (e5fa45f) - [@abougouffa](https://github.com/abougouffa)
- beautify hydra menus - (d77704a) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (b55c182) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (b29eda4) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (3cd0eac) - [@abougouffa](https://github.com/abougouffa)
- better use of `executable-find` - (a976cab) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (01ddbfc) - [@abougouffa](https://github.com/abougouffa)

- - -

## v0.4.0 - 2023-05-27
#### Bug Fixes
- **(citar)** avoid using `all-the-icons` until it is loaded - (960d978) - [@abougouffa](https://github.com/abougouffa)
- **(code-review)** use fixed fork, unpin closql & forge - (79423c5) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed)** correct key for +elfeed-download-image - (4b69a74) - DarkBuffalo
- **(flymake)** use custom icons only when suitable - (1e84c48) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** use ltex-ls as TCP server, add helpers - (fe290c2) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(backports)** update function documentation - (c5ab9fe) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update comment - (125d82a) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** add header2 support - (0017976) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed)** add yt-dlp support - (8b8e611) - DarkBuffalo
#### Nitpicks, changes with no side effect
- **(defaults)** add an optional argument - (a02e5cc) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** minor edit - (6fc983d) - [@abougouffa](https://github.com/abougouffa)
- **(evil-collection)** cleanup previous fix - (21ddd4d) - [@abougouffa](https://github.com/abougouffa)
- **(project)** code formatting - (3827863) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(backports)** precise condition - (22a09fa) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edit - (a0861ff) - [@abougouffa](https://github.com/abougouffa)
- **(ecryptfs)** partial rewrite - (e1e4034) - [@abougouffa](https://github.com/abougouffa)
- move pcache directory customization to me-defaults - (f10b3cc) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(ltex)** remove server commands - (f02b0a7) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bootstrap)** call build functions interactively - (63b92d4) - [@abougouffa](https://github.com/abougouffa)
- **(bootstrap)** minor refactor - (cefcdac) - [@abougouffa](https://github.com/abougouffa)
- **(bootstrap)** do not ask when running build functions - (3d390ea) - [@abougouffa](https://github.com/abougouffa)
- **(bootstrap)** run build functions on update - (940fdaa) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** add ts modes - (e317bbf) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** move compilation options to `me-prog` - (f949b46) - [@abougouffa](https://github.com/abougouffa)
- **(core)** move package updating routines - (5bbfe68) - [@abougouffa](https://github.com/abougouffa)
- **(core)** update loaddefs - (71eb596) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** add an option to disable it - (ee30640) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** set variables with `setopt` - (75fbb43) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** better scrolling settings - (f1b4639) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** more UI customization - (d7e7f98) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** move `password` and `auth-source` to `me-builtin` - (dc3a0da) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** correctly set `show-trailing-whitespace` - (997f6b2) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** set additional paths - (9e8c7b9) - [@abougouffa](https://github.com/abougouffa)
- **(eaf)** disable pdf-viewer - (c35356b) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** prefer loading newer Elisp files - (9bdf851) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** do not assume servers to be executable - (51dd7b8) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed)** Enhance customizability - (d2f124a) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-numbers)** enable for `conf-mode`, tweak regexp - (849dba3) - [@abougouffa](https://github.com/abougouffa)
- **(init)** use `lisp-interaction-mode` for scratch - (5f755e4) - [@abougouffa](https://github.com/abougouffa)
- **(io)** code refactoring - (c5ef698) - [@abougouffa](https://github.com/abougouffa)
- **(lexic)** fix local keybindigs - (ab56fb4) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** add `tex-mode` - (0e344a8) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** minor edits, add documentation - (8e0421c) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** use Eglot's server/port syntax - (faec1c5) - [@abougouffa](https://github.com/abougouffa)
- **(ltex-ls)** additional languages - (ea19fa6) - [@abougouffa](https://github.com/abougouffa)
- **(macrostep)** remove unneeded hack (fixed upstream) - (0f0b5ef) - [@abougouffa](https://github.com/abougouffa)
- **(macrostep)** use upstream repo, apply a hack - (c048d3c) - [@abougouffa](https://github.com/abougouffa)
- **(media)** automatically open Youtube links in MPV - (1fcc323) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** use `nerd-icons` - (5407ab5) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** set an icon for matlab/octave files - (972d3a4) - [@abougouffa](https://github.com/abougouffa)
- **(realgud)** add local binding for treesit modes - (51d8078) - [@abougouffa](https://github.com/abougouffa)
- **(realgud)** minor tweaks and fixes - (8365877) - [@abougouffa](https://github.com/abougouffa)
- **(realgud)** define commands - (920ab8d) - [@abougouffa](https://github.com/abougouffa)
- **(realgud-lldb)** remove unneeded autoload - (689a1da) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update the skeleton's modules list - (1e86106) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** bind TAB and S-TAB to next/previous - (dff6996) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** remove unused themes - (d8f4e3b) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `nerd-icons` explicitly - (33e3ffe) - [@abougouffa](https://github.com/abougouffa)
- **(xclip)** remove useless `+xclip--enable-in-tty-h` - (0481b8e) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (69e9235) - [@abougouffa](https://github.com/abougouffa)
- use `nerd-icons` instead of `all-the-icons` - (403a3a1) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (6525e40) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (8b90843) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (272bb17) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (bffed94) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (53caf4d) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (c89d5cc) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (693efa0) - [@abougouffa](https://github.com/abougouffa)
- register package-specific build functions - (ef635c4) - [@abougouffa](https://github.com/abougouffa)

- - -

## v0.3.0 - 2023-05-03
#### Bug Fixes
- **(aphelia)** adapt to the new upstream changes - (06f7776) - [@abougouffa](https://github.com/abougouffa)
- **(backports)** fix a bug causing straight to fail (#51) - (ded8596) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** avoid running `mu4e` repeatedly - (c784d05) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** do not show when Emacs started with a file - (ebe1a9a) - [@abougouffa](https://github.com/abougouffa)
- **(io)** fix +html2pdf output file name - (a8d4435) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** add safety guards to avoid conflict with evil - (db20803) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** clone the full packages repos - (4f51035) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** use master on Emacs 28, develop on newer versions - (7f7f33d) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** wrong parenthesis disabling corfu in other modes - (1ad3b33) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** `:pin-ref` problem on Emacs28 (#49) - (e37b984) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** pin problematic packages to working versions - (424fd54) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** fix broken link - (87247a5) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `minemacs-update` command - (5692b8d) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add the `+hook-once!` macro - (1eaa535) - [@abougouffa](https://github.com/abougouffa)
- **(docs)** initial support for `poly-markdown` - (2460e18) - [@abougouffa](https://github.com/abougouffa)
- **(io)** add a helper to save URLs to HTML snapshots - (24e03db) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** WIP optional evil replacement - (954a549) - [@abougouffa](https://github.com/abougouffa)
- **(modeling)** add `medelica-mode` - (86e8c09) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `hy-mode` - (031b5ba) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add all-the-icons for ibuffer - (40d8bb8) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** add the `:pin-ref` keyword to use `straight-x` - (cc4f11b) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(git)** track straight's default pins - (52390b7) - [@abougouffa](https://github.com/abougouffa)
- provide `make update` - (4d6af7d) - [@abougouffa](https://github.com/abougouffa)
- save straight's versions when cleaning - (8795bbf) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(biblio)** minor edit - (cc742e7) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simplify a condition - (4a82a91) - [@abougouffa](https://github.com/abougouffa)
- **(core)** rename a parameter - (4dd3285) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** remove extra spaces - (b8d9d91) - [@abougouffa](https://github.com/abougouffa)
- add files headers and footers - (298543e) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(mu4e)** remove commented code - (a766768) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** small cleanup - (0c3dc30) - [@abougouffa](https://github.com/abougouffa)
- make use of `+hook-once!` - (43b12bb) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(corfu)** restore in `(org/markdown)-mode` - (be458ac) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bootstrap)** revert to straight's develop branch - (797e115) - [@abougouffa](https://github.com/abougouffa)
- **(core)** make sure `+emacs-features-p` returns a boolean - (2c194de) - [@abougouffa](https://github.com/abougouffa)
- **(core)** accept hook symbol in `+hook-once!` - (4a08072) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fallback to a builtin theme if `minemacs-theme` fails - (48b0b62) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** disable in `org-mode` and `markdown-mode` - (9360b69) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** ensure keeping `mu4e` alive in background - (2358e3d) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** set straight branch to develop - (d9688e1) - [@abougouffa](https://github.com/abougouffa)
- **(ecryptfs)** better support - (7034976) - [@abougouffa](https://github.com/abougouffa)
- **(eldoc-box)** remove special case, fixed upstream - (f38adf1) - [@abougouffa](https://github.com/abougouffa)
- **(email)** refine attachement detection regexp - (7b72d76) - [@abougouffa](https://github.com/abougouffa)
- **(evil-collection)** restore `mu4e` - (3c60a72) - [@abougouffa](https://github.com/abougouffa)
- **(gts-translate)** add an option to choose translation langs - (8e4d74f) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** enable `hs-minor-mode` - (29d22a4) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** better fontification - (9d31c01) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** additional tweaks - (ad5adae) - [@abougouffa](https://github.com/abougouffa)
- **(logview)** set custom files paths - (f817a8f) - [@abougouffa](https://github.com/abougouffa)
- **(maxima)** use locally installed packages - (024a05e) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** don't ask for the alias when there is only one - (6bda132) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** minor edits - (94a8c1f) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** better org-roam protocol handling - (9a9748c) - [@abougouffa](https://github.com/abougouffa)
- **(org)** prefer using `latexmk` or `tectonic` when found - (4e1267d) - [@abougouffa](https://github.com/abougouffa)
- **(pcache)** create the cache in the cache directory - (28f2c04) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** update packages - (1dd3044) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** add pin file - (03e13d2) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** restrict `org/markdown` to tempel capf - (439b6aa) - [@abougouffa](https://github.com/abougouffa)
- **(treemacs)** enable `evil` support - (2176941) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - (6a94af1) - [@abougouffa](https://github.com/abougouffa)
- bump package versions - (8372b89) - [@abougouffa](https://github.com/abougouffa)
- bump package versions - (0f6a2fd) - [@abougouffa](https://github.com/abougouffa)
- update loaddefs - (09a9cea) - [@abougouffa](https://github.com/abougouffa)

- - -

## v0.2.0 - 2023-04-01
#### Bug Fixes
- **(auctex)** require `tex` - (104a41e) - [@abougouffa](https://github.com/abougouffa)
- **(backports)** add `scratch-buffer` (#41) - (f01f80b) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** remove accidentally added quote - (69bd4b3) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** no initial fill when in `minibuffer` #37 - (5c30bcd) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** load after `evil-collection` (#42) - (d3b0976) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** setup default hooks early - (720da8c) - [@abougouffa](https://github.com/abougouffa)
- **(docker)** better handling of `Dockerfile`s - (69544e2) - [@abougouffa](https://github.com/abougouffa)
- **(ebnf-mode)** fix a typo - (f63014f) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** temporary disable `unicode-fonts` - (a116b7b) - [@abougouffa](https://github.com/abougouffa)
- **(eglot-box)** better integration with `tab-bar` and `tool-bar` - (f43d7ff) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** temporary disable `evil-escape` - (22f9a6d) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** the right way to use `evil-search` - (a5c61ab) - [@abougouffa](https://github.com/abougouffa)
- **(evil-mc)** avoid inserting the first `evil-escape` char - (99559a8) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** fixes related to evil-collection - (56533ad) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** mu 1.10 UI and evil fixes - (2398bd0) - [@abougouffa](https://github.com/abougouffa)
- **(pdf-tools)** make sure to use it to show PDFs - (d8bc950) - [@abougouffa](https://github.com/abougouffa)
- disable packages causing problems on the last build - (6b2c3ae) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** more use cases of `early-config.el` - (b6c2104) - [@abougouffa](https://github.com/abougouffa)
- add a header image in README - (2130b3c) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(dashboard)** add dashboard - (f0d5a10) - [@abougouffa](https://github.com/abougouffa)
- **(doc-view)** use SVG when available - (b9a7715) - [@abougouffa](https://github.com/abougouffa)
- **(evil-multiedit)** initial support - (535f2ba) - [@abougouffa](https://github.com/abougouffa)
- **(lisp)** add support for Clojure (via cider) - (7546f08) - [@abougouffa](https://github.com/abougouffa)
- **(lisp)** add more geiser backends - (2f95ff1) - [@abougouffa](https://github.com/abougouffa)
- **(lsp-bridge)** initial support (WIP) - (5820016) - [@abougouffa](https://github.com/abougouffa)
- **(mermaid)** initial support - (5718db8) - [@abougouffa](https://github.com/abougouffa)
- add the missing template for `+html2pdf` - (c3c6fd2) - [@abougouffa](https://github.com/abougouffa)
- add more backends to `+html2pdf` - (8b025d0) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(changlog)** remove - (8d54ac2) - [@abougouffa](https://github.com/abougouffa)
- move templates to assets - (4042455) - [@abougouffa](https://github.com/abougouffa)
- move pictures to `assets` - (2f9809a) - [@abougouffa](https://github.com/abougouffa)
- add names for the workflows - (1d41152) - [@abougouffa](https://github.com/abougouffa)
- add `clean_pcache` target in Makefile - (a7e02d1) - [@abougouffa](https://github.com/abougouffa)
- add the "v" prefix in cocogitto - (0a70fbc) - [@abougouffa](https://github.com/abougouffa)
- bump cocogitto version to 3.4 - (a1c5ab7) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(elisp)** minor edits - (bf3a4e0) - [@abougouffa](https://github.com/abougouffa)
- **(macrostep)** update recipe - (9054280) - [@abougouffa](https://github.com/abougouffa)
- **(math)** format recipes - (284c970) - [@abougouffa](https://github.com/abougouffa)
- **(pdf-tools)** simplify - (427c0c1) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** remove unneeded package - (6baf7b0) - [@abougouffa](https://github.com/abougouffa)
- minor edits - (81dc6a6) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(robot)** make ROS commands customizable - (c041258) - [@abougouffa](https://github.com/abougouffa)
- **(writeroom-mode)** hook via `use-package` - (7c0832e) - [@abougouffa](https://github.com/abougouffa)
- move `transient` to `me-builtin` - (0761b3d) - [@abougouffa](https://github.com/abougouffa)
- define MinEmacs sub-groups - (a8f563c) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- restore disabled packages, using emacs@6bf441ff11540 - (6e00f68) - [@abougouffa](https://github.com/abougouffa)
- replace `writeroom-mode` with simpler config - (4c2255d) - [@abougouffa](https://github.com/abougouffa)
- replace `yasnippet` with `tempel` - (b1edd7e) - [@abougouffa](https://github.com/abougouffa)
- remove `lsp-bridge` - (34ce221) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(+writing-mode)** increase text scale - (ddfb23a) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** pin `map` and `let-alist` - (6eff30b) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** enable ANSI colors, restore savehist integration - (c701113) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** add a message on toggle burying buffer - (6c7c28d) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove obsolete keybinding aliases - (48c53de) - [@abougouffa](https://github.com/abougouffa)
- **(core)** define a group for MinEmacs' custom variables - (56c37a8) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** set `custom-theme-directory` - (fa621b6) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** enable drag and drop of regions - (fb1d75d) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** do not use system tooltips - (3adf802) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** update the main modeline layout - (f8c367e) - [@abougouffa](https://github.com/abougouffa)
- **(eaf)** minor edits, start in emacs state - (d818ab6) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** disable annoying reporting in echo area - (23af99e) - [@abougouffa](https://github.com/abougouffa)
- **(ein)** load org-babel the right way - (2a68535) - [@abougouffa](https://github.com/abougouffa)
- **(init)** simplify the `file-name-handler-alist` hack - (9dd1345) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** edit keybindings - (f6fbe96) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** disable the new `mu4e-modeline-mode` - (e7af964) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** small UI tweak - (b3588e9) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e-alert)** better filtering of spams - (0c54543) - [@abougouffa](https://github.com/abougouffa)
- **(org)** update keybindings - (7675c7a) - [@abougouffa](https://github.com/abougouffa)
- **(org)** dynamically set latex fragments scale - (26f1fd9) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** add repo hash to the build directory - (4ad4c3a) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** decrease default fonts size - (ba61d68) - [@abougouffa](https://github.com/abougouffa)
- **(window)** wider window for `lexic` - (6d2550c) - [@abougouffa](https://github.com/abougouffa)
- **(writeroom)** minor edits - (a1bc1a8) - [@abougouffa](https://github.com/abougouffa)
- make `yasnippet` conf obsolete - (e8025e9) - [@abougouffa](https://github.com/abougouffa)

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).
