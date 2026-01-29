# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## [v13.22.0](https://github.com/abougouffa/minemacs/compare/013b0ff07905787420e568a6320235cecbc6a88d..v13.22.0) - 2026-01-29
#### Tweaks
- (**aidermacs**) set the Ollama API URL from environment when available - ([6aa5141](https://github.com/abougouffa/minemacs/commit/6aa51415fa76d8f315b86f2d2f38593d80141c19)) - [@abougouffa](https://github.com/abougouffa)
- (**llm-ollama**) add some missing models - ([6765840](https://github.com/abougouffa/minemacs/commit/6765840875b3a42c87d5a7d2552f670ad91f45dd)) - [@abougouffa](https://github.com/abougouffa)
- (**symbol-overlay**) use a lighter face for `symbol-overlay-mode` - ([7096407](https://github.com/abougouffa/minemacs/commit/7096407adcf892391add9ac37d0211bb5e72f99d)) - [@abougouffa](https://github.com/abougouffa)
- (**whisper**) remove unneeded custom command - ([013b0ff](https://github.com/abougouffa/minemacs/commit/013b0ff07905787420e568a6320235cecbc6a88d)) - [@abougouffa](https://github.com/abougouffa)
- (**window**) display the "Async Shell Command" buffet at bottom - ([fec7d57](https://github.com/abougouffa/minemacs/commit/fec7d57b27e8dfc3e84f5b17287f4a8a9c5a2ad9)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([4b65c80](https://github.com/abougouffa/minemacs/commit/4b65c808856f9af26524a4853372520e8682a11c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([09ccc8b](https://github.com/abougouffa/minemacs/commit/09ccc8b4492e88c4bac0fcb5ab38459fe6fb77e2)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- (**ai**) add support for `mcp` - ([8a9d90c](https://github.com/abougouffa/minemacs/commit/8a9d90c547ec1065349091c4f7334d01b96d88f6)) - [@abougouffa](https://github.com/abougouffa)
- (**ai**) remove `elisa` - ([9e12fa6](https://github.com/abougouffa/minemacs/commit/9e12fa69caa3b5688a21310aea5116e8e72ee306)) - [@abougouffa](https://github.com/abougouffa)
- (**editor**) restore `symbol-overlay` - ([1b7d94b](https://github.com/abougouffa/minemacs/commit/1b7d94b423b58d220cd2cf79afb1aae6d555aced)) - [@abougouffa](https://github.com/abougouffa)
- smarter `C-a` and `C-e` stolen from Doom Emacs - ([0fa21ff](https://github.com/abougouffa/minemacs/commit/0fa21ff5c96e2adc2bdf45e00543bed6b9b699b2)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- (**ellama**) remove unneeded customization - ([8fcfc42](https://github.com/abougouffa/minemacs/commit/8fcfc42734f87147adbcea648a597a212d7621db)) - [@abougouffa](https://github.com/abougouffa)
- (**llm-ollama**) remove unneeded custom functions - ([168300e](https://github.com/abougouffa/minemacs/commit/168300e532dcb7a2f9b661d18487d9a0941e1b19)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.21.0](https://github.com/abougouffa/minemacs/compare/0b1a60b62a96ab7d05f632ceefe53b44e24cd2db..v13.21.0) - 2026-01-25
#### Tweaks
- (**eglot**) disable tramp direct async process in Eglot connections - ([5abd22f](https://github.com/abougouffa/minemacs/commit/5abd22f8491142f7c4a04c2176a62c074aa69419)) - [@abougouffa](https://github.com/abougouffa)
- (**on-demand/plantuml**) update the PlanUML JAR release - ([0be8426](https://github.com/abougouffa/minemacs/commit/0be8426ba6f2ae43661752631ed902d4f764e7c3)) - [@abougouffa](https://github.com/abougouffa)
- (**smartparens**) enable on the first file - ([5372097](https://github.com/abougouffa/minemacs/commit/5372097e45a573897bbc3da45795f0549e399af6)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([53c706b](https://github.com/abougouffa/minemacs/commit/53c706bba73ace620d836a267be8a0e099f3b307)) - [@abougouffa](https://github.com/abougouffa)
- support wildcards in `+compilation-db-find-file` - ([c9647f4](https://github.com/abougouffa/minemacs/commit/c9647f4e8f74dfba8edd3e2c0d08fedb321db9a3)) - [@abougouffa](https://github.com/abougouffa)
- handle themes and packages in `+describe-at-point` - ([e6ebcbd](https://github.com/abougouffa/minemacs/commit/e6ebcbd35a8066922103be10e68df4613e19a359)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c88b31d](https://github.com/abougouffa/minemacs/commit/c88b31d89e33e14b44977fd1f9b91d1e2edff88c)) - [@abougouffa](https://github.com/abougouffa)
- add Google Sans Code NF - ([bb9885d](https://github.com/abougouffa/minemacs/commit/bb9885d1227dd678d01cf18d3f163762adef8db5)) - [@abougouffa](https://github.com/abougouffa)
- prefer the new Google Sans Code font - ([b064dbd](https://github.com/abougouffa/minemacs/commit/b064dbd8c243f4cbcacdf3f1a1cf98c17debc3de)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([355156c](https://github.com/abougouffa/minemacs/commit/355156cbad5b595f427c166b07adb218e919ce34)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- (**editor**) restore `smartparens` - ([637b92c](https://github.com/abougouffa/minemacs/commit/637b92cb7148a7f4834a177bbd65baff2e8bd13c)) - [@abougouffa](https://github.com/abougouffa)
- (**files**) make `cascading-dir-locals` obsolete, unpredictable behavior - ([0b1a60b](https://github.com/abougouffa/minemacs/commit/0b1a60b62a96ab7d05f632ceefe53b44e24cd2db)) - [@abougouffa](https://github.com/abougouffa)
#### Bug Fixes
- remove reference to deleted function - ([39876ff](https://github.com/abougouffa/minemacs/commit/39876ff230fddef2cc16ffad5e850fb24624a005)) - [@abougouffa](https://github.com/abougouffa)
- handling of nil variables in `+describe-at-point` - ([498ed46](https://github.com/abougouffa/minemacs/commit/498ed46e0f8160a8c67c38bd3bf3b5ce1c01994b)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- (**window**) restore my initial window configuration - ([e1a1c6e](https://github.com/abougouffa/minemacs/commit/e1a1c6ee60a15be5441e9872334f8ebcc8615c8a)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([abe6c56](https://github.com/abougouffa/minemacs/commit/abe6c567b80b3f88028f3d6252f840ac9bf4031e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- remove an unneeded command - ([6ff3769](https://github.com/abougouffa/minemacs/commit/6ff3769abd192d9621694f9eba823db384609161)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.20.0](https://github.com/abougouffa/minemacs/compare/8b060d9e75512dd2b17dedc125ac6ba8755215cd..v13.20.0) - 2026-01-17
#### Tweaks
- (**blamer**) use 20% smaller font size instead of 15% - ([f9bd115](https://github.com/abougouffa/minemacs/commit/f9bd1154e7b73310be56039b5ad27c94df4258fb)) - [@abougouffa](https://github.com/abougouffa)
- (**eat**) add super-project variant of `eat-project` - ([8b060d9](https://github.com/abougouffa/minemacs/commit/8b060d9e75512dd2b17dedc125ac6ba8755215cd)) - [@abougouffa](https://github.com/abougouffa)
- (**rg**) add super-project variant of `rg-project` - ([be417d3](https://github.com/abougouffa/minemacs/commit/be417d38f2d76b1932f526ad54d66382db348e63)) - [@abougouffa](https://github.com/abougouffa)
- minor refactor - ([78cadbf](https://github.com/abougouffa/minemacs/commit/78cadbfdea01058551c62d770d944ef169869f95)) - [@abougouffa](https://github.com/abougouffa)
- minor edit - ([95c25ac](https://github.com/abougouffa/minemacs/commit/95c25ac37543deea5728be97826e80b3d28f8008)) - [@abougouffa](https://github.com/abougouffa)
- better implementation of `+super-project-define-commands` - ([9a05ba5](https://github.com/abougouffa/minemacs/commit/9a05ba5571d585a17b16d55544f38d97d8390888)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- (**vc**) restore `blamer` - ([7773d50](https://github.com/abougouffa/minemacs/commit/7773d5076ee5ed20f330d276b884c0da04995fe9)) - [@abougouffa](https://github.com/abougouffa)
- better implementation of `+describe-at-point` - ([82cc8cd](https://github.com/abougouffa/minemacs/commit/82cc8cdbfe1e2acc09da98c1ee2c5f35908da8ca)) - [@abougouffa](https://github.com/abougouffa)
#### Bug Fixes
- (**obsolete/blamer**) fix height calculation - ([fc32687](https://github.com/abougouffa/minemacs/commit/fc3268755e381dbd1ac14e77c1e5301c4b64f852)) - [@abougouffa](https://github.com/abougouffa)
- correct regexps of auto mode matching for some special files - ([607ca42](https://github.com/abougouffa/minemacs/commit/607ca42a434a52a83ecc4ee087af216431c2c3e9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.19.0](https://github.com/abougouffa/minemacs/compare/a9a24d7c37c27e29b68228c35dd663b7d7071bea..v13.19.0) - 2026-01-11
#### Nitpicks, changes with no side effect
- (**hl-todo**) code formatting - ([07becda](https://github.com/abougouffa/minemacs/commit/07becda7081310ed7cb1e9a46a95231541ed46fd)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- (**devcontainer**) auto enable - ([702d86e](https://github.com/abougouffa/minemacs/commit/702d86ecf5cd9251169fd5257e59914b50ec58f4)) - [@abougouffa](https://github.com/abougouffa)
- (**eglot**) minor refactor - ([7ebb72c](https://github.com/abougouffa/minemacs/commit/7ebb72cb690d3f2a13b319c33362c17902b3573d)) - [@abougouffa](https://github.com/abougouffa)
- (**git-modes**) user `gitignore-mode` in `.dumbjump` files - ([e5c8352](https://github.com/abougouffa/minemacs/commit/e5c835280d0a64225904a9a323625850f28f35cc)) - [@abougouffa](https://github.com/abougouffa)
- (**mason**) auto ensure, but with laziness - ([78b0455](https://github.com/abougouffa/minemacs/commit/78b04553419d6d3c3c6c293d227713c40ab5a132)) - [@abougouffa](https://github.com/abougouffa)
- (**with-editor**) remove unneeded customization - ([517b01f](https://github.com/abougouffa/minemacs/commit/517b01ffa0b255a1bdfc3367b11f5217614f614e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([efe3849](https://github.com/abougouffa/minemacs/commit/efe38495592faa4d908ef7cf3526524222b52707)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6427642](https://github.com/abougouffa/minemacs/commit/642764225dc1c6a323d4d3949172a6b61db6beb0)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- (**modeline**) show selected number of lines - ([ec52f8b](https://github.com/abougouffa/minemacs/commit/ec52f8b059e033dc259a86db4abe84ca69374df0)) - [@abougouffa](https://github.com/abougouffa)
- add `+repo-projects` helper function - ([97a21bf](https://github.com/abougouffa/minemacs/commit/97a21bf9d54d88a002ec879e1bb2ca34c09d65c2)) - [@abougouffa](https://github.com/abougouffa)
#### Bug Fixes
- (**corfu**) use the default recipe - ([17b0725](https://github.com/abougouffa/minemacs/commit/17b072524cafcd64d95d912f1a38ca728794f948)) - [@abougouffa](https://github.com/abougouffa)
- (**devcontainer**) fix a bug on non-project files - ([797571a](https://github.com/abougouffa/minemacs/commit/797571a5d8f19bb4c66e3942c7b77c8f5985c7f1)) - [@abougouffa](https://github.com/abougouffa)
- (**mason**) properly set the mason bin directory in PATH - ([a9a24d7](https://github.com/abougouffa/minemacs/commit/a9a24d7c37c27e29b68228c35dd663b7d7071bea)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.18.0](https://github.com/abougouffa/minemacs/compare/ab482b64d347de325e64738d24edd8686a77cd80..v13.18.0) - 2025-12-25
#### Tweaks
- (**consult**) remove obsolete references - ([e316e0b](https://github.com/abougouffa/minemacs/commit/e316e0ba56cedfd874d1edbba076b78817060d91)) - [@abougouffa](https://github.com/abougouffa)
- (**dash-docs**) register some docsets for C/C++ - ([6b43935](https://github.com/abougouffa/minemacs/commit/6b439357d0fba89a1cdcebab9f66bc4c2944926a)) - [@abougouffa](https://github.com/abougouffa)
- (**docs**) remove `pdf-tools`, already included in `on-demand/me-pdf` - ([ab482b6](https://github.com/abougouffa/minemacs/commit/ab482b64d347de325e64738d24edd8686a77cd80)) - [@abougouffa](https://github.com/abougouffa)
- (**dumb-jump**) use both `xref` the backend and the legacy commands - ([77fa3a1](https://github.com/abougouffa/minemacs/commit/77fa3a1c36359f0ad20e91531cf5dbb6cb13320e)) - [@abougouffa](https://github.com/abougouffa)
- (**eglot**) disable fontifying semantic tokens - ([82cfbee](https://github.com/abougouffa/minemacs/commit/82cfbee1e2c9e05194b1315e10bd1cdd7df5b3de)) - [@abougouffa](https://github.com/abougouffa)
- (**hideshow**) remove obsolete option - ([9c181d0](https://github.com/abougouffa/minemacs/commit/9c181d09a88436ada0463852307090471e943015)) - [@abougouffa](https://github.com/abougouffa)
- (**lib**) remove unneeded variable - ([491254f](https://github.com/abougouffa/minemacs/commit/491254f3d246d72a31518fc908c7f9fe888c2b26)) - [@abougouffa](https://github.com/abougouffa)
- (**magit-gerrit**) remove no more needed customization - ([12754d3](https://github.com/abougouffa/minemacs/commit/12754d3c61eacf2a383704ae318e98a00ad373db)) - [@abougouffa](https://github.com/abougouffa)
- (**magit-gerrit**) bind `magit-gerrit-dispatch` to `M-_` - ([3b078b4](https://github.com/abougouffa/minemacs/commit/3b078b4b70270c437bacd9c423411b648a72334b)) - [@abougouffa](https://github.com/abougouffa)
- (**modeline**) display region size in mode-line - ([afae9db](https://github.com/abougouffa/minemacs/commit/afae9db27a142e6954ca3a34fbccf255b0d2565d)) - [@abougouffa](https://github.com/abougouffa)
- (**on-demand/dotnet**) make use of the `with-memoization` macro - ([6a2bd21](https://github.com/abougouffa/minemacs/commit/6a2bd2188dfbabc3f9586174008dca620ece624e)) - [@abougouffa](https://github.com/abougouffa)
- (**satch**) cleanup and remove unneeded stuff - ([50cfd66](https://github.com/abougouffa/minemacs/commit/50cfd664551606e313cb11b70d8e4414b0be32be)) - [@abougouffa](https://github.com/abougouffa)
- (**speedbar**) remove unneeded customization - ([629656f](https://github.com/abougouffa/minemacs/commit/629656f180596ad364c066a5ce717d24f5ec726f)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([4997e66](https://github.com/abougouffa/minemacs/commit/4997e666f3b569c45b48428bc98340a3281da9e1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4eb29cb](https://github.com/abougouffa/minemacs/commit/4eb29cb2f251771566c85c6afb5881be40f5bd05)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([124612b](https://github.com/abougouffa/minemacs/commit/124612ba2bb2962d397e0b0831752651425b3f4e)) - [@abougouffa](https://github.com/abougouffa)
- remove unused stolen code - ([8493d55](https://github.com/abougouffa/minemacs/commit/8493d5582ef6eb4f2af4690050e4599780e40f42)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- (**on-demand/arduino**) add the `+arduino-cli-set-board` command - ([a7336cd](https://github.com/abougouffa/minemacs/commit/a7336cd97af9f3432b6abcfc95060f54d2d334c2)) - [@abougouffa](https://github.com/abougouffa)
- (**on-demand/arduino**) replace obsolete mode, add `arduino-cli-mode` - ([aa30725](https://github.com/abougouffa/minemacs/commit/aa307250e6e725930b2fe7640cbedd11fe56e5db)) - [@abougouffa](https://github.com/abougouffa)
#### Bug Fixes
- (**magit**) auto refreshing after saving buffers can be slow - ([2257c21](https://github.com/abougouffa/minemacs/commit/2257c21fd24bdbb9c6e9ed35994c47bd551eeda8)) - [@abougouffa](https://github.com/abougouffa)
- (**skel**) update example, remove examples for obsolete packages - ([d4c5206](https://github.com/abougouffa/minemacs/commit/d4c52063b1bc93d2810c660062e41fede813c5fe)) - [@abougouffa](https://github.com/abougouffa)
- (**skel**) fix out of date configuration example - ([efbe620](https://github.com/abougouffa/minemacs/commit/efbe62002d2f6b1fa47c4af9084208aba4e2932f)) - [@abougouffa](https://github.com/abougouffa)
- add the window configuration file - ([309d616](https://github.com/abougouffa/minemacs/commit/309d616ad734b2d80f3d2d11d9aeb16e16ad6eb5)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([4106dfb](https://github.com/abougouffa/minemacs/commit/4106dfb08f5948549b894b3bba32b7a193858c88)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- move a file to the right directory - ([a5108bf](https://github.com/abougouffa/minemacs/commit/a5108bf9a70b4b853b9e67033a4f16792809af5b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.17.0](https://github.com/abougouffa/minemacs/compare/cec3169d7e6f0bd4334033c7d7356c922e721144..v13.17.0) - 2025-12-11
#### Tweaks
- (**eglot**) don't show pending requests in mode-line - ([cec3169](https://github.com/abougouffa/minemacs/commit/cec3169d7e6f0bd4334033c7d7356c922e721144)) - [@abougouffa](https://github.com/abougouffa)
- (**eglot-x**) add LSP multiplexer for `pyright`, `ty` and `ruff` - ([e043a8b](https://github.com/abougouffa/minemacs/commit/e043a8bbb6c5d6519c05e64c9591a9d052ec9cbf)) - [@abougouffa](https://github.com/abougouffa)
- (**external-tools**) add `rassumfrassum`, remove `emacs-lsp-booster` - ([a95e16e](https://github.com/abougouffa/minemacs/commit/a95e16eaec69a83165b56c62063b835bf2cc0f1a)) - [@abougouffa](https://github.com/abougouffa)
- (**mason**) don't load, just add the bin directory to `exec-path` - ([d2a291b](https://github.com/abougouffa/minemacs/commit/d2a291b302324968f1b70185d88937ab01b2f7a7)) - [@abougouffa](https://github.com/abougouffa)
- (**on-demand/dotnet**) minor simplification in `+dotnet-get-templates` - ([c7dc46b](https://github.com/abougouffa/minemacs/commit/c7dc46bd9650040ba3f7589ba089a3bcd90cb83a)) - [@abougouffa](https://github.com/abougouffa)
- (**project-x**) autolaod `+def-project-mode!` - ([bbb0b74](https://github.com/abougouffa/minemacs/commit/bbb0b7472b5025161ff395e054ae0214ee1dd83b)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([45e6c8a](https://github.com/abougouffa/minemacs/commit/45e6c8a9c45d4703a78127ff7d0056274f527f6f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([57fd55e](https://github.com/abougouffa/minemacs/commit/57fd55e1e7ee42ee1cf72b711db7104a19c7d0e9)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([ef442a2](https://github.com/abougouffa/minemacs/commit/ef442a2ad9a37e0c2e1cdbd1699884787ab1f0a1)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([2ee5767](https://github.com/abougouffa/minemacs/commit/2ee5767f917809675bcf9d8968c19ffa23742bea)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- (**lib**) add smarter `+open-with` command - ([7a20b20](https://github.com/abougouffa/minemacs/commit/7a20b20d2e4000fb31236563554f9cb0dd807ab0)) - [@abougouffa](https://github.com/abougouffa)
- (**natural-langs**) make `reverso` obsolete - ([25ae1a9](https://github.com/abougouffa/minemacs/commit/25ae1a9b4b95184a190f2c2ce634b050ebda7787)) - [@abougouffa](https://github.com/abougouffa)
- (**on-demand/dotnet**) create `+dotnet-proj-mode` derived from `nxml-mode` - ([d492d92](https://github.com/abougouffa/minemacs/commit/d492d9233d5df58448a7e7b49cd645025c606987)) - [@abougouffa](https://github.com/abougouffa)
- (**on-demand/dotnet**) remove `csproj-mode` - ([e252179](https://github.com/abougouffa/minemacs/commit/e2521799316f06ca9ab94f9982e48dd8eade9fc9)) - [@abougouffa](https://github.com/abougouffa)
- (**on-demand/qt**) add `qml-ts-mode` - ([2217ac0](https://github.com/abougouffa/minemacs/commit/2217ac06070579b65285f6d7dc5261d36465b35c)) - [@abougouffa](https://github.com/abougouffa)
- (**tools**) add `mason` - ([d9ad6d7](https://github.com/abougouffa/minemacs/commit/d9ad6d79a96daf669ae4a88c048e8ff44ec4f960)) - [@abougouffa](https://github.com/abougouffa)
- (**ui**) make the `casual` suite obsolete - ([639900f](https://github.com/abougouffa/minemacs/commit/639900ff814e99d6e8923698550ee81c4fa58beb)) - [@abougouffa](https://github.com/abougouffa)
- (**ui**) make `easysession` obsolete - ([3f7c820](https://github.com/abougouffa/minemacs/commit/3f7c820b39e96b4cefbbb31d52de130329a8d4ce)) - [@abougouffa](https://github.com/abougouffa)
- (**vc**) restore `gerrit` - ([01ba678](https://github.com/abougouffa/minemacs/commit/01ba67856e45c9f8437447db07f3a06c441f229e)) - [@abougouffa](https://github.com/abougouffa)
#### Bug Fixes
- (**jiralib**) fix `marginalia` decoration - ([4cb4768](https://github.com/abougouffa/minemacs/commit/4cb47684aa0d45c5822ad5e6c718bf451af8ea34)) - [@abougouffa](https://github.com/abougouffa)
- (**lib**) make sure no questions are asked when reloading dir-locals - ([f7ec91e](https://github.com/abougouffa/minemacs/commit/f7ec91ed8f4465699192060433cea3a6dbfe5267)) - [@abougouffa](https://github.com/abougouffa)
- (**lib**) automatically load on-demand companion packages - ([f8a66a2](https://github.com/abougouffa/minemacs/commit/f8a66a29b0a6ea25db07d499e4c68e2aa22efd7f)) - [@abougouffa](https://github.com/abougouffa)
- (**mason**) make sure to invoke `mason-ensure` - ([9dfe453](https://github.com/abougouffa/minemacs/commit/9dfe4534f915f1707860788de89701057cfc29c2)) - [@abougouffa](https://github.com/abougouffa)
- (**on-demand/dotnet**) fix the `marginalia` integration - ([7d4e16b](https://github.com/abougouffa/minemacs/commit/7d4e16b5ad521379bf00c3986d53b0a29555f852)) - [@abougouffa](https://github.com/abougouffa)
- (**on-demand/vbnet-mode**) fix flymake-related error - ([0b32b95](https://github.com/abougouffa/minemacs/commit/0b32b95c1a6dc153957fbf694adc387b2016f39e)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- (**faq**) remove irrelevant old question - ([65ec310](https://github.com/abougouffa/minemacs/commit/65ec31070e8fc021d62b80d29079f142ce72379c)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([ee045f9](https://github.com/abougouffa/minemacs/commit/ee045f995aa066cc0675d78408ce69b9267ba9ec)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.16.0](https://github.com/abougouffa/minemacs/compare/e0ea49ae59d595f0d12d204d092aa46be7122a8b..v13.16.0) - 2025-11-30
#### Tweaks
- (**hl-line**) enable in `profiler-report-mode` - ([e8fa657](https://github.com/abougouffa/minemacs/commit/e8fa65753d23d66e3843a9682ccd0ce100f37494)) - [@abougouffa](https://github.com/abougouffa)
- (**modus-themes**) remove unneeded face customization - ([8e09f21](https://github.com/abougouffa/minemacs/commit/8e09f21460ada65b095179952333808ab3f4fe4c)) - [@abougouffa](https://github.com/abougouffa)
- (**org-contrib**) change mirror's URL - ([b01aa0c](https://github.com/abougouffa/minemacs/commit/b01aa0c4f1e5ab0b472638e4d720f6e08894cab0)) - [@abougouffa](https://github.com/abougouffa)
- (**pet**) enhance performances by disabling recursive finder - ([6e5b5a4](https://github.com/abougouffa/minemacs/commit/6e5b5a449de3d0cadbf16b58baa061aa69cc6925)) - [@abougouffa](https://github.com/abougouffa)
- (**window**) display compilation buffers at the bottom - ([9e895d3](https://github.com/abougouffa/minemacs/commit/9e895d3799de155f0cb600356c8ca5bd12164def)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([41bef29](https://github.com/abougouffa/minemacs/commit/41bef29fca113f5dbbcfd4d902be2ed59e515c4e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([96d60e5](https://github.com/abougouffa/minemacs/commit/96d60e5f2631fce367766041f3e3c84e3fca8d01)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- (**natural-langs**) add support for `gt` - ([e116230](https://github.com/abougouffa/minemacs/commit/e116230e3db2b136592ba9413eb1e017641888db)) - [@abougouffa](https://github.com/abougouffa)
- (**obsolete**) remove `me-go-translate`, package no longer exist - ([031dc47](https://github.com/abougouffa/minemacs/commit/031dc475a33a05ec13d8adaf0c34b38877081a26)) - [@abougouffa](https://github.com/abougouffa)
- (**project-x**) steal `+def-project-mode!` from Doom Emacs - ([27c32ab](https://github.com/abougouffa/minemacs/commit/27c32abf3181b20d23537e0f153e7fc348407244)) - [@abougouffa](https://github.com/abougouffa)
- (**pulsar**) make buggy `pulsar` obsolete - ([2262e1b](https://github.com/abougouffa/minemacs/commit/2262e1b0107949a4ba74be2e74679cbaa60ff436)) - [@abougouffa](https://github.com/abougouffa)
#### Bug Fixes
- (**doom-themes**) avoid face-inheritance cycles, forbidden in Emacs 30.2+ - ([4ddba35](https://github.com/abougouffa/minemacs/commit/4ddba359dd4e07f27355c72210661b8050893a8f)) - [@abougouffa](https://github.com/abougouffa)
- (**pulsar**) remove reverence to obsolete variable - ([e0ea49a](https://github.com/abougouffa/minemacs/commit/e0ea49ae59d595f0d12d204d092aa46be7122a8b)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- (**prog**) restore `consult-eglot` - ([e0cabef](https://github.com/abougouffa/minemacs/commit/e0cabef3341341ec147678bd53ca9e83ca540f34)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([b5a24c9](https://github.com/abougouffa/minemacs/commit/b5a24c957253b6f22467a0795a075af60d45a5ce)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.15.0](https://github.com/abougouffa/minemacs/compare/55b48d2f27793fe6f152a1b6bca928f1ae4dd29a..v13.15.0) - 2025-11-16
#### Tweaks
- (**llm-ollama**) update the list of embedding models - ([55b48d2](https://github.com/abougouffa/minemacs/commit/55b48d2f27793fe6f152a1b6bca928f1ae4dd29a)) - [@abougouffa](https://github.com/abougouffa)
- (**octave**) enable `octave-mode` for `*.m` files - ([b1b90df](https://github.com/abougouffa/minemacs/commit/b1b90df5e1363d26d346d716e4c3a9fa48867a3f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0fb08ba](https://github.com/abougouffa/minemacs/commit/0fb08baa196db009bc44a87a465958c0ea59a025)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d2558c8](https://github.com/abougouffa/minemacs/commit/d2558c881c028fe5eed085b4f1890a205f02a929)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6b9625e](https://github.com/abougouffa/minemacs/commit/6b9625e18c2ff8cd87087aed70214261cafd35f9)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- make `me-robot` obsolete (at `obsolete/me-ros`) - ([eb40639](https://github.com/abougouffa/minemacs/commit/eb4063977ad7552a887c04d528013da8ad2d41d8)) - [@abougouffa](https://github.com/abougouffa)
#### Bug Fixes
- (**combobulate**) can't type "q" in `combobulate-query-mode` - ([9dbb6b3](https://github.com/abougouffa/minemacs/commit/9dbb6b389d2789d07da4d283bac3eaea2cdb8544)) - [@abougouffa](https://github.com/abougouffa)
- (**on-demand/jupyter**) use a fixed branch for `ein` - ([a887174](https://github.com/abougouffa/minemacs/commit/a887174d3cd8f34e3b75852e78af4a52f17d5464)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([d6672ab](https://github.com/abougouffa/minemacs/commit/d6672abafce986bbd4ed7db0bf02147492c092fd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.14.0](https://github.com/abougouffa/minemacs/compare/50922d3881d2534e9ad0b52c42f02e238330cfe1..v13.14.0) - 2025-11-07
#### Tweaks
- (**eat**) declare a command to open EAT in a dedicated tab - ([50922d3](https://github.com/abougouffa/minemacs/commit/50922d3881d2534e9ad0b52c42f02e238330cfe1)) - [@abougouffa](https://github.com/abougouffa)
- (**projection**) remove duplicate confusing keybinding - ([bb961b7](https://github.com/abougouffa/minemacs/commit/bb961b7222c2235ed209127652574ccbcdf7ada2)) - [@abougouffa](https://github.com/abougouffa)
- (**ws-butler**) don't enable in `diff-mode` - ([da2c8fb](https://github.com/abougouffa/minemacs/commit/da2c8fbd62e143a668f168442a1c6966c81c9029)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a0b377d](https://github.com/abougouffa/minemacs/commit/a0b377de8b9643ba0eabe59427f49219b3386aa7)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- (**editor**) restore `combobulate` to `me-editor` - ([1904ee9](https://github.com/abougouffa/minemacs/commit/1904ee92b5e406f0b5a164f9ed53fc28b94594df)) - [@abougouffa](https://github.com/abougouffa)
- (**obsolete**) cleanup obsolete modules - ([f7f0986](https://github.com/abougouffa/minemacs/commit/f7f0986b418cd7686b1693dd82489d0bc06e9e7e)) - [@abougouffa](https://github.com/abougouffa)
#### Bug Fixes
- (**diff**) Don't mess up with whitespaces in `diff-mode', they are part of the syntax - ([87c40ef](https://github.com/abougouffa/minemacs/commit/87c40effe1dd9fcb49c6138a674a4c297c13a2a2)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- (**tools**) restore `ssh-deploy` - ([8da640f](https://github.com/abougouffa/minemacs/commit/8da640fe86d711ed3971f266bb955b99877423c1)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.13.0](https://github.com/abougouffa/minemacs/compare/592607169cc591261bf227033958fedb8a007794..v13.13.0) - 2025-11-02
#### Tweaks
- (**iedit**) use `C-;` to start `iedit` from `isearch` results - ([307ebe1](https://github.com/abougouffa/minemacs/commit/307ebe1427f35068aa629f86ef18a8c857142659)) - [@abougouffa](https://github.com/abougouffa)
- (**iedit**) don't save occurrences in kill ring - ([598bfa3](https://github.com/abougouffa/minemacs/commit/598bfa3631a42cddaf92df9c69fc864306a8f4c8)) - [@abougouffa](https://github.com/abougouffa)
- (**modeline**) minor edits - ([2b6b5a7](https://github.com/abougouffa/minemacs/commit/2b6b5a714b707647bcd89be94157560eb51acc79)) - [@abougouffa](https://github.com/abougouffa)
- (**modeline**) add icons for some methods - ([4d25a21](https://github.com/abougouffa/minemacs/commit/4d25a214c3d76429a1608383093db4057c1827b6)) - [@abougouffa](https://github.com/abougouffa)
- (**modeline**) show hosts in echo - ([94b2efa](https://github.com/abougouffa/minemacs/commit/94b2efa4b84e24d950afd159e60c0eb98cb3a856)) - [@abougouffa](https://github.com/abougouffa)
- (**tramp**) persistently save ad-hoc proxies - ([59cf538](https://github.com/abougouffa/minemacs/commit/59cf538ac408627b32c3d60da2122718fa96a875)) - [@abougouffa](https://github.com/abougouffa)
- (**treesit**) better auto enabling + cleanup - ([c8af164](https://github.com/abougouffa/minemacs/commit/c8af16487438edd964f29430a874c54bb06a3703)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([b07912b](https://github.com/abougouffa/minemacs/commit/b07912b69bb7522336305ff1716290476f5e3dcc)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- (**modeline**) display multi-hop protocols in modeline, with memoization - ([5926071](https://github.com/abougouffa/minemacs/commit/592607169cc591261bf227033958fedb8a007794)) - [@abougouffa](https://github.com/abougouffa)
- (**notes**) remove `denote-menu` - ([b64604d](https://github.com/abougouffa/minemacs/commit/b64604db29719e4be72d3f168b8df19e5bca4102)) - [@abougouffa](https://github.com/abougouffa)
- (**prog**) make `consult-eglot` obsolete - ([648158b](https://github.com/abougouffa/minemacs/commit/648158b485e486437a17b0204a3b41ea260005e3)) - [@abougouffa](https://github.com/abougouffa)
- (**snippets**) restore `yasnippet-capf` - ([703e8cf](https://github.com/abougouffa/minemacs/commit/703e8cfcd5ff67e7e80018ac064787b95f3cd98f)) - [@abougouffa](https://github.com/abougouffa)
- (**tags**) make `call-graph` obsolete - ([bb4a038](https://github.com/abougouffa/minemacs/commit/bb4a03841918bfd6c2208c3765d368d8feb8d443)) - [@abougouffa](https://github.com/abougouffa)
- (**ui**) make `info-colors` obsolete - ([1db51f9](https://github.com/abougouffa/minemacs/commit/1db51f95eb76724b811661af73187858dfa20851)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- (**snippets**) make `yasnippet-capf` obsolete again - ([06a0623](https://github.com/abougouffa/minemacs/commit/06a0623536c28c2c11ac71e79912d1f5e4496a5c)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([20f5db1](https://github.com/abougouffa/minemacs/commit/20f5db1e11110c1304042c7e2b374788d1a5128a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.12.0](https://github.com/abougouffa/minemacs/compare/7eb4c8e4583411b5aef008038ad1d4f650b4f743..v13.12.0) - 2025-10-24
#### Tweaks
- (**clang-format**) add support for custom config file in version 14+ - ([2b65378](https://github.com/abougouffa/minemacs/commit/2b65378f32cbb92de741c4e7762febd1f6bee1a3)) - [@abougouffa](https://github.com/abougouffa)
- (**core**) cleanup compilation database functions - ([ee9f45d](https://github.com/abougouffa/minemacs/commit/ee9f45d8299e69357a1c32bbe68735c6c5604ded)) - [@abougouffa](https://github.com/abougouffa)
- (**core**) accept sexps in `+memoize-function` - ([2de91e4](https://github.com/abougouffa/minemacs/commit/2de91e4149df326ddb71f57abab925a33806dfee)) - [@abougouffa](https://github.com/abougouffa)
- (**docs**) regenerate the documentation - ([dee3aa7](https://github.com/abougouffa/minemacs/commit/dee3aa7588922779f6ffa4fdf1d12c394ed738a2)) - [@abougouffa](https://github.com/abougouffa)
- (**magit-gerrit**) use my fork - ([dcbf63a](https://github.com/abougouffa/minemacs/commit/dcbf63a8ac347f5b9559af38b3728cfaad35f87f)) - [@abougouffa](https://github.com/abougouffa)
- (**project-x**) more caching, this helps speeding up `magit` (WIP) - ([7eb4c8e](https://github.com/abougouffa/minemacs/commit/7eb4c8e4583411b5aef008038ad1d4f650b4f743)) - [@abougouffa](https://github.com/abougouffa)
- (**semantic**) remove unused customization - ([2bab9b1](https://github.com/abougouffa/minemacs/commit/2bab9b1923ed72a6d71b05506ed49dbe046d1630)) - [@abougouffa](https://github.com/abougouffa)
- (**treesit**) add grammar for TCL and better infer lang ID from mode - ([965a8ea](https://github.com/abougouffa/minemacs/commit/965a8eacb645526e723f270711281c1ff2396e49)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([45781b4](https://github.com/abougouffa/minemacs/commit/45781b452e687f7c1d069cdcbfe0a00503eb7e05)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([22ef7af](https://github.com/abougouffa/minemacs/commit/22ef7af06df6adb278e4f1c39b241cf9cfb55775)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5d91f62](https://github.com/abougouffa/minemacs/commit/5d91f62f4b899eebf03b7765cc095f941b27b922)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8b77bc9](https://github.com/abougouffa/minemacs/commit/8b77bc9619377528eb0890fe2d32576d9d0aa2e4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([aa76f7d](https://github.com/abougouffa/minemacs/commit/aa76f7ddc0aad0de81fd7c4bada181005fb711a8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cda46cd](https://github.com/abougouffa/minemacs/commit/cda46cd3aca94cb95e879a43d9259ddbc325ae39)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([93ccfea](https://github.com/abougouffa/minemacs/commit/93ccfea5a6720f12ea1bee6feb02945bae12e103)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([057fb59](https://github.com/abougouffa/minemacs/commit/057fb597df93426350aaf54a7c11fdf56d4e0462)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- (**prog**) make `eglot-booster` obsolete - ([755297a](https://github.com/abougouffa/minemacs/commit/755297ae3e784591f1245f6907fd4ded61f5effb)) - [@abougouffa](https://github.com/abougouffa)
- (**prog**) make `xref-union` obsolete - ([480a9df](https://github.com/abougouffa/minemacs/commit/480a9dfd55c0508a51004d5526ea79adfb4bc82c)) - [@abougouffa](https://github.com/abougouffa)
- (**tools**) make `envrc` obsolete - ([1006fd1](https://github.com/abougouffa/minemacs/commit/1006fd1bd1346248bc125c31216a8cc23c5d52d3)) - [@abougouffa](https://github.com/abougouffa)
- (**tools**) make `ssh-deploy` obsolete - ([9115cea](https://github.com/abougouffa/minemacs/commit/9115cea44ea5c787bf4d4b4e67d4209633c42325)) - [@abougouffa](https://github.com/abougouffa)
- (**tools**) make `incus-tramp`, `lxc-tramp` and `lxd-tramp` obsoletes - ([4fd76dc](https://github.com/abougouffa/minemacs/commit/4fd76dc74b49a15743bcd369cc8a78824d32115f)) - [@abougouffa](https://github.com/abougouffa)
- (**vc**) make `gerrit` obsolete - ([3ab4d6c](https://github.com/abougouffa/minemacs/commit/3ab4d6c13e52b43aeca346a224231905ef7febb6)) - [@abougouffa](https://github.com/abougouffa)
- (**vc**) make `multi-magit` obsolete - ([3f018a6](https://github.com/abougouffa/minemacs/commit/3f018a66aeba09f56f0f49a14eab3422df80e155)) - [@abougouffa](https://github.com/abougouffa)
- (**vc**) make `magit-todos` obsolete - ([942005a](https://github.com/abougouffa/minemacs/commit/942005a5330028b931baf3b00490a05246ce2417)) - [@abougouffa](https://github.com/abougouffa)
- (**vc-x**) add `+switch-git-status-buffer` - ([84cab93](https://github.com/abougouffa/minemacs/commit/84cab937997cb61989eeef0be15082137a92a72b)) - [@abougouffa](https://github.com/abougouffa)
#### Bug Fixes
- (**apheleia**) don't format remote files, don't enable if inhibited - ([6e96fa0](https://github.com/abougouffa/minemacs/commit/6e96fa000f5457b245dae8105864a4c64fcd548e)) - [@abougouffa](https://github.com/abougouffa)
- (**builtin**) fix `trailing-whitespace` face - ([2cfc5cb](https://github.com/abougouffa/minemacs/commit/2cfc5cbdd7fac4ca348a4450d65d745fd796dfef)) - [@abougouffa](https://github.com/abougouffa)
- (**pet**) disable on remote files - ([d926107](https://github.com/abougouffa/minemacs/commit/d92610727a61754a677f615edd1100534960206e)) - [@abougouffa](https://github.com/abougouffa)
- (**project**) fix a problem when opening a Git project over Tramp - ([a35e2ba](https://github.com/abougouffa/minemacs/commit/a35e2ba8be4a8e22d675bb5c8a0e7a6e7d07cdbc)) - [@abougouffa](https://github.com/abougouffa)
- (**whitespace**) `whitespace-action` was messing up `ws-butler` - ([d0b37de](https://github.com/abougouffa/minemacs/commit/d0b37de4418b965aff13900c048d6bc0a5eb184e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- (**core**) extract function call memoization helpers to `me-lib` - ([7f19902](https://github.com/abougouffa/minemacs/commit/7f19902393717d361078c5cc6ce8c17c3934df21)) - [@abougouffa](https://github.com/abougouffa)
- (**modeline**) refactor and add an option to disable sections - ([a609bf0](https://github.com/abougouffa/minemacs/commit/a609bf0f14abae0036c56629413bb7f19edd54e7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.11.0](https://github.com/abougouffa/minemacs/compare/c644869acea54693978cd8e5f792407eb44e54ff..v13.11.0) - 2025-09-17
#### Bug Fixes
- **(citre)** minor fix - ([4d9a1b3](https://github.com/abougouffa/minemacs/commit/4d9a1b3d363e839ee1672f73a47edbf13e5198a0)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** fix an unpredictable issue of `eglot` taking over `xref` and `imenu` - ([6c46cfb](https://github.com/abougouffa/minemacs/commit/6c46cfbf32bdce8ad39dcb5a3eb3be8324767aec)) - [@abougouffa](https://github.com/abougouffa)
- **(clangformat)** don't guess editor config unless the `.clangformat` file exists - ([50e613c](https://github.com/abougouffa/minemacs/commit/50e613cecce08da730c466e9190babcc206859e3)) - [@abougouffa](https://github.com/abougouffa)
- **(modeline)** fix mouse interactions - ([8fef801](https://github.com/abougouffa/minemacs/commit/8fef80166bd981e89c02208e321ef05e4c6868c2)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** don't enable in files with unresolved merge conflicts - ([7312b78](https://github.com/abougouffa/minemacs/commit/7312b78ec26dc569d8c857437789823dd9e1f5ea)) - [@abougouffa](https://github.com/abougouffa)
- remove undefined functions from autoloads file - ([69c37d9](https://github.com/abougouffa/minemacs/commit/69c37d9474d3b51708005b07740fc5cfc2075c8e)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(embedded)** remove `bitbake-ts-mode`, move grammar recipe to `me-treesit-x` - ([aa2dc7d](https://github.com/abougouffa/minemacs/commit/aa2dc7d4cc7272b5146e4c5f5df887eb62075fed)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/wiki)** replace `wikitext-mode` with `mediawiki-file-mode` - ([eb641a4](https://github.com/abougouffa/minemacs/commit/eb641a424f867df527220cb6988284a30fe4dbd2)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `vim-tab-bar` - ([ee36976](https://github.com/abougouffa/minemacs/commit/ee369764c5778138e279fa6deff102bfeac91808)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** cleanup - ([a00ee21](https://github.com/abougouffa/minemacs/commit/a00ee21fea2a352c1d148262c472abc6c7ed2dca)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** remove unneeded backend sorting advice - ([30b5254](https://github.com/abougouffa/minemacs/commit/30b52543b6c67d0dfafbe78aa54e0495dc33a163)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bitbake)** remove no more needed tweak, merged upstream - ([0ed1f05](https://github.com/abougouffa/minemacs/commit/0ed1f05858bfb47819c2ddff785d7298a9b3c56c)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** add alternative `ctrl-x` numeric keybindings suitable for AZERTY keyboards - ([ab23dce](https://github.com/abougouffa/minemacs/commit/ab23dce9ba7cd4724f2e6d5c08532e7aa62c67bf)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-gerrit)** use my fork, toggle `magit-gerrit-mode` with `_` - ([c343c1c](https://github.com/abougouffa/minemacs/commit/c343c1c4c359d64a00434c53c21858300642caec)) - [@abougouffa](https://github.com/abougouffa)
- **(modula2)** add a predicate for Modula 2 definition files - ([c644869](https://github.com/abougouffa/minemacs/commit/c644869acea54693978cd8e5f792407eb44e54ff)) - [@abougouffa](https://github.com/abougouffa)
- **(vim-tab-bar)** ensure applying correctly when in daemon mode - ([68cdab5](https://github.com/abougouffa/minemacs/commit/68cdab5cc2cee65766b29319cf6cf2976df98667)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** add display rules for `eat` buffers - ([ab13cf0](https://github.com/abougouffa/minemacs/commit/ab13cf05e2e770856d795b5b404da3a46b2e5a44)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4733701](https://github.com/abougouffa/minemacs/commit/4733701d30d858bce736d4c9117935e8f04f2616)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([c8df722](https://github.com/abougouffa/minemacs/commit/c8df722d6fb5d35f17e00b7949beaa479a376ef7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a4f023a](https://github.com/abougouffa/minemacs/commit/a4f023a357e4f8ccecfef106fa78d3f44426a971)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([405adfa](https://github.com/abougouffa/minemacs/commit/405adfacb72377c7be2d1710799dc95e847d8a62)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.10.0](https://github.com/abougouffa/minemacs/compare/6bfe727ca6ae30cc32d0b1a50eb20f12bbe62c9a..v13.10.0) - 2025-09-12
#### Bug Fixes
- **(eat)** EAT don't like smaller font sizes! - ([a25ddb8](https://github.com/abougouffa/minemacs/commit/a25ddb8bbb8c3604fab7ae10cc97e621b2bb991a)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([332d2e0](https://github.com/abougouffa/minemacs/commit/332d2e02fe7a29ef4370a6f94a033dae18161089)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(on-demand/latex)** add `xenops` - ([a12033f](https://github.com/abougouffa/minemacs/commit/a12033fa888ac3fc1f3ad50dfd43c423b4a45d10)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `ggtags` obsolete, `citre` should be sufficient - ([6ea943e](https://github.com/abougouffa/minemacs/commit/6ea943ee912f04e6be4942e25df13c6054d73bf3)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(bitbake)** move extra functions to `me-bitbake-x.el` - ([7600f6e](https://github.com/abougouffa/minemacs/commit/7600f6e68c2d3bf21367bb8a22b50e3ef841b74f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(citre)** minor tweaks - ([0600273](https://github.com/abougouffa/minemacs/commit/06002737e6624bc56fdda21de331d000d37b38ad)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** register `dumb-jump` as a Citre backend, append it to the end - ([100c623](https://github.com/abougouffa/minemacs/commit/100c62357877d7bbcd7ccf4785cbfee1da58af56)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** Make Citre manage Eglot's integration with `xref` - ([6bfe727](https://github.com/abougouffa/minemacs/commit/6bfe727ca6ae30cc32d0b1a50eb20f12bbe62c9a)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** add `+dape-gud-switch-keybinding` - ([d6d1ae0](https://github.com/abougouffa/minemacs/commit/d6d1ae0aefa4a16d8858e1e380312d0b9b555e2b)) - [@abougouffa](https://github.com/abougouffa)
- **(logview)** make the thread optional in `ulogcat-long` - ([2399ae3](https://github.com/abougouffa/minemacs/commit/2399ae3f2ea139c623fbf7026931974b34997679)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/latex)** declare `xenops` as a companion package - ([b2dfd1f](https://github.com/abougouffa/minemacs/commit/b2dfd1f5855a577165833f2afa008163f00e5613)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/logview)** add support for `ulogcat` format - ([ddd89a6](https://github.com/abougouffa/minemacs/commit/ddd89a628ce17dd37fd872d862a20d0f267ac7de)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([49b1f04](https://github.com/abougouffa/minemacs/commit/49b1f04797c4a1f072381a293cdd5106c50ed0f5)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cf27ae5](https://github.com/abougouffa/minemacs/commit/cf27ae5baacbdea4f41e3fbb7d3e3f69679b5d16)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.9.0](https://github.com/abougouffa/minemacs/compare/69eba60d2f47e1f4f5c9bf38fed8d3178c00f631..v13.9.0) - 2025-09-09
#### Bug Fixes
- **(cocogitto)** edge case, `+cocogitto-bump` fails always if it fail once - ([eadb5d6](https://github.com/abougouffa/minemacs/commit/eadb5d60dedaf4e823cf7a22be9133868f74da98)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fix `+apply-patch-dwim` - ([69eba60](https://github.com/abougouffa/minemacs/commit/69eba60d2f47e1f4f5c9bf38fed8d3178c00f631)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(tools)** restore `pet`, make `pyenv` obsolete - ([664c65f](https://github.com/abougouffa/minemacs/commit/664c65f5983ea65d762d88925be137de8c366727)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** steal config from Protesilaos Stavrou's configuration (WIP) - ([0f1c2cc](https://github.com/abougouffa/minemacs/commit/0f1c2ccf27f9799e5ac2d1aabeab46bc7055d883)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(dired-x)** don't hide special files, just stick to defaults - ([ed22481](https://github.com/abougouffa/minemacs/commit/ed224816d7c1ba75205334ab9f904a7d65855052)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(adb)** support `dired` in `+adb-push` - ([d30004d](https://github.com/abougouffa/minemacs/commit/d30004d37eb369fb6accbc3dea4324b68ed99fbb)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** don't indicate buffer boundries - ([e7715c4](https://github.com/abougouffa/minemacs/commit/e7715c411b887473021c284e0c224b16fd4362d6)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** bind F1 to `ansi-term` instead of `shell` - ([5589856](https://github.com/abougouffa/minemacs/commit/55898566d92b69ec077099a321e079d96b0136d0)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** use relative line numbers - ([01864e4](https://github.com/abougouffa/minemacs/commit/01864e4264620e45d0de707d9721aba35f363303)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** specify a group for `+dir-locals-autoreload-mode` - ([bce5cb5](https://github.com/abougouffa/minemacs/commit/bce5cb59a35e7fb11fb9438ace368b290495eca2)) - [@abougouffa](https://github.com/abougouffa)
- **(eat)** add `+eat-toggle-dwim`, bind it to F1 - ([0c1742b](https://github.com/abougouffa/minemacs/commit/0c1742bea4f069c28e1e013347bca1604892c8d6)) - [@abougouffa](https://github.com/abougouffa)
- **(empv)** automatically pick an Invidious instance - ([b34b344](https://github.com/abougouffa/minemacs/commit/b34b344cfa792670b0a3f697bbcb7e8632123562)) - [@abougouffa](https://github.com/abougouffa)
- **(python)** project-dedicated shells, no verbose indent-guess - ([ece0382](https://github.com/abougouffa/minemacs/commit/ece0382fb9be4f02e1262909d1ad2fa5feabff16)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6da310e](https://github.com/abougouffa/minemacs/commit/6da310ed15355ec6d85a4b4c3a56812918a8248f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.8.0](https://github.com/abougouffa/minemacs/compare/b4bb6dee693e43a3119a88ec26cc8ab445674658..v13.8.0) - 2025-09-08
#### Features
- **(core)** add `+diff-last-two-kills` - ([8f95753](https://github.com/abougouffa/minemacs/commit/8f9575396b5fb5985e9e59b7ede126eeb23bf033)) - [@abougouffa](https://github.com/abougouffa)
- **(debug)** add `dape-cortex-debug` - ([b4bb6de](https://github.com/abougouffa/minemacs/commit/b4bb6dee693e43a3119a88ec26cc8ab445674658)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** use `toml-ts-mode` for `Cargo.lock` - ([b643a07](https://github.com/abougouffa/minemacs/commit/b643a072f2929ea46f6b827013312b53a935b498)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** create non-existing directories on `copy-file` - ([ac91ace](https://github.com/abougouffa/minemacs/commit/ac91acefbc4fadfa892ddb67c43780c157c8a143)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** add rules for `ruby-ts-mode` and `octave-mode` - ([87fec04](https://github.com/abougouffa/minemacs/commit/87fec04321ee6c04ab9abe8e2eb1fe372f26ff3b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e9b14d6](https://github.com/abougouffa/minemacs/commit/e9b14d604c2c8b04b841ee4152ccf8c35078812f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.7.2](https://github.com/abougouffa/minemacs/compare/1991258d11f5a565381d6b1296c24b87914be3cd..v13.7.2) - 2025-09-04
#### Bug Fixes
- **(lspce)** fix the recipe - ([3d11f90](https://github.com/abougouffa/minemacs/commit/3d11f90577a105ba1d90f87eceed318484d0eda6)) - [@abougouffa](https://github.com/abougouffa)
- **(lspce)** disable on Windows - ([df8a1bc](https://github.com/abougouffa/minemacs/commit/df8a1bcc1bd53bbd3cdf19bafc476c89bd81d803)) - [@abougouffa](https://github.com/abougouffa)
- an error when using Emacs without Tree-sitter support - ([1991258](https://github.com/abougouffa/minemacs/commit/1991258d11f5a565381d6b1296c24b87914be3cd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.7.1](https://github.com/abougouffa/minemacs/compare/459d6b021b71f95137eea4cd7eb846e90a4bf15d..v13.7.1) - 2025-09-04
#### Bug Fixes
- **(editorconfig)** autoload function to avoid void function error - ([a38966d](https://github.com/abougouffa/minemacs/commit/a38966dd66e22b21e4bbcdd46e915b33b9adfc03)) - [@abougouffa](https://github.com/abougouffa)
- **(ready-player)** fix recipe - ([9aeb725](https://github.com/abougouffa/minemacs/commit/9aeb7259898aa909124460176a5fde24acfc923d)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([168c3e2](https://github.com/abougouffa/minemacs/commit/168c3e2a699336a91389c6eb63910185088229f7)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(dired)** minor edit - ([1b0f2ed](https://github.com/abougouffa/minemacs/commit/1b0f2ed2ce9bcd6e56e02324babb5fc249d3f18a)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** check for updates for third-party included packages - ([23adbd2](https://github.com/abougouffa/minemacs/commit/23adbd26b410d372e235abe9f08feff26a2d18d8)) - [@abougouffa](https://github.com/abougouffa)
- add some scheme program examples - ([b4d7048](https://github.com/abougouffa/minemacs/commit/b4d704828738ee3ec94da2c3b40ee37534ce6c54)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(files)** restore `vlf`, still useful for big files - ([4212575](https://github.com/abougouffa/minemacs/commit/4212575cb68fe4334d603ef47a2302e324a88baf)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** restore the hack of `use-package`'s conditionals - ([d052bd0](https://github.com/abougouffa/minemacs/commit/d052bd010eaaf807e4baead3be44056001dd0bd8)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(comint)** read-only prompt - ([68adece](https://github.com/abougouffa/minemacs/commit/68adece75d07b824b1d4cdc1595ff4ecc0a0ffe0)) - [@abougouffa](https://github.com/abougouffa)
- **(comint)** remove duplicate command `+comint-clear-buffer` - ([459d6b0](https://github.com/abougouffa/minemacs/commit/459d6b021b71f95137eea4cd7eb846e90a4bf15d)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** check for Tree-sitter before using ts modes - ([989ea5b](https://github.com/abougouffa/minemacs/commit/989ea5b493633271d113685b31c63dd026c5c9ba)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/cuda)** add `cuda-ts-mode` - ([4b29ae3](https://github.com/abougouffa/minemacs/commit/4b29ae3f667ba2e66a3ab9cbf786fde70b438b7e)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/erlang)** update included third-party package - ([bd89ca3](https://github.com/abougouffa/minemacs/commit/bd89ca38792f861aee43e1214f1efc59db1451c3)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/markdown)** define a loader command - ([eca87d1](https://github.com/abougouffa/minemacs/commit/eca87d1908d01b824347babd4fd3f62e6b068da7)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/rust)** minor tweaks - ([6263245](https://github.com/abougouffa/minemacs/commit/6263245278538d7e4b00743524972e0247d4c9b1)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** enable `project-switch-use-entire-map` - ([573d445](https://github.com/abougouffa/minemacs/commit/573d4457504b2a2eb899ba4b7fb70aae54693618)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([ffc0a76](https://github.com/abougouffa/minemacs/commit/ffc0a761efd132964fd45f6ceaa7c614cbb9c10c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([01cb851](https://github.com/abougouffa/minemacs/commit/01cb851f802a1c999d2d941a0784cde999758d25)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([74488d8](https://github.com/abougouffa/minemacs/commit/74488d820def0c1a5b8280be01602a442510c93f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.7.0](https://github.com/abougouffa/minemacs/compare/e02885fc59651177af0b0757ceecc7910865d6d6..v13.7.0) - 2025-08-31
#### Bug Fixes
- **(builtin)** temporary disable `yaml-ts-mode` - ([a999041](https://github.com/abougouffa/minemacs/commit/a99904185dbca156e1d0dde429a111b0c6e33a83)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(on-demand/dart)** add `dart-ts-mode` - ([4f714be](https://github.com/abougouffa/minemacs/commit/4f714be52d4c765d82eb2758c2e42433166ae8a8)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/rust)** add `rustic` - ([7a5bc25](https://github.com/abougouffa/minemacs/commit/7a5bc25b2ac0feab063a1ae7601932f2f32c6f2f)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `emacs-everywhere` obsolete - ([4d83f28](https://github.com/abougouffa/minemacs/commit/4d83f287fc60c0af10c50fa8adea507f14335e1f)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** restore `eat` - ([a9fc4da](https://github.com/abougouffa/minemacs/commit/a9fc4da913f3356c4b7d7fd5e6ef65d512cdfedc)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `vterm` and `multi-vterm` obsolete, cause Emacs to crash very often - ([e02885f](https://github.com/abougouffa/minemacs/commit/e02885fc59651177af0b0757ceecc7910865d6d6)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit)** add `+treesit-install-all-grammars` - ([3e4e1ec](https://github.com/abougouffa/minemacs/commit/3e4e1ec91a5b860338723f1c61be4cbd4f202fd0)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(on-demand)** move `add-node-modules-path` to an on-demand module - ([84a5d67](https://github.com/abougouffa/minemacs/commit/84a5d67d96d754a3bae2e22e85d18df23c178dd3)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.6.0](https://github.com/abougouffa/minemacs/compare/27eff87c69f0f2f543723678a1dd0b71417f69d4..v13.6.0) - 2025-08-28
#### Bug Fixes
- **(hideif)** fix the definition from `compile_commands.json` - ([9d812b1](https://github.com/abougouffa/minemacs/commit/9d812b14646e3ef2faee623fa4a6ee62c276f19a)) - [@abougouffa](https://github.com/abougouffa)
- **(magit)** explicitly add the `cond-let` dependency - ([8671f41](https://github.com/abougouffa/minemacs/commit/8671f41fad5eb7ecf68ed7eb58923984cd2065a1)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp-adb)** disable the extension + WIP fix - ([efac9f5](https://github.com/abougouffa/minemacs/commit/efac9f5f85544a14f82d9dad82ecdbf1e5f09d2e)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** fix default `minemacs-modules` list - ([d8c8210](https://github.com/abougouffa/minemacs/commit/d8c8210af08b3d932fd9d4cfd0b5d89cea4b2ecb)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** update the list of modules - ([74a2486](https://github.com/abougouffa/minemacs/commit/74a248678b1a89162f14399a47a6fe2908d49c23)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([4d7a089](https://github.com/abougouffa/minemacs/commit/4d7a08901867265557e774faa7d5e62c0cc60e4c)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+sudo-indicator-mode` - ([eded467](https://github.com/abougouffa/minemacs/commit/eded4673ef2bde519e76b43220c28f771702e4e4)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more compilation database helpers - ([39713ce](https://github.com/abougouffa/minemacs/commit/39713cee2f9d4ffe1e75cb2d2ea6ae83e22f8db0)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `sudo-edit` obsolete, just use `tramp-revert-buffer-with-sudo` - ([f58c3a6](https://github.com/abougouffa/minemacs/commit/f58c3a63f21f747dc03d3aabc2a66f4efad8e711)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** restore `fd-dired` - ([ab99172](https://github.com/abougouffa/minemacs/commit/ab99172d438a58868112259bd3047fdb06a4f91b)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** restore `dired-rsync`, useful when working over Tramp - ([46b02a4](https://github.com/abougouffa/minemacs/commit/46b02a4c55452aa034967f88c71cf301d513c83d)) - [@abougouffa](https://github.com/abougouffa)
- **(nav)** remove unused `isearch+` - ([45f5d3c](https://github.com/abougouffa/minemacs/commit/45f5d3c3d9bf5fa441ebf9c6c4b373b533cbb81f)) - [@abougouffa](https://github.com/abougouffa)
- **(nav)** make `treesit-jump` obsolete - ([df8b0d8](https://github.com/abougouffa/minemacs/commit/df8b0d8ef47a313b262e15148cbe97283ee813f7)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** move `apptainer-mode` to `on-demand/me-docker` - ([cf219c7](https://github.com/abougouffa/minemacs/commit/cf219c73a6041cde902b8b8ebc9e024771420383)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit)** add `me-treesit-x` with some extras - ([41e0929](https://github.com/abougouffa/minemacs/commit/41e0929343db8365117dbe64955c9fffe26382c2)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `page-break-lines` obsolete - ([2a92c00](https://github.com/abougouffa/minemacs/commit/2a92c00dd3dc4920d1ecae14cef2bb385fffa793)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `doric-themes` - ([5233006](https://github.com/abougouffa/minemacs/commit/523300650e8b747d4f6ebb5ec32d8ae3a5bc3ff3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(beardbolt)** better integration with compilation-db - ([24d9d8a](https://github.com/abougouffa/minemacs/commit/24d9d8af1a561eee87b08c742f247dcf92b705ce)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** set the default tags file location - ([aa41cfa](https://github.com/abougouffa/minemacs/commit/aa41cfad9d72871edf16a95c45258e434a149d33)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** more ignored directories when generating files list - ([74c6026](https://github.com/abougouffa/minemacs/commit/74c602653cf435e91f6750dace687df5b7e1c3d8)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** check for configurable venv directories only in Python - ([c835c68](https://github.com/abougouffa/minemacs/commit/c835c68cada3b5c7cdd2e0ab5f2086711dd5017d)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** don't extend to `xref` - ([2962b16](https://github.com/abougouffa/minemacs/commit/2962b160c30ab9921dc71aae8bf775fd4d9dd8b6)) - [@abougouffa](https://github.com/abougouffa)
- **(ffap)** enable `ffap` bindings - ([b2b416e](https://github.com/abougouffa/minemacs/commit/b2b416ef53850fc64aaab2dbb07e57dffeb5f441)) - [@abougouffa](https://github.com/abougouffa)
- **(iedit)** some extra keybindings - ([7593060](https://github.com/abougouffa/minemacs/commit/7593060e7c2f842ed5981a7f4574cd2257136f70)) - [@abougouffa](https://github.com/abougouffa)
- **(modeline)** add indicator for `dired-rsync`, support projects on dired - ([bbc81df](https://github.com/abougouffa/minemacs/commit/bbc81df573d11ef56361c8de640e26bf29f2e0e1)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-cursors)** unbind ENTER, let me insert new lines! - ([156be47](https://github.com/abougouffa/minemacs/commit/156be47a60ef5e766c69a97ebac5de2f6be24501)) - [@abougouffa](https://github.com/abougouffa)
- **(rmsbolt)** don't enforce Intel's format, useful for ARM64 targets - ([a08233a](https://github.com/abougouffa/minemacs/commit/a08233a12ea2cc9d5d9838cf82d9389137c068b9)) - [@abougouffa](https://github.com/abougouffa)
- **(rmsbolt)** better integration with compilation-db - ([14b9573](https://github.com/abougouffa/minemacs/commit/14b9573912ab38f4642676efacde9eb3026ae6df)) - [@abougouffa](https://github.com/abougouffa)
- **(smerge)** recenter by default on navigation commands - ([27eff87](https://github.com/abougouffa/minemacs/commit/27eff87c69f0f2f543723678a1dd0b71417f69d4)) - [@abougouffa](https://github.com/abougouffa)
- **(trashed)** bind to `C-c o T` - ([5a67965](https://github.com/abougouffa/minemacs/commit/5a67965451eeea0e8b91568f43399733e3e68fb5)) - [@abougouffa](https://github.com/abougouffa)
- **(whitespace)** enable the new `whitespace-page-delimiters-mode` - ([238d476](https://github.com/abougouffa/minemacs/commit/238d4769167bb601c57411cc30d1be263cfcd5a4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7ab1eb3](https://github.com/abougouffa/minemacs/commit/7ab1eb39d04ac7021ff0463a74e65466d0eed181)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([c175385](https://github.com/abougouffa/minemacs/commit/c1753850cec5a8ae35f077b73e86b57ef71a4be2)) - [@abougouffa](https://github.com/abougouffa)
- use the new definitions - ([e2d8639](https://github.com/abougouffa/minemacs/commit/e2d8639a5e9c0b741bc6dbe2e7098c61763d6f8a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e43fd73](https://github.com/abougouffa/minemacs/commit/e43fd7392a3fe4b0cf78f9aff1c8aa6c998213fe)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([6719b19](https://github.com/abougouffa/minemacs/commit/6719b1951c218af31c73ad124916e530c95c0bb3)) - [@abougouffa](https://github.com/abougouffa)
- enable lexical binding in saved system environment - ([b0d8ebb](https://github.com/abougouffa/minemacs/commit/b0d8ebbafbbb6003f7173e7584f5f8c545a68024)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.5.0](https://github.com/abougouffa/minemacs/compare/13f0e0c40b6c7748ca7bc67bee61062d5ae43534..v13.5.0) - 2025-08-19
#### Bug Fixes
- **(envrc)** fix the keybinding - ([825bb7c](https://github.com/abougouffa/minemacs/commit/825bb7c4ab33b83798c51ca39787bf80c8a13b7f)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** don't assure `nerd-icons` is available immediately - ([13f0e0c](https://github.com/abougouffa/minemacs/commit/13f0e0c40b6c7748ca7bc67bee61062d5ae43534)) - [@abougouffa](https://github.com/abougouffa)
- **(iwyu)** fix `compile_commands.json` finding in `iwyu-reparse` - ([65d059f](https://github.com/abougouffa/minemacs/commit/65d059f1fc455b329cb891133d91d3df83f001e9)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([da0204f](https://github.com/abougouffa/minemacs/commit/da0204f80cdffe3abff676c9ab6094f649b3d8e9)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add some primitives to handle `compile_commands.json` files - ([df22d23](https://github.com/abougouffa/minemacs/commit/df22d2303446263587751f8fc756e81c35a85d2d)) - [@abougouffa](https://github.com/abougouffa)
- **(iwyu)** add support for `include-what-you-use` from Exordium - ([7e2898f](https://github.com/abougouffa/minemacs/commit/7e2898fb996000bab085ce20ddaf9a0e7a88c6b4)) - [@abougouffa](https://github.com/abougouffa)
- delete unneeded `iwyu`, `clangd` can do it - ([f3c2026](https://github.com/abougouffa/minemacs/commit/f3c202626d5e7989d7539611f13a0b5e3b1dd96a)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(dumb-jump)** add a note on which ripgrep version is needed - ([91a48aa](https://github.com/abougouffa/minemacs/commit/91a48aada44452e3c563f90a880ed9d23dcb6e2e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(iwyu)** several minor tweaks - ([2696c24](https://github.com/abougouffa/minemacs/commit/2696c24dde11b4fd7bbc242a9d01d2321b46d837)) - [@abougouffa](https://github.com/abougouffa)
- rename newly added functions - ([2bd190e](https://github.com/abougouffa/minemacs/commit/2bd190ec04e389dc133530136d62960bab786596)) - [@abougouffa](https://github.com/abougouffa)
- cleanup compilation database related stuff - ([ca6ebc8](https://github.com/abougouffa/minemacs/commit/ca6ebc87d00006ff100b98fa31ca1583b78c98fa)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(eglot)** use the right `compile_commands.json` file with `clangd` - ([d7768d9](https://github.com/abougouffa/minemacs/commit/d7768d94bc882612b8ad4de6facf3ab6d7e2cb01)) - [@abougouffa](https://github.com/abougouffa)
- **(hideif)** use definitions from `compile_commands.json` - ([996fbd3](https://github.com/abougouffa/minemacs/commit/996fbd3c60706b63281f970ae7068182a3df37cf)) - [@abougouffa](https://github.com/abougouffa)
- **(semantic)** add file patterns for `c-ts-mode` and `c++-ts-mode` - ([d3e2b57](https://github.com/abougouffa/minemacs/commit/d3e2b57d2be05e8df9dbab691dee14fc16335af2)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** impose a specific order for backends - ([a815e1b](https://github.com/abougouffa/minemacs/commit/a815e1b0fc3e0afde2b58270972403e02625e22f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([554084e](https://github.com/abougouffa/minemacs/commit/554084e415adc4aac38d869ad73a51c4878c82ce)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([bbd37ab](https://github.com/abougouffa/minemacs/commit/bbd37abd61f4fdec1da0aa0fe3073d520367581e)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([808e2e8](https://github.com/abougouffa/minemacs/commit/808e2e88dbeeea1991e59224729886728d05568e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.4.0](https://github.com/abougouffa/minemacs/compare/84e6ff86e0550ac9563d0f3277b8ea9e5647741f..v13.4.0) - 2025-08-15
#### Bug Fixes
- **(org)** don't load `ditaa` in Org Babel - ([82d063d](https://github.com/abougouffa/minemacs/commit/82d063d8dc9f02d6d8b6304c3269f4250f51b7fc)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([1993cfd](https://github.com/abougouffa/minemacs/commit/1993cfd0977d968da2d447ac4da15c8314be1bc3)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(completion)** make `consult-dir` obsolete - ([06a839a](https://github.com/abougouffa/minemacs/commit/06a839afb6947445a55fa37091a068b7615966d3)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** replace `drag-stuff` with `move-dup` - ([27522d0](https://github.com/abougouffa/minemacs/commit/27522d03f534a711e99bda5f18650f68281559a2)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `highlight-numbers` obsolete - ([167fdd2](https://github.com/abougouffa/minemacs/commit/167fdd20ea836940930571ae0f648d9171a52bd1)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** make `kmacro-x` obsolete - ([0eea490](https://github.com/abougouffa/minemacs/commit/0eea4903292bebf141988af3326bac83740fcce9)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `vlf` obsolete, `lf-guard` works well with files up to 1G - ([84e6ff8](https://github.com/abougouffa/minemacs/commit/84e6ff86e0550ac9563d0f3277b8ea9e5647741f)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for C3 - ([de38a8d](https://github.com/abougouffa/minemacs/commit/de38a8df0a9fdef7b99f0a5a94291a4baff40b0f)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for Hare - ([c4f6118](https://github.com/abougouffa/minemacs/commit/c4f6118c58bb52beb74015df16d3656e88a4aacb)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** remove `org-modern-indent`, disable `org-startup-indented` - ([dd79b47](https://github.com/abougouffa/minemacs/commit/dd79b473411e80d05d15fb4f0cd7b87e15be6a01)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `lambda-themes` obsolete - ([c3bdf70](https://github.com/abougouffa/minemacs/commit/c3bdf70016db4cbdf1ffd2df4686d250a077fa09)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** minor edits - ([c359bd1](https://github.com/abougouffa/minemacs/commit/c359bd17bb67e649c34733ccd860037870438eec)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** move generic face tweaks to separate function - ([83521d0](https://github.com/abougouffa/minemacs/commit/83521d06bc05f7858de1a67799a2d4ea0f34a084)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add the `Geist` font family to the default fonts list - ([d8e241b](https://github.com/abougouffa/minemacs/commit/d8e241b4b5f03e0650e7574375a86db777ad4075)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** minor tweak - ([8f45d4f](https://github.com/abougouffa/minemacs/commit/8f45d4f270d393f6f73ff14b0e1b30d81e3b69f1)) - [@abougouffa](https://github.com/abougouffa)
- **(diff-hl)** don't draw ugly borders - ([2980ba4](https://github.com/abougouffa/minemacs/commit/2980ba4a9b2786a76e7fea162a8b8c0d2256becf)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** nicer icons in `margins` and `fringes` - ([b37bd1b](https://github.com/abougouffa/minemacs/commit/b37bd1b3abb459ea66186bd70ef8a39eeb58b9df)) - [@abougouffa](https://github.com/abougouffa)
- **(modus-themes)** minor customization - ([1500196](https://github.com/abougouffa/minemacs/commit/15001966bb2dd573dedb1a46179bb4a3ac87f0b4)) - [@abougouffa](https://github.com/abougouffa)
- **(modus-themes)** apply the custom palette only to `modus-operandi` - ([aebbf10](https://github.com/abougouffa/minemacs/commit/aebbf10ee733171ec6600e004ad33ae0c21b3f87)) - [@abougouffa](https://github.com/abougouffa)
- **(modus-themes)** minor tweaks - ([27458cb](https://github.com/abougouffa/minemacs/commit/27458cbd6fffaa4a884a03d3b08844cce7eeacab)) - [@abougouffa](https://github.com/abougouffa)
- **(modus-themes)** initial tweaks based on `lambda-themes` - ([4506e4b](https://github.com/abougouffa/minemacs/commit/4506e4b1ba2883258bec8c7d7137cf64c85e937f)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/linux)** declare `cocci-mode` extensions - ([8523a0b](https://github.com/abougouffa/minemacs/commit/8523a0b96a1969ac697efb76ba624624fd11a934)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** don't install builtin packages, I'm using a fresh Emacs build - ([86e4bbd](https://github.com/abougouffa/minemacs/commit/86e4bbd85fd3bcbf0a44c3eb3d5d4a4145fdd5f9)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([53c27d0](https://github.com/abougouffa/minemacs/commit/53c27d0e968d0c84b746a81a6b96cf73d530d2ca)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([846d26f](https://github.com/abougouffa/minemacs/commit/846d26f451d01a74312f8f630da80dbb73fa8409)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d7acf64](https://github.com/abougouffa/minemacs/commit/d7acf646714b26c13eaa2e36b64c34ce37fa6838)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4b10f1c](https://github.com/abougouffa/minemacs/commit/4b10f1c2b6b412d36e2298eb8a11c24a56b38921)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.3.0](https://github.com/abougouffa/minemacs/compare/5e175d13482fb5c6c3da0b1f8f441e68035161c3..v13.3.0) - 2025-08-06
#### Bug Fixes
- **(tramp-adb)** edge case of a remote having a minimal version of BusyBox - ([2c94a99](https://github.com/abougouffa/minemacs/commit/2c94a99bf9e0de846ab917801f6c269e9f01f77c)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([e07dfdc](https://github.com/abougouffa/minemacs/commit/e07dfdc5efb01302dfa6aa0cf4a99151eceffc2f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** remove `+what-faces`, `describe-char` do the work - ([c997092](https://github.com/abougouffa/minemacs/commit/c9970922b7255932507b1f4cc7b3d7de167e07b0)) - [@abougouffa](https://github.com/abougouffa)
- **(modus-themes)** some customization - ([a05ab54](https://github.com/abougouffa/minemacs/commit/a05ab54b6b9d8b8031e246d86071664a0d9535f2)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** embrase simplicity, add `lambda-themes` - ([5372310](https://github.com/abougouffa/minemacs/commit/537231010953fbce18d8e29a04ec0917e162b1e5)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(projection)** restore marking compilation commands as safe - ([3647e1f](https://github.com/abougouffa/minemacs/commit/3647e1fcde45d0d49e7b4cfd05003c099310beb5)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** reset the cursor to `t` - ([8bbd400](https://github.com/abougouffa/minemacs/commit/8bbd40002ccbf482d671fe75345dd9195f1cdc9a)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** add project name, minor tweaks - ([66bea4b](https://github.com/abougouffa/minemacs/commit/66bea4b6a2f1b1ebeb293797202ce928fd95cf47)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/typst)** add `ox-typst` - ([5e175d1](https://github.com/abougouffa/minemacs/commit/5e175d13482fb5c6c3da0b1f8f441e68035161c3)) - [@abougouffa](https://github.com/abougouffa)
- **(yaml-pro)** don't mess with yank - ([222a832](https://github.com/abougouffa/minemacs/commit/222a832f7fb284c5ae62d1100e1a9022d31e1a31)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.2.0](https://github.com/abougouffa/minemacs/compare/6fb9d66ed10b3fc3ad247ba7c5435321306aeff5..v13.2.0) - 2025-08-03
#### Bug Fixes
- **(core)** don't revert buffer in `minemacs-on-demand-try`, use `normal-mode` - ([d341f2e](https://github.com/abougouffa/minemacs/commit/d341f2e4087a5639e24c614d88015c32092ffffb)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor fix for `clang-format` integration with `csharp-ts-mode` - ([3039e2c](https://github.com/abougouffa/minemacs/commit/3039e2c30507dcd129b8327358de0a20adeb1919)) - [@abougouffa](https://github.com/abougouffa)
- **(lspce)** check for `cargo` before build - ([8d1595f](https://github.com/abougouffa/minemacs/commit/8d1595f25acf62a956aef9bdd69790b447983b93)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/typst)** register LSP and fix `typst-preview` dependency - ([faac20b](https://github.com/abougouffa/minemacs/commit/faac20b20240d731cfb46820a25775223176b005)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([374441f](https://github.com/abougouffa/minemacs/commit/374441fc2b35eecdcfeaa438a8e64bb715b77a90)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(completion)** remove unneeded `corfu-terminal` - ([7f5e7fa](https://github.com/abougouffa/minemacs/commit/7f5e7fa2cd05b73932e098287a99994b61f6d16b)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `wgrep` obsolete - ([74f386b](https://github.com/abougouffa/minemacs/commit/74f386bdb3c13c73c85a5272d415fb8c2871b288)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** restore `multiple-cursors` - ([7911c8d](https://github.com/abougouffa/minemacs/commit/7911c8dd72544ff01e28a9c5228a302003d9cb46)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `smartparens` obsolete, replace with `electric-pair-mode` - ([9a54fa4](https://github.com/abougouffa/minemacs/commit/9a54fa44359b32d17c80d1cf4264a0e4a7a7ff57)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** make `relint` obsolete - ([ca31942](https://github.com/abougouffa/minemacs/commit/ca31942aa32b5e5a663b6d104050818b5a56cb29)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** add `lspce` - ([2430f85](https://github.com/abougouffa/minemacs/commit/2430f855ccb3b385ceee447fc5120d5fc78e2188)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `cpp-func-impl` - ([6fb9d66](https://github.com/abougouffa/minemacs/commit/6fb9d66ed10b3fc3ad247ba7c5435321306aeff5)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** make `yasnippet-capf` obsolete - ([8f80d30](https://github.com/abougouffa/minemacs/commit/8f80d30d6cd1fd7560649426e433f8b9bd7d36bb)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** make `spdx` obsolete - ([c053132](https://github.com/abougouffa/minemacs/commit/c053132c3fa510e6cf7c538ea2478c4d934c1dde)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** make `doom-snippets` obsolete - ([625b905](https://github.com/abougouffa/minemacs/commit/625b905d16be4c15116efd695f80c7739dd58d62)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `emamux` obsolete - ([92c35e2](https://github.com/abougouffa/minemacs/commit/92c35e24d0c56954504ef38912fed0eb8310884a)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- hook the `copyright-update` locally - ([e37cdcc](https://github.com/abougouffa/minemacs/commit/e37cdcc137cd21e50614db79468a8f4df60382c0)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(mode-line)** add some comments for `me-modeline` - ([c68ffc9](https://github.com/abougouffa/minemacs/commit/c68ffc945a4383ca775c3bf757388c1d691f0b7a)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(mode-line)** rename to `minemacs-modeline-mode` - ([fcde7b9](https://github.com/abougouffa/minemacs/commit/fcde7b932a232320d077217563f9b62f1ec54315)) - [@abougouffa](https://github.com/abougouffa)
- move `tldr`, `devdocs` & `dash-docs` to `me-docs` - ([034e597](https://github.com/abougouffa/minemacs/commit/034e59786d5706d9e92eb807921e8b66f9514781)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- auto select windows on mouse hover - ([e3d25a8](https://github.com/abougouffa/minemacs/commit/e3d25a8871768ca2242348e8701f57b5e5c4f9b3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bitbake-ts-mode)** ensure installing the grammar - ([c6c2954](https://github.com/abougouffa/minemacs/commit/c6c2954ee1329cb8d4c61941abd136a3f36de245)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** bound `C-<mouse-1>` to `xref-find-definitions-at-mouse` - ([80ff53f](https://github.com/abougouffa/minemacs/commit/80ff53f5edad2331278d335fea2e2981adcc77bd)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** bind some commands under `C-z` - ([f10bec0](https://github.com/abougouffa/minemacs/commit/f10bec08cbffa471b26683ed6c11d47643e2b5fb)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** set initial major mode to `fundamental-mode` - ([e7c8251](https://github.com/abougouffa/minemacs/commit/e7c8251350d8676f23d49d1a1421f32ebeed8fca)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add TOML support in `+insert-schema` - ([ce49aa4](https://github.com/abougouffa/minemacs/commit/ce49aa49160b83cd478524b471a21ae934e09a19)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** remove unneeded code - ([7445d3f](https://github.com/abougouffa/minemacs/commit/7445d3f5f1538d5890a2989dadbe5f5c952f83f7)) - [@abougouffa](https://github.com/abougouffa)
- **(hl-todo)** replace the `FIX` keyword with `BUGFIX` - ([0a15c58](https://github.com/abougouffa/minemacs/commit/0a15c5850f8ecac622dce6d4a98319b1d1f10b9a)) - [@abougouffa](https://github.com/abougouffa)
- **(kmacro-x)** bind `C->` and `C-<` - ([0e32508](https://github.com/abougouffa/minemacs/commit/0e32508261d358e8018b24da471fcb0b3e10c6a4)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** make a constant const - ([3894a14](https://github.com/abougouffa/minemacs/commit/3894a149dbc1e65b1c0037acf1e2715e62409851)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** better generic `nerd-icons` function - ([a8fa48a](https://github.com/abougouffa/minemacs/commit/a8fa48af98390a84c473970ae8b6e443be573290)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** add an icon to signal the compilations in progress - ([247d55a](https://github.com/abougouffa/minemacs/commit/247d55afb480e7481296ff23ce2578871bb9b604)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** better integration of `iedit` and `kmacro-x` - ([489b134](https://github.com/abougouffa/minemacs/commit/489b13409f2d6cdef5211b578da0ba44d7677fd4)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** use Matlab icon for `*.m` files - ([e625269](https://github.com/abougouffa/minemacs/commit/e625269bbd04dded6c177f77ecfe04f4f29b218a)) - [@abougouffa](https://github.com/abougouffa)
- **(projection)** don't assume the commands are safe - ([0358602](https://github.com/abougouffa/minemacs/commit/03586027d25014324619ff96245889ddcf02639a)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit)** register tree-sitter grammars for 3rd party packages - ([3832300](https://github.com/abougouffa/minemacs/commit/3832300191fb414ae7fb2544d508d79d106921de)) - [@abougouffa](https://github.com/abougouffa)
- include Typst related stuff in the list of external tools - ([7d59000](https://github.com/abougouffa/minemacs/commit/7d59000e7805ac10e7b1d8a35d9436a8d3684c0d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0033035](https://github.com/abougouffa/minemacs/commit/00330357a1d59bc41c446fcce6d53afb1a90cec0)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0978686](https://github.com/abougouffa/minemacs/commit/09786863f8ffd119ff648b79ea4e4f5099fb9b59)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([eae7c98](https://github.com/abougouffa/minemacs/commit/eae7c987061692455de5185ce46462c9b922252b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.1.0](https://github.com/abougouffa/minemacs/compare/a7d698b89d0db19d722a1d179834341c39ed78e3..v13.1.0) - 2025-07-27
#### Bug Fixes
- **(builtin)** restore `auto-save` customization - ([9c2f3bf](https://github.com/abougouffa/minemacs/commit/9c2f3bf0a78144ead4e0b3864f4e97831585e8b6)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** fix `+mu4e-view-save-mail-as-pdf` - ([b3609f8](https://github.com/abougouffa/minemacs/commit/b3609f8fd5a15f6b2f76584e6d4d75cc4c83e073)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([4b9525a](https://github.com/abougouffa/minemacs/commit/4b9525a8e0056073605e9f8b96a18f079fcb1e79)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([d82c23a](https://github.com/abougouffa/minemacs/commit/d82c23a0e67368dd16844454bae36cee8b34678f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** use Chromium/Brave to convert HTML to PDF - ([3053160](https://github.com/abougouffa/minemacs/commit/3053160ef390510491e47974d39afb2fa0bc2148)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** replace `single-file` facilities with `monolith` - ([c3a778d](https://github.com/abougouffa/minemacs/commit/c3a778d3f621b7b2f9d0bbc2dd8cc8e111ad3d6b)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `rainbow-delimiters` obsolete - ([c442162](https://github.com/abougouffa/minemacs/commit/c44216218ace5b14fa1e01c8f0c98604cd9795e1)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** move `iedit` to `me-editor`, remove `me-multi-cursors` - ([5d57439](https://github.com/abougouffa/minemacs/commit/5d574399982dde28bae11667d8e333091fab3bad)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** add `kmacro-x`, maybe replace `multiple-cursors` - ([68b6ce3](https://github.com/abougouffa/minemacs/commit/68b6ce315f9e81ea0c600ea0f30514deb6d1a3a5)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-cursors)** make `multiple-cursors` obsolete - ([f96717d](https://github.com/abougouffa/minemacs/commit/f96717da18651d7a2d7421f9549da001daa44a4b)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** cleanup unneeded stuff - ([e0ed344](https://github.com/abougouffa/minemacs/commit/e0ed34419e88768e8a60a6976f45370de925045f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove some unneeded stuff - ([a9355d7](https://github.com/abougouffa/minemacs/commit/a9355d7d30ce2638d025b155612f4baa7d583a4c)) - [@abougouffa](https://github.com/abougouffa)
- **(dtrl-indent)** minor tweaks in `+dtrt-indent-tab-to-tab-stop` - ([b0f81fc](https://github.com/abougouffa/minemacs/commit/b0f81fc4d82a0f40f2aa937e6796b9cdad3e2699)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** simplify and add `me-modeline-mode` - ([ce0469f](https://github.com/abougouffa/minemacs/commit/ce0469f483f749fa1266deaafe936fe782e2e901)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** minor cleanup - ([4049620](https://github.com/abougouffa/minemacs/commit/404962032ca258cab4a07ac1d2d093e057f6c881)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(highlight-selection)** boxed looks ugly on multi-line regions - ([3f91020](https://github.com/abougouffa/minemacs/commit/3f910204d4f7aee2d039c142873e7761b6d864f9)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** minor tweak - ([afb6b48](https://github.com/abougouffa/minemacs/commit/afb6b48aa38cb613e7b09c4bab2c70ffe70e967d)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** update the programs list - ([0ecc778](https://github.com/abougouffa/minemacs/commit/0ecc778c32fc23d5d94ff2ba765b9a1469f2bd24)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-selection)** better looking face for highlighted regions - ([6e5dddc](https://github.com/abougouffa/minemacs/commit/6e5dddc568bed16e0dc74d2edb99d8d4b1e1049b)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** better looking mode-line - ([c6afac5](https://github.com/abougouffa/minemacs/commit/c6afac5e423c6620a471c373eabb6acfb4817494)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** minor edits - ([5a8b0d3](https://github.com/abougouffa/minemacs/commit/5a8b0d3ff60718028fd21798bcb6a9ded9434b62)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** shorten branch names longer than 15 chars - ([0deb28d](https://github.com/abougouffa/minemacs/commit/0deb28daa867bf6affeb730c5917d27e4fa3e34e)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** better icon for dedicated buffers - ([a7d698b](https://github.com/abougouffa/minemacs/commit/a7d698b89d0db19d722a1d179834341c39ed78e3)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** cleanup `project-vc-extra-root-markers` - ([867b860](https://github.com/abougouffa/minemacs/commit/867b860123fc145b4c238a34825b43fbd96962a5)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** remove `requirements.txt` from project markers - ([53968aa](https://github.com/abougouffa/minemacs/commit/53968aab80e332e0d3adba1c658133a0d44feccc)) - [@abougouffa](https://github.com/abougouffa)
- **(recentf)** don't exclude remote files from the saved recent files - ([1d56aa2](https://github.com/abougouffa/minemacs/commit/1d56aa2f08f819afae9e72c3c0d0a6a495726f7c)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** follow symlinks - ([15af03b](https://github.com/abougouffa/minemacs/commit/15af03b99f43efb6178aa3978d684fe88a84a824)) - [@abougouffa](https://github.com/abougouffa)
- **(vundo)** remove duplicates (ex. `vundo-previous` uses `vundo-next`) - ([4499a7d](https://github.com/abougouffa/minemacs/commit/4499a7d2b2118e6be7f4f3c934de361d61f3f632)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4544dd4](https://github.com/abougouffa/minemacs/commit/4544dd495242b7699a714624cb7f35af08e90959)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([abeeebd](https://github.com/abougouffa/minemacs/commit/abeeebddc5bb1209ce52c04835d80402230f25d0)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cfd2efe](https://github.com/abougouffa/minemacs/commit/cfd2efe668c8c3de7422199bc65bc00b5cab98ca)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v13.0.0](https://github.com/abougouffa/minemacs/compare/b5accf003652d669911a95f2111c2f7d4c90e120..v13.0.0) - 2025-07-23
#### Bug Fixes
- **(builtin)** disable `visual-wrap` until properly identify the issue - ([740c32b](https://github.com/abougouffa/minemacs/commit/740c32b471556dc23ae0adaaa937501ee24748a3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fix compatibility with the builtin `editorconfig` - ([24dd252](https://github.com/abougouffa/minemacs/commit/24dd25299117b39a702644eb34f34525c7bc54ce)) - [@abougouffa](https://github.com/abougouffa)
- **(diff-hl)** don't pollute the shared memory devfs - ([8a2ac39](https://github.com/abougouffa/minemacs/commit/8a2ac3991727d87992da0acda81f7447f9d23232)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** fix the advices - ([51125da](https://github.com/abougouffa/minemacs/commit/51125da365111667ae9084b21281a129a0df1361)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-numbers)** take into account negative numbers - ([29970ff](https://github.com/abougouffa/minemacs/commit/29970ff5b69a1230da7ba701c262e3d2d3de76cf)) - [@abougouffa](https://github.com/abougouffa)
- **(ligature)** disable buggy ligatures for `<<<` and `>>>` - ([5279e67](https://github.com/abougouffa/minemacs/commit/5279e67d145e719d1c9356cebf3cbceb80121a74)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** add a space between `flymake` icons - ([ea91d58](https://github.com/abougouffa/minemacs/commit/ea91d5804c1c3801ac39c6aa98e3673be68e3571)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** use always a box for fake cursors - ([ff784cf](https://github.com/abougouffa/minemacs/commit/ff784cf90d48709787cc44b05bb10243635b53f6)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** better bindings - ([3abe4a7](https://github.com/abougouffa/minemacs/commit/3abe4a7b013c6e46856d9d896f66547070b08cd5)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** don't create paths like `/some/path/to/./proj` - ([b5accf0](https://github.com/abougouffa/minemacs/commit/b5accf003652d669911a95f2111c2f7d4c90e120)) - [@abougouffa](https://github.com/abougouffa)
- **(pyenv)** make sure to use the right path, suppress activation message - ([dbb6688](https://github.com/abougouffa/minemacs/commit/dbb6688e1f655b1c48966be0c8c86fa693641ea7)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make sure to decorate `tab-bar` and `mode-line` in Emacs daemon - ([0e16585](https://github.com/abougouffa/minemacs/commit/0e16585dd0a2faa627b38b0b07ee53586886adda)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([e518f86](https://github.com/abougouffa/minemacs/commit/e518f86735dcfd6ef74c0e554c0eafe3c56b4d6b)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([7328b0b](https://github.com/abougouffa/minemacs/commit/7328b0b1578e8dc407f9412a9b292b32c9aceb79)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `minemacs-prefix-map` bound to `C-z` - ([f5a12a8](https://github.com/abougouffa/minemacs/commit/f5a12a8b544a3b2e712a69d9843d7d3177ecacc2)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+describe-at-point` - ([50bc584](https://github.com/abougouffa/minemacs/commit/50bc5846bb10d1f340fbe72f7e431cd2b8ad5f35)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** make `helpful` obsolete - ([c1dc1be](https://github.com/abougouffa/minemacs/commit/c1dc1be3f3e02f9b692350083bea971fbcb028bd)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `trashed` - ([48d589a](https://github.com/abougouffa/minemacs/commit/48d589ad982bfa62f6fb07d3c208c55af3ca647e)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `fd-dired` obsolete - ([b9f1f6f](https://github.com/abougouffa/minemacs/commit/b9f1f6fb529e48153ddd21de9f99312cbd204a76)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `dired-rsync` - ([b607815](https://github.com/abougouffa/minemacs/commit/b607815087b6c50bdb5b18b74d02cbc01f672382)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/linux)** move `cocci` to `on-demand/me-linux` - ([071fe1a](https://github.com/abougouffa/minemacs/commit/071fe1a5f2a0f3fd26bf169059070e7dab1644eb)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/python)** don't fetch data for completion in `pip-requirements` - ([69d11e0](https://github.com/abougouffa/minemacs/commit/69d11e0e37f19abc23e1866487272e7014e3e817)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `simpc-mode` - ([06118e0](https://github.com/abougouffa/minemacs/commit/06118e02447d09209c82b62be3ebfea784bdc94b)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `quickrun` obsolete - ([e57efe9](https://github.com/abougouffa/minemacs/commit/e57efe90b086320d4de9412e4817ae0d0f983607)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `gambol` obsolete - ([acd927d](https://github.com/abougouffa/minemacs/commit/acd927d6a84de7e9772f0d7ec57339a6b2501f69)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `sr-speedbar` obsolete - ([ebfeacc](https://github.com/abougouffa/minemacs/commit/ebfeacc53d3f4a71cd50467332368b6cb4adfefa)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `treesit-auto` obsolete - ([3601eb9](https://github.com/abougouffa/minemacs/commit/3601eb9e51e8a480589952d8326a9a8a93564a9d)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** make `ffip` obsolete - ([85f1824](https://github.com/abougouffa/minemacs/commit/85f18247733512ae4432eee202f785fc18b0e8c1)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `pyenv` - ([a0a1676](https://github.com/abougouffa/minemacs/commit/a0a1676d47415dead1495855ad3da6e4793afa36)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `pet` obsolete, too slow - ([84c3f63](https://github.com/abougouffa/minemacs/commit/84c3f638983b5aea16cac1dfdfda4974b37e49f7)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `enlight` obsolete, rarely used - ([c226076](https://github.com/abougouffa/minemacs/commit/c226076fab87ee842a2869f15298ff80215f8c2e)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `doom-modeline` obsolete, beautiful but slows things down - ([10ebbf0](https://github.com/abougouffa/minemacs/commit/10ebbf076a46e81bb5e21e4ece4d0f51241d93c5)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `solaire-mode` obsolete - ([f085dee](https://github.com/abougouffa/minemacs/commit/f085dee4d616297df07007ac64b86f56ddd0c584)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** remove `doom-two-tone-themes` - ([a6544a4](https://github.com/abougouffa/minemacs/commit/a6544a443e78120ab942b416debf9fd3bb86efcf)) - [@abougouffa](https://github.com/abougouffa)
- steal the mode-line customization from Protesilaos' config - ([2fb3109](https://github.com/abougouffa/minemacs/commit/2fb3109b1f22ba37ff9b84cf3e68554bd1e39b45)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** drop compatibility with Emacs 29 - ([803d59a](https://github.com/abougouffa/minemacs/commit/803d59a3424c9cab978e852fe2178772d4603337)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(core)** untabify - ([171ee66](https://github.com/abougouffa/minemacs/commit/171ee666d50362c54fc74e95992208a44c555e92)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(bookmark)** move `+bookmark-set-at-mouse` to `me-lib-x` - ([357b41a](https://github.com/abougouffa/minemacs/commit/357b41ac57a5d5af87c4a3c8228477176f2966a4)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** cleanup and simplify - ([279172c](https://github.com/abougouffa/minemacs/commit/279172c20d5fc0936db79fc384178c5f966302c7)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** minor refactoring - ([18b770f](https://github.com/abougouffa/minemacs/commit/18b770f4d2138f664a3a9369072da5f8ffe4fc23)) - [@abougouffa](https://github.com/abougouffa)
- **(casual)** remove `+casual-smerge-tmenu` - ([735e71f](https://github.com/abougouffa/minemacs/commit/735e71f6731416e50f769105545e3adc7b46190f)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** minor refactoring - ([b04a594](https://github.com/abougouffa/minemacs/commit/b04a594ffb2b33559525951eb072bfc28dab1090)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** minor edit - ([cec5609](https://github.com/abougouffa/minemacs/commit/cec560903a81301caa4f3467e5c0d4abe0c764a4)) - [@abougouffa](https://github.com/abougouffa)
- **(sudo-edit)** remove `+sudo-edit-save` - ([272e3fe](https://github.com/abougouffa/minemacs/commit/272e3feb30d3ab7a83b4672ff6bd58050a4f3d57)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** minor tweaks - ([f7cb023](https://github.com/abougouffa/minemacs/commit/f7cb0233e7d34adefafdaeb5fda221ea963694ed)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** set `cursor-type` to `bar` - ([2ab1603](https://github.com/abougouffa/minemacs/commit/2ab1603b5298cb8c36ade5fdb66f55fddd51b5e1)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** use `conf-mode` to view `*.rc` files - ([d83cb6c](https://github.com/abougouffa/minemacs/commit/d83cb6c29b2bdbb2df85447cbfd4e7e9e97c1adf)) - [@abougouffa](https://github.com/abougouffa)
- **(c-ts-mode)** enable Doxygen support - ([82baa8f](https://github.com/abougouffa/minemacs/commit/82baa8f9a2db616e171292f2ec338a9dc1cedcb3)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** bind `+citre-navigation-map` to `C-z C-p` - ([438e0cd](https://github.com/abougouffa/minemacs/commit/438e0cd949c2037bfb2d259e7a07f3be54ce6150)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** remove unneeded, duplicate and unused keybindings - ([0f28579](https://github.com/abougouffa/minemacs/commit/0f28579b5b7f835944bb0ad4676883c5657cb019)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** don't suppress messages when calling commands interactively - ([e42de17](https://github.com/abougouffa/minemacs/commit/e42de179dd141013267060664d602d89f4aaeb34)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** handle the inactive mode-line in `+subtle-mode-line` - ([8bf7b08](https://github.com/abougouffa/minemacs/commit/8bf7b082c3764b1ce9603a5881b0df5ed061c5cf)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+project-have-compile-commands-p` + usage example - ([530d971](https://github.com/abougouffa/minemacs/commit/530d97176d52589fd57d8ad96990545b34e932e8)) - [@abougouffa](https://github.com/abougouffa)
- **(crux)** add some recommended keybinding to duplicate line or region - ([f3f000e](https://github.com/abougouffa/minemacs/commit/f3f000e31713a87b74a473f353fb1eadfdbe10e2)) - [@abougouffa](https://github.com/abougouffa)
- **(dired)** hide details and enable `hl-line-mode` - ([bcd1cee](https://github.com/abougouffa/minemacs/commit/bcd1ceeb1d5b3c5f423dc7716fe0f76df3c1b25c)) - [@abougouffa](https://github.com/abougouffa)
- **(dired)** better listing switches - ([91102d2](https://github.com/abougouffa/minemacs/commit/91102d2eb6be010a7c727c239ef00b4095a030db)) - [@abougouffa](https://github.com/abougouffa)
- **(dired-hacks)** bind `dired-subtree` commands - ([5c720a4](https://github.com/abougouffa/minemacs/commit/5c720a4023236d251aa4e95eb750f2eb0f9c966b)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** suppress `eglot` and `jsonrpc` messages - ([8b88598](https://github.com/abougouffa/minemacs/commit/8b8859813ae2ceedce80780f283397ca089400c8)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** advertise cancellation - ([c97a183](https://github.com/abougouffa/minemacs/commit/c97a183f2ad15b85e2ebb52c5b426938f79cdf68)) - [@abougouffa](https://github.com/abougouffa)
- **(elisp-mode)** mark MinEmacs files as trusted - ([e36e980](https://github.com/abougouffa/minemacs/commit/e36e9807ca1ca5c33529ad6877a13f84ef9a5895)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** minor tweaks - ([3d0b050](https://github.com/abougouffa/minemacs/commit/3d0b050808fc7d9f5a2deb501579cffca54d1a68)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** use red icon when editing files as root - ([001f6a3](https://github.com/abougouffa/minemacs/commit/001f6a30a43ad668e5c33078ace65a6c309beec4)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** several tweaks and fixes - ([1c79c78](https://github.com/abougouffa/minemacs/commit/1c79c78a8752f2d5909f24b0148713aa198013ca)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** integrate `multiple-cursors` and `iedit` - ([75baf33](https://github.com/abougouffa/minemacs/commit/75baf33991af416c57bee9ab5f0d90f206904d57)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** minor tweaks - ([d21017f](https://github.com/abougouffa/minemacs/commit/d21017f75b785524a3dfaf7619fd781f6cf7553c)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** minor tweaks - ([460af1d](https://github.com/abougouffa/minemacs/commit/460af1d3e76a2147b73be0cb725a596366186977)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** better UI for the mode-line based on `prot-modeline` - ([9f16a2d](https://github.com/abougouffa/minemacs/commit/9f16a2dd9f633d2d04c913e4c474b042602aebfb)) - [@abougouffa](https://github.com/abougouffa)
- **(mode-line)** better looking mode-line (with `nerd-icons`) - ([864379c](https://github.com/abougouffa/minemacs/commit/864379caa42f5969f5c51cb8918165c7569480ea)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** remove unused `+mc/transient` - ([5ea7f23](https://github.com/abougouffa/minemacs/commit/5ea7f235089f82d0bf9a47523dd3a39fd355cb47)) - [@abougouffa](https://github.com/abougouffa)
- **(pet)** don't assume all ADB systems don't support `file-attributes` - ([506becc](https://github.com/abougouffa/minemacs/commit/506becc9822fc54f51f83d685998a58f6985b8c8)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** ensure loading the `projectile` wrapper functions - ([f79b568](https://github.com/abougouffa/minemacs/commit/f79b568e66fb6584c29a47d90c23017e479a980a)) - [@abougouffa](https://github.com/abougouffa)
- **(projection)** bind `projection-find-other-file` to `C-x P f` - ([3700157](https://github.com/abougouffa/minemacs/commit/3700157358670e44d68a63ee7dd7003bb76ba67d)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** pulse on region changes - ([98436b5](https://github.com/abougouffa/minemacs/commit/98436b510ed3531b678a18ac3d598e4be5626465)) - [@abougouffa](https://github.com/abougouffa)
- **(recentf)** don't bind `C-c c r`, already done on `C-c f` - ([7c08d46](https://github.com/abougouffa/minemacs/commit/7c08d46d5a2743e640929dec9b0b7d35802f5836)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** simpler tab-bar, inspired by `vim-tab-bar` - ([182ac97](https://github.com/abougouffa/minemacs/commit/182ac97af5d28390e0606f23ec6e4f425351782e)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit)** remap more modes - ([8e0683a](https://github.com/abougouffa/minemacs/commit/8e0683a434f5891f951e5170c18b51e142d09f3a)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit)** remap more modes - ([9faa2d5](https://github.com/abougouffa/minemacs/commit/9faa2d51cbaee7756e0df4f089e3d9a0fbaca6a8)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit)** remap `python-mode` to `python-ts-mode` - ([14b6021](https://github.com/abougouffa/minemacs/commit/14b60210404db8c8adb91f8133fe6cce54865e99)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit)** prefer tree-sitter based modes - ([7d8b251](https://github.com/abougouffa/minemacs/commit/7d8b25161c0dacb59ca1bc451cff9b9ad16b5c94)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([750c735](https://github.com/abougouffa/minemacs/commit/750c735774271fa79599a85254f184fd77a24457)) - [@abougouffa](https://github.com/abougouffa)
- auto select windows on mouse hover - ([67e2ec6](https://github.com/abougouffa/minemacs/commit/67e2ec658c91eaec8e64ba1e3d65f84c92cf797a)) - [@abougouffa](https://github.com/abougouffa)
- decrease internal border width - ([d1f4bf7](https://github.com/abougouffa/minemacs/commit/d1f4bf7da7423dcb13574d60354bab9121727f3e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5ba591b](https://github.com/abougouffa/minemacs/commit/5ba591b862b0748707e056dbdd401c27c0c569b1)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.42.1](https://github.com/abougouffa/minemacs/compare/e8a01ac92c078ad686538b6a494d423fde913bc6..v12.42.1) - 2025-07-13
#### Bug Fixes
- **(casual)** fix the binding of `casual-eshell-tmenu` - ([de46aa6](https://github.com/abougouffa/minemacs/commit/de46aa6a50ea11705f737b3a3d1f7e13bb4e01a0)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- minor comment edits - ([240f4a2](https://github.com/abougouffa/minemacs/commit/240f4a25000f814b2e2a6adbe665ccb7ab2ac406)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(multiple-cursors)** make `transient-noop` run once - ([fb11a92](https://github.com/abougouffa/minemacs/commit/fb11a923e0037b3e60cd05749aeb9a0213aa95da)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** don't modify the behavior of `project-name` - ([c925c73](https://github.com/abougouffa/minemacs/commit/c925c731d78486889dc37bb96bfcea9b7e154b11)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** add an option to disable using `fd` in remote projects - ([e8a01ac](https://github.com/abougouffa/minemacs/commit/e8a01ac92c078ad686538b6a494d423fde913bc6)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d1b2723](https://github.com/abougouffa/minemacs/commit/d1b2723b927d7690e30681beb035647787ffba30)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.42.0](https://github.com/abougouffa/minemacs/compare/bc252202c16d0c2fb2ca4f2d36ae0998afe6d370..v12.42.0) - 2025-07-11
#### Bug Fixes
- **(gee)** add support for basic HTTP authentication - ([bc25220](https://github.com/abougouffa/minemacs/commit/bc252202c16d0c2fb2ca4f2d36ae0998afe6d370)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** ensure loading the package correctly - ([734dc9a](https://github.com/abougouffa/minemacs/commit/734dc9a17c88a71f3f428bf61913536249ede4bc)) - [@abougouffa](https://github.com/abougouffa)
- **(pet)** don't enable when editing files via ADB - ([276d189](https://github.com/abougouffa/minemacs/commit/276d189fbefd25725142b27cf36a74ee66cae9a1)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** Alchemy modules aren't necessarily separate projects - ([8e889ba](https://github.com/abougouffa/minemacs/commit/8e889ba6fac6d03f1a9dc0c81900f917c591f1a7)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(tramp-x)** enhance TRAMP performances WIP - ([d310c7e](https://github.com/abougouffa/minemacs/commit/d310c7e21f20c2206494ced8c4c69b37479621ae)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add support for `gerrit` via REST API - ([63d4b89](https://github.com/abougouffa/minemacs/commit/63d4b890d0124ae13467d3ed828eec72e09d005e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(project-x)** simplify finding the `fd` executable - ([1c4bdaa](https://github.com/abougouffa/minemacs/commit/1c4bdaa059890512c04f5cf936d7f538f6083506)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(tramp-x)** don't memoize TRAMP projects - ([9b8f629](https://github.com/abougouffa/minemacs/commit/9b8f62952aa3d2bc8723cbbd6037769a012fcfe2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** enable line numbers in `authinfo-mode` - ([5a0f0d0](https://github.com/abougouffa/minemacs/commit/5a0f0d0706abd84f0dc245702fd3c516848e7905)) - [@abougouffa](https://github.com/abougouffa)
- **(casual)** bind the new `casual-eshell-tmenu` - ([c4b751c](https://github.com/abougouffa/minemacs/commit/c4b751c66bda40ed25f5511389c10d96503308be)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** add some more commands to run only once - ([8700ee8](https://github.com/abougouffa/minemacs/commit/8700ee816ebb04e1ac334b9290a51b0232ffa25c)) - [@abougouffa](https://github.com/abougouffa)
- **(reverso)** enable persistent history - ([1bb01de](https://github.com/abougouffa/minemacs/commit/1bb01dea494fc6d29d6013b4f17b1ac0931090bb)) - [@abougouffa](https://github.com/abougouffa)
- **(reverso)** bind to `C-o r` - ([e5e9bae](https://github.com/abougouffa/minemacs/commit/e5e9bae929f10b3ff29de7b0720618f5c49606bd)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([fa60b46](https://github.com/abougouffa/minemacs/commit/fa60b46ea01c5453b1d6d15671b9ded6f92aca57)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.41.0](https://github.com/abougouffa/minemacs/compare/c02765381b815d0866c3507d945ad6e3d4e131a3..v12.41.0) - 2025-07-09
#### Bug Fixes
- **(core)** check for disabled when using only built-in packages - ([d8a0493](https://github.com/abougouffa/minemacs/commit/d8a0493cdf3d137d7081ceb6cc4f787662669d00)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** don't use FD when in remote server - ([4107abe](https://github.com/abougouffa/minemacs/commit/4107abee92a22ae905eea57f0fea7189c014cb6d)) - [@abougouffa](https://github.com/abougouffa)
- **(projection)** disable `projection-hook`, buggy on ADB over Tramp - ([123ed36](https://github.com/abougouffa/minemacs/commit/123ed36c450f6ddca38cb27c0d0ad4faaef290b9)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(tags)** minor comment edits - ([82f74f0](https://github.com/abougouffa/minemacs/commit/82f74f0b7e8276ed68020260af6407f6851e5966)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([29bb6a5](https://github.com/abougouffa/minemacs/commit/29bb6a50ee4c9456d3fde75b7739d85fe05c48df)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** make `symbol-overlay` obsolete - ([1886102](https://github.com/abougouffa/minemacs/commit/18861020d79c32384aa2c1e93c44d983cceeb93f)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make unused `crdt` obsolete - ([b4cb2da](https://github.com/abougouffa/minemacs/commit/b4cb2dacf419f8c2f15aa42fb555d8fc1781774a)) - [@abougouffa](https://github.com/abougouffa)
- **(email)** make `org-mime` obsolete - ([d0e2ad3](https://github.com/abougouffa/minemacs/commit/d0e2ad388c3814d1b465e518cced1cf947908f5f)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** make `webkit` obsolete - ([ba03156](https://github.com/abougouffa/minemacs/commit/ba03156dc2b7dfda49431fc76b7541c4b46ffbe1)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** make `eglot-x` obsolete - ([c027653](https://github.com/abougouffa/minemacs/commit/c02765381b815d0866c3507d945ad6e3d4e131a3)) - [@abougouffa](https://github.com/abougouffa)
- **(fun)** make `me-fun` obsolete - ([25f2962](https://github.com/abougouffa/minemacs/commit/25f29626d141411a4b2ad0cb1020b4e2095a1c5b)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** make unused `clue` obsolete - ([bd15756](https://github.com/abougouffa/minemacs/commit/bd15756528104213f04c222660e75a3fb907627a)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** make `ox-hugo` obsolete - ([46b40d0](https://github.com/abougouffa/minemacs/commit/46b40d04a26e774d8ce052e1df7fd90726fb7748)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `reformatter` obsolete - ([58cc571](https://github.com/abougouffa/minemacs/commit/58cc571c4d8e2f7f141dfed90e257c9d1a9bee44)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** move `dash-docs` and `consult-dash` to `me-prog` - ([9b8b952](https://github.com/abougouffa/minemacs/commit/9b8b9520f551c36ff5c88d040d094de9fe8c69d8)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `xcscope` and `consult-cscope` obsolete - ([47731c7](https://github.com/abougouffa/minemacs/commit/47731c7450df321e60f970449edcf1c2ad7f73d7)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `clink` obsolete - ([bc77d03](https://github.com/abougouffa/minemacs/commit/bc77d03b65aa4f38b7ed8ccd4694ba3d4e6555d3)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `symbol-overlay` and its dependencies obsolete - ([566f26b](https://github.com/abougouffa/minemacs/commit/566f26b9763994e0b42296b2063cb629a2177165)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult)** enable consult extensions only when consult is enabled - ([5a1a8b4](https://github.com/abougouffa/minemacs/commit/5a1a8b48d53bc6441b1fe6734ec8f15e32bcf880)) - [@abougouffa](https://github.com/abougouffa)
- **(embark)** prefer `embark` over `which-key` - ([6fb8ea4](https://github.com/abougouffa/minemacs/commit/6fb8ea41947a64ae40aab9772f614c13ecea7f90)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** add support for running remote `fd/fdfind` - ([2673b14](https://github.com/abougouffa/minemacs/commit/2673b1475c9fc699baf45d8a642ad15b633fc09b)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** don't change the default remote shell - ([295d4c7](https://github.com/abougouffa/minemacs/commit/295d4c73d2b3bfc583a85b9728c1d1e5387b2d4f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0218223](https://github.com/abougouffa/minemacs/commit/0218223d91e1082dac4d69d2abc817fe189295dd)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([f4ac6d6](https://github.com/abougouffa/minemacs/commit/f4ac6d67990243556528290f6e302d2557afa777)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.40.0](https://github.com/abougouffa/minemacs/compare/5e4984e134ede78aa7eebd9102d5e571232626d0..v12.40.0) - 2025-07-07
#### Bug Fixes
- **(on-demand/maxima)** ensure adding `maxima-mode` to `auto-mode-alist` - ([cae0f80](https://github.com/abougouffa/minemacs/commit/cae0f8003ac9bdf25b0e9da5d1125ca1e923960e)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/ocaml)** ensure adding `dune-mode` in `auto-mode-alist` - ([ce196c7](https://github.com/abougouffa/minemacs/commit/ce196c775e2cfa6417f8d6e43c4a42f82a7996f5)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/protobuf)** ensure adding `protobuf-mode` to `auto-mode-alist` - ([f759370](https://github.com/abougouffa/minemacs/commit/f759370bdf74ed7d53ccb2115b0d957baccb36c6)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([39c3dfa](https://github.com/abougouffa/minemacs/commit/39c3dfa8c274d99d5438f55bffa9e3e832fdb83b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(on-demand/logs)** make `logview` an on-demand module - ([20c4e48](https://github.com/abougouffa/minemacs/commit/20c4e483926fec964c84f9d476ac77b5782a21a8)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/systemd)** make `systemd` an on-demand module - ([9fdeb24](https://github.com/abougouffa/minemacs/commit/9fdeb24a76c5a60b21ab8f9166690502ea58a1a7)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make unused `eat` obsolete - ([8a0698e](https://github.com/abougouffa/minemacs/commit/8a0698e06923275c183faae098ead2c3bf6e68c6)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** rename `me-lib-extra` to `me-lib-x` - ([c2db8b3](https://github.com/abougouffa/minemacs/commit/c2db8b31b7c770309d141c3660c76dccb5eca6b7)) - [@abougouffa](https://github.com/abougouffa)
- **(jinx)** simplify `+spellcheck-mode` - ([6a65d87](https://github.com/abougouffa/minemacs/commit/6a65d87678399bf684f90460aeb2536b10e05192)) - [@abougouffa](https://github.com/abougouffa)
- **(ob-ditaa)** minor edit - ([caa3056](https://github.com/abougouffa/minemacs/commit/caa30564320120ceb6fb9a530ae7d29a2b1cfff7)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** cleanup config, stick to some defaults - ([5e4984e](https://github.com/abougouffa/minemacs/commit/5e4984e134ede78aa7eebd9102d5e571232626d0)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** don't auto format when there is unresolved Git conflicts - ([c94f70e](https://github.com/abougouffa/minemacs/commit/c94f70e43f9661c458a5c61a04dd6d637b6044e3)) - [@abougouffa](https://github.com/abougouffa)
- **(git)** allow multiple types for prefixed commits - ([32bb622](https://github.com/abougouffa/minemacs/commit/32bb6222d1923254c77d5458d715a7a18feec1dc)) - [@abougouffa](https://github.com/abougouffa)
- **(org-x)** minor tweaks and cleanups - ([e43a9aa](https://github.com/abougouffa/minemacs/commit/e43a9aa9785a6321521706ad9ab815a628dcf82a)) - [@abougouffa](https://github.com/abougouffa)
- **(ox-latex)** cleanup unused customizations - ([df1bfe7](https://github.com/abougouffa/minemacs/commit/df1bfe7f84d925f365715507d0521c34c51af928)) - [@abougouffa](https://github.com/abougouffa)
- **(rainbow-delimiters)** minor edit - ([13f9e02](https://github.com/abougouffa/minemacs/commit/13f9e02037513175d8f36b73734fd8e0fe6611f1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8635481](https://github.com/abougouffa/minemacs/commit/86354817864d537478f8460d6556cdb0eecbfbae)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([4aeb117](https://github.com/abougouffa/minemacs/commit/4aeb1175b4286ed2c47f312a24783bc8cac43a08)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.39.2](https://github.com/abougouffa/minemacs/compare/13eaab119f0a1140b67bc72d1ff7114de52bcb6e..v12.39.2) - 2025-07-05
#### Bug Fixes
- **(dired-rsync)** fix the binding to `dired-mode-map` - ([3e2f3da](https://github.com/abougouffa/minemacs/commit/3e2f3da43b2dd16bfacf1f577967d4f9159551cc)) - [@abougouffa](https://github.com/abougouffa)
- **(dumb-jump)** activate correctly - ([6e43e78](https://github.com/abougouffa/minemacs/commit/6e43e78139cebfe5cb979e56891e115eaf579055)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** don't trigger in PDF files (patch sent upstream) - ([f849d4a](https://github.com/abougouffa/minemacs/commit/f849d4a6a04e6d2b931ad27f69be3c6149eae991)) - [@abougouffa](https://github.com/abougouffa)
- add the new `core/extras` to `load-path` - ([bc47687](https://github.com/abougouffa/minemacs/commit/bc476877fa62ad6330b1438263c7c8231a94994c)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([13eaab1](https://github.com/abougouffa/minemacs/commit/13eaab119f0a1140b67bc72d1ff7114de52bcb6e)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(org)** remove dead code - ([fd44b89](https://github.com/abougouffa/minemacs/commit/fd44b89410abc56e1b080e62230c8bc86cac840b)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** reorganize the builtin extra functionalities - ([f60deea](https://github.com/abougouffa/minemacs/commit/f60deea342373f6fc33e35036100055655a09a59)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** merge `me-eglot-x` and `me-eglot-ltex` - ([a5403d1](https://github.com/abougouffa/minemacs/commit/a5403d1990ada9ffac2a981222c7b9bbc42ec9c2)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** move `me-org-extras` to `me-org-x` - ([d97a796](https://github.com/abougouffa/minemacs/commit/d97a7964e6777b649472b008440a7464f39da800)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(guard-lf)** restore original repo, fix merged upstream - ([afd7106](https://github.com/abougouffa/minemacs/commit/afd7106a9560869633ef060004207d72e9b10c4e)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(casual)** remove `+casual-smerge-tmenu-auto-open-maybe` - ([f77a252](https://github.com/abougouffa/minemacs/commit/f77a252922c099fd372c3d6f6b23ab84fcbbdae8)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** bind `calendar` and some `denote` commands - ([7b0201a](https://github.com/abougouffa/minemacs/commit/7b0201aa25dbc7bb5f751ed42dc9652ce7a8d39e)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** remove unneeded customization - ([b43e64d](https://github.com/abougouffa/minemacs/commit/b43e64dbe7678a5f6d930d763696e2e799fb2690)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** no Org levels font size increase + `org-indent` by default - ([6abba53](https://github.com/abougouffa/minemacs/commit/6abba535721fe71f3b88b78e9d3127b379f995fc)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** no need for `docker-tramp`, `ros` don't depend on it anymore - ([d883996](https://github.com/abougouffa/minemacs/commit/d8839969d608a69f98a14d9128d1284da58a848a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6706ef2](https://github.com/abougouffa/minemacs/commit/6706ef22b5af5e30c1a4d884be65cb9a68652bf8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.39.1](https://github.com/abougouffa/minemacs/compare/bdf777a6cf4cea0dd42231bdc38d868c023eb08a..v12.39.1) - 2025-06-30
#### Bug Fixes
- **(native-compile)** remove the buggy on-battery customization - ([31c3b34](https://github.com/abougouffa/minemacs/commit/31c3b34b16a1b885ec0ce7f1025c73cc2fc299bd)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(project-x)** merge "fd" backend with caching - ([14a98dd](https://github.com/abougouffa/minemacs/commit/14a98dd177d8408429de18cd89fbb8d181fe585a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** don't show buffer boundaries in some modes - ([2d6a65c](https://github.com/abougouffa/minemacs/commit/2d6a65c278d59344674a9d1d9c63088ab20a839c)) - [@abougouffa](https://github.com/abougouffa)
- **(casual)** bind `casual-help` and `casual-man` - ([775cb0d](https://github.com/abougouffa/minemacs/commit/775cb0d358dc47c369923a5f371aa90be6acd605)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-dash)** auto fill with symbol/region - ([dedddfe](https://github.com/abougouffa/minemacs/commit/dedddfee1258cade522a266c3b8a2fc826503ca4)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** minor tweaks - ([baf0d74](https://github.com/abougouffa/minemacs/commit/baf0d74032af890f652d674b90ae10ea84cbfce6)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f7ddac6](https://github.com/abougouffa/minemacs/commit/f7ddac6b2394810b44f2cfba8a1d2a2eec6fb8ba)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([85a366c](https://github.com/abougouffa/minemacs/commit/85a366c3c898300547309c47fd515ac1923eb214)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bdf777a](https://github.com/abougouffa/minemacs/commit/bdf777a6cf4cea0dd42231bdc38d868c023eb08a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.39.0](https://github.com/abougouffa/minemacs/compare/8bd99e76d0a53cb59a206245f9ad5ff3706a117b..v12.39.0) - 2025-06-28
#### Bug Fixes
- **(eglot-ltex)** fix the setter of `eglot-ltex-ls-path` - ([8bd99e7](https://github.com/abougouffa/minemacs/commit/8bd99e76d0a53cb59a206245f9ad5ff3706a117b)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([e279f28](https://github.com/abougouffa/minemacs/commit/e279f2832bdbc154412666ad3260dd707b6328f9)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([8b7e955](https://github.com/abougouffa/minemacs/commit/8b7e9559b58b9a9e336a882c24292a1d0a6dc555)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(eglot)** restore Eglot customizations - ([c936d86](https://github.com/abougouffa/minemacs/commit/c936d86f903c576a109a26a039162df248840356)) - [@abougouffa](https://github.com/abougouffa)
- **(extra)** remove `run-in-dir` - ([e948e07](https://github.com/abougouffa/minemacs/commit/e948e07d3118eae9172ee96838a196d54122b1d2)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add `me-linux` module with `kconfig-mode` - ([fd612de](https://github.com/abougouffa/minemacs/commit/fd612de5018110e3f30394d3038e87cbf5b1797d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(external-tools)** add `harper-ls` - ([91e45cb](https://github.com/abougouffa/minemacs/commit/91e45cb707bb992602c67a344eb1d6530a4411ec)) - [@abougouffa](https://github.com/abougouffa)
- **(git-modes)** use `gitignore-mode` for several variants of `.*ignore` - ([f510882](https://github.com/abougouffa/minemacs/commit/f510882d5d80ae6765995c8397e2232f2c258e1b)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** add `+fd-ignores` - ([3305868](https://github.com/abougouffa/minemacs/commit/33058684bea723e172b19de79143217c670dd9cc)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8462b90](https://github.com/abougouffa/minemacs/commit/8462b9072fa3c4e31c30d34abea0a2c8475814d2)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([5365bf8](https://github.com/abougouffa/minemacs/commit/5365bf8813fd77b68d6ad10e817b2480d2c61fd9)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([54e2111](https://github.com/abougouffa/minemacs/commit/54e21111b345faf44184239bc482e7484acc74d4)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.38.0](https://github.com/abougouffa/minemacs/compare/ab6945d27aa6c4c3c7e08f8f0ca5021556b06fe1..v12.38.0) - 2025-06-26
#### Bug Fixes
- **(compile)** fix the buffer name function for existing compilation buffers - ([bc10d17](https://github.com/abougouffa/minemacs/commit/bc10d175dcd186e68c6e97591662597639df01df)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(on-demand/cron)** add a comment - ([538e7a0](https://github.com/abougouffa/minemacs/commit/538e7a0b53e19d62902316bd48df6af7d9510c5a)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([34ace1b](https://github.com/abougouffa/minemacs/commit/34ace1bb841be1f2c5829a89545215dc79aa7a2b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(obsolete)** remove `obsolete/project-x` to avoid confusion - ([4a49175](https://github.com/abougouffa/minemacs/commit/4a491757ce1ca9ebd5295f6cf15e2cb3656e1995)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** move project caching to `me-project-x` - ([c45958b](https://github.com/abougouffa/minemacs/commit/c45958b44ab9c45817baa249a2a2dcba477a3d27)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** use `fd` instead of `find` to browse project files - ([d62018a](https://github.com/abougouffa/minemacs/commit/d62018aa5c4e17926f8d8a710f442a5d79ca28ba)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** clear cache per-project - ([e550d88](https://github.com/abougouffa/minemacs/commit/e550d88aa09f404c6b2910489d51eea701b5983d)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(eglot)** move Eglot configuration to `extras/me-eglot` - ([5398c0f](https://github.com/abougouffa/minemacs/commit/5398c0fe91286a29865c42ab766966861d6969de)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** simplify the code - ([6ff9850](https://github.com/abougouffa/minemacs/commit/6ff9850e2bb3913eb501b40e399bcb2a66868a24)) - [@abougouffa](https://github.com/abougouffa)
- use `interactive-p` shortcut - ([fa0f842](https://github.com/abougouffa/minemacs/commit/fa0f842ec78290d597edb5a5e7537aee930aa10a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cc-mode)** register the `*.c.GEN` extension - ([babc34b](https://github.com/abougouffa/minemacs/commit/babc34b36bdb8991f1ae0a453ca020f7558a1b98)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** prefix the compilation buffer with the project name - ([ab6945d](https://github.com/abougouffa/minemacs/commit/ab6945d27aa6c4c3c7e08f8f0ca5021556b06fe1)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** disable buffer boundaries in `enlight` - ([1c3f110](https://github.com/abougouffa/minemacs/commit/1c3f1106e47165945efa11a5e80d0a5a627b7f4d)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** recenter/refresh always when `enlight` is visible - ([cc7fbeb](https://github.com/abougouffa/minemacs/commit/cc7fbebd2eee2bec4c4166a45dbfc104646edbef)) - [@abougouffa](https://github.com/abougouffa)
- **(magit)** make `magit` collaborate with `tramp-direct-async-process` - ([e088266](https://github.com/abougouffa/minemacs/commit/e088266a18602d0b0f7383708d8818af7e681005)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** cache files list when using the generic `find` backend - ([f0393ac](https://github.com/abougouffa/minemacs/commit/f0393ac42f6bc18bbfe0f9dd338b31ab030b49c7)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** making TRAMP go Brrr - ([a7fbdcb](https://github.com/abougouffa/minemacs/commit/a7fbdcbcc86af9bda5ac85c24040568d9f7ef712)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9fce0a8](https://github.com/abougouffa/minemacs/commit/9fce0a8e02db1efbf172af5375a3c2066a85a77b)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([4c70a99](https://github.com/abougouffa/minemacs/commit/4c70a992db51ce5784c5dc64cc26f08660802f67)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([fc68a8d](https://github.com/abougouffa/minemacs/commit/fc68a8d930b7ca808075a3ce79114b6171f15884)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6aab2f2](https://github.com/abougouffa/minemacs/commit/6aab2f24d48fb49eb5d736ec00defb6aab982a4e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.37.0](https://github.com/abougouffa/minemacs/compare/73f9b5ffb58d23ddcb295eb91f3b1d37e601973a..v12.37.0) - 2025-06-23
#### Documentation
- **(git)** minor docstring edit - ([f66040f](https://github.com/abougouffa/minemacs/commit/f66040fced06512382a30623b73c35250cb8fd5e)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(on-demand)** add `crontab-mode` - ([f961a11](https://github.com/abougouffa/minemacs/commit/f961a11a4c17484cd52a74fc89e358c638ded20a)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(breadcrumb)** fix typo in comment - ([19c095c](https://github.com/abougouffa/minemacs/commit/19c095c9afa80058d9a081256531aba633e67d79)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(macrostep)** move from `me-emacs-lisp` to `me-prog`, enable for C/C++ - ([f5fea1f](https://github.com/abougouffa/minemacs/commit/f5fea1ff85067cc23de745c46f1ccbdf0c037089)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(autorevert)** don't be chatty unless in verbose mode - ([c0e911c](https://github.com/abougouffa/minemacs/commit/c0e911cc3d296daeb3e72f1cb78664e58e1aec2f)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** show buffer boundaries - ([ac4c295](https://github.com/abougouffa/minemacs/commit/ac4c295c426864f690e35759d3cc4c3347dd9c17)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** remove the disabled property from some useful commands - ([ba80743](https://github.com/abougouffa/minemacs/commit/ba807433e404c3b1d58bfdc873e878ac47bba6a9)) - [@abougouffa](https://github.com/abougouffa)
- **(casual)** bind `+smerge-vc-next-conflict-recenter` for `+casual-smerge` - ([73f9b5f](https://github.com/abougouffa/minemacs/commit/73f9b5ffb58d23ddcb295eb91f3b1d37e601973a)) - [@abougouffa](https://github.com/abougouffa)
- **(csv)** add space to the list of default separators - ([1a4a61e](https://github.com/abougouffa/minemacs/commit/1a4a61e129787d4ed0dee01a4cdc1e36e562de95)) - [@abougouffa](https://github.com/abougouffa)
- **(envrc)** bind `envrc-command-map` to `C-c o v` - ([d484b3f](https://github.com/abougouffa/minemacs/commit/d484b3f384dc565bb29e111263e804bae9f57ff6)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/csv)** mark `eplot` as a companion package for `csv-mode` - ([1e0fad2](https://github.com/abougouffa/minemacs/commit/1e0fad29839df4773dcdbd3a43440e4b70e440f6)) - [@abougouffa](https://github.com/abougouffa)
- **(ready-player)** enable when at least one supported player is installed - ([e4a7b6c](https://github.com/abougouffa/minemacs/commit/e4a7b6cbf076ddb51fd2c58599b3cd572cd46f77)) - [@abougouffa](https://github.com/abougouffa)
- **(ready-player)** bind to `C-c o p`, default `C-c m` is used by `macrostep` - ([ae19d34](https://github.com/abougouffa/minemacs/commit/ae19d3422ae7147f5064ec9a21909a106747c764)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([483f9d4](https://github.com/abougouffa/minemacs/commit/483f9d40ef1e7e0341eb26d698897df1e6abedad)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.36.2](https://github.com/abougouffa/minemacs/compare/392bdb075fab4727622d7332b814619c6f3a2151..v12.36.2) - 2025-06-22
#### Bug Fixes
- **(parinfer-rust)** don't auto enable in read-only buffers - ([e46ba1d](https://github.com/abougouffa/minemacs/commit/e46ba1db6c0471542f8f4f36c88a3212936b7180)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** add `evil-numbers` - ([392bdb0](https://github.com/abougouffa/minemacs/commit/392bdb075fab4727622d7332b814619c6f3a2151)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** move Git commit prefix stuff to `extras/me-git` - ([d0836a7](https://github.com/abougouffa/minemacs/commit/d0836a7c3d6205a7872efb9e1e037cf918f32434)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(git-timemachine)** minor edit - ([0789199](https://github.com/abougouffa/minemacs/commit/07891998ec3655b74d296b6d2d11558216fd44d9)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(editor)** remove `evil-numbers` - ([77b03ae](https://github.com/abougouffa/minemacs/commit/77b03aeb0db9a7a931c9d1b329c47afc730305cb)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** minor change in a log message - ([e8e67e5](https://github.com/abougouffa/minemacs/commit/e8e67e529c515ffd706ad72097914dda1450cc5d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0b80c20](https://github.com/abougouffa/minemacs/commit/0b80c20bcb2d7ed271f5d8d73a6eb25a3b957b2a)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([8c7bac2](https://github.com/abougouffa/minemacs/commit/8c7bac24fc6cee189058cb82180361e88e575373)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.36.1](https://github.com/abougouffa/minemacs/compare/e78bf3a772e084a87c092145c90e03634b6522bf..v12.36.1) - 2025-06-20
#### Bug Fixes
- **(apheleia)** fix a typo - ([44a0e33](https://github.com/abougouffa/minemacs/commit/44a0e33400c8cbf81d6231f8d05d2591f3c302c8)) - [@abougouffa](https://github.com/abougouffa)
- **(dash-docs)** remove reference to old custom command - ([e78bf3a](https://github.com/abougouffa/minemacs/commit/e78bf3a772e084a87c092145c90e03634b6522bf)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(reformatter)** minor edit - ([d94d7cc](https://github.com/abougouffa/minemacs/commit/d94d7ccd97034e78060f94f5f1b58451b0f74dd2)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(smerge)** restore some useful custom command - ([12a37f0](https://github.com/abougouffa/minemacs/commit/12a37f00ae3264d84404a4ec29d5e79e1778c093)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(adb)** add `+adb-after-command-functions` - ([2c08863](https://github.com/abougouffa/minemacs/commit/2c088631ae74770d70cc72f54cac7fb43a306554)) - [@abougouffa](https://github.com/abougouffa)
- **(apheleia)** more intelligent way of setting `xmllint` indentation - ([b0886d5](https://github.com/abougouffa/minemacs/commit/b0886d59fd9e180b6a271d7c180744dbb48617a4)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** open `*.prop` files in `conf-mode` - ([f96989f](https://github.com/abougouffa/minemacs/commit/f96989f942643039cd4583d8117d04f8424a768e)) - [@abougouffa](https://github.com/abougouffa)
- **(casual)** recenter after jumping to next/prev conflict - ([97c2146](https://github.com/abougouffa/minemacs/commit/97c21465d1a29c648db2f54b6d3e08a241c86bb3)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** ignore case when checking TAB config - ([f6f912d](https://github.com/abougouffa/minemacs/commit/f6f912d44af8d50d5575a2f84f246f30ffd1ba13)) - [@abougouffa](https://github.com/abougouffa)
- **(dash-docs)** register more docsets - ([ade03bb](https://github.com/abougouffa/minemacs/commit/ade03bbdc4e3285df0ad1ce726cc14d847523ce2)) - [@abougouffa](https://github.com/abougouffa)
- **(dash-docs)** register docsets for Bash and Python - ([791d195](https://github.com/abougouffa/minemacs/commit/791d19505372a4e717339669a58e130cd53452d7)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot-booster)** use IO only mode in Emacs 30+ - ([1a32840](https://github.com/abougouffa/minemacs/commit/1a32840e4bc4398abdb5c464b5f55fcb5e8e29ca)) - [@abougouffa](https://github.com/abougouffa)
- **(magit)** don't show Gravatars - ([7d7dc99](https://github.com/abougouffa/minemacs/commit/7d7dc998a4a39bf4a3a6d19034dc59a286a73775)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-multimodal)** enable in `vc-dir-mode` - ([fef618d](https://github.com/abougouffa/minemacs/commit/fef618dfe0dc85323d0a5342e77c889328ef3065)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** remove deleted variable - ([c1c0aaf](https://github.com/abougouffa/minemacs/commit/c1c0aaf253636ee510f10b4f9fd5d15c5796c2cd)) - [@abougouffa](https://github.com/abougouffa)
- **(smerge)** more useful custom commands - ([80b7ad3](https://github.com/abougouffa/minemacs/commit/80b7ad33837b529adfb8a504d775a79df10971f0)) - [@abougouffa](https://github.com/abougouffa)
- **(smerge)** automatically call `+casual-smerge-tmenu` - ([37d4f6d](https://github.com/abougouffa/minemacs/commit/37d4f6d2906b9147216aae1f60fc83a4e69157a9)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([498df1b](https://github.com/abougouffa/minemacs/commit/498df1bf71b11d694b0658e898c1de5e5863008e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a66ea49](https://github.com/abougouffa/minemacs/commit/a66ea49617551775d381a689351af599634305a3)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([fb8d150](https://github.com/abougouffa/minemacs/commit/fb8d150ae2114f4adab10d777431257a72ec3283)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.36.0](https://github.com/abougouffa/minemacs/compare/d395f53d5339661229358f7f6354c0a131f04135..v12.36.0) - 2025-06-17
#### Bug Fixes
- **(adb)** restore accidentally remove variable - ([57e814e](https://github.com/abougouffa/minemacs/commit/57e814e0ea0074975ce03666e4f364873efa778d)) - [@abougouffa](https://github.com/abougouffa)
- **(cocogitto)** use the right predicate - ([6324511](https://github.com/abougouffa/minemacs/commit/6324511b03a308532e5b5a6f656fc5d402dfc949)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** load `subr-x` early to avoid problems - ([51ad0bc](https://github.com/abougouffa/minemacs/commit/51ad0bc38cd11d87458dad5d184310e80aa38955)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([15d416c](https://github.com/abougouffa/minemacs/commit/15d416c0464c295f1ea9a7645e1eb27aa37e7096)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add the `:define-loader` option when declaring on-demand modules - ([976476e](https://github.com/abougouffa/minemacs/commit/976476e5e9d744f3d53cae78a7a66606c98f2f0d)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** add `dash-docs` and `consult-dash` - ([7698a81](https://github.com/abougouffa/minemacs/commit/7698a810e90e180349658638ac71af3a0fff19c7)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for Scallop - ([db50e02](https://github.com/abougouffa/minemacs/commit/db50e02c3c485e56c762561200311a7a08d0c5d2)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `doom-two-tone-themes` - ([7bbc648](https://github.com/abougouffa/minemacs/commit/7bbc6481f44d735fdc7fddeccc730f4257e4371a)) - [@abougouffa](https://github.com/abougouffa)
- remove `obsolete/me-solaire` - ([d395f53](https://github.com/abougouffa/minemacs/commit/d395f53d5339661229358f7f6354c0a131f04135)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(pet)** add a comment - ([04fd36d](https://github.com/abougouffa/minemacs/commit/04fd36d13522626f3754be7ca066676dfd167268)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(adb)** add a completion predicate to the commands - ([be1758f](https://github.com/abougouffa/minemacs/commit/be1758f590a8ac3f69886352e9c0ad01c61f84e3)) - [@abougouffa](https://github.com/abougouffa)
- **(cocogitto)** better completion predicate (use memoization) - ([5f60df2](https://github.com/abougouffa/minemacs/commit/5f60df245462f720c578830fd9baef4bd850937c)) - [@abougouffa](https://github.com/abougouffa)
- **(cocogitto)** show `+cocogitto-bump` only when relevant - ([d6d7f85](https://github.com/abougouffa/minemacs/commit/d6d7f85599bf8415b24190c643cf1f1c3303e778)) - [@abougouffa](https://github.com/abougouffa)
- **(dash-docs)** remove custom command, integrated in the package - ([8bbeb38](https://github.com/abougouffa/minemacs/commit/8bbeb38d8511ca0c4179bfe34038695b76781e7a)) - [@abougouffa](https://github.com/abougouffa)
- **(eww)** auto rename the buffer to the page's title - ([2f09a1e](https://github.com/abougouffa/minemacs/commit/2f09a1e192dcc848fb92969e9949fa077f5983ed)) - [@abougouffa](https://github.com/abougouffa)
- **(ffip)** remove unneeded hack, support added upstream - ([fc0fef5](https://github.com/abougouffa/minemacs/commit/fc0fef52f38c22bb841a489bda5c8c06dad95b3a)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** set `user-emacs-directory` in `me-vars`, remove old hack - ([e0e8cab](https://github.com/abougouffa/minemacs/commit/e0e8cabff7b9000f380403c57a738eac9c549533)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** make use of the new `:define-loader` option - ([60116b9](https://github.com/abougouffa/minemacs/commit/60116b9c49dcf0382d590aeac169ad2001640b09)) - [@abougouffa](https://github.com/abougouffa)
- **(projection)** add Modula-2 header/source mapping - ([99f4364](https://github.com/abougouffa/minemacs/commit/99f43643ccee0ddb630c45daeb23746847fb520d)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([19e30f3](https://github.com/abougouffa/minemacs/commit/19e30f32fff132e4f6b22d6a1ca6d8d96aae41bb)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6c72158](https://github.com/abougouffa/minemacs/commit/6c721585dabfa14973a543396d2ec6dc16ab7664)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5bfc146](https://github.com/abougouffa/minemacs/commit/5bfc146ead1e60e506249d66f49dc4a0aa02e8c1)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([7f700a2](https://github.com/abougouffa/minemacs/commit/7f700a29d97cbd2a5c8c2e0dc2d176b945a5bb41)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([c75e3a8](https://github.com/abougouffa/minemacs/commit/c75e3a879930503087f95b603cca066bbc955a77)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([42e5088](https://github.com/abougouffa/minemacs/commit/42e5088786c5078d42de1a89dc2be74402080d2e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6bc10b2](https://github.com/abougouffa/minemacs/commit/6bc10b2bbbe409d578eff9a4e32b0e99814d1559)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.35.0](https://github.com/abougouffa/minemacs/compare/c086cd243dd53e3b4b730d8aeb0e01c9580837b9..v12.35.0) - 2025-06-14
#### Features
- **(desktop)** remove customization, switching to `easysession` - ([3aebd6c](https://github.com/abougouffa/minemacs/commit/3aebd6c039bb2bf085db990b30d2720779f6568f)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `solaire-mode` - ([92c0841](https://github.com/abougouffa/minemacs/commit/92c08415a4e746daad35c2d7a2da4c55847738ae)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `easysession` - ([7832e7f](https://github.com/abougouffa/minemacs/commit/7832e7fa8ac5e771aa4f895686fef28b1e5b31fb)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- auto update the copyright date on save - ([14f2e3d](https://github.com/abougouffa/minemacs/commit/14f2e3d02fff95f3b53d1513becd238988f907d4)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(init)** handle `MINEMACS_BENCHMARK` in `early-init` - ([d3ea8b8](https://github.com/abougouffa/minemacs/commit/d3ea8b8451d3bc779cc55944826fc35c941d4be9)) - [@abougouffa](https://github.com/abougouffa)
- don't repeat `use-package`'s `:hook` blocks in `me-builtin` - ([fb11fd4](https://github.com/abougouffa/minemacs/commit/fb11fd42e556b7e38799b64e492f8450c85e19b6)) - [@abougouffa](https://github.com/abougouffa)
- move `x86-lookup` from `me-embedded` to `on-demand/me-assembly` - ([a1155a8](https://github.com/abougouffa/minemacs/commit/a1155a802186af0debff176fe3c991a6d364f46d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(rmsbolt)** minor edit - ([c086cd2](https://github.com/abougouffa/minemacs/commit/c086cd243dd53e3b4b730d8aeb0e01c9580837b9)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** renaming the initial tab is assured by `otpp` - ([0314c1f](https://github.com/abougouffa/minemacs/commit/0314c1fa311fdbd02fd0681118b28f1310cd3f1b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([13b3032](https://github.com/abougouffa/minemacs/commit/13b3032f154e730c57918e7da0111543a082c212)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([47c75e0](https://github.com/abougouffa/minemacs/commit/47c75e033fa4f934bcf6fb8746e5f0cd617a76d8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.34.3](https://github.com/abougouffa/minemacs/compare/b2e6b148401e89f644a927ac78425332f8e74a10..v12.34.3) - 2025-06-14
#### Bug Fixes
- **(core)** don't miss on-demand modules when applicable - ([b2e6b14](https://github.com/abougouffa/minemacs/commit/b2e6b148401e89f644a927ac78425332f8e74a10)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor edit - ([76f4297](https://github.com/abougouffa/minemacs/commit/76f42974441310dfae170ada510d16cc6cc952d1)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** move `demangle-mode` to `on-demand/me-llvm` - ([2552330](https://github.com/abougouffa/minemacs/commit/2552330bb5329b34361c8fd64d1d2a0e5f2d6191)) - [@abougouffa](https://github.com/abougouffa)
- don't repeat `use-package`'s `:hook` blocks - ([afbb986](https://github.com/abougouffa/minemacs/commit/afbb986f26e4ca00a8a055926f9da3ec1994fa55)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cape)** use `setopt` to set `text-mode-ispell-word-completion` - ([d5a25fd](https://github.com/abougouffa/minemacs/commit/d5a25fdf013ab33b46679f784f74f30a044f54e6)) - [@abougouffa](https://github.com/abougouffa)
- **(helpful)** don't show the "Source Code" section - ([d653eb2](https://github.com/abougouffa/minemacs/commit/d653eb2aacb1901d6166bd2aefc1d3dce6188888)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([62e0004](https://github.com/abougouffa/minemacs/commit/62e0004b9a21fc48140421b7b52afe202b424b46)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([92514b0](https://github.com/abougouffa/minemacs/commit/92514b0574998421328103d74024bd609ec9ec44)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.34.2](https://github.com/abougouffa/minemacs/compare/d1836613f81fa6fdac71daafd80c39a0c443dfaf..v12.34.2) - 2025-06-13
#### Documentation
- strip long hashes in changelog - ([3ce4ec1](https://github.com/abougouffa/minemacs/commit/3ce4ec127ca622979efbf08ff774707518059c1e)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** better cleaning rules - ([c3ebf43](https://github.com/abougouffa/minemacs/commit/c3ebf4392c63d2d21c589d9892a9842c6fc4a80a)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** remove unused rules and add `build` - ([a9e73d2](https://github.com/abougouffa/minemacs/commit/a9e73d2910680080ebaf0239584995ae63bae85f)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(core)** fix formatting issue - ([d183661](https://github.com/abougouffa/minemacs/commit/d1836613f81fa6fdac71daafd80c39a0c443dfaf)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** some simplifications - ([29cdb78](https://github.com/abougouffa/minemacs/commit/29cdb787a8c37cb3643d1ea97e2cc276804331f8)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(logview)** customize the cache file path - ([4cc2afd](https://github.com/abougouffa/minemacs/commit/4cc2afdaa80b83d7df115f39b466c11d48dbf4c7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([be9ce5c](https://github.com/abougouffa/minemacs/commit/be9ce5c501c370d4f3d64c4ea441719d57283d30)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.34.1](https://github.com/abougouffa/minemacs/compare/ce9e1d3..v12.34.1) - 2025-06-13
#### Bug Fixes
- **(clang-format)** handle the unlimited column limit case (ColumnLimit=0) - ([302bff3](https://github.com/abougouffa/minemacs/commit/302bff3)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** fix issue caused by `native-comp-async-on-battery-power` - ([ce9e1d3](https://github.com/abougouffa/minemacs/commit/ce9e1d3)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([c2ff403](https://github.com/abougouffa/minemacs/commit/c2ff403)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(clang-format)** extract common patterns - ([2a29a42](https://github.com/abougouffa/minemacs/commit/2a29a42)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** function to command + signal errors accordingly - ([e9604b7](https://github.com/abougouffa/minemacs/commit/e9604b7)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** rename a function - ([e9baac7](https://github.com/abougouffa/minemacs/commit/e9baac7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(casual)** mark some `+casual-smerge-tmenu` commands as transient - ([08132df](https://github.com/abougouffa/minemacs/commit/08132df)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** guess `fill-column` from `.clang-format` - ([3802b21](https://github.com/abougouffa/minemacs/commit/3802b21)) - [@abougouffa](https://github.com/abougouffa)
- mark `+editorconfig-guess-style-from-clang-format` as command - ([9ed6d65](https://github.com/abougouffa/minemacs/commit/9ed6d65)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0b4741d](https://github.com/abougouffa/minemacs/commit/0b4741d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.34.0](https://github.com/abougouffa/minemacs/compare/f0be55b..v12.34.0) - 2025-06-11
#### Bug Fixes
- **(compile-multi)** bind to F9 instead of already used F8 - ([9401b8c](https://github.com/abougouffa/minemacs/commit/9401b8c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** revert buffer to apply the loaded on-demand mode immediately - ([ee2f873](https://github.com/abougouffa/minemacs/commit/ee2f873)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** correct check in `+super-project-define-commands` - ([f0be55b](https://github.com/abougouffa/minemacs/commit/f0be55b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** move `xclip` to `me-editor`, delete `me-tty` - ([cb54a51](https://github.com/abougouffa/minemacs/commit/cb54a51)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** make unused Gerrit/repo stuff obsolete - ([5d2cdc4](https://github.com/abougouffa/minemacs/commit/5d2cdc4)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** make `eglot-inactive-regions-mode` obsolete - ([e5495ec](https://github.com/abougouffa/minemacs/commit/e5495ec)) - [@abougouffa](https://github.com/abougouffa)
- **(igist)** move `igist` from `me-experimental` to `me-services` - ([b1731ca](https://github.com/abougouffa/minemacs/commit/b1731ca)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** move `magit-gerrit` from `me-experimental` to `me-vc` - ([a3f3725](https://github.com/abougouffa/minemacs/commit/a3f3725)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(citre)** mark some project-related variables as buffer-locals - ([6f42384](https://github.com/abougouffa/minemacs/commit/6f42384)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** make `minemacs-on-demand-modules-alist` a regular variable - ([0b54c87](https://github.com/abougouffa/minemacs/commit/0b54c87)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simplify conditions in `minemacs-on-demand-try` - ([a47fc1a](https://github.com/abougouffa/minemacs/commit/a47fc1a)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(diff-hl)** use the fringe, the margin is used by `eglot` actions - ([ec4c6ae](https://github.com/abougouffa/minemacs/commit/ec4c6ae)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(doom-modeline)** increase the height of `doom-modeline` - ([c590e9c](https://github.com/abougouffa/minemacs/commit/c590e9c)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add Alchemy project markers - ([0a43e3b](https://github.com/abougouffa/minemacs/commit/0a43e3b)) - [@abougouffa](https://github.com/abougouffa)
- move native compile config to `early-init` and disable on battery - ([b752e93](https://github.com/abougouffa/minemacs/commit/b752e93)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f6584ed](https://github.com/abougouffa/minemacs/commit/f6584ed)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.33.0](https://github.com/abougouffa/minemacs/compare/7b0f34d..v12.33.0) - 2025-06-09
#### Documentation
- regenerate the documentation - ([05e33d3](https://github.com/abougouffa/minemacs/commit/05e33d3)) - [@abougouffa](https://github.com/abougouffa)
- update the benchmark section of README - ([3dcd13b](https://github.com/abougouffa/minemacs/commit/3dcd13b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(builtin)** move `markdown-ts-mode` configuration to `me-builtin` - ([5e194df](https://github.com/abougouffa/minemacs/commit/5e194df)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add `magit-delta` - ([45dce0a](https://github.com/abougouffa/minemacs/commit/45dce0a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(casual)** add a menu for `smerge-mode` - ([0f61d2e](https://github.com/abougouffa/minemacs/commit/0f61d2e)) - [@abougouffa](https://github.com/abougouffa)
- **(casual)** bind new `casual-calendar` and unify the binding to `C-o` - ([75e7a9c](https://github.com/abougouffa/minemacs/commit/75e7a9c)) - [@abougouffa](https://github.com/abougouffa)
- **(diff-hl)** don't show `+/-/!` symbols in the margin - ([18fd96b](https://github.com/abougouffa/minemacs/commit/18fd96b)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** add `delta` to the list - ([cde5680](https://github.com/abougouffa/minemacs/commit/cde5680)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** allow benchmark at `minemacs-{after-startup,lazy}_hook` - ([7b0f34d](https://github.com/abougouffa/minemacs/commit/7b0f34d)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-imerge)** condition on the presence of `git-imerge` - ([2d3aa75](https://github.com/abougouffa/minemacs/commit/2d3aa75)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** register extra "run for all" commands - ([9632d23](https://github.com/abougouffa/minemacs/commit/9632d23)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** use default Markdown grammar recipes - ([85aadf1](https://github.com/abougouffa/minemacs/commit/85aadf1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6ede019](https://github.com/abougouffa/minemacs/commit/6ede019)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.32.1](https://github.com/abougouffa/minemacs/compare/328216f..v12.32.1) - 2025-06-08
#### Bug Fixes
- **(cocogitto)** don't error when invoking `+cocogitto-bump` from `magit` - ([4632af7](https://github.com/abougouffa/minemacs/commit/4632af7)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([68688c1](https://github.com/abougouffa/minemacs/commit/68688c1)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(straight)** extract a common pattern to a macro - ([328216f](https://github.com/abougouffa/minemacs/commit/328216f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(editor)** adapt to renaming `vim-modelines` to `vim-file-locals` - ([9f1fd3c](https://github.com/abougouffa/minemacs/commit/9f1fd3c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5eb7a64](https://github.com/abougouffa/minemacs/commit/5eb7a64)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4ff8367](https://github.com/abougouffa/minemacs/commit/4ff8367)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.32.0](https://github.com/abougouffa/minemacs/compare/4d23fa2..v12.32.0) - 2025-06-06
#### Bug Fixes
- **(core)** autoload `clang-format` functions - ([bd5f40c](https://github.com/abougouffa/minemacs/commit/bd5f40c)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(core)** document the `+super-project-current` function - ([cba86df](https://github.com/abougouffa/minemacs/commit/cba86df)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([36f0cf0](https://github.com/abougouffa/minemacs/commit/36f0cf0)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([c92b1a9](https://github.com/abougouffa/minemacs/commit/c92b1a9)) - [@abougouffa](https://github.com/abougouffa)
- fix email address - ([c3e2e92](https://github.com/abougouffa/minemacs/commit/c3e2e92)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+super-project-current` - ([399b040](https://github.com/abougouffa/minemacs/commit/399b040)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for `bazel` - ([4d23fa2](https://github.com/abougouffa/minemacs/commit/4d23fa2)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/csv)** add support for `eplot` - ([c0c7c30](https://github.com/abougouffa/minemacs/commit/c0c7c30)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** rename `locked` to `upgrade` - ([7f29bfe](https://github.com/abougouffa/minemacs/commit/7f29bfe)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(clang-format)** expose some API functions - ([1a2419e](https://github.com/abougouffa/minemacs/commit/1a2419e)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** move `lab` from `me-vc` to `on-demand/me-gitlab` - ([2e68093](https://github.com/abougouffa/minemacs/commit/2e68093)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(adb)** use `compile` to call the ADB commands - ([ea4d373](https://github.com/abougouffa/minemacs/commit/ea4d373)) - [@abougouffa](https://github.com/abougouffa)
- **(apheleia)** enable automatically when Clang config is available - ([4936a21](https://github.com/abougouffa/minemacs/commit/4936a21)) - [@abougouffa](https://github.com/abougouffa)
- **(cascading-dir-locals)** enable debug mode when needed - ([fa57574](https://github.com/abougouffa/minemacs/commit/fa57574)) - [@abougouffa](https://github.com/abougouffa)
- **(compile-multi)** bind `compile-multi` to F8 - ([75844dc](https://github.com/abougouffa/minemacs/commit/75844dc)) - [@abougouffa](https://github.com/abougouffa)
- **(diff-hl)** display diffs on margin, leave fringe for other stuff - ([b550a06](https://github.com/abougouffa/minemacs/commit/b550a06)) - [@abougouffa](https://github.com/abougouffa)
- **(difftastic)** check for `difft` on the system - ([5bb8a30](https://github.com/abougouffa/minemacs/commit/5bb8a30)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c1cbb63](https://github.com/abougouffa/minemacs/commit/c1cbb63)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([aa88f77](https://github.com/abougouffa/minemacs/commit/aa88f77)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0f0cf9f](https://github.com/abougouffa/minemacs/commit/0f0cf9f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.31.0](https://github.com/abougouffa/minemacs/compare/5fcafb3..v12.31.0) - 2025-06-04
#### Bug Fixes
- **(adb)** don't export EDITOR when running ADB commands - ([6c27bc5](https://github.com/abougouffa/minemacs/commit/6c27bc5)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove debug message from `minemacs-on-demand-try` - ([ae74649](https://github.com/abougouffa/minemacs/commit/ae74649)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** properly load commands from included third-party packages - ([937bb81](https://github.com/abougouffa/minemacs/commit/937bb81)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** move `math-preview` to `on-demand/me-jupyter` - ([8c73d33](https://github.com/abougouffa/minemacs/commit/8c73d33)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/maxima)** define `minemacs-maxima-load` only when needed - ([2a0adfc](https://github.com/abougouffa/minemacs/commit/2a0adfc)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/maxima)** fix loading `on-demand/me-maxima` - ([5fcafb3](https://github.com/abougouffa/minemacs/commit/5fcafb3)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** replace `vim-modeline` by `vim-modelines` - ([d261f6f](https://github.com/abougouffa/minemacs/commit/d261f6f)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** support Vim modeline options via `vim-modeline` - ([6bfc5ca](https://github.com/abougouffa/minemacs/commit/6bfc5ca)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** initial support for `cascading-dir-locals` - ([1ef9a7e](https://github.com/abougouffa/minemacs/commit/1ef9a7e)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** make unused `erlang` obsolete, too big repo - ([ce6c90e](https://github.com/abougouffa/minemacs/commit/ce6c90e)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/erlang)** restore `erlang` to be included as a third-party - ([31f61fc](https://github.com/abougouffa/minemacs/commit/31f61fc)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/maxima)** include Maxima integration from the official repo - ([de14c85](https://github.com/abougouffa/minemacs/commit/de14c85)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/ocaml)** include `dune` as a third-party - ([036aaa0](https://github.com/abougouffa/minemacs/commit/036aaa0)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/ocaml)** restore `dune` to include it as a third-party - ([f1bc4ec](https://github.com/abougouffa/minemacs/commit/f1bc4ec)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/ocaml)** make unused `dune` obsolete, too big repo - ([afa8a9a](https://github.com/abougouffa/minemacs/commit/afa8a9a)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/protobuf)** include `protobuf-mode`, the repo is too big - ([fc2f9b4](https://github.com/abougouffa/minemacs/commit/fc2f9b4)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `treesit-fold` obsolete, `hideshow` should work fine - ([759261e](https://github.com/abougouffa/minemacs/commit/759261e)) - [@abougouffa](https://github.com/abougouffa)
- **(vim-modeline)** add support for `filetype` - ([7a15f62](https://github.com/abougouffa/minemacs/commit/7a15f62)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** move `minemacs-load-module` to `me-lib` - ([f849902](https://github.com/abougouffa/minemacs/commit/f849902)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** remove unneeded files from third-party packages - ([c363d92](https://github.com/abougouffa/minemacs/commit/c363d92)) - [@abougouffa](https://github.com/abougouffa)
- **(reformatter)** make use of `+clang-format--get-lang` - ([68085f4](https://github.com/abougouffa/minemacs/commit/68085f4)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(adb)** print the command set to `adb` - ([c585a93](https://github.com/abougouffa/minemacs/commit/c585a93)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** collapse lighters in mode line (Emacs 31+) - ([f874364](https://github.com/abougouffa/minemacs/commit/f874364)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** set reduced font size to 82% - ([d8e3685](https://github.com/abougouffa/minemacs/commit/d8e3685)) - [@abougouffa](https://github.com/abougouffa)
- **(gambol)** bind `M-g ,` and `M-g .` only in `gambol-mode-map` - ([7f2a6ca](https://github.com/abougouffa/minemacs/commit/7f2a6ca)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/erlang)** include `erlang` from `otp` as a third-party - ([a5f557b](https://github.com/abougouffa/minemacs/commit/a5f557b)) - [@abougouffa](https://github.com/abougouffa)
- **(vim-modeline)** use `editorconfig` to set the offset for `shiftwidth` - ([9511dd3](https://github.com/abougouffa/minemacs/commit/9511dd3)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a73115e](https://github.com/abougouffa/minemacs/commit/a73115e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0671fb8](https://github.com/abougouffa/minemacs/commit/0671fb8)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([ab61e60](https://github.com/abougouffa/minemacs/commit/ab61e60)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([61081fe](https://github.com/abougouffa/minemacs/commit/61081fe)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.30.0](https://github.com/abougouffa/minemacs/compare/65c75b4..v12.30.0) - 2025-05-31
#### Bug Fixes
- **(nerd-icons)** avoid re-installing the font when running Emacs daemon - ([aab3051](https://github.com/abougouffa/minemacs/commit/aab3051)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([2ab4bee](https://github.com/abougouffa/minemacs/commit/2ab4bee)) - [@abougouffa](https://github.com/abougouffa)
- update the screenshot - ([cc55184](https://github.com/abougouffa/minemacs/commit/cc55184)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([263f324](https://github.com/abougouffa/minemacs/commit/263f324)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(math)** split `me-math` into `on-demand/me-{jupyter,maxima}` - ([0be3ebf](https://github.com/abougouffa/minemacs/commit/0be3ebf)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** refactor and simplify the code for on-demand modules - ([9031421](https://github.com/abougouffa/minemacs/commit/9031421)) - [@abougouffa](https://github.com/abougouffa)
- **(vundo)** make the `+vundo-diff-commands` variable customizable - ([0bed67a](https://github.com/abougouffa/minemacs/commit/0bed67a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(nerd-icons)** ensure installing the font - ([65c75b4](https://github.com/abougouffa/minemacs/commit/65c75b4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2d6900b](https://github.com/abougouffa/minemacs/commit/2d6900b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3c01bdd](https://github.com/abougouffa/minemacs/commit/3c01bdd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.29.0](https://github.com/abougouffa/minemacs/compare/a5df51c..v12.29.0) - 2025-05-29
#### Bug Fixes
- **(citre)** add a way to ignore enabling `citre-mode` in some modes - ([a5df51c](https://github.com/abougouffa/minemacs/commit/a5df51c)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-vterm)** use `shell-quote-argument` in the CWD advice - ([2d26a21](https://github.com/abougouffa/minemacs/commit/2d26a21)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(adb)** add `+adb-root` and `adb reboot edl` for Qualcomm boards - ([7105ec3](https://github.com/abougouffa/minemacs/commit/7105ec3)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(core)** change some log messages - ([283654b](https://github.com/abougouffa/minemacs/commit/283654b)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(hexl)** cleanup `hexl` extensions - ([8d5df7e](https://github.com/abougouffa/minemacs/commit/8d5df7e)) - [@abougouffa](https://github.com/abougouffa)
- remove unused `minemacs-after-loading-modules-hook` - ([31fbd36](https://github.com/abougouffa/minemacs/commit/31fbd36)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(adb)** display output in a side buffer in bottom - ([cf2ce4a](https://github.com/abougouffa/minemacs/commit/cf2ce4a)) - [@abougouffa](https://github.com/abougouffa)
- **(objdump-disassemble)** enable globally - ([7aeb0b7](https://github.com/abougouffa/minemacs/commit/7aeb0b7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1f5bf18](https://github.com/abougouffa/minemacs/commit/1f5bf18)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([58e2a4b](https://github.com/abougouffa/minemacs/commit/58e2a4b)) - [@abougouffa](https://github.com/abougouffa)
- Don't display annoying warning messages about `[if|when]-let` - ([92ea104](https://github.com/abougouffa/minemacs/commit/92ea104)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2fafa2f](https://github.com/abougouffa/minemacs/commit/2fafa2f)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([305675c](https://github.com/abougouffa/minemacs/commit/305675c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.28.1](https://github.com/abougouffa/minemacs/compare/380d67a..v12.28.1) - 2025-05-26
#### Bug Fixes
- **(devcontainer)** fix the recipe, package renamed - ([7827e32](https://github.com/abougouffa/minemacs/commit/7827e32)) - [@abougouffa](https://github.com/abougouffa)
- **(term)** fix `+serial-running-p` - ([978df75](https://github.com/abougouffa/minemacs/commit/978df75)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(viper)** edits comments - ([380d67a](https://github.com/abougouffa/minemacs/commit/380d67a)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** remove obsolete function - ([3b6a085](https://github.com/abougouffa/minemacs/commit/3b6a085)) - [@abougouffa](https://github.com/abougouffa)
- **(viper)** cleanup some copy/paste dirt - ([660d13c](https://github.com/abougouffa/minemacs/commit/660d13c)) - [@abougouffa](https://github.com/abougouffa)
- remove references to the removed `me-window` module - ([a9cef2e](https://github.com/abougouffa/minemacs/commit/a9cef2e)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** change the implementation of `+net-get-ip-address` - ([12ee1ec](https://github.com/abougouffa/minemacs/commit/12ee1ec)) - [@abougouffa](https://github.com/abougouffa)
- **(viper)** add bindings for `dw` and `d$` - ([fac1ab2](https://github.com/abougouffa/minemacs/commit/fac1ab2)) - [@abougouffa](https://github.com/abougouffa)
- **(viper)** bind `SPC h` to `help-map` - ([259a5e0](https://github.com/abougouffa/minemacs/commit/259a5e0)) - [@abougouffa](https://github.com/abougouffa)
- **(wgrep)** disable on Emacs 31+ in favor of `grep-edit-mode` - ([442a1e2](https://github.com/abougouffa/minemacs/commit/442a1e2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([74a2058](https://github.com/abougouffa/minemacs/commit/74a2058)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.28.0](https://github.com/abougouffa/minemacs/compare/6d85f00..v12.28.0) - 2025-05-25
#### Bug Fixes
- **(core)** ensure `minemacs-lazy-hook` hooks are run before `+with-delayed!` blocks - ([28c348c](https://github.com/abougouffa/minemacs/commit/28c348c)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(viper)** add some extensions for `viper-mode` from Emacs Solo - ([c4e1f75](https://github.com/abougouffa/minemacs/commit/c4e1f75)) - [@abougouffa](https://github.com/abougouffa)
- remove the `me-window` module, move `ace-window` to `me-nav` - ([2cdf50b](https://github.com/abougouffa/minemacs/commit/2cdf50b)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(obsolete/copilot)** cleanup spaces - ([5eb07a6](https://github.com/abougouffa/minemacs/commit/5eb07a6)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(init)** minor refactoring and move version check to `early-init.el` - ([694a476](https://github.com/abougouffa/minemacs/commit/694a476)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** move `minemacs-generate-loaddefs` to `me-lib` - ([787ddc3](https://github.com/abougouffa/minemacs/commit/787ddc3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(adb)** more tweaks and commands for ADB - ([6d85f00](https://github.com/abougouffa/minemacs/commit/6d85f00)) - [@abougouffa](https://github.com/abougouffa)
- **(aidermacs)** add `devstral` to the list - ([1069600](https://github.com/abougouffa/minemacs/commit/1069600)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([2f1e593](https://github.com/abougouffa/minemacs/commit/2f1e593)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([074611c](https://github.com/abougouffa/minemacs/commit/074611c)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([8105fe4](https://github.com/abougouffa/minemacs/commit/8105fe4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0ebfc24](https://github.com/abougouffa/minemacs/commit/0ebfc24)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.27.0](https://github.com/abougouffa/minemacs/compare/5a33c00..v12.27.0) - 2025-05-23
#### Bug Fixes
- **(clang-format)** properly handle edge cases - ([70ef83a](https://github.com/abougouffa/minemacs/commit/70ef83a)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** Special case when config file doesn't include a lanugage - ([8cccbe7](https://github.com/abougouffa/minemacs/commit/8cccbe7)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** proper use of `no-opt` arg in `+clang-format-get-style` - ([5a33c00](https://github.com/abougouffa/minemacs/commit/5a33c00)) - [@abougouffa](https://github.com/abougouffa)
- **(git-commit)** fix a type leading to bugs with the `prefix` convention - ([09be6c1](https://github.com/abougouffa/minemacs/commit/09be6c1)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** the keybindings conflicts with `recentf-open-files` - ([8250054](https://github.com/abougouffa/minemacs/commit/8250054)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(adb)** add some wrappers for ADB commands - ([ba744d9](https://github.com/abougouffa/minemacs/commit/ba744d9)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(clang-format)** fall back to default options for all modes - ([f26dddb](https://github.com/abougouffa/minemacs/commit/f26dddb)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** remove unused variable - ([a9b6f65](https://github.com/abougouffa/minemacs/commit/a9b6f65)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simpler implementation of `+make-buffer-conds` - ([cab8493](https://github.com/abougouffa/minemacs/commit/cab8493)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add and make use of `+reverse-args` - ([d4fd1cd](https://github.com/abougouffa/minemacs/commit/d4fd1cd)) - [@abougouffa](https://github.com/abougouffa)
- move `+nerd-icons-icon` to `me-lib` + make use of it - ([c754431](https://github.com/abougouffa/minemacs/commit/c754431)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** use `clang-format` for `protobuf[-ts]-mode` - ([373eed2](https://github.com/abougouffa/minemacs/commit/373eed2)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** add JSON and Verilog to `+clang-format-mode-alist` - ([de11f0e](https://github.com/abougouffa/minemacs/commit/de11f0e)) - [@abougouffa](https://github.com/abougouffa)
- **(dtrt-indent)** register indentation variable for `protobuf-ts-mode` - ([755700f](https://github.com/abougouffa/minemacs/commit/755700f)) - [@abougouffa](https://github.com/abougouffa)
- **(editorconfig)** register indentation variable for `protobuf-ts-mode` - ([cf4546a](https://github.com/abougouffa/minemacs/commit/cf4546a)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** more robust `+hs-toggle-all` - ([6baaf18](https://github.com/abougouffa/minemacs/commit/6baaf18)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-gerrit)** load after `magit` for seamless integration - ([58dc712](https://github.com/abougouffa/minemacs/commit/58dc712)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([da9b1cd](https://github.com/abougouffa/minemacs/commit/da9b1cd)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([737d39a](https://github.com/abougouffa/minemacs/commit/737d39a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5e68976](https://github.com/abougouffa/minemacs/commit/5e68976)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.26.1](https://github.com/abougouffa/minemacs/compare/4030d83..v12.26.1) - 2025-05-18
#### Bug Fixes
- **(on-demand/julia)** remove duplicate `julia-mode`, fix interpreter mode - ([9839b4f](https://github.com/abougouffa/minemacs/commit/9839b4f)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** refactor on-demand modules loading functions - ([ba79ca3](https://github.com/abougouffa/minemacs/commit/ba79ca3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** robust tests when trying to create non-existing paths - ([4030d83](https://github.com/abougouffa/minemacs/commit/4030d83)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** minor refactoring of `projectile` wrappers - ([b7cfce1](https://github.com/abougouffa/minemacs/commit/b7cfce1)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(treesit-auto)** don't exclude `org-mode` from `treesit` parsers - ([7bbf5f2](https://github.com/abougouffa/minemacs/commit/7bbf5f2)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([a36a365](https://github.com/abougouffa/minemacs/commit/a36a365)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.26.0](https://github.com/abougouffa/minemacs/compare/8d656e3..v12.26.0) - 2025-05-18
#### Bug Fixes
- **(clang-format)** fix the "-style" option deduction - ([10d8a45](https://github.com/abougouffa/minemacs/commit/10d8a45)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** bypass `display-buffer-alist` rules in `+def-dedicated-tab!` - ([f3ac6d5](https://github.com/abougouffa/minemacs/commit/f3ac6d5)) - [@abougouffa](https://github.com/abougouffa)
- **(diff-hl)** check for `diff-hl-mode` before updating - ([52508d0](https://github.com/abougouffa/minemacs/commit/52508d0)) - [@abougouffa](https://github.com/abougouffa)
- **(hexl)** use the `+hexl-` prefix and autoload `+hexl-buffer-p` - ([ccdb54b](https://github.com/abougouffa/minemacs/commit/ccdb54b)) - [@abougouffa](https://github.com/abougouffa)
- **(igist)** don't blindly generate revisions on every `C-x C-s` - ([625babc](https://github.com/abougouffa/minemacs/commit/625babc)) - [@abougouffa](https://github.com/abougouffa)
- **(magit)** use a fresh `transient` version for `magit` - ([6fa864b](https://github.com/abougouffa/minemacs/commit/6fa864b)) - [@abougouffa](https://github.com/abougouffa)
- **(vterm)** ensure hiding the `vterm` install buffer - ([7b41454](https://github.com/abougouffa/minemacs/commit/7b41454)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([584ae64](https://github.com/abougouffa/minemacs/commit/584ae64)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(services)** make `org-jira` obsolete, leave only `jiralib` - ([0fad5e3](https://github.com/abougouffa/minemacs/commit/0fad5e3)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** make unused `tributary` obsolete - ([ce28421](https://github.com/abougouffa/minemacs/commit/ce28421)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(git-timemachine)** minor edits - ([8d656e3](https://github.com/abougouffa/minemacs/commit/8d656e3)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-magit)** remove unused `+multi-magit-discover-repos` - ([3fa2f57](https://github.com/abougouffa/minemacs/commit/3fa2f57)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(awqat)** minor change in the mode line format - ([d236a55](https://github.com/abougouffa/minemacs/commit/d236a55)) - [@abougouffa](https://github.com/abougouffa)
- **(denote)** add `org-capture` templates for `denote` journal and refs - ([752d497](https://github.com/abougouffa/minemacs/commit/752d497)) - [@abougouffa](https://github.com/abougouffa)
- **(denote-journal)** enable `denote-journal-calendar-mode` - ([26a8c0b](https://github.com/abougouffa/minemacs/commit/26a8c0b)) - [@abougouffa](https://github.com/abougouffa)
- **(dumb-jump)** register extra project markers from `project` - ([5fd0ed2](https://github.com/abougouffa/minemacs/commit/5fd0ed2)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** add support for `ty` LSP server - ([854e9b1](https://github.com/abougouffa/minemacs/commit/854e9b1)) - [@abougouffa](https://github.com/abougouffa)
- **(git-timemachine)** add an icon from `nerd-icons` in the header line - ([c7968e9](https://github.com/abougouffa/minemacs/commit/c7968e9)) - [@abougouffa](https://github.com/abougouffa)
- **(git-timemachine)** simplify displaying revision in header-line - ([c5842f3](https://github.com/abougouffa/minemacs/commit/c5842f3)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** ignore `+hexl-mode-maybe` - ([62d3f4b](https://github.com/abougouffa/minemacs/commit/62d3f4b)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** use `pyproject.toml` as a project marker for Python's `uv` - ([258663b](https://github.com/abougouffa/minemacs/commit/258663b)) - [@abougouffa](https://github.com/abougouffa)
- **(quickrun)** reduce font size in `quickrun` buffers - ([27a8af1](https://github.com/abougouffa/minemacs/commit/27a8af1)) - [@abougouffa](https://github.com/abougouffa)
- **(sudo-edit)** minor refactoring - ([a807e14](https://github.com/abougouffa/minemacs/commit/a807e14)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** add a rule for `quickrun--mode` - ([0ffb412](https://github.com/abougouffa/minemacs/commit/0ffb412)) - [@abougouffa](https://github.com/abougouffa)
- **(yasnippet)** don't define old `yas/*` aliases - ([a466fd2](https://github.com/abougouffa/minemacs/commit/a466fd2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bd65679](https://github.com/abougouffa/minemacs/commit/bd65679)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9608235](https://github.com/abougouffa/minemacs/commit/9608235)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.25.0](https://github.com/abougouffa/minemacs/compare/9f2c52e..v12.25.0) - 2025-05-13
#### Bug Fixes
- **(citre)** fix the keymap binding - ([d8e9a1f](https://github.com/abougouffa/minemacs/commit/d8e9a1f)) - [@abougouffa](https://github.com/abougouffa)
- **(diff-hl)** enable globally and fix integration with `magit` - ([9e66b84](https://github.com/abougouffa/minemacs/commit/9e66b84)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete/me-evil)** ensure loading `general` - ([1ba00d0](https://github.com/abougouffa/minemacs/commit/1ba00d0)) - [@abougouffa](https://github.com/abougouffa)
- **(vc-jj)** load only when the `jj` executable is available - ([86cef6e](https://github.com/abougouffa/minemacs/commit/86cef6e)) - [@abougouffa](https://github.com/abougouffa)
- **(vc-jj)** ensure enabling the `vc-jj` backend - ([1c46c29](https://github.com/abougouffa/minemacs/commit/1c46c29)) - [@abougouffa](https://github.com/abougouffa)
- **(webkit)** disable and don't build the module on Windows - ([fdac44b](https://github.com/abougouffa/minemacs/commit/fdac44b)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** buffer display rules for `helpful` and `pp-eval-expression` - ([ad9b1d8](https://github.com/abougouffa/minemacs/commit/ad9b1d8)) - [@abougouffa](https://github.com/abougouffa)
- check for Tree-Sitter support before installing `*-ts-mode`s - ([accc068](https://github.com/abougouffa/minemacs/commit/accc068)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([1853ef8](https://github.com/abougouffa/minemacs/commit/1853ef8)) - [@abougouffa](https://github.com/abougouffa)
- manually fix the change log for v12.20.0 - ([4d63ede](https://github.com/abougouffa/minemacs/commit/4d63ede)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** automatically guess indentation style from `.clang-format` - ([9f87940](https://github.com/abougouffa/minemacs/commit/9f87940)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more polished utilities for `clang-format` - ([246b15c](https://github.com/abougouffa/minemacs/commit/246b15c)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** restore `repo-transient` and add `gerrit` - ([1d477ba](https://github.com/abougouffa/minemacs/commit/1d477ba)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** remove `gerrit` - ([1fc5b2c](https://github.com/abougouffa/minemacs/commit/1fc5b2c)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** add initial support for `magit-gerrit` - ([eae994c](https://github.com/abougouffa/minemacs/commit/eae994c)) - [@abougouffa](https://github.com/abougouffa)
- **(git-commit)** add a helper to insert conventional commits - ([bcf66fd](https://github.com/abougouffa/minemacs/commit/bcf66fd)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for Typst - ([8481415](https://github.com/abougouffa/minemacs/commit/8481415)) - [@abougouffa](https://github.com/abougouffa)
- **(webkit)** autoload and bind `webkit-dark-toggle` - ([0b41205](https://github.com/abougouffa/minemacs/commit/0b41205)) - [@abougouffa](https://github.com/abougouffa)
- add the option to select a specific `clang-format` version - ([847c4f0](https://github.com/abougouffa/minemacs/commit/847c4f0)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(gitignore)** don't track `.cache/` - ([ab9018e](https://github.com/abougouffa/minemacs/commit/ab9018e)) - [@abougouffa](https://github.com/abougouffa)
- update the `.dir-locals.el` - ([95c67b2](https://github.com/abougouffa/minemacs/commit/95c67b2)) - [@abougouffa](https://github.com/abougouffa)
- enable conventional commits for the project - ([037b2a2](https://github.com/abougouffa/minemacs/commit/037b2a2)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(core)** code formatting - ([f7bdd41](https://github.com/abougouffa/minemacs/commit/f7bdd41)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** move `hexl` related customization to `me-builtin` - ([368711d](https://github.com/abougouffa/minemacs/commit/368711d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unused `+project-scan-for-projects` - ([f5c480d](https://github.com/abougouffa/minemacs/commit/f5c480d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** make use of `url-http-end-of-headers` - ([3672fea](https://github.com/abougouffa/minemacs/commit/3672fea)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** simplify and remove references to old options - ([baad80f](https://github.com/abougouffa/minemacs/commit/baad80f)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** remove buggy `flymake` fringe marker customization - ([233a524](https://github.com/abougouffa/minemacs/commit/233a524)) - [@abougouffa](https://github.com/abougouffa)
- **(git-commit)** generic commit prefix, available per-project - ([cfa25a2](https://github.com/abougouffa/minemacs/commit/cfa25a2)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(builtin)** reset `tab-width` to 8 by default - ([75e751c](https://github.com/abougouffa/minemacs/commit/75e751c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(autorevert)** no messages on reverting externally modified buffers - ([b882110](https://github.com/abougouffa/minemacs/commit/b882110)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** remove a non longer needed setting - ([9f2c52e](https://github.com/abougouffa/minemacs/commit/9f2c52e)) - [@abougouffa](https://github.com/abougouffa)
- **(editorconfig)** polish guessing indentation style from `.clang-format` - ([d445ec8](https://github.com/abougouffa/minemacs/commit/d445ec8)) - [@abougouffa](https://github.com/abougouffa)
- **(ligature)** register only arrows in `text-mode` (not working) - ([95daf3c](https://github.com/abougouffa/minemacs/commit/95daf3c)) - [@abougouffa](https://github.com/abougouffa)
- **(ligature)** add `;;` and `;;;` for LISPs - ([015e82f](https://github.com/abougouffa/minemacs/commit/015e82f)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-gerrit)** use my fork until PR gets eventually merged - ([aa3ece9](https://github.com/abougouffa/minemacs/commit/aa3ece9)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-gerrit)** Use `_` instead of `R` as a prefix - ([7b00b42](https://github.com/abougouffa/minemacs/commit/7b00b42)) - [@abougouffa](https://github.com/abougouffa)
- **(webkit)** disable on Windows - ([60fae95](https://github.com/abougouffa/minemacs/commit/60fae95)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([adb2bc2](https://github.com/abougouffa/minemacs/commit/adb2bc2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([935efb2](https://github.com/abougouffa/minemacs/commit/935efb2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([eb52ff8](https://github.com/abougouffa/minemacs/commit/eb52ff8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bdbff6f](https://github.com/abougouffa/minemacs/commit/bdbff6f)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([a95b88d](https://github.com/abougouffa/minemacs/commit/a95b88d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([b3d9038](https://github.com/abougouffa/minemacs/commit/b3d9038)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.24.0](https://github.com/abougouffa/minemacs/compare/801a6ef..v12.24.0) - 2025-05-07
#### Bug Fixes
- **(core)** fix the super-project project root derivation - ([7459923](https://github.com/abougouffa/minemacs/commit/7459923)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** don't fail if one of the build functions fails - ([1a3aa39](https://github.com/abougouffa/minemacs/commit/1a3aa39)) - [@abougouffa](https://github.com/abougouffa)
- **(projection)** declare the right commands variables as safe - ([c6ab383](https://github.com/abougouffa/minemacs/commit/c6ab383)) - [@abougouffa](https://github.com/abougouffa)
- **(ssh-deploy)** install from GNU ELPA, upstream repo has been deleted - ([801a6ef](https://github.com/abougouffa/minemacs/commit/801a6ef)) - [@abougouffa](https://github.com/abougouffa)
- **(vterm)** don't depend on the `libvterm` of the system - ([c3e2207](https://github.com/abougouffa/minemacs/commit/c3e2207)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([23661f1](https://github.com/abougouffa/minemacs/commit/23661f1)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([2e20273](https://github.com/abougouffa/minemacs/commit/2e20273)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([86e0f89](https://github.com/abougouffa/minemacs/commit/86e0f89)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add the `minemacs-user-config` command - ([fb17c1a](https://github.com/abougouffa/minemacs/commit/fb17c1a)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** add `webkit` - ([4122bcc](https://github.com/abougouffa/minemacs/commit/4122bcc)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** add `gerrit` - ([391d118](https://github.com/abougouffa/minemacs/commit/391d118)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** restore `gee` - ([42406dd](https://github.com/abougouffa/minemacs/commit/42406dd)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** add support for `clue`, integrate with `citre` - ([0a0548c](https://github.com/abougouffa/minemacs/commit/0a0548c)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add `boogie-mode`, `dafny-mode` and `z3-smt2-mode` - ([841f9fa](https://github.com/abougouffa/minemacs/commit/841f9fa)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** restore `ligature` - ([8972713](https://github.com/abougouffa/minemacs/commit/8972713)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(make)** fix filename glob for some GNU Make 4.3 - ([87d30eb](https://github.com/abougouffa/minemacs/commit/87d30eb)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** refactor & remove unneeded code - ([7dea3d6](https://github.com/abougouffa/minemacs/commit/7dea3d6)) - [@abougouffa](https://github.com/abougouffa)
- cleanup some dead code - ([8935d10](https://github.com/abougouffa/minemacs/commit/8935d10)) - [@abougouffa](https://github.com/abougouffa)
- don't use the obsolete form of `derived-mode-p` - ([5f963ac](https://github.com/abougouffa/minemacs/commit/5f963ac)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(on-demand)** `boogie-friends` adds `company` and `flycheck` as  dependencies - ([d034614](https://github.com/abougouffa/minemacs/commit/d034614)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** restore the default `tab-width` - ([2c0d804](https://github.com/abougouffa/minemacs/commit/2c0d804)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** bind some navigation commands to `C-c C-p` in `citre-mode` - ([581d336](https://github.com/abougouffa/minemacs/commit/581d336)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** don't override the initial input for some commands - ([8f98923](https://github.com/abougouffa/minemacs/commit/8f98923)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-eglot)** use the initial input from region or symbol at point - ([24832e2](https://github.com/abougouffa/minemacs/commit/24832e2)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** reduce the default font size of Cascadia Code - ([a1b95ba](https://github.com/abougouffa/minemacs/commit/a1b95ba)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** change font order, prefer fonts with ligatures - ([c0f86fb](https://github.com/abougouffa/minemacs/commit/c0f86fb)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more invocation options for `+project-scan-for-projects` - ([c764728](https://github.com/abougouffa/minemacs/commit/c764728)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** don't display a message when loading `custom-vars.el` - ([a6440dd](https://github.com/abougouffa/minemacs/commit/a6440dd)) - [@abougouffa](https://github.com/abougouffa)
- **(ligature)** enable common ligatures in `text-mode` - ([b4ffd55](https://github.com/abougouffa/minemacs/commit/b4ffd55)) - [@abougouffa](https://github.com/abougouffa)
- **(ligature)** fine-tuned list of ligatures in a per-language basis - ([5e2d2af](https://github.com/abougouffa/minemacs/commit/5e2d2af)) - [@abougouffa](https://github.com/abougouffa)
- **(ligature)** compile a list of ligatures from several fonts - ([ae65071](https://github.com/abougouffa/minemacs/commit/ae65071)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** don't ask when upgrading packages - ([c4f3e55](https://github.com/abougouffa/minemacs/commit/c4f3e55)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** ignore Gnome session variables in `+env-save` - ([59be709](https://github.com/abougouffa/minemacs/commit/59be709)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** smaller font size for `xref` buffer - ([9bacd46](https://github.com/abougouffa/minemacs/commit/9bacd46)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the loaddefs - ([27378ad](https://github.com/abougouffa/minemacs/commit/27378ad)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d900fbd](https://github.com/abougouffa/minemacs/commit/d900fbd)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9823855](https://github.com/abougouffa/minemacs/commit/9823855)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([61be24c](https://github.com/abougouffa/minemacs/commit/61be24c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.23.0](https://github.com/abougouffa/minemacs/compare/e89d27b..v12.23.0) - 2025-05-04
#### Bug Fixes
- **(builtin)** install `which-key` and `editorconfig` on Emacs 29 - ([9e7dbf5](https://github.com/abougouffa/minemacs/commit/9e7dbf5)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fix `+file-name-incremental` - ([590f41f](https://github.com/abougouffa/minemacs/commit/590f41f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** rename and fix the `+delete-current-file-and-buffer` - ([43da678](https://github.com/abougouffa/minemacs/commit/43da678)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** `warn` in verbose mode, it triggers the debugger - ([1149d19](https://github.com/abougouffa/minemacs/commit/1149d19)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/web)** provide the right feature - ([7693bb9](https://github.com/abougouffa/minemacs/commit/7693bb9)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** fix a condition in `display-buffer-alist` - ([3205a36](https://github.com/abougouffa/minemacs/commit/3205a36)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** display compilation buffers in side windows - ([147cf24](https://github.com/abougouffa/minemacs/commit/147cf24)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([ece5044](https://github.com/abougouffa/minemacs/commit/ece5044)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([93d5bfd](https://github.com/abougouffa/minemacs/commit/93d5bfd)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add helper function `+make-buffer-conds` - ([cdacbe9](https://github.com/abougouffa/minemacs/commit/cdacbe9)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** add `denote-(silo|journal|sequence|org|markdown)` - ([e89d27b](https://github.com/abougouffa/minemacs/commit/e89d27b)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** some cleanup - ([ee79e95](https://github.com/abougouffa/minemacs/commit/ee79e95)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edit - ([975fee0](https://github.com/abougouffa/minemacs/commit/975fee0)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** rename `+copy-this-file-name` to `+copy-current-file-name` - ([b54ef3d](https://github.com/abougouffa/minemacs/commit/b54ef3d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove some unused or unnecessary commands - ([bb9a5f7](https://github.com/abougouffa/minemacs/commit/bb9a5f7)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** move unused command to `obsolete/me-evil` - ([3d14aaf](https://github.com/abougouffa/minemacs/commit/3d14aaf)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** minor edit - ([3610ce0](https://github.com/abougouffa/minemacs/commit/3610ce0)) - [@abougouffa](https://github.com/abougouffa)
- **(term)** move the exit advice to `me-lib-extra` - ([81b045a](https://github.com/abougouffa/minemacs/commit/81b045a)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(tools)** restore `devcontainer-mode` - ([f65555f](https://github.com/abougouffa/minemacs/commit/f65555f)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** restore the `minemacs-disabled-packages` hack - ([fa13237](https://github.com/abougouffa/minemacs/commit/fa13237)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** better defaults - ([30c7c8f](https://github.com/abougouffa/minemacs/commit/30c7c8f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** reduce log level in `+load-theme` - ([1bb04b8](https://github.com/abougouffa/minemacs/commit/1bb04b8)) - [@abougouffa](https://github.com/abougouffa)
- **(eat)** kill buffer after exit - ([f9b0946](https://github.com/abougouffa/minemacs/commit/f9b0946)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** more robust configuration for `display-buffer-alist` - ([3301a5c](https://github.com/abougouffa/minemacs/commit/3301a5c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([344ce91](https://github.com/abougouffa/minemacs/commit/344ce91)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bc7555c](https://github.com/abougouffa/minemacs/commit/bc7555c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6ac4b50](https://github.com/abougouffa/minemacs/commit/6ac4b50)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4c4d99b](https://github.com/abougouffa/minemacs/commit/4c4d99b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.22.0](https://github.com/abougouffa/minemacs/compare/b2ef9c9..v12.22.0) - 2025-05-02
#### Features
- **(prog)** restore `sr-speedbar` - ([6ef77b7](https://github.com/abougouffa/minemacs/commit/6ef77b7)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** replace `devcontainer-mode` with `devcontainer` - ([6e33df4](https://github.com/abougouffa/minemacs/commit/6e33df4)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** don't depend on `cl-lib` in `me-vars` - ([d50d8e3](https://github.com/abougouffa/minemacs/commit/d50d8e3)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** move some variables to `early-init` - ([933f742](https://github.com/abougouffa/minemacs/commit/933f742)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** move `display-buffer-alist` stuff to `me-builtin` - ([2a9bc89](https://github.com/abougouffa/minemacs/commit/2a9bc89)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(multi-vterm)** better `+multi-vterm-toggle-dwim` - ([05f8f79](https://github.com/abougouffa/minemacs/commit/05f8f79)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete/nano)** minor edits - ([b2ef9c9](https://github.com/abougouffa/minemacs/commit/b2ef9c9)) - [@abougouffa](https://github.com/abougouffa)
- **(with-editor)** better integration - ([4b14ba0](https://github.com/abougouffa/minemacs/commit/4b14ba0)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.21.0](https://github.com/abougouffa/minemacs/compare/71900cf..v12.21.0) - 2025-05-01
#### Documentation
- cleanup some unneeded docs - ([12bb42c](https://github.com/abougouffa/minemacs/commit/12bb42c)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(tools)** add TRAMP support for LXD containers - ([1f920a8](https://github.com/abougouffa/minemacs/commit/1f920a8)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add TRAMP support for LXC containers - ([349cf3a](https://github.com/abougouffa/minemacs/commit/349cf3a)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(snippets)** add creation and modification times in the templates - ([32866ee](https://github.com/abougouffa/minemacs/commit/32866ee)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(enlight)** make the recentering hook buffer-local to `enlight` - ([71900cf](https://github.com/abougouffa/minemacs/commit/71900cf)) - [@abougouffa](https://github.com/abougouffa)
- **(envrc)** enable on remote SSH and Docker machines - ([500b4a6](https://github.com/abougouffa/minemacs/commit/500b4a6)) - [@abougouffa](https://github.com/abougouffa)
- **(pet)** better integration with `quickrun` - ([62b8a5e](https://github.com/abougouffa/minemacs/commit/62b8a5e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a3875ba](https://github.com/abougouffa/minemacs/commit/a3875ba)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.20.0](https://github.com/abougouffa/minemacs/compare/1cdce16..v12.20.0) - 2025-04-30
#### Bug Fixes
- **(ltex-plus)** fix the download of LSP server for aarch64 - ([ba09562e](https://github.com/abougouffa/minemacs/commit/ba09562e)) - ([@abougouffa](https://github.com/abougouffa))

#### Documentation
- regenerate the documentation - ([5603d77b](https://github.com/abougouffa/minemacs/commit/5603d77b)) - ([@abougouffa](https://github.com/abougouffa))
- regenerate the documentation - ([e296765a](https://github.com/abougouffa/minemacs/commit/e296765a)) - ([@abougouffa](https://github.com/abougouffa))

#### Miscellaneous Chores
- add `.dir-locals.el` - ([2b367c2d](https://github.com/abougouffa/minemacs/commit/2b367c2d)) - ([@abougouffa](https://github.com/abougouffa))

#### Nitpicks, changes with no side effect
- add file creation timestamps - ([e8f8de89](https://github.com/abougouffa/minemacs/commit/e8f8de89)) - ([@abougouffa](https://github.com/abougouffa))
- add modification time stamps - ([1a207080](https://github.com/abougouffa/minemacs/commit/1a207080)) - ([@abougouffa](https://github.com/abougouffa))

#### Refactoring
- **(core)** remove unneeded autoloads - ([38e65eed](https://github.com/abougouffa/minemacs/commit/38e65eed)) - ([@abougouffa](https://github.com/abougouffa))
- Use `featurep` instead of `+emacs-options-p` - ([56249618](https://github.com/abougouffa/minemacs/commit/56249618)) - ([@abougouffa](https://github.com/abougouffa))
- move `straight` related stuff to `me-bootstrap` - ([e8e8ce2a](https://github.com/abougouffa/minemacs/commit/e8e8ce2a)) - ([@abougouffa](https://github.com/abougouffa))

#### Tweaks
- **(core)** better automation in `minemacs-bump-packages` - ([811f6700](https://github.com/abougouffa/minemacs/commit/811f6700)) - ([@abougouffa](https://github.com/abougouffa))
- **(init)** mark Emacs 30 as the recommended version - ([b36f5eac](https://github.com/abougouffa/minemacs/commit/b36f5eac)) - ([@abougouffa](https://github.com/abougouffa))
- **(on-demand/csv)** automatically enable `rainbow-csv` in small files - ([4aff2b6e](https://github.com/abougouffa/minemacs/commit/4aff2b6e)) - ([@abougouffa](https://github.com/abougouffa))
- **(on-demand/web)** don't use `web-mode` for HTML - ([1cdce160](https://github.com/abougouffa/minemacs/commit/1cdce160)) - ([@abougouffa](https://github.com/abougouffa))
- **(re-builder)** better error handling in `+reb-replace-regexp` - ([7f6f9e97](https://github.com/abougouffa/minemacs/commit/7f6f9e97)) - ([@abougouffa](https://github.com/abougouffa))
- bump packages versions - ([39905b36](https://github.com/abougouffa/minemacs/commit/39905b36)) - ([@abougouffa](https://github.com/abougouffa))
- bump packages versions - ([b6189914](https://github.com/abougouffa/minemacs/commit/b6189914)) - ([@abougouffa](https://github.com/abougouffa))
- bump packages versions - ([ee7da69c](https://github.com/abougouffa/minemacs/commit/ee7da69c)) - ([@abougouffa](https://github.com/abougouffa))
- regenerate loaddefs - ([8297797d](https://github.com/abougouffa/minemacs/commit/8297797d)) - ([@abougouffa](https://github.com/abougouffa))

- - -

## [v12.19.0](https://github.com/abougouffa/minemacs/compare/2bbd236..v12.19.0) - 2025-04-27
#### Bug Fixes
- **(core)** accept multiple modes in `minemacs-register-on-demand-module` - ([8dc1aaa](https://github.com/abougouffa/minemacs/commit/8dc1aaa)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** properly set non-custom variables/constants - ([aeff52c](https://github.com/abougouffa/minemacs/commit/aeff52c)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([7a6f56e](https://github.com/abougouffa/minemacs/commit/7a6f56e)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(on-demand/sql)** add `flymake-sqlfluff` - ([ae850d8](https://github.com/abougouffa/minemacs/commit/ae850d8)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- cleanup modules list in `me-vars` - ([e41ffb8](https://github.com/abougouffa/minemacs/commit/e41ffb8)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- simplify async Org export by using the default init file - ([72119da](https://github.com/abougouffa/minemacs/commit/72119da)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(gambol)** remove confusing `+gambol:occur-dwim` - ([d617b1e](https://github.com/abougouffa/minemacs/commit/d617b1e)) - [@abougouffa](https://github.com/abougouffa)
- **(git-timemachine)** it seems there is no need to the font-lock hack - ([2bbd236](https://github.com/abougouffa/minemacs/commit/2bbd236)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** better message for multi-file async export - ([e48a6de](https://github.com/abougouffa/minemacs/commit/e48a6de)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.18.0](https://github.com/abougouffa/minemacs/compare/5583382..v12.18.0) - 2025-04-27
#### Features
- **(emacs-lisp)** make `inspector` obsolete - ([75a08e3](https://github.com/abougouffa/minemacs/commit/75a08e3)) - [@abougouffa](https://github.com/abougouffa)
- **(eros+octave)** remove unreliable `+eros-octave-eval-last-sexp` - ([cde0a54](https://github.com/abougouffa/minemacs/commit/cde0a54)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/assembly)** add support for `arm-mode` - ([fa152d6](https://github.com/abougouffa/minemacs/commit/fa152d6)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** replace `jujutsushi` with `vc-jj` - ([febef56](https://github.com/abougouffa/minemacs/commit/febef56)) - [@abougouffa](https://github.com/abougouffa)
- **(vundo)** add `+vundo-diff-mode` to automatically trigger `vundo-diff` - ([6d3d7a9](https://github.com/abougouffa/minemacs/commit/6d3d7a9)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(editor)** minor edits - ([07cb925](https://github.com/abougouffa/minemacs/commit/07cb925)) - [@abougouffa](https://github.com/abougouffa)
- **(helpful)** minor edits - ([7cd0b79](https://github.com/abougouffa/minemacs/commit/7cd0b79)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** minor edits - ([9ef5c72](https://github.com/abougouffa/minemacs/commit/9ef5c72)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** simplify and enhance code - ([7f43446](https://github.com/abougouffa/minemacs/commit/7f43446)) - [@abougouffa](https://github.com/abougouffa)
- **(with-editor)** minor edits - ([5733220](https://github.com/abougouffa/minemacs/commit/5733220)) - [@abougouffa](https://github.com/abougouffa)
- remove unneeded tweak - ([d0bff7f](https://github.com/abougouffa/minemacs/commit/d0bff7f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** disable `xterm-mouse-mode`, not working under Alacritty - ([d217fce](https://github.com/abougouffa/minemacs/commit/d217fce)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** minor cleanup - ([2392a92](https://github.com/abougouffa/minemacs/commit/2392a92)) - [@abougouffa](https://github.com/abougouffa)
- **(expreg)** use the default recipe - ([fd74381](https://github.com/abougouffa/minemacs/commit/fd74381)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib)** better completion candidates - ([f8b779f](https://github.com/abougouffa/minemacs/commit/f8b779f)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/assembly)** better completion candidates - ([9a57834](https://github.com/abougouffa/minemacs/commit/9a57834)) - [@abougouffa](https://github.com/abougouffa)
- **(rmsbolt)** disable `flymake` in disassembled code - ([9f3a830](https://github.com/abougouffa/minemacs/commit/9f3a830)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** hide the buffer showed by `treesit-auto-install-all` - ([08c1cff](https://github.com/abougouffa/minemacs/commit/08c1cff)) - [@abougouffa](https://github.com/abougouffa)
- **(ws-butler)** customize the local repo name to avoid conflicts - ([5583382](https://github.com/abougouffa/minemacs/commit/5583382)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([92952ff](https://github.com/abougouffa/minemacs/commit/92952ff)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.17.0](https://github.com/abougouffa/minemacs/compare/c892be9..v12.17.0) - 2025-04-25
#### Documentation
- **(readme)** minor edit - ([670e3f6](https://github.com/abougouffa/minemacs/commit/670e3f6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(comint)** add `+comint-clear-buffer`, bind it to `C-l` - ([153c1e6](https://github.com/abougouffa/minemacs/commit/153c1e6)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `apptainer-mode` - ([5df70e4](https://github.com/abougouffa/minemacs/commit/5df70e4)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(bootstrap)** minor edits - ([24b24cb](https://github.com/abougouffa/minemacs/commit/24b24cb)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** merge `minemacs-lazy` into `minemacs-loaded` - ([b9521ec](https://github.com/abougouffa/minemacs/commit/b9521ec)) - [@abougouffa](https://github.com/abougouffa)
- **(with-editor)** group `+shutup!` blocks - ([a24d9e4](https://github.com/abougouffa/minemacs/commit/a24d9e4)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([8c4ef08](https://github.com/abougouffa/minemacs/commit/8c4ef08)) - [@abougouffa](https://github.com/abougouffa)
- remove obsolete definitions - ([8ee9f9c](https://github.com/abougouffa/minemacs/commit/8ee9f9c)) - [@abougouffa](https://github.com/abougouffa)
- simplify and move `use-package` config to `init.el` - ([96858f4](https://github.com/abougouffa/minemacs/commit/96858f4)) - [@abougouffa](https://github.com/abougouffa)
- make sure to install the last versions of builtin packages - ([2ce35e9](https://github.com/abougouffa/minemacs/commit/2ce35e9)) - [@abougouffa](https://github.com/abougouffa)
- rename `minemacs-reduced-font-size` and make it a command - ([a2d1290](https://github.com/abougouffa/minemacs/commit/a2d1290)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** some cleanup and removal of unused stuff - ([fe42a2c](https://github.com/abougouffa/minemacs/commit/fe42a2c)) - [@abougouffa](https://github.com/abougouffa)
- **(comint)** better tweaks - ([c892be9](https://github.com/abougouffa/minemacs/commit/c892be9)) - [@abougouffa](https://github.com/abougouffa)
- **(dired-aux)** add a rule to compress to `.tgz` file - ([01dfa80](https://github.com/abougouffa/minemacs/commit/01dfa80)) - [@abougouffa](https://github.com/abougouffa)
- **(dired-aux)** add a rule to compress (!) to `.tar` file - ([01cab59](https://github.com/abougouffa/minemacs/commit/01cab59)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** add `devcontainer` - ([f965f3c](https://github.com/abougouffa/minemacs/commit/f965f3c)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** minor edit in the `+flymake-transient` - ([afebe91](https://github.com/abougouffa/minemacs/commit/afebe91)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** ask before stopping async native compilation on exit - ([97b1fc9](https://github.com/abougouffa/minemacs/commit/97b1fc9)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** remove the hack of `use-package` - ([053d1e6](https://github.com/abougouffa/minemacs/commit/053d1e6)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** set `window-width` to 80 for REPL buffers - ([30645ae](https://github.com/abougouffa/minemacs/commit/30645ae)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5ee95fc](https://github.com/abougouffa/minemacs/commit/5ee95fc)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([107e75d](https://github.com/abougouffa/minemacs/commit/107e75d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bf09f28](https://github.com/abougouffa/minemacs/commit/bf09f28)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([525fdfe](https://github.com/abougouffa/minemacs/commit/525fdfe)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.16.0](https://github.com/abougouffa/minemacs/compare/1c35ed0..v12.16.0) - 2025-04-22
#### Documentation
- regenerate the documentation - ([8ed6769](https://github.com/abougouffa/minemacs/commit/8ed6769)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(builtin)** enable `minemacs-reduced-font-size` in some modes - ([f6f9466](https://github.com/abougouffa/minemacs/commit/f6f9466)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `minemacs-reduced-font-size` to be used as a hook - ([cb2e024](https://github.com/abougouffa/minemacs/commit/cb2e024)) - [@abougouffa](https://github.com/abougouffa)
- **(experimental)** restore `eglot-x` to experiment with it - ([77f2808](https://github.com/abougouffa/minemacs/commit/77f2808)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add initial support for `eat` - ([c1db896](https://github.com/abougouffa/minemacs/commit/c1db896)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add initial support for `devcontainer-mode` - ([9d105ff](https://github.com/abougouffa/minemacs/commit/9d105ff)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(citre)** rename an internal function - ([2a934bc](https://github.com/abougouffa/minemacs/commit/2a934bc)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** new mode `+dir-locals-autoreload-mode` with better implementation - ([e3f7bbc](https://github.com/abougouffa/minemacs/commit/e3f7bbc)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove `+dir-locals-open-or-create` - ([7c043f9](https://github.com/abougouffa/minemacs/commit/7c043f9)) - [@abougouffa](https://github.com/abougouffa)
- move `+clang-format-get-style` to `me-lib-extra` - ([8d40562](https://github.com/abougouffa/minemacs/commit/8d40562)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(window)** remove the font size hack, not generic enough - ([34cd1f7](https://github.com/abougouffa/minemacs/commit/34cd1f7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bookmark)** make use of `+ignore-root` - ([c9a4f7f](https://github.com/abougouffa/minemacs/commit/c9a4f7f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better implementation of `minemacs-bump-packages-async` - ([fa2aecc](https://github.com/abougouffa/minemacs/commit/fa2aecc)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edits in the `project` extensions - ([4456b27](https://github.com/abougouffa/minemacs/commit/4456b27)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better implementation of `+project-safe-root` - ([f62f5a1](https://github.com/abougouffa/minemacs/commit/f62f5a1)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** cleanup `+def-dedicated-tab!`, not returns `+CMD-dedicated-tab` - ([d7a6e2e](https://github.com/abougouffa/minemacs/commit/d7a6e2e)) - [@abougouffa](https://github.com/abougouffa)
- **(eat)** enable reduced font size - ([f6db2b3](https://github.com/abougouffa/minemacs/commit/f6db2b3)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/dotnet)** minor edit - ([1c35ed0](https://github.com/abougouffa/minemacs/commit/1c35ed0)) - [@abougouffa](https://github.com/abougouffa)
- **(tldr)** enable more docs - ([28fe7f2](https://github.com/abougouffa/minemacs/commit/28fe7f2)) - [@abougouffa](https://github.com/abougouffa)
- **(vterm)** reduced font size, restore default `vterm-tramp-shells` - ([530b887](https://github.com/abougouffa/minemacs/commit/530b887)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d430b82](https://github.com/abougouffa/minemacs/commit/d430b82)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4d754db](https://github.com/abougouffa/minemacs/commit/4d754db)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([78feca3](https://github.com/abougouffa/minemacs/commit/78feca3)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.15.0](https://github.com/abougouffa/minemacs/compare/55c47a6..v12.15.0) - 2025-04-20
#### Bug Fixes
- **(on-demand/dotnet)** correctly handle paths in `dotnet-new` - ([33491a5](https://github.com/abougouffa/minemacs/commit/33491a5)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([ffa1f06](https://github.com/abougouffa/minemacs/commit/ffa1f06)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([e6d09fc](https://github.com/abougouffa/minemacs/commit/e6d09fc)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([55c47a6](https://github.com/abougouffa/minemacs/commit/55c47a6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+completion-mark-category` - ([636eb75](https://github.com/abougouffa/minemacs/commit/636eb75)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add `me-dotnet` to better support .NET development - ([4845ee7](https://github.com/abougouffa/minemacs/commit/4845ee7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bookmark)** add the bookmark file to `recentf`'s ignore list - ([77aa682](https://github.com/abougouffa/minemacs/commit/77aa682)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/dotnet)** smart handling of templates - ([d2cf4e0](https://github.com/abougouffa/minemacs/commit/d2cf4e0)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** register Visual Studio's "*.sln" & `*.fsproj` as markers - ([6bba980](https://github.com/abougouffa/minemacs/commit/6bba980)) - [@abougouffa](https://github.com/abougouffa)
- **(rainbow-delimiters)** don't enable in `makefile-mode` - ([272d9fc](https://github.com/abougouffa/minemacs/commit/272d9fc)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([a96f7a9](https://github.com/abougouffa/minemacs/commit/a96f7a9)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([c62dee1](https://github.com/abougouffa/minemacs/commit/c62dee1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([b99b3e2](https://github.com/abougouffa/minemacs/commit/b99b3e2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.14.0](https://github.com/abougouffa/minemacs/compare/e814095..v12.14.0) - 2025-04-19
#### Documentation
- regenerate the documentation - ([59ccb63](https://github.com/abougouffa/minemacs/commit/59ccb63)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** restore `reformatter` - ([669319e](https://github.com/abougouffa/minemacs/commit/669319e)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `format-all` obsolete - ([7b95501](https://github.com/abougouffa/minemacs/commit/7b95501)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `combobulate` obsolete - ([9514bdc](https://github.com/abougouffa/minemacs/commit/9514bdc)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `ts-movement` obsolete - ([2160953](https://github.com/abougouffa/minemacs/commit/2160953)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** make `sx` obsolete - ([708b52d](https://github.com/abougouffa/minemacs/commit/708b52d)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** add `consult-yasnippet` - ([3a59e75](https://github.com/abougouffa/minemacs/commit/3a59e75)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `ef-themes` obsolete - ([98bf992](https://github.com/abougouffa/minemacs/commit/98bf992)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `lacarte` obsolete - ([d641b50](https://github.com/abougouffa/minemacs/commit/d641b50)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `keycast` obsolete - ([660a8e7](https://github.com/abougouffa/minemacs/commit/660a8e7)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** make `window-purpose` obsolete - ([17610e1](https://github.com/abougouffa/minemacs/commit/17610e1)) - [@abougouffa](https://github.com/abougouffa)
- make `org-gtd` and `me-gtd` obsolete - ([80a45ae](https://github.com/abougouffa/minemacs/commit/80a45ae)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(git)** ignore `aidermacs` files - ([7aac703](https://github.com/abougouffa/minemacs/commit/7aac703)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `ess` and `julia-mode` to `on-demand/me-statistics` - ([55f48f5](https://github.com/abougouffa/minemacs/commit/55f48f5)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(casual)** bind `casual-make-tmenu` - ([661de0d](https://github.com/abougouffa/minemacs/commit/661de0d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fallback to `~/org` in `minemacs-default-org-dir` - ([e814095](https://github.com/abougouffa/minemacs/commit/e814095)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-cursor)** register `+dtrt-indent-tab-to-tab-stop` command - ([22d8bc5](https://github.com/abougouffa/minemacs/commit/22d8bc5)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0652a6c](https://github.com/abougouffa/minemacs/commit/0652a6c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.13.1](https://github.com/abougouffa/minemacs/compare/31032fe..v12.13.1) - 2025-04-18
#### Bug Fixes
- **(aidermacs)** update for the new way of choosing the default model - ([a4ef986](https://github.com/abougouffa/minemacs/commit/a4ef986)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** fix opening files with Emacs when Enlight is displayed - ([31032fe](https://github.com/abougouffa/minemacs/commit/31032fe)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** scale the text in the right buffer - ([e9dc470](https://github.com/abougouffa/minemacs/commit/e9dc470)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(window)** group and refactor common stuff - ([c73d556](https://github.com/abougouffa/minemacs/commit/c73d556)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([feb9875](https://github.com/abougouffa/minemacs/commit/feb9875)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.13.0](https://github.com/abougouffa/minemacs/compare/ed0a206..v12.13.0) - 2025-04-17
#### Bug Fixes
- **(on-demand/powershell)** fix the install path for PowerShell LSP - ([aa1c991](https://github.com/abougouffa/minemacs/commit/aa1c991)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([514f6df](https://github.com/abougouffa/minemacs/commit/514f6df)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+fn-sans-advice` - ([dea7412](https://github.com/abougouffa/minemacs/commit/dea7412)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** add support for shortening URLs using `is.gd` - ([e6dfa52](https://github.com/abougouffa/minemacs/commit/e6dfa52)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `verb` and `impostman` obsoletes - ([e670968](https://github.com/abougouffa/minemacs/commit/e670968)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** enhance `minemacs-bump-packages` - ([e197a21](https://github.com/abougouffa/minemacs/commit/e197a21)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot-booster)** don't enable by default, incompatible with `pet-mode` - ([ed0a206](https://github.com/abougouffa/minemacs/commit/ed0a206)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/powershell)** revert the install dir fix, merged :upstream - ([8dcc5f3](https://github.com/abougouffa/minemacs/commit/8dcc5f3)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/qt)** use `xml-mode` for `*.qrc` files - ([92ca504](https://github.com/abougouffa/minemacs/commit/92ca504)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** register `.magic` as a project marker for Mojo projects - ([e116492](https://github.com/abougouffa/minemacs/commit/e116492)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ff979fa](https://github.com/abougouffa/minemacs/commit/ff979fa)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e652460](https://github.com/abougouffa/minemacs/commit/e652460)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.12.1](https://github.com/abougouffa/minemacs/compare/eb264d7..v12.12.1) - 2025-04-16
#### Documentation
- regenerate the documentation - ([cbe5c75](https://github.com/abougouffa/minemacs/commit/cbe5c75)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(eglot-ltex)** move the LTeX+ LS options to `me-eglot-ltex` - ([f7c9e75](https://github.com/abougouffa/minemacs/commit/f7c9e75)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dape)** minor tweaks and remove unused `+dape-transient` - ([96a6082](https://github.com/abougouffa/minemacs/commit/96a6082)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** recenter the Enlight buffer on window size change - ([8990049](https://github.com/abougouffa/minemacs/commit/8990049)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex-ls-plus)** use `compile` - ([eb264d7](https://github.com/abougouffa/minemacs/commit/eb264d7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5fe9653](https://github.com/abougouffa/minemacs/commit/5fe9653)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.12.0](https://github.com/abougouffa/minemacs/compare/5d32d34..v12.12.0) - 2025-04-15
#### Documentation
- add information about `MINEMACS_ORG_DIR` - ([eb2bf18](https://github.com/abougouffa/minemacs/commit/eb2bf18)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([53b9737](https://github.com/abougouffa/minemacs/commit/53b9737)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** better implementation of `+github-download-release` - ([e2ed9b6](https://github.com/abougouffa/minemacs/commit/e2ed9b6)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `minemacs-bump-packages-async` - ([2807922](https://github.com/abougouffa/minemacs/commit/2807922)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(submodule)** bump version for `benchmark-init` - ([724386a](https://github.com/abougouffa/minemacs/commit/724386a)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- use `condition-case` when appropriate - ([5d32d34](https://github.com/abougouffa/minemacs/commit/5d32d34)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** better automatic detection of Org directory - ([aa5814c](https://github.com/abougouffa/minemacs/commit/aa5814c)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** enable `corfu-indexed-mode` - ([acf7c1f](https://github.com/abougouffa/minemacs/commit/acf7c1f)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** reference the `ltex-ls-plus` - ([0dd7771](https://github.com/abougouffa/minemacs/commit/0dd7771)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex-ls-plus)** use the new fork `LTeX+`, add a command to download it - ([bde9dbb](https://github.com/abougouffa/minemacs/commit/bde9dbb)) - [@abougouffa](https://github.com/abougouffa)
- **(vertico)** enable `vertico-indexed-mode` - ([5c5fa76](https://github.com/abougouffa/minemacs/commit/5c5fa76)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f904f5a](https://github.com/abougouffa/minemacs/commit/f904f5a)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([7d1bc09](https://github.com/abougouffa/minemacs/commit/7d1bc09)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.11.0](https://github.com/abougouffa/minemacs/compare/129c585..v12.11.0) - 2025-04-14
#### Documentation
- regenerate the documentation - ([3e34aa4](https://github.com/abougouffa/minemacs/commit/3e34aa4)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(cocogitto)** call `+cocogitto-bump` with `C-u` to auto bump - ([a605050](https://github.com/abougouffa/minemacs/commit/a605050)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** `org-indent` styling for `org-modern` via `org-modern-indent` - ([6165aab](https://github.com/abougouffa/minemacs/commit/6165aab)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** regenerate loaddefs - ([ea5d16b](https://github.com/abougouffa/minemacs/commit/ea5d16b)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** mark the `+project-` prefix as project aware commands - ([6f4c2ab](https://github.com/abougouffa/minemacs/commit/6f4c2ab)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** make `.jj` a project marker for Jujutsu projects - ([b8592d0](https://github.com/abougouffa/minemacs/commit/b8592d0)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** add spacing when `tab-bar-tab-hints` is `nil` - ([129c585](https://github.com/abougouffa/minemacs/commit/129c585)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([dd62553](https://github.com/abougouffa/minemacs/commit/dd62553)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ff904a8](https://github.com/abougouffa/minemacs/commit/ff904a8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.10.4](https://github.com/abougouffa/minemacs/compare/7da1453..v12.10.4) - 2025-04-13
#### Refactoring
- **(lib)** remove the unused `+mode-alist-add-ts-modes!` - ([fd8b94c](https://github.com/abougouffa/minemacs/commit/fd8b94c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(beardbolt)** only add `rust-ts-mode` to `beardbolt-languages` - ([0ac2069](https://github.com/abougouffa/minemacs/commit/0ac2069)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** don't ask about reloading init-file after bumping packages - ([6b93142](https://github.com/abougouffa/minemacs/commit/6b93142)) - [@abougouffa](https://github.com/abougouffa)
- **(crm)** make use of the new Emacs 31 `crm-prompt` & port it back - ([067162c](https://github.com/abougouffa/minemacs/commit/067162c)) - [@abougouffa](https://github.com/abougouffa)
- **(rmsbolt)** no need to manually add treesit modes - ([3331cc7](https://github.com/abougouffa/minemacs/commit/3331cc7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([56c93ba](https://github.com/abougouffa/minemacs/commit/56c93ba)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7da1453](https://github.com/abougouffa/minemacs/commit/7da1453)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.10.3](https://github.com/abougouffa/minemacs/compare/2060de4..v12.10.3) - 2025-04-11
#### Bug Fixes
- **(window)** avoid an error when running in headless mode - ([2060de4](https://github.com/abougouffa/minemacs/commit/2060de4)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([52a33ba](https://github.com/abougouffa/minemacs/commit/52a33ba)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.10.2](https://github.com/abougouffa/minemacs/compare/941d4ac..v12.10.2) - 2025-04-11
#### Bug Fixes
- **(auto-revert)** fix immediate auto-reverting when the file is deleted - ([a772fbd](https://github.com/abougouffa/minemacs/commit/a772fbd)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** bug fix in the multi-file Org export advice - ([c0d41ed](https://github.com/abougouffa/minemacs/commit/c0d41ed)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(on-demand/opencl)** add the `.clc` and `.opencl` extensions - ([941d4ac](https://github.com/abougouffa/minemacs/commit/941d4ac)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.10.1](https://github.com/abougouffa/minemacs/compare/edb5755..v12.10.1) - 2025-04-08
#### Bug Fixes
- **(apheleia)** properly use the `.clang-format` file when available - ([4bc3c40](https://github.com/abougouffa/minemacs/commit/4bc3c40)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- move some code around - ([edb5755](https://github.com/abougouffa/minemacs/commit/edb5755)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(treesit)** minor recipes edits - ([4a25714](https://github.com/abougouffa/minemacs/commit/4a25714)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f3f15ab](https://github.com/abougouffa/minemacs/commit/f3f15ab)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.10.0](https://github.com/abougouffa/minemacs/compare/4d87a32..v12.10.0) - 2025-04-05
#### Bug Fixes
- **(window)** decrease scale only once - ([81ebb96](https://github.com/abougouffa/minemacs/commit/81ebb96)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** update information relative to the tested Emacs versions - ([6135339](https://github.com/abougouffa/minemacs/commit/6135339)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(window)** set zoom level for some buffers after `display-buffer` - ([78c5bc8](https://github.com/abougouffa/minemacs/commit/78c5bc8)) - [@abougouffa](https://github.com/abougouffa)
- remove the unused `+window-adjust-size-transient` - ([2b20c6a](https://github.com/abougouffa/minemacs/commit/2b20c6a)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** explicitly add Emacs 30.1 to the test matrix - ([4d87a32](https://github.com/abougouffa/minemacs/commit/4d87a32)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(window)** make terminal windows take 20% instead of 30% height - ([17f6086](https://github.com/abougouffa/minemacs/commit/17f6086)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a4dcd86](https://github.com/abougouffa/minemacs/commit/a4dcd86)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.9.0](https://github.com/abougouffa/minemacs/compare/395ada3..v12.9.0) - 2025-04-01
#### Bug Fixes
- **(ws-butler)** use the right repo with the right parameters - ([4a5e4a0](https://github.com/abougouffa/minemacs/commit/4a5e4a0)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- update copyright year - ([4098768](https://github.com/abougouffa/minemacs/commit/4098768)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([387e93f](https://github.com/abougouffa/minemacs/commit/387e93f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(lib)** don't ask stupid questions when bumping packages versions - ([8069e4d](https://github.com/abougouffa/minemacs/commit/8069e4d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(init)** better detection of native compilation support - ([f6e13a5](https://github.com/abougouffa/minemacs/commit/f6e13a5)) - [@abougouffa](https://github.com/abougouffa)
- **(lib)** minor edits - ([0eebec3](https://github.com/abougouffa/minemacs/commit/0eebec3)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** `markdown-ts-mode` is now built-in - ([969a287](https://github.com/abougouffa/minemacs/commit/969a287)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([142cf5c](https://github.com/abougouffa/minemacs/commit/142cf5c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([90b0c62](https://github.com/abougouffa/minemacs/commit/90b0c62)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4743259](https://github.com/abougouffa/minemacs/commit/4743259)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1cc9419](https://github.com/abougouffa/minemacs/commit/1cc9419)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([395ada3](https://github.com/abougouffa/minemacs/commit/395ada3)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.8.1](https://github.com/abougouffa/minemacs/compare/594e6fd..v12.8.1) - 2025-03-07
#### Nitpicks, changes with no side effect
- rename an internal constant - ([5d7ab43](https://github.com/abougouffa/minemacs/commit/5d7ab43)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([3333f6a](https://github.com/abougouffa/minemacs/commit/3333f6a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1a0d890](https://github.com/abougouffa/minemacs/commit/1a0d890)) - [@abougouffa](https://github.com/abougouffa)
- move an obsolete constant - ([0d272b1](https://github.com/abougouffa/minemacs/commit/0d272b1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([b942d79](https://github.com/abougouffa/minemacs/commit/b942d79)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8573f61](https://github.com/abougouffa/minemacs/commit/8573f61)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([594e6fd](https://github.com/abougouffa/minemacs/commit/594e6fd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.8.0](https://github.com/abougouffa/minemacs/compare/987c771..v12.8.0) - 2025-02-24
#### Features
- **(ai)** add initial support for `aidermacs`, prefer local LLMs - ([f79b2b8](https://github.com/abougouffa/minemacs/commit/f79b2b8)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** make `org-rich-yank` obsolete - ([4d20284](https://github.com/abougouffa/minemacs/commit/4d20284)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(whisper)** better defaults - ([987c771](https://github.com/abougouffa/minemacs/commit/987c771)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0d89424](https://github.com/abougouffa/minemacs/commit/0d89424)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.7.0](https://github.com/abougouffa/minemacs/compare/d2f1053..v12.7.0) - 2025-02-09
#### Documentation
- regenerate the documentation - ([975d810](https://github.com/abougouffa/minemacs/commit/975d810)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(magit)** replace `magit-iconify` with the new builtin icons support - ([bb69461](https://github.com/abougouffa/minemacs/commit/bb69461)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** replace `magit-file-icons` with `magit-iconify` - ([d2f1053](https://github.com/abougouffa/minemacs/commit/d2f1053)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(elisa)** register `elisa-download-sqlite-vss` as build function - ([bdc9eed](https://github.com/abougouffa/minemacs/commit/bdc9eed)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6f53f79](https://github.com/abougouffa/minemacs/commit/6f53f79)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e6949f5](https://github.com/abougouffa/minemacs/commit/e6949f5)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.6.0](https://github.com/abougouffa/minemacs/compare/67db86b..v12.6.0) - 2025-02-05
#### Bug Fixes
- **(magit-file-icons)** enable back, fixed incompatibility with new Magit - ([e9e8f57](https://github.com/abougouffa/minemacs/commit/e9e8f57)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(ai)** add a comment - ([83fa406](https://github.com/abougouffa/minemacs/commit/83fa406)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ai)** make `copilot` obsolete (not used, and I prefer local agents) - ([17dd038](https://github.com/abougouffa/minemacs/commit/17dd038)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(ellama)** rename a function - ([67db86b](https://github.com/abougouffa/minemacs/commit/67db86b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(casual)** add keybindings for `casual-image` - ([a16758d](https://github.com/abougouffa/minemacs/commit/a16758d)) - [@abougouffa](https://github.com/abougouffa)
- **(elisa)** use the same providers as Ellama - ([c313cf0](https://github.com/abougouffa/minemacs/commit/c313cf0)) - [@abougouffa](https://github.com/abougouffa)
- **(ellama)** make use of the new helpers, prefer fast embedding models - ([275675b](https://github.com/abougouffa/minemacs/commit/275675b)) - [@abougouffa](https://github.com/abougouffa)
- **(ellama)** make `+ellama-set-available-providers` a command - ([d35d1da](https://github.com/abougouffa/minemacs/commit/d35d1da)) - [@abougouffa](https://github.com/abougouffa)
- **(llm)** add some Ollama helpers - ([04ef6d3](https://github.com/abougouffa/minemacs/commit/04ef6d3)) - [@abougouffa](https://github.com/abougouffa)
- **(llm-ollama)** remove server management, should be done externally - ([e658569](https://github.com/abougouffa/minemacs/commit/e658569)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-magit)** remove project-specific defaults - ([f4ed4e6](https://github.com/abougouffa/minemacs/commit/f4ed4e6)) - [@abougouffa](https://github.com/abougouffa)
- **(plantuml)** bump the version of PlanUML JAR - ([4071745](https://github.com/abougouffa/minemacs/commit/4071745)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([42c1e07](https://github.com/abougouffa/minemacs/commit/42c1e07)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.5.1](https://github.com/abougouffa/minemacs/compare/39fce90..v12.5.1) - 2025-01-28
#### Bug Fixes
- **(magit-file-icons)** temporary disable, not working with new Magit version - ([be40918](https://github.com/abougouffa/minemacs/commit/be40918)) - [@abougouffa](https://github.com/abougouffa)
- use `if-let*` instead of the obsolete `if-let` - ([fe5342f](https://github.com/abougouffa/minemacs/commit/fe5342f)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** document the `MINEMACS_BUILTIN_ONLY` environment variable - ([0077b51](https://github.com/abougouffa/minemacs/commit/0077b51)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(make)** minor updates - ([39fce90](https://github.com/abougouffa/minemacs/commit/39fce90)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- add an example script for monitoring a directory - ([807debb](https://github.com/abougouffa/minemacs/commit/807debb)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move obsolete definitions to `me-obsolete-defs` - ([2b610c9](https://github.com/abougouffa/minemacs/commit/2b610c9)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult)** bind `fd` to `M-s F` to avoid conflict in `dired-mode` - ([a27c385](https://github.com/abougouffa/minemacs/commit/a27c385)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** reuse the same buffer in `+shellcheck-describe-error` (WIP) - ([47b6021](https://github.com/abougouffa/minemacs/commit/47b6021)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1c1c048](https://github.com/abougouffa/minemacs/commit/1c1c048)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.5.0](https://github.com/abougouffa/minemacs/compare/67de673..v12.5.0) - 2025-01-15
#### Bug Fixes
- **(core)** better file matching in `+json-schema-for-file` - ([a75eab0](https://github.com/abougouffa/minemacs/commit/a75eab0)) - [@abougouffa](https://github.com/abougouffa)
- **(recentf)** explicitly set the file path - ([4ec182e](https://github.com/abougouffa/minemacs/commit/4ec182e)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** make links relative - ([33ce5ea](https://github.com/abougouffa/minemacs/commit/33ce5ea)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([8b5c152](https://github.com/abougouffa/minemacs/commit/8b5c152)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([a6a441e](https://github.com/abougouffa/minemacs/commit/a6a441e)) - [@abougouffa](https://github.com/abougouffa)
- regenerate documentation - ([67de673](https://github.com/abougouffa/minemacs/commit/67de673)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+explainshell` - ([3052104](https://github.com/abougouffa/minemacs/commit/3052104)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib)** add `+jira-commit-auto-insert-ticket-id` - ([c16e786](https://github.com/abougouffa/minemacs/commit/c16e786)) - [@abougouffa](https://github.com/abougouffa)
- generic implementation of inserting YAML schemas - ([6cc5ebe](https://github.com/abougouffa/minemacs/commit/6cc5ebe)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(docs)** rename documentation files - ([2e867d1](https://github.com/abougouffa/minemacs/commit/2e867d1)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- cleanup - ([c48737c](https://github.com/abougouffa/minemacs/commit/c48737c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- make `+ansi-color-apply-on-buffer` a command, move to lib - ([e65402a](https://github.com/abougouffa/minemacs/commit/e65402a)) - [@abougouffa](https://github.com/abougouffa)
- move subtle color hack to `+color-subtle` and fix an edge case - ([c39dc7f](https://github.com/abougouffa/minemacs/commit/c39dc7f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(copilot)** complete with TAB - ([b181e8b](https://github.com/abougouffa/minemacs/commit/b181e8b)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edits - ([9ca0aaf](https://github.com/abougouffa/minemacs/commit/9ca0aaf)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `minemacs-default-org-dir` - ([d6aee19](https://github.com/abougouffa/minemacs/commit/d6aee19)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** don't mess with YASnippet - ([c39ed24](https://github.com/abougouffa/minemacs/commit/c39ed24)) - [@abougouffa](https://github.com/abougouffa)
- **(nav)** move `p-search` from `me-experimental` to `me-nav` - ([7e178be](https://github.com/abougouffa/minemacs/commit/7e178be)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** accept multiple dirs in `+project-scan-for-projects` - ([d38da3d](https://github.com/abougouffa/minemacs/commit/d38da3d)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** move config example for `jiralib` to `skel/config.el` - ([005870c](https://github.com/abougouffa/minemacs/commit/005870c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bf303dd](https://github.com/abougouffa/minemacs/commit/bf303dd)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([aa23913](https://github.com/abougouffa/minemacs/commit/aa23913)) - [@abougouffa](https://github.com/abougouffa)
- better implementation of `+jira-commit-auto-insert-ticket-id` - ([ceec2f3](https://github.com/abougouffa/minemacs/commit/ceec2f3)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([65b86e1](https://github.com/abougouffa/minemacs/commit/65b86e1)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.4.0](https://github.com/abougouffa/minemacs/compare/a2a8f7f..v12.4.0) - 2024-12-26
#### Bug Fixes
- **(ansible)** fix the recipe - ([821d7c4](https://github.com/abougouffa/minemacs/commit/821d7c4)) - [@abougouffa](https://github.com/abougouffa)
- **(transient)** install a newer version at the right place - ([2540cbb](https://github.com/abougouffa/minemacs/commit/2540cbb)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ai)** initial support for Copilot - ([009f7a2](https://github.com/abougouffa/minemacs/commit/009f7a2)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add a command to display ShellCheck errors description - ([7be3de3](https://github.com/abougouffa/minemacs/commit/7be3de3)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/gitlab-ci)** add a helper command `+gitlab-ci-add-schema` - ([b45c89a](https://github.com/abougouffa/minemacs/commit/b45c89a)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/yaml)** add `ansible-mode` - ([7b82935](https://github.com/abougouffa/minemacs/commit/7b82935)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** inhibit compacting font caches - ([2e777e0](https://github.com/abougouffa/minemacs/commit/2e777e0)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** larger kill ring size - ([d1eb039](https://github.com/abougouffa/minemacs/commit/d1eb039)) - [@abougouffa](https://github.com/abougouffa)
- **(copilot)** customize the installation directory - ([705d6c6](https://github.com/abougouffa/minemacs/commit/705d6c6)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** minor tweaks in the integration with `symbol-overlay` - ([55eb96d](https://github.com/abougouffa/minemacs/commit/55eb96d)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** change the ControlPath socket name format - ([7788f5b](https://github.com/abougouffa/minemacs/commit/7788f5b)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** more responsive SSH editing - ([a2a8f7f](https://github.com/abougouffa/minemacs/commit/a2a8f7f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([baff206](https://github.com/abougouffa/minemacs/commit/baff206)) - [@abougouffa](https://github.com/abougouffa)
- edit the message displayed after loading Emacs - ([a975058](https://github.com/abougouffa/minemacs/commit/a975058)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([f665741](https://github.com/abougouffa/minemacs/commit/f665741)) - [@abougouffa](https://github.com/abougouffa)
- better implementation of `+shellcheck-describe-error` - ([46a4064](https://github.com/abougouffa/minemacs/commit/46a4064)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([ea65ade](https://github.com/abougouffa/minemacs/commit/ea65ade)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.3.0](https://github.com/abougouffa/minemacs/compare/b943fe9..v12.3.0) - 2024-12-17
#### Bug Fixes
- **(builtin)** fix a couple of issues with Emacs 29 - ([95ce8e9](https://github.com/abougouffa/minemacs/commit/95ce8e9)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** compatibility with the built-in version of Eglot in Emacs 29 - ([77d6ac2](https://github.com/abougouffa/minemacs/commit/77d6ac2)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** require `compat`, fixes issues on Emacs 29 - ([1692e34](https://github.com/abougouffa/minemacs/commit/1692e34)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** cleanup references to `straight-x` - ([008e789](https://github.com/abougouffa/minemacs/commit/008e789)) - [@abougouffa](https://github.com/abougouffa)
- **(transient)** `forge` needs a recent version - ([5ad1189](https://github.com/abougouffa/minemacs/commit/5ad1189)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** update information about the supported version of Emacs - ([64c2e4b](https://github.com/abougouffa/minemacs/commit/64c2e4b)) - [@abougouffa](https://github.com/abougouffa)
- update comments - ([effcd05](https://github.com/abougouffa/minemacs/commit/effcd05)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([f6ca225](https://github.com/abougouffa/minemacs/commit/f6ca225)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- add an option to load only built-in package, cleanup the config - ([362b6ee](https://github.com/abougouffa/minemacs/commit/362b6ee)) - [@abougouffa](https://github.com/abougouffa)
- remove `me-daemon` - ([b943fe9](https://github.com/abougouffa/minemacs/commit/b943fe9)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** only test Emacs 29.4 - ([9cc5b8f](https://github.com/abougouffa/minemacs/commit/9cc5b8f)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(mu4e)** move the daemon integration to `me-mu4e` - ([ef3ac20](https://github.com/abougouffa/minemacs/commit/ef3ac20)) - [@abougouffa](https://github.com/abougouffa)
- include `satch` and `once` as internal dependencies - ([52661b2](https://github.com/abougouffa/minemacs/commit/52661b2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(pcap-mode)** bind `n` and `p` + add more extensions - ([06031b7](https://github.com/abougouffa/minemacs/commit/06031b7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([38146f8](https://github.com/abougouffa/minemacs/commit/38146f8)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([965e59f](https://github.com/abougouffa/minemacs/commit/965e59f)) - [@abougouffa](https://github.com/abougouffa)
- better and simpler error catching in `+apply-patch-dwim` - ([d882484](https://github.com/abougouffa/minemacs/commit/d882484)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9cf35f1](https://github.com/abougouffa/minemacs/commit/9cf35f1)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([115cbd8](https://github.com/abougouffa/minemacs/commit/115cbd8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.2.0](https://github.com/abougouffa/minemacs/compare/0309810..v12.2.0) - 2024-12-11
#### Bug Fixes
- **(corfu)** binding `corfu-send` to `RET` isn't a good option (ex. ielm) - ([c240749](https://github.com/abougouffa/minemacs/commit/c240749)) - [@abougouffa](https://github.com/abougouffa)
- **(repo)** correctly treat the ANSI colors in `repo-status` buffer - ([5691ade](https://github.com/abougouffa/minemacs/commit/5691ade)) - [@abougouffa](https://github.com/abougouffa)
- remove references to the obsolete `me-god` module - ([5053cf5](https://github.com/abougouffa/minemacs/commit/5053cf5)) - [@abougouffa](https://github.com/abougouffa)
- don't use unlimited GC threshold, cases too much memory usage - ([0309810](https://github.com/abougouffa/minemacs/commit/0309810)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(nav)** replace `goto-last-change` with `goto-chg` - ([f0c984a](https://github.com/abougouffa/minemacs/commit/f0c984a)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for SmallTalk - ([d50c7fd](https://github.com/abougouffa/minemacs/commit/d50c7fd)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for `jira-markup-mode` - ([8291d2c](https://github.com/abougouffa/minemacs/commit/8291d2c)) - [@abougouffa](https://github.com/abougouffa)
- **(valgrind)** initial `valgrind-mode` (WIP) - ([8640ba5](https://github.com/abougouffa/minemacs/commit/8640ba5)) - [@abougouffa](https://github.com/abougouffa)
- add the `me-experimental` module - ([e11138d](https://github.com/abougouffa/minemacs/commit/e11138d)) - [@abougouffa](https://github.com/abougouffa)
- make `me-god` obsolete, not used from a long time - ([f5aeaf4](https://github.com/abougouffa/minemacs/commit/f5aeaf4)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(core)** minor edits - ([526451a](https://github.com/abougouffa/minemacs/commit/526451a)) - [@abougouffa](https://github.com/abougouffa)
- **(god)** which-key integration is no longer experimental - ([c721298](https://github.com/abougouffa/minemacs/commit/c721298)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** cleanup and remove dependency on `satch` and `once` - ([7ba5522](https://github.com/abougouffa/minemacs/commit/7ba5522)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** cleanup and refactor `init.el` and `early-ini.el` - ([2a07e41](https://github.com/abougouffa/minemacs/commit/2a07e41)) - [@abougouffa](https://github.com/abougouffa)
- move `crdt` from `me-docs` to `me-editor` - ([22c97d0](https://github.com/abougouffa/minemacs/commit/22c97d0)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(multiple-cursors)** use with `parinfer-rust` - ([bd14cf8](https://github.com/abougouffa/minemacs/commit/bd14cf8)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** better defaults for fallback packages - ([ffac8b6](https://github.com/abougouffa/minemacs/commit/ffac8b6)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** use the default cursor type - ([e07c0ed](https://github.com/abougouffa/minemacs/commit/e07c0ed)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** restore the default behavior for `ESC` - ([dbdf163](https://github.com/abougouffa/minemacs/commit/dbdf163)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** use `modus-operandi` as fallback theme - ([7400c6e](https://github.com/abougouffa/minemacs/commit/7400c6e)) - [@abougouffa](https://github.com/abougouffa)
- **(valgrind)** use the new `valgrind-mode` in `valgrind` - ([ae24e1d](https://github.com/abougouffa/minemacs/commit/ae24e1d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e8c17d1](https://github.com/abougouffa/minemacs/commit/e8c17d1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7a43831](https://github.com/abougouffa/minemacs/commit/7a43831)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([6e54cc6](https://github.com/abougouffa/minemacs/commit/6e54cc6)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1c30caa](https://github.com/abougouffa/minemacs/commit/1c30caa)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.1.0](https://github.com/abougouffa/minemacs/compare/ade2563..v12.1.0) - 2024-12-05
#### Bug Fixes
- **(citre)** fix the problem with SSHFS and other protocols - ([ea595f2](https://github.com/abougouffa/minemacs/commit/ea595f2)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** fail silently when the mode don't support `hs-minor-mode` - ([ade2563](https://github.com/abougouffa/minemacs/commit/ade2563)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(mu4e)** beautify the UI using `nerd-icons` - ([ab41a3d](https://github.com/abougouffa/minemacs/commit/ab41a3d)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** add a generic function `+nerd-icons-icon` - ([d0ebccd](https://github.com/abougouffa/minemacs/commit/d0ebccd)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** load Coccinelle integration when available - ([fb8f7f2](https://github.com/abougouffa/minemacs/commit/fb8f7f2)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** add initial support for `call-graph` - ([53c6a3f](https://github.com/abougouffa/minemacs/commit/53c6a3f)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `focus` obsolete - ([e847c74](https://github.com/abougouffa/minemacs/commit/e847c74)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `olivetti` obsolete - ([3e81d9f](https://github.com/abougouffa/minemacs/commit/3e81d9f)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add Gitlab integration - ([898ccc5](https://github.com/abougouffa/minemacs/commit/898ccc5)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(citar)** remove unneeded stuff - ([ff385e0](https://github.com/abougouffa/minemacs/commit/ff385e0)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib)** minor edit - ([e2d714a](https://github.com/abougouffa/minemacs/commit/e2d714a)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(tools)** restore `emamux`, pretty useful - ([dd7a45f](https://github.com/abougouffa/minemacs/commit/dd7a45f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** minor edits, make byte-compiler happy - ([cdc39bb](https://github.com/abougouffa/minemacs/commit/cdc39bb)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** better deferring of Org related packages - ([5edcdd9](https://github.com/abougouffa/minemacs/commit/5edcdd9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.0.1](https://github.com/abougouffa/minemacs/compare/db487f7..v12.0.1) - 2024-12-01
#### Bug Fixes
- **(mu4e)** refactor and fix some deprecated stuff - ([c303c96](https://github.com/abougouffa/minemacs/commit/c303c96)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** load the right LaTeX module when exporting Org files - ([db487f7](https://github.com/abougouffa/minemacs/commit/db487f7)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- some code formatting - ([8138a9a](https://github.com/abougouffa/minemacs/commit/8138a9a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v12.0.0](https://github.com/abougouffa/minemacs/compare/c66f43b..v12.0.0) - 2024-12-01
#### Bug Fixes
- **(core)** rename the module `me-search` to `me-nav` - ([39c9ea7](https://github.com/abougouffa/minemacs/commit/39c9ea7)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fix a mistake in `minemacs-load-module` - ([1d53ac8](https://github.com/abougouffa/minemacs/commit/1d53ac8)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** fix the crash on invalid color - ([d3582c9](https://github.com/abougouffa/minemacs/commit/d3582c9)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** regression when `early-init.el` isn't loaded - ([a64fd03](https://github.com/abougouffa/minemacs/commit/a64fd03)) - [@abougouffa](https://github.com/abougouffa)
- **(media)** use `browse-url-handlers` to handle YouTube links - ([caa5684](https://github.com/abougouffa/minemacs/commit/caa5684)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** fix the Org export init script - ([2fcd076](https://github.com/abougouffa/minemacs/commit/2fcd076)) - [@abougouffa](https://github.com/abougouffa)
- **(smergs)** remove reference to deleted command, autoload the others - ([6e72f12](https://github.com/abougouffa/minemacs/commit/6e72f12)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([a30e68c](https://github.com/abougouffa/minemacs/commit/a30e68c)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(bitbake-ts-mode)** temporary disable, too poor font lock - ([85b2301](https://github.com/abougouffa/minemacs/commit/85b2301)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for Nushell - ([614fc62](https://github.com/abougouffa/minemacs/commit/614fc62)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** add `treesit-jump` - ([3b90d77](https://github.com/abougouffa/minemacs/commit/3b90d77)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- use the default indentation function - ([c84363d](https://github.com/abougouffa/minemacs/commit/c84363d)) - [@abougouffa](https://github.com/abougouffa)
- update links in comments to cope with `goto-addr` - ([6c19e9c](https://github.com/abougouffa/minemacs/commit/6c19e9c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** several small fixes - ([68b8ff0](https://github.com/abougouffa/minemacs/commit/68b8ff0)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** minor tweak to make the byte-compiler happy - ([f8066bf](https://github.com/abougouffa/minemacs/commit/f8066bf)) - [@abougouffa](https://github.com/abougouffa)
- **(electric)** extract sh keywords the right way - ([1cae438](https://github.com/abougouffa/minemacs/commit/1cae438)) - [@abougouffa](https://github.com/abougouffa)
- **(sudo-edit)** minor edit - ([53adef3](https://github.com/abougouffa/minemacs/commit/53adef3)) - [@abougouffa](https://github.com/abougouffa)
- cleanup and simplify - ([d8b071d](https://github.com/abougouffa/minemacs/commit/d8b071d)) - [@abougouffa](https://github.com/abougouffa)
- rename module `me-search` to `me-nav` - ([6b34b4b](https://github.com/abougouffa/minemacs/commit/6b34b4b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(citre)** better defaults + Xref `elisp` backend - ([e7843f8](https://github.com/abougouffa/minemacs/commit/e7843f8)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** better `gtags` defaults - ([c248633](https://github.com/abougouffa/minemacs/commit/c248633)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** minor keybinding tweaks - ([e20d366](https://github.com/abougouffa/minemacs/commit/e20d366)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** shorten the generated description in `+make-first-file-hook!` - ([74ea75c](https://github.com/abougouffa/minemacs/commit/74ea75c)) - [@abougouffa](https://github.com/abougouffa)
- **(elisp-plus)** use the default lisp-indent - ([f42cc92](https://github.com/abougouffa/minemacs/commit/f42cc92)) - [@abougouffa](https://github.com/abougouffa)
- **(gambol)** enable the `embark` integration - ([d5bd8ea](https://github.com/abougouffa/minemacs/commit/d5bd8ea)) - [@abougouffa](https://github.com/abougouffa)
- **(ggtags)** better defaults - ([2c25dbf](https://github.com/abougouffa/minemacs/commit/2c25dbf)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** remove reference to obsolete `make-directory-autoloads` - ([b937022](https://github.com/abougouffa/minemacs/commit/b937022)) - [@abougouffa](https://github.com/abougouffa)
- **(octave)** better integration of octave with eros - ([a73cc7d](https://github.com/abougouffa/minemacs/commit/a73cc7d)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** reduce animation time (iterations) - ([5004a72](https://github.com/abougouffa/minemacs/commit/5004a72)) - [@abougouffa](https://github.com/abougouffa)
- **(recentf)** add serialized variables' path to ignored list - ([c66f43b](https://github.com/abougouffa/minemacs/commit/c66f43b)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** reduce disk usage by `--filter=tree:0` - ([19f3c9b](https://github.com/abougouffa/minemacs/commit/19f3c9b)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** simplify the advice and consider `:disabled` - ([0a17989](https://github.com/abougouffa/minemacs/commit/0a17989)) - [@abougouffa](https://github.com/abougouffa)
- **(valgrind)** make the buffer name customizable - ([73b19ef](https://github.com/abougouffa/minemacs/commit/73b19ef)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([65a42b0](https://github.com/abougouffa/minemacs/commit/65a42b0)) - [@abougouffa](https://github.com/abougouffa)
- use `global-completion-preview-mode` when `corfu` isn't enabled - ([bb5134d](https://github.com/abougouffa/minemacs/commit/bb5134d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e330724](https://github.com/abougouffa/minemacs/commit/e330724)) - [@abougouffa](https://github.com/abougouffa)
- minor tweaks - ([077c83b](https://github.com/abougouffa/minemacs/commit/077c83b)) - [@abougouffa](https://github.com/abougouffa)
- enable `bug-reference-prog-mode` and set its face - ([ad03775](https://github.com/abougouffa/minemacs/commit/ad03775)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.7.0](https://github.com/abougouffa/minemacs/compare/187d7f0..v11.7.0) - 2024-11-27
#### Bug Fixes
- **(enlight)** `pulsar` cases annoying flashed in `enlight` - ([deea54b](https://github.com/abougouffa/minemacs/commit/deea54b)) - [@abougouffa](https://github.com/abougouffa)
- defer some packages - ([93efe42](https://github.com/abougouffa/minemacs/commit/93efe42)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** list some of the main features of MinEmacs - ([fca43c6](https://github.com/abougouffa/minemacs/commit/fca43c6)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([26009b6](https://github.com/abougouffa/minemacs/commit/26009b6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** reduce flickering by loading the theme's background-color early - ([55d1891](https://github.com/abougouffa/minemacs/commit/55d1891)) - [@abougouffa](https://github.com/abougouffa)
- **(dtrt-indent)** add `+dtrt-indent-tab-to-tab-stop` - ([9b2703e](https://github.com/abougouffa/minemacs/commit/9b2703e)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/python)** add `pip-requirements` - ([ed5000b](https://github.com/abougouffa/minemacs/commit/ed5000b)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add support for `difftastic` - ([8d77f7e](https://github.com/abougouffa/minemacs/commit/8d77f7e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(treesit)** enhance `+treesit-create-parser-in-buffer` and remove redundant code - ([daffc36](https://github.com/abougouffa/minemacs/commit/daffc36)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** rename a variable and a command - ([d0499c8](https://github.com/abougouffa/minemacs/commit/d0499c8)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(eglot)** don't automatically enable `eglot-inlay-hints-mode` - ([cc41559](https://github.com/abougouffa/minemacs/commit/cc41559)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot-booster)** automatically enable when available - ([3644eb0](https://github.com/abougouffa/minemacs/commit/3644eb0)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** minor tweak - ([187d7f0](https://github.com/abougouffa/minemacs/commit/187d7f0)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([3bee1e4](https://github.com/abougouffa/minemacs/commit/3bee1e4)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.6.0](https://github.com/abougouffa/minemacs/compare/86bc9fd..v11.6.0) - 2024-11-26
#### Bug Fixes
- **(pet)** use `fd` to avoid freezing Emacs in large codebases - ([e0e6e7a](https://github.com/abougouffa/minemacs/commit/e0e6e7a)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** return values of `projectile` emulation primitives - ([dc1a199](https://github.com/abougouffa/minemacs/commit/dc1a199)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** update the configuration examples - ([a612f01](https://github.com/abougouffa/minemacs/commit/a612f01)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+package-configured-p` - ([2dd5d4d](https://github.com/abougouffa/minemacs/commit/2dd5d4d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+with-no-proxies!` - ([86bc9fd](https://github.com/abougouffa/minemacs/commit/86bc9fd)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** restore `ffip` - ([0bc5f8b](https://github.com/abougouffa/minemacs/commit/0bc5f8b)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(pet)** remove the `fd` hack, merged upstream - ([859f487](https://github.com/abougouffa/minemacs/commit/859f487)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** restore `treesit-fold` obsolete - ([63c2092](https://github.com/abougouffa/minemacs/commit/63c2092)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(ffip)** integrate with `nerd-icons-completion-mode` - ([66702a5](https://github.com/abougouffa/minemacs/commit/66702a5)) - [@abougouffa](https://github.com/abougouffa)
- **(gud)** remove useless customization - ([9af93fe](https://github.com/abougouffa/minemacs/commit/9af93fe)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** update for the latest version - ([f43f8cc](https://github.com/abougouffa/minemacs/commit/f43f8cc)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([385d49a](https://github.com/abougouffa/minemacs/commit/385d49a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.5.0](https://github.com/abougouffa/minemacs/compare/c964e6e..v11.5.0) - 2024-11-24
#### Documentation
- regenerate the documentation - ([d941b72](https://github.com/abougouffa/minemacs/commit/d941b72)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(debug)** make `disaster` obsolete - ([8248eef](https://github.com/abougouffa/minemacs/commit/8248eef)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `neotree` obsolete - ([079d60c](https://github.com/abougouffa/minemacs/commit/079d60c)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** make `consult-project-extra` obsolete - ([27a7d91](https://github.com/abougouffa/minemacs/commit/27a7d91)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `gumshoe` obsolete - ([2ef76ca](https://github.com/abougouffa/minemacs/commit/2ef76ca)) - [@abougouffa](https://github.com/abougouffa)
- spin off persistent scratch buffers as a new package `pscratch` - ([a251ba2](https://github.com/abougouffa/minemacs/commit/a251ba2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(ace-window)** restore the default for `aw-dispatch-always` - ([92d14bd](https://github.com/abougouffa/minemacs/commit/92d14bd)) - [@abougouffa](https://github.com/abougouffa)
- **(forge)** lazier loading - ([c964e6e](https://github.com/abougouffa/minemacs/commit/c964e6e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.4.0](https://github.com/abougouffa/minemacs/compare/76ee765..v11.4.0) - 2024-11-23
#### Bug Fixes
- **(on-demand/hurl)** ensure loading dependencies - ([dcf07fd](https://github.com/abougouffa/minemacs/commit/dcf07fd)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** make `eglot-x` obsolete - ([0d7fc67](https://github.com/abougouffa/minemacs/commit/0d7fc67)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `reformatter` obsolete - ([01b62dc](https://github.com/abougouffa/minemacs/commit/01b62dc)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `rainbow-mode` obsolete - ([329490f](https://github.com/abougouffa/minemacs/commit/329490f)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `cognitive-complexity` obsolete - ([4194f03](https://github.com/abougouffa/minemacs/commit/4194f03)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `treesitter-context` obsolete - ([79f0cf2](https://github.com/abougouffa/minemacs/commit/79f0cf2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `treesit-fold` obsolete - ([50b160c](https://github.com/abougouffa/minemacs/commit/50b160c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- cleanup some parts - ([f8c8a1e](https://github.com/abougouffa/minemacs/commit/f8c8a1e)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(awqat)** colorize the icon in modeline - ([76ee765](https://github.com/abougouffa/minemacs/commit/76ee765)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** load all modules before bumping packages - ([56ab415](https://github.com/abougouffa/minemacs/commit/56ab415)) - [@abougouffa](https://github.com/abougouffa)
- **(jujutsushi)** remove unneeded customization - ([9d730c1](https://github.com/abougouffa/minemacs/commit/9d730c1)) - [@abougouffa](https://github.com/abougouffa)
- **(man)** select the `*man*` buffer when opened - ([74df4d7](https://github.com/abougouffa/minemacs/commit/74df4d7)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** run comment commands for all cursors - ([4b71512](https://github.com/abougouffa/minemacs/commit/4b71512)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** temporary use my fork - ([afe7fc0](https://github.com/abougouffa/minemacs/commit/afe7fc0)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([894cb84](https://github.com/abougouffa/minemacs/commit/894cb84)) - [@abougouffa](https://github.com/abougouffa)
- enable `fido-vertical-mode` when `vertico` isn't available - ([c5195ff](https://github.com/abougouffa/minemacs/commit/c5195ff)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cd5c5c4](https://github.com/abougouffa/minemacs/commit/cd5c5c4)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.3.2](https://github.com/abougouffa/minemacs/compare/3ecee92..v11.3.2) - 2024-11-21
#### Bug Fixes
- **(autorevert)** make sure to save the initial mtime on opening the file - ([94d02d9](https://github.com/abougouffa/minemacs/commit/94d02d9)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- use full links in comments to play well with `goto-addr` - ([0566438](https://github.com/abougouffa/minemacs/commit/0566438)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(auctex)** remove unneeded/outdated customization - ([dba5527](https://github.com/abougouffa/minemacs/commit/dba5527)) - [@abougouffa](https://github.com/abougouffa)
- **(calendar)** set the default date style to ISO - ([535db5d](https://github.com/abougouffa/minemacs/commit/535db5d)) - [@abougouffa](https://github.com/abougouffa)
- **(git-timemachine)** show revision in header line, only do font lock - ([3ecee92](https://github.com/abougouffa/minemacs/commit/3ecee92)) - [@abougouffa](https://github.com/abougouffa)
- **(goto-addr)** customize the URL face - ([5e9b8f5](https://github.com/abougouffa/minemacs/commit/5e9b8f5)) - [@abougouffa](https://github.com/abougouffa)
- **(isearch)** restore the default `C-[M]-s` binding, bind `C-[M]-ù` - ([1d878e9](https://github.com/abougouffa/minemacs/commit/1d878e9)) - [@abougouffa](https://github.com/abougouffa)
- **(re-builder)** add `+reb-replace-regexp` - ([db9c6c2](https://github.com/abougouffa/minemacs/commit/db9c6c2)) - [@abougouffa](https://github.com/abougouffa)
- enable `goto-address-prog-mode` in `prog-mode` & `conf-mode` - ([e88f508](https://github.com/abougouffa/minemacs/commit/e88f508)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.3.1](https://github.com/abougouffa/minemacs/compare/c774165..v11.3.1) - 2024-11-21
#### Bug Fixes
- **(flymake-collection)** use the upstream repo, new checkers merged - ([c774165](https://github.com/abougouffa/minemacs/commit/c774165)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.3.0](https://github.com/abougouffa/minemacs/compare/dc2eca5..v11.3.0) - 2024-11-21
#### Bug Fixes
- **(multi-magit)** fix `+multi-magit-discover-repos` - ([b2ac632](https://github.com/abougouffa/minemacs/commit/b2ac632)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** declare it as unsupported for `multiple-cursors` - ([569dccc](https://github.com/abougouffa/minemacs/commit/569dccc)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** respect the projects list in `+project-list-cleanup` - ([7ca7d5d](https://github.com/abougouffa/minemacs/commit/7ca7d5d)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([292a5ac](https://github.com/abougouffa/minemacs/commit/292a5ac)) - [@abougouffa](https://github.com/abougouffa)
- more comments - ([fc39ddd](https://github.com/abougouffa/minemacs/commit/fc39ddd)) - [@abougouffa](https://github.com/abougouffa)
- generate the documentation - ([9bf607c](https://github.com/abougouffa/minemacs/commit/9bf607c)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** make `zones` obsolete - ([fc92c66](https://github.com/abougouffa/minemacs/commit/fc92c66)) - [@abougouffa](https://github.com/abougouffa)
- **(embedded)** make `platformio` obsolete - ([bc8763c](https://github.com/abougouffa/minemacs/commit/bc8763c)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `sr-speedbar` obsolete - ([abdbe82](https://github.com/abougouffa/minemacs/commit/abdbe82)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `phi-grep` obsolete - ([9a1bed3](https://github.com/abougouffa/minemacs/commit/9a1bed3)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `isearch-mb` obsolete - ([fd3c63d](https://github.com/abougouffa/minemacs/commit/fd3c63d)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `deadgrep` obsolete - ([0eef649](https://github.com/abougouffa/minemacs/commit/0eef649)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `rtags` obsolete - ([73a3e39](https://github.com/abougouffa/minemacs/commit/73a3e39)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `mixed-pitch` obsolete - ([60624cb](https://github.com/abougouffa/minemacs/commit/60624cb)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** make `repo-transient` obsolete - ([aa0cc80](https://github.com/abougouffa/minemacs/commit/aa0cc80)) - [@abougouffa](https://github.com/abougouffa)
- make `me-writing-mode` obsolete - ([6fe9d42](https://github.com/abougouffa/minemacs/commit/6fe9d42)) - [@abougouffa](https://github.com/abougouffa)
- remove useless `+project-add-project` - ([d8246ac](https://github.com/abougouffa/minemacs/commit/d8246ac)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- several performance improvements, remove unused functions - ([dc2eca5](https://github.com/abougouffa/minemacs/commit/dc2eca5)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- restore the `:package` keyword in `:bind` blocks - ([6680fa5](https://github.com/abougouffa/minemacs/commit/6680fa5)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** optionally print logs in `+shutup!` - ([7a0a468](https://github.com/abougouffa/minemacs/commit/7a0a468)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** performance tweaks - ([d6ce5e0](https://github.com/abougouffa/minemacs/commit/d6ce5e0)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-magit)** remember projects in `+multi-magit-discover-repos` - ([e072c33](https://github.com/abougouffa/minemacs/commit/e072c33)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([de24b16](https://github.com/abougouffa/minemacs/commit/de24b16)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([a0b6be4](https://github.com/abougouffa/minemacs/commit/a0b6be4)) - [@abougouffa](https://github.com/abougouffa)
- minor edits in `goto-last-change` - ([c5bd045](https://github.com/abougouffa/minemacs/commit/c5bd045)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.2.0](https://github.com/abougouffa/minemacs/compare/59776f3..v11.2.0) - 2024-11-19
#### Bug Fixes
- **(citre)** don't use global cache directory - ([ed5a5d8](https://github.com/abougouffa/minemacs/commit/ed5a5d8)) - [@abougouffa](https://github.com/abougouffa)
- **(combobulate)** fix problematic combobulate loading - ([2b75eda](https://github.com/abougouffa/minemacs/commit/2b75eda)) - [@abougouffa](https://github.com/abougouffa)
- **(gambol)** fix the implementation `+gambol:occur-dwim` - ([22e18af](https://github.com/abougouffa/minemacs/commit/22e18af)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** temporary fixes for problematic pulsing - ([35c7803](https://github.com/abougouffa/minemacs/commit/35c7803)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- update the documentation - ([580731e](https://github.com/abougouffa/minemacs/commit/580731e)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+describe-random-command` - ([2bd4e05](https://github.com/abougouffa/minemacs/commit/2bd4e05)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** add `flyspell-correct`, remove usage of `spell-fu` - ([5af36df](https://github.com/abougouffa/minemacs/commit/5af36df)) - [@abougouffa](https://github.com/abougouffa)
- **(sudo-edit)** add the missing `+sudo-edit-save` - ([ac27c2e](https://github.com/abougouffa/minemacs/commit/ac27c2e)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `emamux` obsolete - ([59776f3](https://github.com/abougouffa/minemacs/commit/59776f3)) - [@abougouffa](https://github.com/abougouffa)
- remove some commands, alternatives in `crux` and `sudo-edit` - ([7bd5239](https://github.com/abougouffa/minemacs/commit/7bd5239)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** simplify the implementation of `+autoload-region` - ([c3175ec](https://github.com/abougouffa/minemacs/commit/c3175ec)) - [@abougouffa](https://github.com/abougouffa)
- remove confusing `:package` keyword from `:bind` blocks - ([3c089c5](https://github.com/abougouffa/minemacs/commit/3c089c5)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(vc)** replace `magit-file-icons` with `magit-iconify` - ([386c2c4](https://github.com/abougouffa/minemacs/commit/386c2c4)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** better initial value for `+html2pdf-default-backend` - ([e31e147](https://github.com/abougouffa/minemacs/commit/e31e147)) - [@abougouffa](https://github.com/abougouffa)
- **(projection)** bind `projection-dape` & `projection-multi-compile` - ([adc199b](https://github.com/abougouffa/minemacs/commit/adc199b)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** use `plink` on Windows when available - ([479e425](https://github.com/abougouffa/minemacs/commit/479e425)) - [@abougouffa](https://github.com/abougouffa)
- **(ztree)** draw Unicode lines - ([c7ec30e](https://github.com/abougouffa/minemacs/commit/c7ec30e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d587fb5](https://github.com/abougouffa/minemacs/commit/d587fb5)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([60ce18b](https://github.com/abougouffa/minemacs/commit/60ce18b)) - [@abougouffa](https://github.com/abougouffa)
- remove unused `+compile-functions` - ([77200fd](https://github.com/abougouffa/minemacs/commit/77200fd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.1.0](https://github.com/abougouffa/minemacs/compare/724cdef..v11.1.0) - 2024-11-17
#### Bug Fixes
- **(combobulate)** fix bindings for obsolete commands - ([ed70d24](https://github.com/abougouffa/minemacs/commit/ed70d24)) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed-protocol)** properly initialize after loading `elfeed` - ([780cc62](https://github.com/abougouffa/minemacs/commit/780cc62)) - [@abougouffa](https://github.com/abougouffa)
- **(selinux-policy)** fix the recipe - ([c6858b4](https://github.com/abougouffa/minemacs/commit/c6858b4)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(natural-langs)** update the description of `me-eglot-ltex` - ([60361c8](https://github.com/abougouffa/minemacs/commit/60361c8)) - [@abougouffa](https://github.com/abougouffa)
- update the documentation - ([0407a87](https://github.com/abougouffa/minemacs/commit/0407a87)) - [@abougouffa](https://github.com/abougouffa)
- generate the modules list - ([391205a](https://github.com/abougouffa/minemacs/commit/391205a)) - [@abougouffa](https://github.com/abougouffa)
- add descriptions for on-demand packages - ([d42fb84](https://github.com/abougouffa/minemacs/commit/d42fb84)) - [@abougouffa](https://github.com/abougouffa)
- add a list of modules/packages - ([4aa707d](https://github.com/abougouffa/minemacs/commit/4aa707d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(elfeed)** enable again + bind `+elfeed` to `C-c C-o f` - ([e14faa4](https://github.com/abougouffa/minemacs/commit/e14faa4)) - [@abougouffa](https://github.com/abougouffa)
- **(rss)** add support for extra protocols via `elfeed-protocol` - ([138d889](https://github.com/abougouffa/minemacs/commit/138d889)) - [@abougouffa](https://github.com/abougouffa)
- add `minemacs-extract-packages-descriptions` - ([be95e29](https://github.com/abougouffa/minemacs/commit/be95e29)) - [@abougouffa](https://github.com/abougouffa)
- move `yasnippet` and the like to the new module `me-snippets` - ([3eb87ad](https://github.com/abougouffa/minemacs/commit/3eb87ad)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** automatically generate all the docs - ([39b7ec1](https://github.com/abougouffa/minemacs/commit/39b7ec1)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(elfeed)** minor edits - ([1edcb85](https://github.com/abougouffa/minemacs/commit/1edcb85)) - [@abougouffa](https://github.com/abougouffa)
- move a command to `me-lib-extra` - ([9c787ea](https://github.com/abougouffa/minemacs/commit/9c787ea)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(jiralib)** better documentation for my custom commands - ([c9a3235](https://github.com/abougouffa/minemacs/commit/c9a3235)) - [@abougouffa](https://github.com/abougouffa)
- **(sx)** correctly set the cache directory - ([2f4dfc3](https://github.com/abougouffa/minemacs/commit/2f4dfc3)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** add display rule for `envrc` warnings buffer - ([13c1b79](https://github.com/abougouffa/minemacs/commit/13c1b79)) - [@abougouffa](https://github.com/abougouffa)
- add on-demands mods in `minemacs-extract-packages-descriptions` - ([1c19e11](https://github.com/abougouffa/minemacs/commit/1c19e11)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([724cdef](https://github.com/abougouffa/minemacs/commit/724cdef)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.0.1](https://github.com/abougouffa/minemacs/compare/bc3690b..v11.0.1) - 2024-11-17
#### Bug Fixes
- **(ztree)** fix the keybindings for `n` and `p` - ([c540c80](https://github.com/abougouffa/minemacs/commit/c540c80)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([bc3690b](https://github.com/abougouffa/minemacs/commit/bc3690b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dired)** enable wrapping next-line at top/bottom - ([b8d5df5](https://github.com/abougouffa/minemacs/commit/b8d5df5)) - [@abougouffa](https://github.com/abougouffa)
- **(dired)** remove unneeded hack - ([07b85b1](https://github.com/abougouffa/minemacs/commit/07b85b1)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-multimodal)** enable in `ztree-mode` - ([e01aa0b](https://github.com/abougouffa/minemacs/commit/e01aa0b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v11.0.0](https://github.com/abougouffa/minemacs/compare/3b688e1..v11.0.0) - 2024-11-16
#### Bug Fixes
- **(desktop)** fix saving/reading desktop session files - ([e683b05](https://github.com/abougouffa/minemacs/commit/e683b05)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** correctly configure the new `flymake-indicator-type` - ([7f2fd4a](https://github.com/abougouffa/minemacs/commit/7f2fd4a)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** better implementation of `+parinfer-rust-mode-maybe` - ([2f7d842](https://github.com/abougouffa/minemacs/commit/2f7d842)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** temporary disable for `multiple-cursors` - ([dbe43d8](https://github.com/abougouffa/minemacs/commit/dbe43d8)) - [@abougouffa](https://github.com/abougouffa)
- fix some keymap bindings - ([33074f0](https://github.com/abougouffa/minemacs/commit/33074f0)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(extra)** add support for my new package `run-in-dir` - ([8d6adb9](https://github.com/abougouffa/minemacs/commit/8d6adb9)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add initial support for `fd-dired` - ([fc57ef1](https://github.com/abougouffa/minemacs/commit/fc57ef1)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-magit)** better implementation of `+multi-magit-discover-repos` - ([a765730](https://github.com/abougouffa/minemacs/commit/a765730)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-magit)** add `+multi-magit-select-repos-under-directory` - ([8644ccf](https://github.com/abougouffa/minemacs/commit/8644ccf)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** remove `git-undo`, not stable - ([b0e1b10](https://github.com/abougouffa/minemacs/commit/b0e1b10)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** additional support for `multi-magit` - ([b5cd6c2](https://github.com/abougouffa/minemacs/commit/b5cd6c2)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(pages)** fix the branch - ([3b688e1](https://github.com/abougouffa/minemacs/commit/3b688e1)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(desktop)** rename a variable - ([9e69fe7](https://github.com/abougouffa/minemacs/commit/9e69fe7)) - [@abougouffa](https://github.com/abougouffa)
- use `+emacs-options-p` to detect all options - ([eab3369](https://github.com/abougouffa/minemacs/commit/eab3369)) - [@abougouffa](https://github.com/abougouffa)
- make use of `directory-files-no-dot-files-regexp` - ([9f3c060](https://github.com/abougouffa/minemacs/commit/9f3c060)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(desktop)** autoload `+desktop-read-session` - ([34aa1e8](https://github.com/abougouffa/minemacs/commit/34aa1e8)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** remove old hack, merged upstream - ([8fe6c67](https://github.com/abougouffa/minemacs/commit/8fe6c67)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-vterm)** implement `+multi-vterm-toggle-dedicated-dwim` - ([a62216d](https://github.com/abougouffa/minemacs/commit/a62216d)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** whitelist extra commands, add transient menu - ([eec057f](https://github.com/abougouffa/minemacs/commit/eec057f)) - [@abougouffa](https://github.com/abougouffa)
- **(ztree)** add some keybindings - ([cdd0620](https://github.com/abougouffa/minemacs/commit/cdd0620)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2d81f58](https://github.com/abougouffa/minemacs/commit/2d81f58)) - [@abougouffa](https://github.com/abougouffa)
- minor keymap edits - ([6b5398e](https://github.com/abougouffa/minemacs/commit/6b5398e)) - [@abougouffa](https://github.com/abougouffa)
- better setup of custom keybindings - ([7feb5a9](https://github.com/abougouffa/minemacs/commit/7feb5a9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.6.0](https://github.com/abougouffa/minemacs/compare/a27c957..v10.6.0) - 2024-11-12
#### Bug Fixes
- **(flymake)** properly setup the custom fringe symbols - ([fc2abde](https://github.com/abougouffa/minemacs/commit/fc2abde)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(desktop)** automatically make timestamped copies of saved sessions - ([b2e20e5](https://github.com/abougouffa/minemacs/commit/b2e20e5)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add initial support for `org-rich-yank` - ([2616f99](https://github.com/abougouffa/minemacs/commit/2616f99)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add support for `git-undo` - ([3484d08](https://github.com/abougouffa/minemacs/commit/3484d08)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(dogears)** minor edit - ([af2f678](https://github.com/abougouffa/minemacs/commit/af2f678)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(desktop)** automatically save the session - ([3558643](https://github.com/abougouffa/minemacs/commit/3558643)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** add an option for `+desktop-read-session` - ([901d9c1](https://github.com/abougouffa/minemacs/commit/901d9c1)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** provide an option to restore the session - ([7ba12d5](https://github.com/abougouffa/minemacs/commit/7ba12d5)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** simpler implementation of `+mu4e-view-save-all-attachments` - ([2abbec4](https://github.com/abougouffa/minemacs/commit/2abbec4)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** minor tweaks - ([d9da42b](https://github.com/abougouffa/minemacs/commit/d9da42b)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-vterm)** add `+multi-vterm-project-toggle` - ([718c0c7](https://github.com/abougouffa/minemacs/commit/718c0c7)) - [@abougouffa](https://github.com/abougouffa)
- **(smartparens)** disable on `minibuffer-inactive-mode`, add a rule - ([c8ad7c5](https://github.com/abougouffa/minemacs/commit/c8ad7c5)) - [@abougouffa](https://github.com/abougouffa)
- **(smartparens)** several tweaks - ([a27c957](https://github.com/abougouffa/minemacs/commit/a27c957)) - [@abougouffa](https://github.com/abougouffa)
- **(ssh-deploy)** bind `C-c C-z` to prefix map instead of hydra - ([0281946](https://github.com/abougouffa/minemacs/commit/0281946)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([063e336](https://github.com/abougouffa/minemacs/commit/063e336)) - [@abougouffa](https://github.com/abougouffa)
- better use of `once-x-call` - ([2656553](https://github.com/abougouffa/minemacs/commit/2656553)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4994361](https://github.com/abougouffa/minemacs/commit/4994361)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.5.1](https://github.com/abougouffa/minemacs/compare/d0259dc..v10.5.1) - 2024-11-11
#### Bug Fixes
- **(cape/corfu)** fixes for Emacs 31 - ([ef72ffa](https://github.com/abougouffa/minemacs/commit/ef72ffa)) - [@abougouffa](https://github.com/abougouffa)
- fix several bugs (detected on Emacs 31) - ([c396c63](https://github.com/abougouffa/minemacs/commit/c396c63)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- stick to recommended version of Ditaa & PlantUML - ([bd9802d](https://github.com/abougouffa/minemacs/commit/bd9802d)) - [@abougouffa](https://github.com/abougouffa)
- replace `when/if-let` with `when/if-let*` (deprecated in v31) - ([d0259dc](https://github.com/abougouffa/minemacs/commit/d0259dc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.5.0](https://github.com/abougouffa/minemacs/compare/6e012e9..v10.5.0) - 2024-11-10
#### Bug Fixes
- **(combobulate)** hack to prevent `combobulate` from loading at startup - ([67b9fb2](https://github.com/abougouffa/minemacs/commit/67b9fb2)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** fix `(error "Selecting deleted buffer")` - ([69cfc7c](https://github.com/abougouffa/minemacs/commit/69cfc7c)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** ensure enabling the mode in the right buffer - ([5d097d3](https://github.com/abougouffa/minemacs/commit/5d097d3)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** add a sample debug trick to `early-config.el` - ([bf59ef2](https://github.com/abougouffa/minemacs/commit/bf59ef2)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([469a5f8](https://github.com/abougouffa/minemacs/commit/469a5f8)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(calendar)** add initial support for `org-caldav` - ([ffa68bc](https://github.com/abougouffa/minemacs/commit/ffa68bc)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** add support for StackExchange via `sx` - ([8ce9313](https://github.com/abougouffa/minemacs/commit/8ce9313)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** replace `magit-file-icons` with `magit-iconify` - ([392dcb4](https://github.com/abougouffa/minemacs/commit/392dcb4)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(calendar)** set the week starting day - ([e5d4dd2](https://github.com/abougouffa/minemacs/commit/e5d4dd2)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-eglot)** cleanup old `lsp-mode` related code - ([1c5a603](https://github.com/abougouffa/minemacs/commit/1c5a603)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unused/useless commands and functions - ([128a211](https://github.com/abougouffa/minemacs/commit/128a211)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** propose to create existent paths when renaming files - ([f3dbd99](https://github.com/abougouffa/minemacs/commit/f3dbd99)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** use UTF-8 for language environment - ([f6380c3](https://github.com/abougouffa/minemacs/commit/f6380c3)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** enhance the `tool-bar-setup` hack - ([7c2a7f7](https://github.com/abougouffa/minemacs/commit/7c2a7f7)) - [@abougouffa](https://github.com/abougouffa)
- **(newcomment)** minor tweaks - ([f3e63e0](https://github.com/abougouffa/minemacs/commit/f3e63e0)) - [@abougouffa](https://github.com/abougouffa)
- **(paren)** better defaults for `show-paren-mode` - ([e44a98a](https://github.com/abougouffa/minemacs/commit/e44a98a)) - [@abougouffa](https://github.com/abougouffa)
- **(plantuml)** prefer executable, correctly use the JAR file - ([7df0234](https://github.com/abougouffa/minemacs/commit/7df0234)) - [@abougouffa](https://github.com/abougouffa)
- **(recentf)** increase the maximum number of saved items - ([45c0703](https://github.com/abougouffa/minemacs/commit/45c0703)) - [@abougouffa](https://github.com/abougouffa)
- **(smartparens)** enable in the minibuffer - ([65c7131](https://github.com/abougouffa/minemacs/commit/65c7131)) - [@abougouffa](https://github.com/abougouffa)
- **(vundo)** bind `vundo` to `C-c o u` - ([6e012e9](https://github.com/abougouffa/minemacs/commit/6e012e9)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([efe0022](https://github.com/abougouffa/minemacs/commit/efe0022)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7c3a535](https://github.com/abougouffa/minemacs/commit/7c3a535)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.4.0](https://github.com/abougouffa/minemacs/compare/3d038df..v10.4.0) - 2024-11-08
#### Features
- **(core)** bind `M-:` to `pp-eval-expression` - ([b3abd1b](https://github.com/abougouffa/minemacs/commit/b3abd1b)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `disk-usage` - ([d99f1f0](https://github.com/abougouffa/minemacs/commit/d99f1f0)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `rscope` obsolete - ([d46370a](https://github.com/abougouffa/minemacs/commit/d46370a)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `package-lint` to `me-checkers` - ([4f58497](https://github.com/abougouffa/minemacs/commit/4f58497)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(elfeed)** temporary disable `elfeed` since it is unusable - ([189d18d](https://github.com/abougouffa/minemacs/commit/189d18d)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit)** enable `show-paren-local-mode` in tree explorer - ([65d69e2](https://github.com/abougouffa/minemacs/commit/65d69e2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([410f4d7](https://github.com/abougouffa/minemacs/commit/410f4d7)) - [@abougouffa](https://github.com/abougouffa)
- restore default `C-x k` - ([0cb409e](https://github.com/abougouffa/minemacs/commit/0cb409e)) - [@abougouffa](https://github.com/abougouffa)
- bump `parinfer-rust-mode` - ([1365843](https://github.com/abougouffa/minemacs/commit/1365843)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3d038df](https://github.com/abougouffa/minemacs/commit/3d038df)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.3.0](https://github.com/abougouffa/minemacs/compare/eb0fe53..v10.3.0) - 2024-11-07
#### Bug Fixes
- **(citre)** simplify loading the default configuration - ([6e0abca](https://github.com/abougouffa/minemacs/commit/6e0abca)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(citre)** more customization, add Bitbake aware list of files - ([b10db3c](https://github.com/abougouffa/minemacs/commit/b10db3c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+github-download-release` and tweak `+github-latest-release` - ([8079515](https://github.com/abougouffa/minemacs/commit/8079515)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `cmake-build` `czm-cpp` obsoletes - ([c4d6e51](https://github.com/abougouffa/minemacs/commit/c4d6e51)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `irony-mode` obsolete - ([6e0aff7](https://github.com/abougouffa/minemacs/commit/6e0aff7)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- minor edit - ([eaea01c](https://github.com/abougouffa/minemacs/commit/eaea01c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bitbake)** autoload my custom commands - ([d436db9](https://github.com/abougouffa/minemacs/commit/d436db9)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-cscope)** use `+region-or-thing-at-point` for initial input - ([d6b665c](https://github.com/abougouffa/minemacs/commit/d6b665c)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-cscope)** better integration with super-projects - ([eb0fe53](https://github.com/abougouffa/minemacs/commit/eb0fe53)) - [@abougouffa](https://github.com/abougouffa)
- **(ditaa)** auto download using `+github-download-release` - ([25efe9f](https://github.com/abougouffa/minemacs/commit/25efe9f)) - [@abougouffa](https://github.com/abougouffa)
- **(ditaa)** automatically download the latest version from GitHub - ([b1b56ac](https://github.com/abougouffa/minemacs/commit/b1b56ac)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/plantuml)** auto download PlantUML JAR file - ([ec16177](https://github.com/abougouffa/minemacs/commit/ec16177)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** auto load `ol-man` (Org link to man pages) - ([9b4c27b](https://github.com/abougouffa/minemacs/commit/9b4c27b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.2.0](https://github.com/abougouffa/minemacs/compare/51d3158..v10.2.0) - 2024-11-05
#### Bug Fixes
- **(cmake-build)** set `cmake-build-local-options-file` very early - ([6e6893e](https://github.com/abougouffa/minemacs/commit/6e6893e)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** restore `tool-bar-setup` after startup - ([7bd7693](https://github.com/abougouffa/minemacs/commit/7bd7693)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-pmd)** correct the version detection - ([a4908ec](https://github.com/abougouffa/minemacs/commit/a4908ec)) - [@abougouffa](https://github.com/abougouffa)
- **(gambol)** fix implementation of `+gambol:occur-dwim` - ([3ba07fb](https://github.com/abougouffa/minemacs/commit/3ba07fb)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(bitbake-ts-mode)** add initial support - ([95b0d44](https://github.com/abougouffa/minemacs/commit/95b0d44)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** enable `visual-wrap-prefix-mode` in Emacs 30+ - ([c96e25a](https://github.com/abougouffa/minemacs/commit/c96e25a)) - [@abougouffa](https://github.com/abougouffa)
- **(compile-multi)** add `nerd-icons` support - ([1cc372d](https://github.com/abougouffa/minemacs/commit/1cc372d)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** add `+consult-xref-history` - ([51d3158](https://github.com/abougouffa/minemacs/commit/51d3158)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+project-root-initialize` - ([e7f1ca8](https://github.com/abougouffa/minemacs/commit/e7f1ca8)) - [@abougouffa](https://github.com/abougouffa)
- **(email)** add support for sending invitations via `varuga` - ([5bb1ff0](https://github.com/abougouffa/minemacs/commit/5bb1ff0)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for GenExpr files - ([3fee077](https://github.com/abougouffa/minemacs/commit/3fee077)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for `selinuxpolicy-mode` - ([72c082a](https://github.com/abougouffa/minemacs/commit/72c082a)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/web)** add support for `flymake-biome` - ([a4aca87](https://github.com/abougouffa/minemacs/commit/a4aca87)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand/zig)** add support for `zig-ts-mode` - ([4cac851](https://github.com/abougouffa/minemacs/commit/4cac851)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** restore `treesitter-context` with better UI integration - ([f6aca22](https://github.com/abougouffa/minemacs/commit/f6aca22)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `gambol` support - ([9f469ae](https://github.com/abougouffa/minemacs/commit/9f469ae)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add TRAMP support for Incus containers - ([212838c](https://github.com/abougouffa/minemacs/commit/212838c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([ed49e73](https://github.com/abougouffa/minemacs/commit/ed49e73)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([da603cd](https://github.com/abougouffa/minemacs/commit/da603cd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.1.0](https://github.com/abougouffa/minemacs/compare/616765d..v10.1.0) - 2024-11-03
#### Features
- **(on-demand/python)** add support for `python-pytest` - ([9c56a6f](https://github.com/abougouffa/minemacs/commit/9c56a6f)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- make use of new (29+) base modes - ([2d2ffb2](https://github.com/abougouffa/minemacs/commit/2d2ffb2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(pet)** add integration for `quickrun` - ([4345fcf](https://github.com/abougouffa/minemacs/commit/4345fcf)) - [@abougouffa](https://github.com/abougouffa)
- **(pet)** make sure the `pet-mode` hook is executed before other hooks - ([616765d](https://github.com/abougouffa/minemacs/commit/616765d)) - [@abougouffa](https://github.com/abougouffa)
- **(quickrun)** bind `quickrun` to F5 - ([516ad41](https://github.com/abougouffa/minemacs/commit/516ad41)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v10.0.0](https://github.com/abougouffa/minemacs/compare/da0728d..v10.0.0) - 2024-11-02
#### Bug Fixes
- **(core)** fix the `:trigger-commands` argument of `use-package` - ([3aed505](https://github.com/abougouffa/minemacs/commit/3aed505)) - [@abougouffa](https://github.com/abougouffa)
- **(jujutsushi)** use my mirror (sourcehut is often offline) - ([60dd738](https://github.com/abougouffa/minemacs/commit/60dd738)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-file-icons)** load before `magit`, but not too early - ([543724c](https://github.com/abougouffa/minemacs/commit/543724c)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- minor edits - ([9442493](https://github.com/abougouffa/minemacs/commit/9442493)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** officially drop support for Emacs 28 - ([6572063](https://github.com/abougouffa/minemacs/commit/6572063)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+file-type` - ([71afd8e](https://github.com/abougouffa/minemacs/commit/71afd8e)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** add initial support for `gumshoe` - ([fbf36fd](https://github.com/abougouffa/minemacs/commit/fbf36fd)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** add initial support for `deadgrep` - ([38b7eec](https://github.com/abougouffa/minemacs/commit/38b7eec)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add Jujutsu (`jj`) support - ([f96e481](https://github.com/abougouffa/minemacs/commit/f96e481)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** remove unneeded code - ([9f89b99](https://github.com/abougouffa/minemacs/commit/9f89b99)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** make `minemacs-load-module` more generic - ([da0728d](https://github.com/abougouffa/minemacs/commit/da0728d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(awqat)** change the icon - ([30e8a9c](https://github.com/abougouffa/minemacs/commit/30e8a9c)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** remove hack, the change has been merged upstream - ([4654975](https://github.com/abougouffa/minemacs/commit/4654975)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor tweaks - ([7f0c960](https://github.com/abougouffa/minemacs/commit/7f0c960)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `os/android` variable - ([ad951d5](https://github.com/abougouffa/minemacs/commit/ad951d5)) - [@abougouffa](https://github.com/abougouffa)
- **(ob-ditaa)** include JAR files for Ditaa - ([901d93d](https://github.com/abougouffa/minemacs/commit/901d93d)) - [@abougouffa](https://github.com/abougouffa)
- **(with-editor)** add support for `JJ_EDITOR` - ([d701a39](https://github.com/abougouffa/minemacs/commit/d701a39)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([be77906](https://github.com/abougouffa/minemacs/commit/be77906)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c84e461](https://github.com/abougouffa/minemacs/commit/c84e461)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0162893](https://github.com/abougouffa/minemacs/commit/0162893)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([f37723d](https://github.com/abougouffa/minemacs/commit/f37723d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.4.1](https://github.com/abougouffa/minemacs/compare/c72e929..v9.4.1) - 2024-10-27
#### Documentation
- **(documentation)** regenerate the documentation - ([c72e929](https://github.com/abougouffa/minemacs/commit/c72e929)) - [@abougouffa](https://github.com/abougouffa)
- update external dependencies - ([34722b1](https://github.com/abougouffa/minemacs/commit/34722b1)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(cape)** use `satch-advice-add` when possible - ([44847d9](https://github.com/abougouffa/minemacs/commit/44847d9)) - [@abougouffa](https://github.com/abougouffa)
- move pseudo-packages declaration to `me-bootstrap` - ([c682141](https://github.com/abougouffa/minemacs/commit/c682141)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cape)** remove a tweak for the unsupported Emacs 28 - ([6bce8b3](https://github.com/abougouffa/minemacs/commit/6bce8b3)) - [@abougouffa](https://github.com/abougouffa)
- **(casual)** bind `casual-editkit-main-tmenu` to `C-o` - ([d05429a](https://github.com/abougouffa/minemacs/commit/d05429a)) - [@abougouffa](https://github.com/abougouffa)
- **(compat)** force using the latest version from GNU ELPA mirror - ([ae6cb1e](https://github.com/abougouffa/minemacs/commit/ae6cb1e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3d3b95b](https://github.com/abougouffa/minemacs/commit/3d3b95b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.4.0](https://github.com/abougouffa/minemacs/compare/90ee195..v9.4.0) - 2024-10-26
#### Bug Fixes
- **(ui)** upgrade the Casual suite, merged into one package - ([02e3845](https://github.com/abougouffa/minemacs/commit/02e3845)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(completion)** add descriptions for each package - ([7c6e0be](https://github.com/abougouffa/minemacs/commit/7c6e0be)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add descriptions for each package - ([0fcebe6](https://github.com/abougouffa/minemacs/commit/0fcebe6)) - [@abougouffa](https://github.com/abougouffa)
- add descriptions for included packages - ([4dd8e63](https://github.com/abougouffa/minemacs/commit/4dd8e63)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** better support for CMake via `cmake-build` & `czm-cpp` - ([c540e73](https://github.com/abougouffa/minemacs/commit/c540e73)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** restore `magit-file-icons` - ([90ee195](https://github.com/abougouffa/minemacs/commit/90ee195)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `editorconfig` to `me-builtin` - ([93be81d](https://github.com/abougouffa/minemacs/commit/93be81d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cmake-build)** set the file path - ([b0e583d](https://github.com/abougouffa/minemacs/commit/b0e583d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cbc5068](https://github.com/abougouffa/minemacs/commit/cbc5068)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d89b7b2](https://github.com/abougouffa/minemacs/commit/d89b7b2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.6](https://github.com/abougouffa/minemacs/compare/993d21e..v9.3.6) - 2024-10-18
#### Bug Fixes
- **(core)** fix the regexp for failed patches in `+apply-patch-dwim` - ([8e2b367](https://github.com/abougouffa/minemacs/commit/8e2b367)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** restore accidentally removed hook - ([103a556](https://github.com/abougouffa/minemacs/commit/103a556)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult)** don't preview GPG files - ([9f193f2](https://github.com/abougouffa/minemacs/commit/9f193f2)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** enable view mode in patch output - ([135d269](https://github.com/abougouffa/minemacs/commit/135d269)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better UX for `+apply-patch-dwim` - ([6b7414c](https://github.com/abougouffa/minemacs/commit/6b7414c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better user experience in `+apply-patch-dwim` - ([361eba1](https://github.com/abougouffa/minemacs/commit/361eba1)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simple and efficient implementation of `+apply-patch-dwim` - ([993d21e](https://github.com/abougouffa/minemacs/commit/993d21e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8dcf56c](https://github.com/abougouffa/minemacs/commit/8dcf56c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.5](https://github.com/abougouffa/minemacs/compare/aead8e0..v9.3.5) - 2024-10-06
#### Documentation
- **(readme)** update the branch on CI badges - ([aead8e0](https://github.com/abougouffa/minemacs/commit/aead8e0)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([256c145](https://github.com/abougouffa/minemacs/commit/256c145)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.4](https://github.com/abougouffa/minemacs/compare/43659ed..v9.3.4) - 2024-10-03
#### Bug Fixes
- **(core)** edge case in `+apply-patch-dwim` - ([40922d8](https://github.com/abougouffa/minemacs/commit/40922d8)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add pre/post functions for `+apply-patch-dwim` - ([b7d64e5](https://github.com/abougouffa/minemacs/commit/b7d64e5)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** enable `--ignore-whitespace` for `+apply-patch-dwim` - ([43659ed](https://github.com/abougouffa/minemacs/commit/43659ed)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.3](https://github.com/abougouffa/minemacs/compare/9ece678..v9.3.3) - 2024-09-30
#### Bug Fixes
- **(forge)** ensure `markdown-mode` is available - ([4733196](https://github.com/abougouffa/minemacs/commit/4733196)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** complete the implementation of `+apply-patch-dwim` - ([2e810ec](https://github.com/abougouffa/minemacs/commit/2e810ec)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+apply-patch-dwim` - ([a975f88](https://github.com/abougouffa/minemacs/commit/a975f88)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** initial support for `causal-editkit` and `casual-symbol-overlay` - ([6b92988](https://github.com/abougouffa/minemacs/commit/6b92988)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bitbake)** add a meaningful prompt to `+bitbake-insert-poky-sources` - ([9ece678](https://github.com/abougouffa/minemacs/commit/9ece678)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** regenerate loaddefs - ([fe555c0](https://github.com/abougouffa/minemacs/commit/fe555c0)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete/code-review)** require `on-demand/markdown` when used - ([1b316fc](https://github.com/abougouffa/minemacs/commit/1b316fc)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([88c531b](https://github.com/abougouffa/minemacs/commit/88c531b)) - [@abougouffa](https://github.com/abougouffa)
- rename `+yank-this-file-name` to `+copy-this-file-name` - ([b0dcadc](https://github.com/abougouffa/minemacs/commit/b0dcadc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.2](https://github.com/abougouffa/minemacs/compare/5b40821..v9.3.2) - 2024-09-15
#### Documentation
- **(readme)** remove note about new/legacy MinEmacs - ([5b40821](https://github.com/abougouffa/minemacs/commit/5b40821)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** ensure `minemacs-try-load-extra-mode` is called the called - ([161cfd0](https://github.com/abougouffa/minemacs/commit/161cfd0)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f36751e](https://github.com/abougouffa/minemacs/commit/f36751e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.1](https://github.com/abougouffa/minemacs/compare/6129665..v9.3.1) - 2024-08-31
#### Features
- **(core)** add `+autoload-region` - ([89b95c6](https://github.com/abougouffa/minemacs/commit/89b95c6)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add `gcode-mode` - ([c98d9bc](https://github.com/abougouffa/minemacs/commit/c98d9bc)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dired)** better file listing - ([4fd0421](https://github.com/abougouffa/minemacs/commit/4fd0421)) - [@abougouffa](https://github.com/abougouffa)
- **(diredfl)** add more compressed files extensions - ([d0b8523](https://github.com/abougouffa/minemacs/commit/d0b8523)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** run `+mc/mark-all-symbol-overlays` only once - ([dd9998c](https://github.com/abougouffa/minemacs/commit/dd9998c)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** add more pulsing commands - ([6129665](https://github.com/abougouffa/minemacs/commit/6129665)) - [@abougouffa](https://github.com/abougouffa)
- **(xclip)** log errors to `*Messages*` but don't display them - ([33b05ff](https://github.com/abougouffa/minemacs/commit/33b05ff)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8b4e0e2](https://github.com/abougouffa/minemacs/commit/8b4e0e2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.3.0](https://github.com/abougouffa/minemacs/compare/19c83ef..v9.3.0) - 2024-08-29
#### Bug Fixes
- **(mojo)** fix the extension binding and the autoloads - ([7e46bc2](https://github.com/abougouffa/minemacs/commit/7e46bc2)) - [@abougouffa](https://github.com/abougouffa)
- **(platformio-mode)** avoid permanently disabling `projectile` - ([ed3177e](https://github.com/abougouffa/minemacs/commit/ed3177e)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(embedded)** initial support for `platformio-mode` - ([0241abd](https://github.com/abougouffa/minemacs/commit/0241abd)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `dired-hacks` - ([a3ec35b](https://github.com/abougouffa/minemacs/commit/a3ec35b)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `dired+` - ([63045ec](https://github.com/abougouffa/minemacs/commit/63045ec)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `dirvish` obsolete, too much problems! - ([e18ff4d](https://github.com/abougouffa/minemacs/commit/e18ff4d)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** make `matlab-mode` obsolete - ([8047d47](https://github.com/abougouffa/minemacs/commit/8047d47)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for the Mojo language - ([b237a9c](https://github.com/abougouffa/minemacs/commit/b237a9c)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** add initial support for `irony-mode` and `irony-eldoc` - ([6a53610](https://github.com/abougouffa/minemacs/commit/6a53610)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** add support for `consult-cscope` - ([7b9dc33](https://github.com/abougouffa/minemacs/commit/7b9dc33)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `diredfl` - ([d07c103](https://github.com/abougouffa/minemacs/commit/d07c103)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** replace `nerd-icons-archive` with `nerd-icons-multimodal` - ([41d7f30](https://github.com/abougouffa/minemacs/commit/41d7f30)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `nerd-icons-dired` - ([9ec9f12](https://github.com/abougouffa/minemacs/commit/9ec9f12)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- remove `me-biblio`, move `citar` & `citar-embark` to `me-org` - ([02f433e](https://github.com/abougouffa/minemacs/commit/02f433e)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(files)** remove `dired+`, too much opinionated - ([7a7d2fa](https://github.com/abougouffa/minemacs/commit/7a7d2fa)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** remove `nerd-icons-dired` - ([864119f](https://github.com/abougouffa/minemacs/commit/864119f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bitbake)** use `completing-read` instead of `widget-choose` - ([a993a20](https://github.com/abougouffa/minemacs/commit/a993a20)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete/projectile)** check if the package hasn't been disabled - ([0f544c2](https://github.com/abougouffa/minemacs/commit/0f544c2)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** better fake functions for `projectile` - ([d342a70](https://github.com/abougouffa/minemacs/commit/d342a70)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** don't auto-create parser for Org buffers - ([19c83ef](https://github.com/abougouffa/minemacs/commit/19c83ef)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([40e6237](https://github.com/abougouffa/minemacs/commit/40e6237)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.2.1](https://github.com/abougouffa/minemacs/compare/678f6a8..v9.2.1) - 2024-08-25
#### Bug Fixes
- **(git-timemachine)** add a hack to fix the font lock issue - ([95eef36](https://github.com/abougouffa/minemacs/commit/95eef36)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `compat` to `me-builtin` - ([709e28c](https://github.com/abougouffa/minemacs/commit/709e28c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(+browse-html-file)** create a temporary file for non-file visiting buffers - ([df66315](https://github.com/abougouffa/minemacs/commit/df66315)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([678f6a8](https://github.com/abougouffa/minemacs/commit/678f6a8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.2.0](https://github.com/abougouffa/minemacs/compare/0438bb9..v9.2.0) - 2024-08-24
#### Features
- **(emacs-lisp)** make `elisp-demos` obsolete - ([7835185](https://github.com/abougouffa/minemacs/commit/7835185)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `casual-agenda` - ([410519c](https://github.com/abougouffa/minemacs/commit/410519c)) - [@abougouffa](https://github.com/abougouffa)
- more on-demand modes - ([bb8f43d](https://github.com/abougouffa/minemacs/commit/bb8f43d)) - [@abougouffa](https://github.com/abougouffa)
- more on-demand modes - ([0438bb9](https://github.com/abougouffa/minemacs/commit/0438bb9)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(ui)** move `info-colors` from `me-emacs-lisp` to `me-ui` - ([874a32b](https://github.com/abougouffa/minemacs/commit/874a32b)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(docs)** restore `pdf-tools` to `me-docs` - ([2bf2baa](https://github.com/abougouffa/minemacs/commit/2bf2baa)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(multiple-cursors)** add some commands to run once or on all cursors - ([f4ab2e3](https://github.com/abougouffa/minemacs/commit/f4ab2e3)) - [@abougouffa](https://github.com/abougouffa)
- **(x86-lookup)** use the available PDF reader, set the cache directory - ([a11ab2b](https://github.com/abougouffa/minemacs/commit/a11ab2b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.1.0](https://github.com/abougouffa/minemacs/compare/476ae10..v9.1.0) - 2024-08-22
#### Features
- more on-demand modes - ([fb8bcd5](https://github.com/abougouffa/minemacs/commit/fb8bcd5)) - [@abougouffa](https://github.com/abougouffa)
- more on-demand modes - ([7c9daa3](https://github.com/abougouffa/minemacs/commit/7c9daa3)) - [@abougouffa](https://github.com/abougouffa)
- more on-demand packages - ([476ae10](https://github.com/abougouffa/minemacs/commit/476ae10)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- change the mail address in some files - ([61f6970](https://github.com/abougouffa/minemacs/commit/61f6970)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(obsolete/realgud)** avoid using Evil stuff unless used, simplify code - ([9706d4e](https://github.com/abougouffa/minemacs/commit/9706d4e)) - [@abougouffa](https://github.com/abougouffa)
- add `+prog-mode-run-hooks` - ([50bf659](https://github.com/abougouffa/minemacs/commit/50bf659)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(align)** fix OpenCL integration - ([2c36085](https://github.com/abougouffa/minemacs/commit/2c36085)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-completion)** remove a fix, merged upstream - ([b294ace](https://github.com/abougouffa/minemacs/commit/b294ace)) - [@abougouffa](https://github.com/abougouffa)
- **(visual-basic)** enable line numbers in `visual-basic-mode` - ([10126ae](https://github.com/abougouffa/minemacs/commit/10126ae)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ae97767](https://github.com/abougouffa/minemacs/commit/ae97767)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.0.0](https://github.com/abougouffa/minemacs/compare/v9.0.0-rc2..v9.0.0) - 2024-08-16
#### Bug Fixes
- ensure asking for loading on-demand packages when choosen - ([06c4fda](https://github.com/abougouffa/minemacs/commit/06c4fda)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([43918a1](https://github.com/abougouffa/minemacs/commit/43918a1)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ai)** add initial support for `whisper` - ([9f7fa77](https://github.com/abougouffa/minemacs/commit/9f7fa77)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+project-safe-root` - ([c8f2976](https://github.com/abougouffa/minemacs/commit/c8f2976)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add support for DjVu files - ([cc0ab8f](https://github.com/abougouffa/minemacs/commit/cc0ab8f)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `pandoc-mode` to an on-demand module - ([1a4e279](https://github.com/abougouffa/minemacs/commit/1a4e279)) - [@abougouffa](https://github.com/abougouffa)
- move PDF related packages to on-demand module `me-pdf` - ([1029365](https://github.com/abougouffa/minemacs/commit/1029365)) - [@abougouffa](https://github.com/abougouffa)
- make use of `+project-safe-root` - ([d12e0af](https://github.com/abougouffa/minemacs/commit/d12e0af)) - [@abougouffa](https://github.com/abougouffa)
- load `flymake-cppcheck` and `flymake-guile` on-demand - ([2ce317d](https://github.com/abougouffa/minemacs/commit/2ce317d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(ltex-ls)** use for `markdown-ts-mode` - ([9b1d84f](https://github.com/abougouffa/minemacs/commit/9b1d84f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([49230a8](https://github.com/abougouffa/minemacs/commit/49230a8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bced43c](https://github.com/abougouffa/minemacs/commit/bced43c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.0.0-rc2](https://github.com/abougouffa/minemacs/compare/v9.0.0-rc1..v9.0.0-rc2) - 2024-08-16
#### Bug Fixes
- **(obsolete/evil)** fix configurations due to modules changes - ([2e5214d](https://github.com/abougouffa/minemacs/commit/2e5214d)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([8eedb7e](https://github.com/abougouffa/minemacs/commit/8eedb7e)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** regenerate documentation - ([d231057](https://github.com/abougouffa/minemacs/commit/d231057)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** document loading `on-demand` modules - ([8d65455](https://github.com/abougouffa/minemacs/commit/8d65455)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(on-demand)** more on-demand packages, remove `me-data` - ([ecefe3b](https://github.com/abougouffa/minemacs/commit/ecefe3b)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `clang-format` obsolete (use `apheleia` and `format-all`) - ([63a0ca2](https://github.com/abougouffa/minemacs/commit/63a0ca2)) - [@abougouffa](https://github.com/abougouffa)
- remove `me-keybindings` (contains only unused `key-chord`) - ([c9fb57f](https://github.com/abougouffa/minemacs/commit/c9fb57f)) - [@abougouffa](https://github.com/abougouffa)
- support more modes as on-demand modules - ([8093802](https://github.com/abougouffa/minemacs/commit/8093802)) - [@abougouffa](https://github.com/abougouffa)
- support more modes as on-demand modules - ([898a10b](https://github.com/abougouffa/minemacs/commit/898a10b)) - [@abougouffa](https://github.com/abougouffa)
- support more modes as on-demand modules - ([35630d2](https://github.com/abougouffa/minemacs/commit/35630d2)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(version)** v9.0.0-rc2 - ([8f681cb](https://github.com/abougouffa/minemacs/commit/8f681cb)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(editor)** minor edit - ([af3691c](https://github.com/abougouffa/minemacs/commit/af3691c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(obsolete/me-evil)** minor edits - ([e0f5d1f](https://github.com/abougouffa/minemacs/commit/e0f5d1f)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** implement `:interpreter-mode` and refactor the code - ([92052f6](https://github.com/abougouffa/minemacs/commit/92052f6)) - [@abougouffa](https://github.com/abougouffa)
- move `otpp` to `me-project`, remove module `me-workspaces` - ([4e27e89](https://github.com/abougouffa/minemacs/commit/4e27e89)) - [@abougouffa](https://github.com/abougouffa)
- make `me-latex` an on-demand module - ([6486845](https://github.com/abougouffa/minemacs/commit/6486845)) - [@abougouffa](https://github.com/abougouffa)
- merge `me-undo` into `me-editor` - ([e2a5ad1](https://github.com/abougouffa/minemacs/commit/e2a5ad1)) - [@abougouffa](https://github.com/abougouffa)
- move `add-node-modules-path` to `me-tools` - ([9a9a712](https://github.com/abougouffa/minemacs/commit/9a9a712)) - [@abougouffa](https://github.com/abougouffa)
- make Markdown integration an on-demand module - ([ce12126](https://github.com/abougouffa/minemacs/commit/ce12126)) - [@abougouffa](https://github.com/abougouffa)
- change the signature of `minemacs-modules` - ([c80f8d3](https://github.com/abougouffa/minemacs/commit/c80f8d3)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- add an option to disable all on-demand modules - ([d0d7d7f](https://github.com/abougouffa/minemacs/commit/d0d7d7f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(guard-lf)** don't trigger on IPython notebooks - ([fa7f60f](https://github.com/abougouffa/minemacs/commit/fa7f60f)) - [@abougouffa](https://github.com/abougouffa)
- **(matlab)** install `matlab-mode` unconditionally, exclude `company` backend - ([0e2e0dc](https://github.com/abougouffa/minemacs/commit/0e2e0dc)) - [@abougouffa](https://github.com/abougouffa)
- **(pcap-mode)** truncate lines by default - ([e05c8d3](https://github.com/abougouffa/minemacs/commit/e05c8d3)) - [@abougouffa](https://github.com/abougouffa)
- **(rust)** make `rust-mode` an on-demand module, remove `rustic` and `cargo` - ([1ae67d5](https://github.com/abougouffa/minemacs/commit/1ae67d5)) - [@abougouffa](https://github.com/abougouffa)
- rename `one-tab-per-project` to `otpp` - ([856b054](https://github.com/abougouffa/minemacs/commit/856b054)) - [@abougouffa](https://github.com/abougouffa)
- update the modules list - ([0118173](https://github.com/abougouffa/minemacs/commit/0118173)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4e42677](https://github.com/abougouffa/minemacs/commit/4e42677)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v9.0.0-rc1](https://github.com/abougouffa/minemacs/compare/c0fca51..v9.0.0-rc1) - 2024-08-16
#### Bug Fixes
- **(julia)** restore `julia-mode` in `me-math` - ([8ba7b50](https://github.com/abougouffa/minemacs/commit/8ba7b50)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(calendar)** make `calfw` obsolete - ([cccfff2](https://github.com/abougouffa/minemacs/commit/cccfff2)) - [@abougouffa](https://github.com/abougouffa)
- **(media)** remove `emms` - ([c0fca51](https://github.com/abougouffa/minemacs/commit/c0fca51)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** add more on-demand modules - ([6e41226](https://github.com/abougouffa/minemacs/commit/6e41226)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** make `project-x` obsolete - ([dbb7cc9](https://github.com/abougouffa/minemacs/commit/dbb7cc9)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** make `eopengrok` obsolete - ([a0b7a94](https://github.com/abougouffa/minemacs/commit/a0b7a94)) - [@abougouffa](https://github.com/abougouffa)
- add more on-demand modules - ([a4e5d0b](https://github.com/abougouffa/minemacs/commit/a4e5d0b)) - [@abougouffa](https://github.com/abougouffa)
- add Nim support as on-demand module - ([2138f30](https://github.com/abougouffa/minemacs/commit/2138f30)) - [@abougouffa](https://github.com/abougouffa)
- support OCaml as an on-demand module - ([05d836b](https://github.com/abougouffa/minemacs/commit/05d836b)) - [@abougouffa](https://github.com/abougouffa)
- add `demangle-mode` as an on-demand package - ([8718235](https://github.com/abougouffa/minemacs/commit/8718235)) - [@abougouffa](https://github.com/abougouffa)
- move `alloy-mode` from `obsolete/me-formal` to `modes/me-alloy` - ([f4de723](https://github.com/abougouffa/minemacs/commit/f4de723)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(snippets)** rename the on-demand module snippet - ([0616038](https://github.com/abougouffa/minemacs/commit/0616038)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** fix `memode` snippet - ([edcbd69](https://github.com/abougouffa/minemacs/commit/edcbd69)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** add a snippet for creating extra modes modules - ([0a280bc](https://github.com/abougouffa/minemacs/commit/0a280bc)) - [@abougouffa](https://github.com/abougouffa)
- **(version)** v9.0.0-rc1 - ([27033f4](https://github.com/abougouffa/minemacs/commit/27033f4)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(empv)** tweak the download playlist command - ([dba318c](https://github.com/abougouffa/minemacs/commit/dba318c)) - [@abougouffa](https://github.com/abougouffa)
- make Zig and CMake integration into on-demand modules - ([87fdf43](https://github.com/abougouffa/minemacs/commit/87fdf43)) - [@abougouffa](https://github.com/abougouffa)
- remove unneeded arguments `minemacs-register-on-demand-module` - ([cbe9afa](https://github.com/abougouffa/minemacs/commit/cbe9afa)) - [@abougouffa](https://github.com/abougouffa)
- move `modules/modes` to `modules/on-demand` - ([ef39e7c](https://github.com/abougouffa/minemacs/commit/ef39e7c)) - [@abougouffa](https://github.com/abougouffa)
- load less used modes only on-demand when needed - ([5fadc62](https://github.com/abougouffa/minemacs/commit/5fadc62)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- rebuild all packages in `minemacs-restore-locked-packages` - ([e005059](https://github.com/abougouffa/minemacs/commit/e005059)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** allow loading on-demand modules via `minemacs-load-module` - ([afbc799](https://github.com/abougouffa/minemacs/commit/afbc799)) - [@abougouffa](https://github.com/abougouffa)
- **(on-demand)** ask only for visible buffers - ([d69f181](https://github.com/abougouffa/minemacs/commit/d69f181)) - [@abougouffa](https://github.com/abougouffa)
- add an option to disable all on-demand modules - ([2644605](https://github.com/abougouffa/minemacs/commit/2644605)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2ed8d2e](https://github.com/abougouffa/minemacs/commit/2ed8d2e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.8.3](https://github.com/abougouffa/minemacs/compare/9ab3077..v8.8.3) - 2024-08-09
#### Features
- make `org-re-reveal` obsolete (never used!) - ([9ab3077](https://github.com/abougouffa/minemacs/commit/9ab3077)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([fb7a2ef](https://github.com/abougouffa/minemacs/commit/fb7a2ef)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.8.2](https://github.com/abougouffa/minemacs/compare/814946e..v8.8.2) - 2024-08-09
#### Bug Fixes
- **(init)** fix the format in error messages - ([022244a](https://github.com/abougouffa/minemacs/commit/022244a)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate documentation - ([eabcf86](https://github.com/abougouffa/minemacs/commit/eabcf86)) - [@abougouffa](https://github.com/abougouffa)
- update the logo - ([814946e](https://github.com/abougouffa/minemacs/commit/814946e)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ui)** initial support for `ef-themes` - ([19492e2](https://github.com/abougouffa/minemacs/commit/19492e2)) - [@abougouffa](https://github.com/abougouffa)
- add `+browse-html-file` - ([17254c7](https://github.com/abougouffa/minemacs/commit/17254c7)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** use `me-lib-extra.el` to generate documentation - ([a254550](https://github.com/abougouffa/minemacs/commit/a254550)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** change fonts priority - ([21209fe](https://github.com/abougouffa/minemacs/commit/21209fe)) - [@abougouffa](https://github.com/abougouffa)
- **(hurl-mode)** restore the upstream repo, issue fixed - ([250698b](https://github.com/abougouffa/minemacs/commit/250698b)) - [@abougouffa](https://github.com/abougouffa)
- **(mlscroll)** restore `+mlscroll-right-mode` to the obsolete module - ([8f22cbf](https://github.com/abougouffa/minemacs/commit/8f22cbf)) - [@abougouffa](https://github.com/abougouffa)
- **(pet)** minor tweak for Org advice - ([9d963bc](https://github.com/abougouffa/minemacs/commit/9d963bc)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add Hurl grammar - ([7d2b0db](https://github.com/abougouffa/minemacs/commit/7d2b0db)) - [@abougouffa](https://github.com/abougouffa)
- prefer `Iosevka Comfy Motion Duo` for `:variable-pitch` - ([de081de](https://github.com/abougouffa/minemacs/commit/de081de)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([4ddf408](https://github.com/abougouffa/minemacs/commit/4ddf408)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.8.1](https://github.com/abougouffa/minemacs/compare/ec56cc0..v8.8.1) - 2024-08-04
#### Documentation
- **(documentation)** regenerate the documentation - ([23838fc](https://github.com/abougouffa/minemacs/commit/23838fc)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(elisp)** make `flymake-relint` obsolete and keep `relint` - ([511481a](https://github.com/abougouffa/minemacs/commit/511481a)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** add a comment - ([ec56cc0](https://github.com/abougouffa/minemacs/commit/ec56cc0)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** remove `me-gc` from the core modules - ([5cd0d21](https://github.com/abougouffa/minemacs/commit/5cd0d21)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- add a script to generate Cppcheck rules from MISRA PDF - ([0bce095](https://github.com/abougouffa/minemacs/commit/0bce095)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- rename some functions - ([d6e565e](https://github.com/abougouffa/minemacs/commit/d6e565e)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dape)** disable buggy GUD-like window arrangement - ([14acc80](https://github.com/abougouffa/minemacs/commit/14acc80)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.8.0](https://github.com/abougouffa/minemacs/compare/3ca7982..v8.8.0) - 2024-08-03
#### Bug Fixes
- **(hurl-mode)** temporary switch to my fork until PR gets merged - ([9fcd26e](https://github.com/abougouffa/minemacs/commit/9fcd26e)) - [@abougouffa](https://github.com/abougouffa)
- **(mlscroll)** make sure `mlscroll` is loaded on `+mlscroll-right-mode` - ([3e5c17e](https://github.com/abougouffa/minemacs/commit/3e5c17e)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** correct the `+clang-format-get-style` for classic C/C++ modes - ([1acc7fd](https://github.com/abougouffa/minemacs/commit/1acc7fd)) - [@abougouffa](https://github.com/abougouffa)
- **(restclient)** correct the recipe - ([3ca7982](https://github.com/abougouffa/minemacs/commit/3ca7982)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** correct interactive `+treesit-create-parser-in-buffer` - ([529b54a](https://github.com/abougouffa/minemacs/commit/529b54a)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(treesit-auto)** update some comments - ([1b3f45f](https://github.com/abougouffa/minemacs/commit/1b3f45f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(keybindings)** remove unused `hydra` stuff - ([1bb0e7b](https://github.com/abougouffa/minemacs/commit/1bb0e7b)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** add support for `denote-menu` - ([c379699](https://github.com/abougouffa/minemacs/commit/c379699)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `restclient` obsolete (use `hurl-mode` or `verb`) - ([e4945df](https://github.com/abougouffa/minemacs/commit/e4945df)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `mlscroll` with tweaked mode `+mlscroll-right-mode` - ([50eb555](https://github.com/abougouffa/minemacs/commit/50eb555)) - [@abougouffa](https://github.com/abougouffa)
- make `emacs-gdb` obsolete - ([b8a731c](https://github.com/abougouffa/minemacs/commit/b8a731c)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(prog)** minor edit in `+clang-format-get-style` - ([6eb9723](https://github.com/abougouffa/minemacs/commit/6eb9723)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([6445517](https://github.com/abougouffa/minemacs/commit/6445517)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- tweak conditional installation for `obsolete/me-evil` - ([147bb70](https://github.com/abougouffa/minemacs/commit/147bb70)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(treesit-auto)** the grammar of C++ seems Ok in Emacs 30 - ([714f4e1](https://github.com/abougouffa/minemacs/commit/714f4e1)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** remove `+mlscroll-right-mode` - ([4224ae7](https://github.com/abougouffa/minemacs/commit/4224ae7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(app-launcher)** bind `C-c o a` to `app-launcher-run-app` - ([5fb4473](https://github.com/abougouffa/minemacs/commit/5fb4473)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** bigger fonts for variable pitch - ([b63c00c](https://github.com/abougouffa/minemacs/commit/b63c00c)) - [@abougouffa](https://github.com/abougouffa)
- **(gdb-mi)** better defaults for Emacs 30 - ([f836d5d](https://github.com/abougouffa/minemacs/commit/f836d5d)) - [@abougouffa](https://github.com/abougouffa)
- **(gdb-mi)** remove old unnecessary tweaks - ([b09bb6c](https://github.com/abougouffa/minemacs/commit/b09bb6c)) - [@abougouffa](https://github.com/abougouffa)
- **(grep)** use Emacs 30 rg-like headings view - ([790d612](https://github.com/abougouffa/minemacs/commit/790d612)) - [@abougouffa](https://github.com/abougouffa)
- **(gud)** Emacs 30 supports highlighting the current line - ([2e9c6b9](https://github.com/abougouffa/minemacs/commit/2e9c6b9)) - [@abougouffa](https://github.com/abougouffa)
- **(hurl-mode)** use for the `*.hurl` extension - ([633e5a5](https://github.com/abougouffa/minemacs/commit/633e5a5)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** warn on Emacs v28 - ([be9415b](https://github.com/abougouffa/minemacs/commit/be9415b)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** mark some commands to run for all cursors - ([69165dd](https://github.com/abougouffa/minemacs/commit/69165dd)) - [@abougouffa](https://github.com/abougouffa)
- **(org-jira)** set the default working directory - ([ef5741b](https://github.com/abougouffa/minemacs/commit/ef5741b)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** set background for the alternate face also - ([61496e3](https://github.com/abougouffa/minemacs/commit/61496e3)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([737d9ab](https://github.com/abougouffa/minemacs/commit/737d9ab)) - [@abougouffa](https://github.com/abougouffa)
- remove `gcmh` and simplify GC management - ([a6e6082](https://github.com/abougouffa/minemacs/commit/a6e6082)) - [@abougouffa](https://github.com/abougouffa)
- rebuild only when necessary in `minemacs-restore-locked-packages` - ([51ab896](https://github.com/abougouffa/minemacs/commit/51ab896)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.7.0](https://github.com/abougouffa/minemacs/compare/18cece3..v8.7.0) - 2024-07-31
#### Bug Fixes
- **(jiralib)** use the right host name argument in auto-login - ([ff92e7f](https://github.com/abougouffa/minemacs/commit/ff92e7f)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** require `color` before using `color-*-name` - ([33c971a](https://github.com/abougouffa/minemacs/commit/33c971a)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(bitbake)** add some helper commands - ([a2d6892](https://github.com/abougouffa/minemacs/commit/a2d6892)) - [@abougouffa](https://github.com/abougouffa)
- **(completion)** remove `consult-web` - ([b1481f6](https://github.com/abougouffa/minemacs/commit/b1481f6)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add a `spacious-padding` like subtle mode-line look - ([a187ece](https://github.com/abougouffa/minemacs/commit/a187ece)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `format-all-the-code` - ([f8b548d](https://github.com/abougouffa/minemacs/commit/f8b548d)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `loccur` obsolete, `consult-focus-lines` does the job! - ([e62767e](https://github.com/abougouffa/minemacs/commit/e62767e)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `visual-format` - ([8f47c7a](https://github.com/abougouffa/minemacs/commit/8f47c7a)) - [@abougouffa](https://github.com/abougouffa)
- visual format buffer (early WIP) - ([c7483ab](https://github.com/abougouffa/minemacs/commit/c7483ab)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- mark my temporary forks of packages as forks - ([bfd3422](https://github.com/abougouffa/minemacs/commit/bfd3422)) - [@abougouffa](https://github.com/abougouffa)
- prefer using the list form for `:commands` - ([c677dfc](https://github.com/abougouffa/minemacs/commit/c677dfc)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** add the `-style` option for the `clang-format` command - ([725ae78](https://github.com/abougouffa/minemacs/commit/725ae78)) - [@abougouffa](https://github.com/abougouffa)
- **(clang-format)** better handling of the `-style` argument - ([36f0cfe](https://github.com/abougouffa/minemacs/commit/36f0cfe)) - [@abougouffa](https://github.com/abougouffa)
- **(combobulate)** don't enable by default (very annoying in Python) - ([18cece3](https://github.com/abougouffa/minemacs/commit/18cece3)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** customize more commands with initial input - ([868842b](https://github.com/abougouffa/minemacs/commit/868842b)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** don't load until needed - ([fb4cd11](https://github.com/abougouffa/minemacs/commit/fb4cd11)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** setup some hooks for better integration - ([52f6a84](https://github.com/abougouffa/minemacs/commit/52f6a84)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** simplify and restore the vertical bar - ([e66b37c](https://github.com/abougouffa/minemacs/commit/e66b37c)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** add a shortcut to browse Org directory in Dired - ([1662fc1](https://github.com/abougouffa/minemacs/commit/1662fc1)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-selection)** use my fork until it gets merged - ([a3567e3](https://github.com/abougouffa/minemacs/commit/a3567e3)) - [@abougouffa](https://github.com/abougouffa)
- **(opencl-c-mode)** use the correct package/mode name - ([fc3e53f](https://github.com/abougouffa/minemacs/commit/fc3e53f)) - [@abougouffa](https://github.com/abougouffa)
- **(reformatter)** define some formatters - ([6dfecd4](https://github.com/abougouffa/minemacs/commit/6dfecd4)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** rename `visual-format` the `virtual-format` - ([4beb68d](https://github.com/abougouffa/minemacs/commit/4beb68d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([fee238e](https://github.com/abougouffa/minemacs/commit/fee238e)) - [@abougouffa](https://github.com/abougouffa)
- apply a lighter/darker color for trailing whitespace - ([7892c51](https://github.com/abougouffa/minemacs/commit/7892c51)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6e1c330](https://github.com/abougouffa/minemacs/commit/6e1c330)) - [@abougouffa](https://github.com/abougouffa)
- don't reinvent the wheel, `color-lighten-name` is a thing! - ([fce9e4e](https://github.com/abougouffa/minemacs/commit/fce9e4e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.6.4](https://github.com/abougouffa/minemacs/compare/1b82605..v8.6.4) - 2024-07-22
#### Bug Fixes
- **(flymake)** properly defer fringe tweaks - ([1b82605](https://github.com/abougouffa/minemacs/commit/1b82605)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** fix buggy case on Emacs 30 - ([5a98de3](https://github.com/abougouffa/minemacs/commit/5a98de3)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** server crashing silently due to face setting - ([d7ba61f](https://github.com/abougouffa/minemacs/commit/d7ba61f)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** dynamically set the face color - ([026eee0](https://github.com/abougouffa/minemacs/commit/026eee0)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** fix the `__elpkg` snippet - ([59e4995](https://github.com/abougouffa/minemacs/commit/59e4995)) - [@abougouffa](https://github.com/abougouffa)
- **(xref-union)** make sure we disable `etags` in `xref-union` - ([fcc092d](https://github.com/abougouffa/minemacs/commit/fcc092d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+color-brighter-or-darker` - ([2a5b320](https://github.com/abougouffa/minemacs/commit/2a5b320)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+set-indent-width` - ([87f7fc0](https://github.com/abougouffa/minemacs/commit/87f7fc0)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** remove `license-snippets` (outdated and messy) - ([a45a1b4](https://github.com/abougouffa/minemacs/commit/a45a1b4)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `casual-bookmarks` - ([34e38ea](https://github.com/abougouffa/minemacs/commit/34e38ea)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- update logo - ([d756545](https://github.com/abougouffa/minemacs/commit/d756545)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(ecryptfs)** move the `ecryptfs` integration to separate package - ([cd8f0d9](https://github.com/abougouffa/minemacs/commit/cd8f0d9)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** make use of `+color-brighter-or-darker` - ([33eca80](https://github.com/abougouffa/minemacs/commit/33eca80)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(dumb-jump)** no need to add `dumb-jump` for `xref-union` to work - ([7fc842d](https://github.com/abougouffa/minemacs/commit/7fc842d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** respect the percentage in `+color-brighter-or-darker` - ([b36a71b](https://github.com/abougouffa/minemacs/commit/b36a71b)) - [@abougouffa](https://github.com/abougouffa)
- **(dtrt-indent)** better defaults and triggering predicate - ([cf12878](https://github.com/abougouffa/minemacs/commit/cf12878)) - [@abougouffa](https://github.com/abougouffa)
- **(ecryptfs)** add bindings for `ecryptfs-toggle-mount-private` - ([b0119a9](https://github.com/abougouffa/minemacs/commit/b0119a9)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** add a dedicated mode for `guard-lf` files - ([2cfdabc](https://github.com/abougouffa/minemacs/commit/2cfdabc)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([14f2c33](https://github.com/abougouffa/minemacs/commit/14f2c33)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([bd207b3](https://github.com/abougouffa/minemacs/commit/bd207b3)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.6.3](https://github.com/abougouffa/minemacs/compare/a6f7ec6..v8.6.3) - 2024-07-21
#### Features
- **(editor)** restore `spdx` - ([a6f7ec6](https://github.com/abougouffa/minemacs/commit/a6f7ec6)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(autorevert)** auto revert files immediately - ([4933b0c](https://github.com/abougouffa/minemacs/commit/4933b0c)) - [@abougouffa](https://github.com/abougouffa)
- **(csv-mode)** add the vertical bar `|` to the separators list - ([82734d5](https://github.com/abougouffa/minemacs/commit/82734d5)) - [@abougouffa](https://github.com/abougouffa)
- **(neotree)** switch to upstream (`nerd-icons` integration merged) - ([0bc58f8](https://github.com/abougouffa/minemacs/commit/0bc58f8)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** remove `unique-dir-name`, it is currently integrated - ([0f7758c](https://github.com/abougouffa/minemacs/commit/0f7758c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6f8e5af](https://github.com/abougouffa/minemacs/commit/6f8e5af)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.6.2](https://github.com/abougouffa/minemacs/compare/a0303d9..v8.6.2) - 2024-07-20
#### Features
- **(goto-last-change)** rewrite `goto-last-change` based on the original - ([1d9444c](https://github.com/abougouffa/minemacs/commit/1d9444c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(parinfer-rust)** move the hooks to `:hook` - ([a0303d9](https://github.com/abougouffa/minemacs/commit/a0303d9)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(goto-last-change)** bind `goto-last-change` to `M-é` - ([c8db33a](https://github.com/abougouffa/minemacs/commit/c8db33a)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-file-icons)** disable temporary, seems broken - ([7ad7363](https://github.com/abougouffa/minemacs/commit/7ad7363)) - [@abougouffa](https://github.com/abougouffa)
- **(rainbow-csv)** switch back to upstream - ([81bcca4](https://github.com/abougouffa/minemacs/commit/81bcca4)) - [@abougouffa](https://github.com/abougouffa)
- **(rainbow-csv)** switch to my fork until it gets merged - ([b868881](https://github.com/abougouffa/minemacs/commit/b868881)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([e78427e](https://github.com/abougouffa/minemacs/commit/e78427e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.6.1](https://github.com/abougouffa/minemacs/compare/5cf3158..v8.6.1) - 2024-07-20
#### Bug Fixes
- **(org-msg)** use a different fork that fixes `mu` 1.12 issues - ([ed852e8](https://github.com/abougouffa/minemacs/commit/ed852e8)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** try to fix the mode being not taken into account - ([75f9805](https://github.com/abougouffa/minemacs/commit/75f9805)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(workspaces)** remove unused `burly` and `bufler` - ([ca36e9b](https://github.com/abougouffa/minemacs/commit/ca36e9b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cognitive-complexity)** update the repo link - ([39e32bf](https://github.com/abougouffa/minemacs/commit/39e32bf)) - [@abougouffa](https://github.com/abougouffa)
- **(dumb-jump)** make sure it is locally added to the xref backends - ([5bf2777](https://github.com/abougouffa/minemacs/commit/5bf2777)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** add keybindings to skip cursors - ([5cf3158](https://github.com/abougouffa/minemacs/commit/5cf3158)) - [@abougouffa](https://github.com/abougouffa)
- **(org-msg)** replace `+org-msg-make-signature` with `+org-msg-signature` - ([1a30a35](https://github.com/abougouffa/minemacs/commit/1a30a35)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** display info messages on disable/enable - ([e3152fb](https://github.com/abougouffa/minemacs/commit/e3152fb)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f9222c9](https://github.com/abougouffa/minemacs/commit/f9222c9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.6.0](https://github.com/abougouffa/minemacs/compare/0c697be..v8.6.0) - 2024-07-19
#### Bug Fixes
- **(citre)** don't signal an error when `find-references` fails - ([17438cf](https://github.com/abougouffa/minemacs/commit/17438cf)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more generic args propagation - ([94364c8](https://github.com/abougouffa/minemacs/commit/94364c8)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** correctly propagate args in `+super-project-define-commands` - ([2299224](https://github.com/abougouffa/minemacs/commit/2299224)) - [@abougouffa](https://github.com/abougouffa)
- **(empv)** change invidious instance - ([3a6ae2d](https://github.com/abougouffa/minemacs/commit/3a6ae2d)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-completion)** restore the icons on the new `marginalia` - ([6e05d4a](https://github.com/abougouffa/minemacs/commit/6e05d4a)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** load the consult tweaks in the right order - ([18a7315](https://github.com/abougouffa/minemacs/commit/18a7315)) - [@abougouffa](https://github.com/abougouffa)
- **(systemd)** prefer the builtin capf backend over the `company` one - ([4012f63](https://github.com/abougouffa/minemacs/commit/4012f63)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** fix the tramp auto-save directory - ([21bce01](https://github.com/abougouffa/minemacs/commit/21bce01)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+save-buffer-preserving-modtime` - ([8649350](https://github.com/abougouffa/minemacs/commit/8649350)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+setq-advice!` - ([32ed6e2](https://github.com/abougouffa/minemacs/commit/32ed6e2)) - [@abougouffa](https://github.com/abougouffa)
- **(data)** add support for GraphQL via `graphql-mode` - ([9232fed](https://github.com/abougouffa/minemacs/commit/9232fed)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add initial support for `symbol-overlay` - ([3a3deea](https://github.com/abougouffa/minemacs/commit/3a3deea)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** add support for `package-lint` - ([828715e](https://github.com/abougouffa/minemacs/commit/828715e)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** add initial support for `inspector` - ([252abd3](https://github.com/abougouffa/minemacs/commit/252abd3)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `cognitive-complexity` - ([e091d79](https://github.com/abougouffa/minemacs/commit/e091d79)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `emacs-everywhere` - ([0c697be](https://github.com/abougouffa/minemacs/commit/0c697be)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `casual-re-builder` - ([b672beb](https://github.com/abougouffa/minemacs/commit/b672beb)) - [@abougouffa](https://github.com/abougouffa)
- add initial support for `goto-last-change` - ([b13a223](https://github.com/abougouffa/minemacs/commit/b13a223)) - [@abougouffa](https://github.com/abougouffa)
- add initial support for `inotify-revert` - ([739cce8](https://github.com/abougouffa/minemacs/commit/739cce8)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- correct a typo - ([4ee7712](https://github.com/abougouffa/minemacs/commit/4ee7712)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** remove unnecessary `+move-this-file` - ([7bed09c](https://github.com/abougouffa/minemacs/commit/7bed09c)) - [@abougouffa](https://github.com/abougouffa)
- **(empv)** cleanup and refactor - ([7dad83d](https://github.com/abougouffa/minemacs/commit/7dad83d)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- remove `inotify-revert`, no gain over `auto-revert` - ([db7ad96](https://github.com/abougouffa/minemacs/commit/db7ad96)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(casual)** use default MELPA recipes - ([50efb6c](https://github.com/abougouffa/minemacs/commit/50efb6c)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** add initial input for `consult-fd` - ([9933fcc](https://github.com/abougouffa/minemacs/commit/9933fcc)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** bind `+consult-insert-thing-at-point` to `M-i` - ([dc7033d](https://github.com/abougouffa/minemacs/commit/dc7033d)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-denote)** prefer `ripgrep` and `fd` - ([0642ba1](https://github.com/abougouffa/minemacs/commit/0642ba1)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** bind `corfu-send` to `RET` - ([58e81d5](https://github.com/abougouffa/minemacs/commit/58e81d5)) - [@abougouffa](https://github.com/abougouffa)
- **(csv-mode)** correctly guess the separator character - ([1e69dae](https://github.com/abougouffa/minemacs/commit/1e69dae)) - [@abougouffa](https://github.com/abougouffa)
- **(isearch)** use regexp based search by default - ([a8a7b9a](https://github.com/abougouffa/minemacs/commit/a8a7b9a)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib)** consider `markdown-ts-mode` in `+jira-insert-ticket-link` - ([8a1dad8](https://github.com/abougouffa/minemacs/commit/8a1dad8)) - [@abougouffa](https://github.com/abougouffa)
- **(markdown)** use `markdown-mode` unless the file is big - ([778d17b](https://github.com/abougouffa/minemacs/commit/778d17b)) - [@abougouffa](https://github.com/abougouffa)
- **(media)** minor changes in MPV `browse-url` integration - ([4f6850e](https://github.com/abougouffa/minemacs/commit/4f6850e)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** integrate with `symbol-overlay` - ([59e7b0f](https://github.com/abougouffa/minemacs/commit/59e7b0f)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete/blamer)** decrease font size for the blame - ([47cd5d1](https://github.com/abougouffa/minemacs/commit/47cd5d1)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** update the configuration for v2.0.0 - ([08ac3a8](https://github.com/abougouffa/minemacs/commit/08ac3a8)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** add some keybindings - ([92e1581](https://github.com/abougouffa/minemacs/commit/92e1581)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** bump package version - ([b008d2a](https://github.com/abougouffa/minemacs/commit/b008d2a)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** add initial input for `consult` commands - ([caeb001](https://github.com/abougouffa/minemacs/commit/caeb001)) - [@abougouffa](https://github.com/abougouffa)
- **(otpp)** enable `otpp-remap-commands-mode` - ([1ea123b](https://github.com/abougouffa/minemacs/commit/1ea123b)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** better implementation of `project-name` - ([c045cb8](https://github.com/abougouffa/minemacs/commit/c045cb8)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add some functions/commands for compatibility with `projectile` - ([69e9460](https://github.com/abougouffa/minemacs/commit/69e9460)) - [@abougouffa](https://github.com/abougouffa)
- **(rainbow-csv)** make use of `+setq-advice!` - ([b2ac604](https://github.com/abougouffa/minemacs/commit/b2ac604)) - [@abougouffa](https://github.com/abougouffa)
- **(rainbow-csv)** automatically guess the separator - ([39ba664](https://github.com/abougouffa/minemacs/commit/39ba664)) - [@abougouffa](https://github.com/abougouffa)
- **(ssh-deploy)** change the revisions directory - ([2041506](https://github.com/abougouffa/minemacs/commit/2041506)) - [@abougouffa](https://github.com/abougouffa)
- **(systemd)** switch to my fork - ([9a3e668](https://github.com/abougouffa/minemacs/commit/9a3e668)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** add group to a custom variable - ([179b321](https://github.com/abougouffa/minemacs/commit/179b321)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** restore Elisp grammar - ([618f93c](https://github.com/abougouffa/minemacs/commit/618f93c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3f813b8](https://github.com/abougouffa/minemacs/commit/3f813b8)) - [@abougouffa](https://github.com/abougouffa)
- move & rename `+insert-thing-at-point`, integrate with `isearch` - ([2096426](https://github.com/abougouffa/minemacs/commit/2096426)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([ff998b7](https://github.com/abougouffa/minemacs/commit/ff998b7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7577005](https://github.com/abougouffa/minemacs/commit/7577005)) - [@abougouffa](https://github.com/abougouffa)
- bump `one-tab-per-project` version - ([e689ff0](https://github.com/abougouffa/minemacs/commit/e689ff0)) - [@abougouffa](https://github.com/abougouffa)
- bump `one-tab-per-project` version - ([d31fe73](https://github.com/abougouffa/minemacs/commit/d31fe73)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0090e94](https://github.com/abougouffa/minemacs/commit/0090e94)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.5.0](https://github.com/abougouffa/minemacs/compare/5ca0374..v8.5.0) - 2024-07-10
#### Documentation
- update external tools - ([b966126](https://github.com/abougouffa/minemacs/commit/b966126)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(media)** initial support for `ready-player` - ([334b4c8](https://github.com/abougouffa/minemacs/commit/334b4c8)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** restore `lexic` integration - ([6ea0e69](https://github.com/abougouffa/minemacs/commit/6ea0e69)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `casual-ibuffer` - ([ec6a345](https://github.com/abougouffa/minemacs/commit/ec6a345)) - [@abougouffa](https://github.com/abougouffa)
- **(workspace)** replace `project-tab-groups` with `one-tab-per-project` - ([cc3a31a](https://github.com/abougouffa/minemacs/commit/cc3a31a)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** rebuild pages when the related CI configuration changes - ([1edf96b](https://github.com/abougouffa/minemacs/commit/1edf96b)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** change branch and update pages actions - ([bc762cc](https://github.com/abougouffa/minemacs/commit/bc762cc)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** remove testing on Emacs 28.2, add timeout - ([5ca0374](https://github.com/abougouffa/minemacs/commit/5ca0374)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(media)** minor edits - ([7f36f98](https://github.com/abougouffa/minemacs/commit/7f36f98)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** better code - ([faca25c](https://github.com/abougouffa/minemacs/commit/faca25c)) - [@abougouffa](https://github.com/abougouffa)
- extract unique naming utilities to an external package - ([cfb649c](https://github.com/abougouffa/minemacs/commit/cfb649c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bookmark)** truncate long lines when creating bookmark title - ([b4e7eb1](https://github.com/abougouffa/minemacs/commit/b4e7eb1)) - [@abougouffa](https://github.com/abougouffa)
- **(browse-url)** don't overwrite the default browser - ([64d7b6b](https://github.com/abougouffa/minemacs/commit/64d7b6b)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** enable `narrow-to-page` and `narrow-to-region` - ([bc01d74](https://github.com/abougouffa/minemacs/commit/bc01d74)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** drop support for Emacs 28 - ([f1a79dc](https://github.com/abougouffa/minemacs/commit/f1a79dc)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** update and correct the transient list - ([9f99d05](https://github.com/abougouffa/minemacs/commit/9f99d05)) - [@abougouffa](https://github.com/abougouffa)
- **(journalctl-mode)** autoload `journalctl-mode` to be used with saved logs - ([5479a44](https://github.com/abougouffa/minemacs/commit/5479a44)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete/eaf)** enable more apps - ([e77ebeb](https://github.com/abougouffa/minemacs/commit/e77ebeb)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** better sentinel messages for org async export - ([f881b24](https://github.com/abougouffa/minemacs/commit/f881b24)) - [@abougouffa](https://github.com/abougouffa)
- **(project-tab-groups)** unregister unique name and rename tabs on closing - ([b8fc926](https://github.com/abougouffa/minemacs/commit/b8fc926)) - [@abougouffa](https://github.com/abougouffa)
- **(smerge)** extract the custom `+smerge-vc-next-conflict-recenter` - ([31ef19a](https://github.com/abougouffa/minemacs/commit/31ef19a)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** rename the first tab to `*default*` - ([0e9d695](https://github.com/abougouffa/minemacs/commit/0e9d695)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([fa340c7](https://github.com/abougouffa/minemacs/commit/fa340c7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.4.0](https://github.com/abougouffa/minemacs/compare/7fa7e17..v8.4.0) - 2024-07-07
#### Bug Fixes
- **(nerd-icons-archive)** fix a typo - ([1d356e4](https://github.com/abougouffa/minemacs/commit/1d356e4)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(use-package)** better comments about the extra MinEmacs features - ([c953329](https://github.com/abougouffa/minemacs/commit/c953329)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add some utilities for unique naming based on a directory - ([24dae81](https://github.com/abougouffa/minemacs/commit/24dae81)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `nerd-icons-archive` - ([0bec9f3](https://github.com/abougouffa/minemacs/commit/0bec9f3)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** cleanup - ([65f82d3](https://github.com/abougouffa/minemacs/commit/65f82d3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(guard-lf)** update the intact major modes - ([7fa7e17](https://github.com/abougouffa/minemacs/commit/7fa7e17)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-archive)** update config to treat it as global mode - ([65e88a3](https://github.com/abougouffa/minemacs/commit/65e88a3)) - [@abougouffa](https://github.com/abougouffa)
- **(project-tab-groups)** better naming of tabs and tab-groups - ([9a15282](https://github.com/abougouffa/minemacs/commit/9a15282)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** show the tab bar by default - ([5d803da](https://github.com/abougouffa/minemacs/commit/5d803da)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** better code generation for `::trigger-commands` - ([2aad9e7](https://github.com/abougouffa/minemacs/commit/2aad9e7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([98f3100](https://github.com/abougouffa/minemacs/commit/98f3100)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([5996c22](https://github.com/abougouffa/minemacs/commit/5996c22)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cc00643](https://github.com/abougouffa/minemacs/commit/cc00643)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.3.0](https://github.com/abougouffa/minemacs/compare/1fd507f..v8.3.0) - 2024-07-04
#### Bug Fixes
- **(consult-notes)** replace the obsolete `denote` function - ([25c7169](https://github.com/abougouffa/minemacs/commit/25c7169)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fix `+super-project-define-commands` - ([dcba2a1](https://github.com/abougouffa/minemacs/commit/dcba2a1)) - [@abougouffa](https://github.com/abougouffa)
- buggy implementation of running hooks in `minemacs-load-module` - ([85c924d](https://github.com/abougouffa/minemacs/commit/85c924d)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([6aa3298](https://github.com/abougouffa/minemacs/commit/6aa3298)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(completeion)** initial support for `consult-web` - ([5a94c6b](https://github.com/abougouffa/minemacs/commit/5a94c6b)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add a helper to define command that act on super-projects - ([9ffa3ec](https://github.com/abougouffa/minemacs/commit/9ffa3ec)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** add `+fzf-super-project` - ([4cf7fbb](https://github.com/abougouffa/minemacs/commit/4cf7fbb)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** add support for `consult-denote` (`consult-notes` replacement) - ([adcc1f6](https://github.com/abougouffa/minemacs/commit/adcc1f6)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** make `consult-notes` obsolete - ([e4c0c90](https://github.com/abougouffa/minemacs/commit/e4c0c90)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** restore back the `affe` support - ([5cc1f79](https://github.com/abougouffa/minemacs/commit/5cc1f79)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** enable test on `release-snapshot` (Emacs 30) - ([3ff8331](https://github.com/abougouffa/minemacs/commit/3ff8331)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** disable the always demand mode - ([b46b7b7](https://github.com/abougouffa/minemacs/commit/b46b7b7)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(citre)** simplify finding the project's root marker - ([6c685f1](https://github.com/abougouffa/minemacs/commit/6c685f1)) - [@abougouffa](https://github.com/abougouffa)
- rename some commands to follow the MinEmacs conventions - ([1fd507f](https://github.com/abougouffa/minemacs/commit/1fd507f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult)** define some super-project commands variants - ([98ffb80](https://github.com/abougouffa/minemacs/commit/98ffb80)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** prefer `Martian Mono` font when available - ([1ea823a](https://github.com/abougouffa/minemacs/commit/1ea823a)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-pmd)** better defaults - ([f2d3e8a](https://github.com/abougouffa/minemacs/commit/f2d3e8a)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** make use of the new `+super-project-define-commands` - ([28f24f2](https://github.com/abougouffa/minemacs/commit/28f24f2)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** consistent naming, autoload custom commands - ([3fb207d](https://github.com/abougouffa/minemacs/commit/3fb207d)) - [@abougouffa](https://github.com/abougouffa)
- **(gaurd-lf)** add more modes to ignore - ([f869bbd](https://github.com/abougouffa/minemacs/commit/f869bbd)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** enable `+spellcheck-mode` in `git-commit-mode` - ([ebe6f07](https://github.com/abougouffa/minemacs/commit/ebe6f07)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** better handling of Python virtual environments in Eglot - ([cf1bc59](https://github.com/abougouffa/minemacs/commit/cf1bc59)) - [@abougouffa](https://github.com/abougouffa)
- **(rustic)** don't depend on `flycheck` please! - ([afd8b77](https://github.com/abougouffa/minemacs/commit/afd8b77)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a6ab417](https://github.com/abougouffa/minemacs/commit/a6ab417)) - [@abougouffa](https://github.com/abougouffa)
- don't pollute messages with logs in `+fn-inhibit-messages!` - ([0a92474](https://github.com/abougouffa/minemacs/commit/0a92474)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.2.0](https://github.com/abougouffa/minemacs/compare/82b837a..v8.2.0) - 2024-07-01
#### Bug Fixes
- **(corfu)** fix the `ispell` issue on Emacs 30 - ([21ae46d](https://github.com/abougouffa/minemacs/commit/21ae46d)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** update the handling of client commands for new Eglot API - ([358d7aa](https://github.com/abougouffa/minemacs/commit/358d7aa)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** fix Python LSP registration - ([1b3fb5c](https://github.com/abougouffa/minemacs/commit/1b3fb5c)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** correctly enable LTeX-LS in (La)TeX modes - ([018ec1e](https://github.com/abougouffa/minemacs/commit/018ec1e)) - [@abougouffa](https://github.com/abougouffa)
- `+eglot-register` failing on Windows - ([b380cc3](https://github.com/abougouffa/minemacs/commit/b380cc3)) - [@abougouffa](https://github.com/abougouffa)
- treat `+with-delayed!` body as one unit - ([21ceb4c](https://github.com/abougouffa/minemacs/commit/21ceb4c)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(builtin)** add a comment - ([f463751](https://github.com/abougouffa/minemacs/commit/f463751)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** regenerate the documentation - ([4eb59cb](https://github.com/abougouffa/minemacs/commit/4eb59cb)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(docs)** add `markdown-ts-mode` - ([367a9e2](https://github.com/abougouffa/minemacs/commit/367a9e2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add initial support fot Eglot protocol extensions `eglot-x` - ([871efb3](https://github.com/abougouffa/minemacs/commit/871efb3)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** add support for `loccur` - ([1f752ba](https://github.com/abougouffa/minemacs/commit/1f752ba)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- minor edit - ([db13ee9](https://github.com/abougouffa/minemacs/commit/db13ee9)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- refactor some core utils - ([7ef10da](https://github.com/abougouffa/minemacs/commit/7ef10da)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([775e1da](https://github.com/abougouffa/minemacs/commit/775e1da)) - [@abougouffa](https://github.com/abougouffa)
- don't set directories unless necessary + minor tweaks - ([2689e13](https://github.com/abougouffa/minemacs/commit/2689e13)) - [@abougouffa](https://github.com/abougouffa)
- move leader key related variables to `obsolete/me-evil` - ([914d4d2](https://github.com/abougouffa/minemacs/commit/914d4d2)) - [@abougouffa](https://github.com/abougouffa)
- move `elisp-demos` to `me-emacs-lisp` - ([21cdb25](https://github.com/abougouffa/minemacs/commit/21cdb25)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bookmark)** set bookmark by double clicking the left fringe - ([54ad314](https://github.com/abougouffa/minemacs/commit/54ad314)) - [@abougouffa](https://github.com/abougouffa)
- **(breadcrumb)** integrate with `nerd-icons` - ([1f36a5f](https://github.com/abougouffa/minemacs/commit/1f36a5f)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** force using GNU ELPA mirror for installed packages - ([1b4eb14](https://github.com/abougouffa/minemacs/commit/1b4eb14)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** exclude commands irrelevant to the current mode in `M-x` - ([30a96e8](https://github.com/abougouffa/minemacs/commit/30a96e8)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** don't auto insert the current candidate - ([103b1da](https://github.com/abougouffa/minemacs/commit/103b1da)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** register `pylyzer` LSP server - ([0b3dc6e](https://github.com/abougouffa/minemacs/commit/0b3dc6e)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** better implementation of `+eglot-register` - ([6f70f77](https://github.com/abougouffa/minemacs/commit/6f70f77)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot-x)** autoload `eglot-x-setup` - ([a242fa2](https://github.com/abougouffa/minemacs/commit/a242fa2)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** switch the repo, again! - ([b9956e5](https://github.com/abougouffa/minemacs/commit/b9956e5)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** switch to my fork - ([3b165f0](https://github.com/abougouffa/minemacs/commit/3b165f0)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** name tabs after the current project - ([eba0bc4](https://github.com/abougouffa/minemacs/commit/eba0bc4)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** fix grammars and refactor - ([06cb69d](https://github.com/abougouffa/minemacs/commit/06cb69d)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add the missing `casual-lib` dependency - ([de31e49](https://github.com/abougouffa/minemacs/commit/de31e49)) - [@abougouffa](https://github.com/abougouffa)
- **(yasnippet)** suppress some annoying and unimportant messages - ([36b262b](https://github.com/abougouffa/minemacs/commit/36b262b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d0440b6](https://github.com/abougouffa/minemacs/commit/d0440b6)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([726dc09](https://github.com/abougouffa/minemacs/commit/726dc09)) - [@abougouffa](https://github.com/abougouffa)
- bump the `guard-lf` version - ([be1ddb4](https://github.com/abougouffa/minemacs/commit/be1ddb4)) - [@abougouffa](https://github.com/abougouffa)
- better performance for big `text-mode` files - ([058602e](https://github.com/abougouffa/minemacs/commit/058602e)) - [@abougouffa](https://github.com/abougouffa)
- better implementation of `minemacs-load-module` - ([abdb445](https://github.com/abougouffa/minemacs/commit/abdb445)) - [@abougouffa](https://github.com/abougouffa)
- add a command to interactively install modules - ([acf190a](https://github.com/abougouffa/minemacs/commit/acf190a)) - [@abougouffa](https://github.com/abougouffa)
- don't persist any of `MINEMACS_*` environment variables - ([098a558](https://github.com/abougouffa/minemacs/commit/098a558)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([632d872](https://github.com/abougouffa/minemacs/commit/632d872)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([82b837a](https://github.com/abougouffa/minemacs/commit/82b837a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.1.0](https://github.com/abougouffa/minemacs/compare/8da9d40..v8.1.0) - 2024-06-22
#### Documentation
- **(early-init)** update comments - ([2664493](https://github.com/abougouffa/minemacs/commit/2664493)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `minemacs-apply-performance-tweaks` - ([8f7b3cb](https://github.com/abougouffa/minemacs/commit/8f7b3cb)) - [@abougouffa](https://github.com/abougouffa)
- **(workspace)** add initial support for `burly` and `bufler` - ([db01fea](https://github.com/abougouffa/minemacs/commit/db01fea)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move `which-key` to `me-builtin`, it is builtin in Emacs 30 - ([5d49030](https://github.com/abougouffa/minemacs/commit/5d49030)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(guard-lf)** restore the upstream repo (fix merged) - ([8da9d40](https://github.com/abougouffa/minemacs/commit/8da9d40)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** remove unneeded tweaks, better defaults - ([dace43e](https://github.com/abougouffa/minemacs/commit/dace43e)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** prefer default directories if they fall under `.emacs.d/local` - ([4bf6d03](https://github.com/abougouffa/minemacs/commit/4bf6d03)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** set only the directories when necessary - ([2c978be](https://github.com/abougouffa/minemacs/commit/2c978be)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** use `.citre-root` instead of `.citre_root` as a root marker - ([43ef63e](https://github.com/abougouffa/minemacs/commit/43ef63e)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** remove `+toggle-bury-compilation-buffer-if-successful` - ([783c776](https://github.com/abougouffa/minemacs/commit/783c776)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** the right way to load the package - ([f8c7154](https://github.com/abougouffa/minemacs/commit/f8c7154)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** update the don't reply to self config - ([b7b89b1](https://github.com/abougouffa/minemacs/commit/b7b89b1)) - [@abougouffa](https://github.com/abougouffa)
- **(pulsar)** pulse line on scroll commands - ([f699764](https://github.com/abougouffa/minemacs/commit/f699764)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** suggest a fix to a common issue in `citre` - ([0eab698](https://github.com/abougouffa/minemacs/commit/0eab698)) - [@abougouffa](https://github.com/abougouffa)
- ask a better question on quitting an Emacs client - ([15ee45a](https://github.com/abougouffa/minemacs/commit/15ee45a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.6..v8.0.0) - 2024-06-21
#### Features
- **(builtin)** disable `so-long` (replaced with `guard-lf`) - ([588ae98](https://github.com/abougouffa/minemacs/commit/588ae98)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(builtin)** rearrange comments - ([4c55d9c](https://github.com/abougouffa/minemacs/commit/4c55d9c)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete)** minor formatting changes - ([0a25976](https://github.com/abougouffa/minemacs/commit/0a25976)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** cleanup some irrelevant configs - ([b9fdcb9](https://github.com/abougouffa/minemacs/commit/b9fdcb9)) - [@abougouffa](https://github.com/abougouffa)
- move `blamer`/`+writing-mode` integration to `me-writing-mode` - ([bfb2fd9](https://github.com/abougouffa/minemacs/commit/bfb2fd9)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(blamer)** don't enable globally - ([231fae7](https://github.com/abougouffa/minemacs/commit/231fae7)) - [@abougouffa](https://github.com/abougouffa)
- **(breadcrumb)** enable in some programming modes - ([bb4655d](https://github.com/abougouffa/minemacs/commit/bb4655d)) - [@abougouffa](https://github.com/abougouffa)
- **(code-review)** switch to `doomelpa` fork - ([c4560d3](https://github.com/abougouffa/minemacs/commit/c4560d3)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** use my fork until jcs-elpa/guard-lf#2 gets merged - ([d3f6e4d](https://github.com/abougouffa/minemacs/commit/d3f6e4d)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** mark some modes as intact - ([d8dc3d6](https://github.com/abougouffa/minemacs/commit/d8dc3d6)) - [@abougouffa](https://github.com/abougouffa)
- **(guard-lf)** switch to upstream (fixes merged) - ([63d94d3](https://github.com/abougouffa/minemacs/commit/63d94d3)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** truncate long lines in the Xref references buffer - ([5a21611](https://github.com/abougouffa/minemacs/commit/5a21611)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7c7e33a](https://github.com/abougouffa/minemacs/commit/7c7e33a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.6](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.3..v8.0.0-alpha.6) - 2024-06-20
#### Documentation
- **(documentation)** regenerate the documentation - ([0de4699](https://github.com/abougouffa/minemacs/commit/0de4699)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(files)** add initial support for `guard-lf` (better than `so-long`!) - ([1113291](https://github.com/abougouffa/minemacs/commit/1113291)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** remove `golden-ratio` - ([333758b](https://github.com/abougouffa/minemacs/commit/333758b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(guard-lf)** use my fork - ([9acd33d](https://github.com/abougouffa/minemacs/commit/9acd33d)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** more project markers - ([c3fe2f2](https://github.com/abougouffa/minemacs/commit/c3fe2f2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([df64559](https://github.com/abougouffa/minemacs/commit/df64559)) - [@abougouffa](https://github.com/abougouffa)
- load `minemacs-lazy` without printing a message - ([0132028](https://github.com/abougouffa/minemacs/commit/0132028)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.5](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.3..v8.0.0-alpha.5) - 2024-06-19
#### Bug Fixes
- **(vlf)** correctly load `vlf-setup` - ([1915a43](https://github.com/abougouffa/minemacs/commit/1915a43)) - [@abougouffa](https://github.com/abougouffa)
- move a `dired` customization to its relevant place - ([eebebdd](https://github.com/abougouffa/minemacs/commit/eebebdd)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(external-tools)** update the list - ([5ad53cf](https://github.com/abougouffa/minemacs/commit/5ad53cf)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** update the documentation of MinEmacs' synchronization hook - ([985a271](https://github.com/abougouffa/minemacs/commit/985a271)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update CI badges links to include the right branch - ([f68d14b](https://github.com/abougouffa/minemacs/commit/f68d14b)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** mention the change log - ([f15bba7](https://github.com/abougouffa/minemacs/commit/f15bba7)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(embedded)** add `pcap-mode` to display network/sensor capture files - ([cbc99f9](https://github.com/abougouffa/minemacs/commit/cbc99f9)) - [@abougouffa](https://github.com/abougouffa)
- **(embedded)** more assembly flavors (fasm, gas, masm and nasm) - ([87de0d3](https://github.com/abougouffa/minemacs/commit/87de0d3)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** initial support for `jupyter` - ([7cbacc2](https://github.com/abougouffa/minemacs/commit/7cbacc2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** initial support for `treesit-fold` - ([76d7d9c](https://github.com/abougouffa/minemacs/commit/76d7d9c)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** add `avy-zap` - ([cde8a6d](https://github.com/abougouffa/minemacs/commit/cde8a6d)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `keycast` with a hack to display it in `doom-modeline` - ([3e94d67](https://github.com/abougouffa/minemacs/commit/3e94d67)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(cocogitto)** don't auto-push after bumping the version - ([3740e19](https://github.com/abougouffa/minemacs/commit/3740e19)) - [@abougouffa](https://github.com/abougouffa)
- **(version)** v8.0.0-alpha.4 - ([3cf06f2](https://github.com/abougouffa/minemacs/commit/3cf06f2)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(corfu)** minor cleanups - ([f1d0590](https://github.com/abougouffa/minemacs/commit/f1d0590)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** better comments, little cleanup - ([94c9f9d](https://github.com/abougouffa/minemacs/commit/94c9f9d)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(git-commit)** minor edit - ([59c82fd](https://github.com/abougouffa/minemacs/commit/59c82fd)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** load the `jiralib` dependency before `org-jira` - ([12357df](https://github.com/abougouffa/minemacs/commit/12357df)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** customize `align` - ([b33fa1c](https://github.com/abougouffa/minemacs/commit/b33fa1c)) - [@abougouffa](https://github.com/abougouffa)
- **(diff-hl)** stick to defaults - ([dfe6777](https://github.com/abougouffa/minemacs/commit/dfe6777)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** ensure loading `dirvish` before `dired` gets called - ([1659326](https://github.com/abougouffa/minemacs/commit/1659326)) - [@abougouffa](https://github.com/abougouffa)
- **(magit)** replace project keybindings for `vc` by `magit` - ([2559ca6](https://github.com/abougouffa/minemacs/commit/2559ca6)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-completion)** enable globally - ([5528884](https://github.com/abougouffa/minemacs/commit/5528884)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** extra project root markers - ([2ed9498](https://github.com/abougouffa/minemacs/commit/2ed9498)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add `+treesit-create-parser-in-buffer` - ([1d2f793](https://github.com/abougouffa/minemacs/commit/1d2f793)) - [@abougouffa](https://github.com/abougouffa)
- provide `minemacs-lazy` at the end of the lazy packages - ([9970096](https://github.com/abougouffa/minemacs/commit/9970096)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.4](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.3..v8.0.0-alpha.4) - 2024-06-18
#### Bug Fixes
- **(vlf)** correctly load `vlf-setup` - ([1915a43](https://github.com/abougouffa/minemacs/commit/1915a43)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** mention the change log - ([f15bba7](https://github.com/abougouffa/minemacs/commit/f15bba7)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(math)** initial support for `jupyter` - ([7cbacc2](https://github.com/abougouffa/minemacs/commit/7cbacc2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** initial support for `treesit-fold` - ([76d7d9c](https://github.com/abougouffa/minemacs/commit/76d7d9c)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(cocogitto)** don't auto-push after bumping the version - ([3740e19](https://github.com/abougouffa/minemacs/commit/3740e19)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(corfu)** minor cleanups - ([f1d0590](https://github.com/abougouffa/minemacs/commit/f1d0590)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** better comments, little cleanup - ([94c9f9d](https://github.com/abougouffa/minemacs/commit/94c9f9d)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(git-commit)** minor edit - ([59c82fd](https://github.com/abougouffa/minemacs/commit/59c82fd)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** load the `jiralib` dependency before `org-jira` - ([12357df](https://github.com/abougouffa/minemacs/commit/12357df)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(diff-hl)** stick to defaults - ([dfe6777](https://github.com/abougouffa/minemacs/commit/dfe6777)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** ensure loading `dirvish` before `dired` gets called - ([1659326](https://github.com/abougouffa/minemacs/commit/1659326)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-completion)** enable globally - ([5528884](https://github.com/abougouffa/minemacs/commit/5528884)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add `+treesit-create-parser-in-buffer` - ([1d2f793](https://github.com/abougouffa/minemacs/commit/1d2f793)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.3](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.2..v8.0.0-alpha.3) - 2024-06-17
#### Documentation
- **(changelog)** remove duplicate entry - ([271a070](https://github.com/abougouffa/minemacs/commit/271a070)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** regenerate the documentation - ([7550704](https://github.com/abougouffa/minemacs/commit/7550704)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update the documenation - ([d4e1b09](https://github.com/abougouffa/minemacs/commit/d4e1b09)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(services)** add support for `webpaste` - ([1f5f8ce](https://github.com/abougouffa/minemacs/commit/1f5f8ce)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(nxml)** auto rename matching tags - ([db6253f](https://github.com/abougouffa/minemacs/commit/db6253f)) - [@abougouffa](https://github.com/abougouffa)
- bump `flymake-collection` to use the right fork - ([c27c1b9](https://github.com/abougouffa/minemacs/commit/c27c1b9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.2](https://github.com/abougouffa/minemacs/compare/v8.0.0-alpha.1..v8.0.0-alpha.2) - 2024-06-16
#### Features
- **(ui)** add the `casual` family of transient menus - ([8c77359](https://github.com/abougouffa/minemacs/commit/8c77359)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(git)** ignore Yasnippet generated files - ([d5d9f77](https://github.com/abougouffa/minemacs/commit/d5d9f77)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- rename a variable - ([f971648](https://github.com/abougouffa/minemacs/commit/f971648)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- correct depth values for hooks - ([938e3fe](https://github.com/abougouffa/minemacs/commit/938e3fe)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(flymake-collection)** switch to my fork with additional checkers - ([3942b32](https://github.com/abougouffa/minemacs/commit/3942b32)) - [@abougouffa](https://github.com/abougouffa)
- **(forge)** add the default keybindings - ([06fc6ad](https://github.com/abougouffa/minemacs/commit/06fc6ad)) - [@abougouffa](https://github.com/abougouffa)
- **(magit)** automatically refresh Magit after save - ([ff4e5e3](https://github.com/abougouffa/minemacs/commit/ff4e5e3)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `eldoc-box` obsolete - very annoying in some LSP sessions - ([9f3b5d5](https://github.com/abougouffa/minemacs/commit/9f3b5d5)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight-mode)** use the `isearch` face instead of `region` - ([2c5e431](https://github.com/abougouffa/minemacs/commit/2c5e431)) - [@abougouffa](https://github.com/abougouffa)
- **(snippets)** add a snippet for Elisp packages - ([24a0b59](https://github.com/abougouffa/minemacs/commit/24a0b59)) - [@abougouffa](https://github.com/abougouffa)
- bump packages verions - ([c65fd10](https://github.com/abougouffa/minemacs/commit/c65fd10)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v8.0.0-alpha.1](https://github.com/abougouffa/minemacs/compare/35ea103..v8.0.0-alpha.1) - 2024-06-15
#### Bug Fixes
- **(daemon)** don't launch `elfeed` automatically - ([3176b95](https://github.com/abougouffa/minemacs/commit/3176b95)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** annoying error related to `god-mode` - ([2497952](https://github.com/abougouffa/minemacs/commit/2497952)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer)** initializing `treesit` grammar in Elisp breaks parinfer - ([a182082](https://github.com/abougouffa/minemacs/commit/a182082)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight)** remove reference to obsolete module - ([a6d7453](https://github.com/abougouffa/minemacs/commit/a6d7453)) - [@abougouffa](https://github.com/abougouffa)
- move a `+nvmap` block to `me-evil` - ([dff3fd2](https://github.com/abougouffa/minemacs/commit/dff3fd2)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** add information about the NG branch - ([a9432d1](https://github.com/abougouffa/minemacs/commit/a9432d1)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** add indent size guessing via `dtrt-indent` - ([5f08274](https://github.com/abougouffa/minemacs/commit/5f08274)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add `ws-butler` for better white space cleanup! - ([a94916a](https://github.com/abougouffa/minemacs/commit/a94916a)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `whitespace-cleanup-mode` obsolete - ([880828d](https://github.com/abougouffa/minemacs/commit/880828d)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add more searching packages - ([419f297](https://github.com/abougouffa/minemacs/commit/419f297)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** restore `elisp-demos` - ([da86b46](https://github.com/abougouffa/minemacs/commit/da86b46)) - [@abougouffa](https://github.com/abougouffa)
- **(extra)** initial support for `dogears` - ([157d3a8](https://github.com/abougouffa/minemacs/commit/157d3a8)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** initial support for `xref-union` (not working!) - ([f25d890](https://github.com/abougouffa/minemacs/commit/f25d890)) - [@abougouffa](https://github.com/abougouffa)
- **(search)** make `better-jumper` obsolete (replaced with `dogears`) - ([17328c0](https://github.com/abougouffa/minemacs/commit/17328c0)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** restore `pulsar`, don't explicitly touch `pulse` - ([9e7f016](https://github.com/abougouffa/minemacs/commit/9e7f016)) - [@abougouffa](https://github.com/abougouffa)
- remove unused `svg-lib` - ([b20c18f](https://github.com/abougouffa/minemacs/commit/b20c18f)) - [@abougouffa](https://github.com/abougouffa)
- make `me-nano` obsolete - ([7146164](https://github.com/abougouffa/minemacs/commit/7146164)) - [@abougouffa](https://github.com/abougouffa)
- remove `show-marks` and `fm` - ([30ac536](https://github.com/abougouffa/minemacs/commit/30ac536)) - [@abougouffa](https://github.com/abougouffa)
- make `me-evil` obsolete - ([35ea103](https://github.com/abougouffa/minemacs/commit/35ea103)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(cocogitto)** add `minemacs-ng` to branch whitelist - ([2349b12](https://github.com/abougouffa/minemacs/commit/2349b12)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- make use of `once-x-call` - ([8b3726b](https://github.com/abougouffa/minemacs/commit/8b3726b)) - [@abougouffa](https://github.com/abougouffa)
- remove unused/obsolete functions and macros - ([0df88d6](https://github.com/abougouffa/minemacs/commit/0df88d6)) - [@abougouffa](https://github.com/abougouffa)
- remove `+after-load!` - ([5236898](https://github.com/abougouffa/minemacs/commit/5236898)) - [@abougouffa](https://github.com/abougouffa)
- replace `+add-hook!`, `+hook-once!` and `+advice-once!` by `satch` - ([58d6b77](https://github.com/abougouffa/minemacs/commit/58d6b77)) - [@abougouffa](https://github.com/abougouffa)
- remove some unused packages - ([63e67a0](https://github.com/abougouffa/minemacs/commit/63e67a0)) - [@abougouffa](https://github.com/abougouffa)
- move `editorconfig` from `me-prog` to `me-editor` - ([5bd5b54](https://github.com/abougouffa/minemacs/commit/5bd5b54)) - [@abougouffa](https://github.com/abougouffa)
- move search and navigation packages to `me-search` - ([fc5c7e1](https://github.com/abougouffa/minemacs/commit/fc5c7e1)) - [@abougouffa](https://github.com/abougouffa)
- remove obsolete variables and references to `me-evil` - ([91f9f9f](https://github.com/abougouffa/minemacs/commit/91f9f9f)) - [@abougouffa](https://github.com/abougouffa)
- move all Evil/General related bindings to `me-evil` - ([ddff771](https://github.com/abougouffa/minemacs/commit/ddff771)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- restore testing for Emacs daemon in the CI - ([5133fdc](https://github.com/abougouffa/minemacs/commit/5133fdc)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bootstrap)** pin `once` and `satch` for better stability - ([c3a6883](https://github.com/abougouffa/minemacs/commit/c3a6883)) - [@abougouffa](https://github.com/abougouffa)
- **(breadcrumb)** don't show the project crumbs - ([b430871](https://github.com/abougouffa/minemacs/commit/b430871)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** ask before quitting Emacs client session - ([fcef084](https://github.com/abougouffa/minemacs/commit/fcef084)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** add `+consult-tab` - ([b608f4d](https://github.com/abougouffa/minemacs/commit/b608f4d)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** don't show documentation unless asked, cleanup old stuff - ([60f0583](https://github.com/abougouffa/minemacs/commit/60f0583)) - [@abougouffa](https://github.com/abougouffa)
- **(dogears)** better integration with the rest of packages - ([603ad0f](https://github.com/abougouffa/minemacs/commit/603ad0f)) - [@abougouffa](https://github.com/abougouffa)
- **(dtrt-indent)** be less verbose - ([d868442](https://github.com/abougouffa/minemacs/commit/d868442)) - [@abougouffa](https://github.com/abougouffa)
- **(isearch)** better defaults - ([98ad75e](https://github.com/abougouffa/minemacs/commit/98ad75e)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** better keybindings - ([090b8bc](https://github.com/abougouffa/minemacs/commit/090b8bc)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add `project-shell` to project switch commands - ([44bf89b](https://github.com/abougouffa/minemacs/commit/44bf89b)) - [@abougouffa](https://github.com/abougouffa)
- **(rtags)** better defaults - ([9bc1471](https://github.com/abougouffa/minemacs/commit/9bc1471)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** use a separate buffer when showing references - ([a91553b](https://github.com/abougouffa/minemacs/commit/a91553b)) - [@abougouffa](https://github.com/abougouffa)
- **(yasnippet)** enable only on `prog-mode`, `text-mode` and `conf-mode` - ([473a904](https://github.com/abougouffa/minemacs/commit/473a904)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([fc915df](https://github.com/abougouffa/minemacs/commit/fc915df)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([52d3303](https://github.com/abougouffa/minemacs/commit/52d3303)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bbf4306](https://github.com/abougouffa/minemacs/commit/bbf4306)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.5.0](https://github.com/abougouffa/minemacs/compare/8367da5..v7.5.0) - 2024-06-11
#### Bug Fixes
- **(god-mode)** defer mode-specific keybindings - ([161c6be](https://github.com/abougouffa/minemacs/commit/161c6be)) - [@abougouffa](https://github.com/abougouffa)
- correct handling of `:prepend` in `+apply-font-or-script` - ([2a8ebe6](https://github.com/abougouffa/minemacs/commit/2a8ebe6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** restore `selection-highlight-mode` - ([a87015b](https://github.com/abougouffa/minemacs/commit/a87015b)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** replace my hack with `whitespace-cleanup-mode` - ([dcfdae7](https://github.com/abougouffa/minemacs/commit/dcfdae7)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** add initial support for `key-chord` - ([6321a04](https://github.com/abougouffa/minemacs/commit/6321a04)) - [@abougouffa](https://github.com/abougouffa)
- **(show-marks)** initial import of `show-marks` and `fm` - ([1d98a5b](https://github.com/abougouffa/minemacs/commit/1d98a5b)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(god-mode)** follow the hook naming conventions - ([72f80fe](https://github.com/abougouffa/minemacs/commit/72f80fe)) - [@abougouffa](https://github.com/abougouffa)
- **(which-key)** move Evil specific tweaks to `me-evil` - ([e619c6b](https://github.com/abougouffa/minemacs/commit/e619c6b)) - [@abougouffa](https://github.com/abougouffa)
- modernize `show-marks` and `fm` and add some features - ([7ef2c6f](https://github.com/abougouffa/minemacs/commit/7ef2c6f)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- don't apply the early background-color hack - ([f6e826c](https://github.com/abougouffa/minemacs/commit/f6e826c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** set indent for `xmllint` based on `nxml-child-indent` - ([8367da5](https://github.com/abougouffa/minemacs/commit/8367da5)) - [@abougouffa](https://github.com/abougouffa)
- **(avy)** bind `M-j` to `avy-goto-char-timer` - ([3824b5c](https://github.com/abougouffa/minemacs/commit/3824b5c)) - [@abougouffa](https://github.com/abougouffa)
- **(bootstrap)** remove unused `system-packages` - ([c4ea07b](https://github.com/abougouffa/minemacs/commit/c4ea07b)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** offer to create parent directories if they don't exist - ([e243589](https://github.com/abougouffa/minemacs/commit/e243589)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** cleanup and fix the keybinding - ([3c101cb](https://github.com/abougouffa/minemacs/commit/3c101cb)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** use the default ELPA recipe - ([3a060a1](https://github.com/abougouffa/minemacs/commit/3a060a1)) - [@abougouffa](https://github.com/abougouffa)
- **(god-mode)** add integration for `which-key` - ([88f6ee2](https://github.com/abougouffa/minemacs/commit/88f6ee2)) - [@abougouffa](https://github.com/abougouffa)
- **(helpful)** bind `helpful-callable` instead of `helpful-function` - ([e6ebe9b](https://github.com/abougouffa/minemacs/commit/e6ebe9b)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons)** don't try to automatically install the fonts - ([fa62917](https://github.com/abougouffa/minemacs/commit/fa62917)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** bind `C-x p a` to `+project-add-project` - ([8c9db7a](https://github.com/abougouffa/minemacs/commit/8c9db7a)) - [@abougouffa](https://github.com/abougouffa)
- **(rg)** add keybindig - ([3553839](https://github.com/abougouffa/minemacs/commit/3553839)) - [@abougouffa](https://github.com/abougouffa)
- **(show-marks)** delete unneeded code, several tweaks (still WIP) - ([7847a6f](https://github.com/abougouffa/minemacs/commit/7847a6f)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** bump `bootstrap-version` to 7 - ([c704ab7](https://github.com/abougouffa/minemacs/commit/c704ab7)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** don't prompt for `xref-find-references` - ([35cc4e7](https://github.com/abougouffa/minemacs/commit/35cc4e7)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** bind `M-<down-mouse-1>` to `xref-find-references-at-mouse` - ([f1ff172](https://github.com/abougouffa/minemacs/commit/f1ff172)) - [@abougouffa](https://github.com/abougouffa)
- **(xref)** add `+xref-find-references-at-point` - ([ac2718d](https://github.com/abougouffa/minemacs/commit/ac2718d)) - [@abougouffa](https://github.com/abougouffa)
- regenerate autoloads - ([12890c0](https://github.com/abougouffa/minemacs/commit/12890c0)) - [@abougouffa](https://github.com/abougouffa)
- smarter kill word commands - ([07065e7](https://github.com/abougouffa/minemacs/commit/07065e7)) - [@abougouffa](https://github.com/abougouffa)
- use `+region-or-thing-at-point` in `+consult-insert-thing-at-point` - ([2005a92](https://github.com/abougouffa/minemacs/commit/2005a92)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([3f87d79](https://github.com/abougouffa/minemacs/commit/3f87d79)) - [@abougouffa](https://github.com/abougouffa)
- update modules list - ([0349a1d](https://github.com/abougouffa/minemacs/commit/0349a1d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.4.0](https://github.com/abougouffa/minemacs/compare/088db1a..v7.4.0) - 2024-06-09
#### Features
- **(elfeed)** obsolete `yt-dl` commands - ([ac9a5b2](https://github.com/abougouffa/minemacs/commit/ac9a5b2)) - [@abougouffa](https://github.com/abougouffa)
- **(fun)** remove `asm-box` - ([088db1a](https://github.com/abougouffa/minemacs/commit/088db1a)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `mlscroll` obsolete - ([19f896f](https://github.com/abougouffa/minemacs/commit/19f896f)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(citar-embark)** minor edit - ([44f9828](https://github.com/abougouffa/minemacs/commit/44f9828)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** move command dedicated tabs creation to relevant modules - ([a779f72](https://github.com/abougouffa/minemacs/commit/a779f72)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** better default keybindings - ([8a30cff](https://github.com/abougouffa/minemacs/commit/8a30cff)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-themes)** enable visual bell extension - ([a13f510](https://github.com/abougouffa/minemacs/commit/a13f510)) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed)** prevent `db/index` from triggering `minemacs-first-file` - ([ec16f4a](https://github.com/abougouffa/minemacs/commit/ec16f4a)) - [@abougouffa](https://github.com/abougouffa)
- **(lib-extra)** remove unneeded `+eglot-optimization-mode` - ([ff128c1](https://github.com/abougouffa/minemacs/commit/ff128c1)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** additional keybindings - ([1511f83](https://github.com/abougouffa/minemacs/commit/1511f83)) - [@abougouffa](https://github.com/abougouffa)
- **(multiple-cursors)** add keybindings, move `meow` stuff to `me-meow` - ([7cfec92](https://github.com/abougouffa/minemacs/commit/7cfec92)) - [@abougouffa](https://github.com/abougouffa)
- **(yasnippet)** add a snippet for obsolete MinEmacs modules - ([aeaee51](https://github.com/abougouffa/minemacs/commit/aeaee51)) - [@abougouffa](https://github.com/abougouffa)
- more keybindings - ([8c780f6](https://github.com/abougouffa/minemacs/commit/8c780f6)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.3.3](https://github.com/abougouffa/minemacs/compare/31bd912..v7.3.3) - 2024-06-09
#### Documentation
- **(readme)** update documentation - ([31bd912](https://github.com/abougouffa/minemacs/commit/31bd912)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- add `+server-restart` - ([47e3507](https://github.com/abougouffa/minemacs/commit/47e3507)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(beardbolt)** switch the the upstream repo, fix merged - ([f5badb7](https://github.com/abougouffa/minemacs/commit/f5badb7)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** use `transient` for `+window-adjust-size-transient` - ([2ee13fc](https://github.com/abougouffa/minemacs/commit/2ee13fc)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** simpler frame title - ([603a574](https://github.com/abougouffa/minemacs/commit/603a574)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.3.2](https://github.com/abougouffa/minemacs/compare/1f605fb..v7.3.2) - 2024-06-09
#### Documentation
- **(documentation)** regenerate the documentation - ([d0fbf32](https://github.com/abougouffa/minemacs/commit/d0fbf32)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update the documentation - ([824ce8e](https://github.com/abougouffa/minemacs/commit/824ce8e)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** fix the `documentation` task - ([44af464](https://github.com/abougouffa/minemacs/commit/44af464)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(init)** remove `MINEMACS_IGNORE_VERSION_CHECK` - ([d2e6026](https://github.com/abougouffa/minemacs/commit/d2e6026)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(mixed-pitch)** don't change the cursor (causes the cursor to hide) - ([1f605fb](https://github.com/abougouffa/minemacs/commit/1f605fb)) - [@abougouffa](https://github.com/abougouffa)
- **(yasnippet)** add snippets in `~/.minemacs.d/snippets/` - ([fbf8480](https://github.com/abougouffa/minemacs/commit/fbf8480)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([de94785](https://github.com/abougouffa/minemacs/commit/de94785)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.3.1](https://github.com/abougouffa/minemacs/compare/328b50c..v7.3.1) - 2024-06-08
#### Refactoring
- ensure moving all `+map!` & `+map-local!` blocks to `me-evil` - ([95d9cb3](https://github.com/abougouffa/minemacs/commit/95d9cb3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(expreg)** use `C-M-SPC` to expand and `S-C-M-SPC` to contract - ([328b50c](https://github.com/abougouffa/minemacs/commit/328b50c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.3.0](https://github.com/abougouffa/minemacs/compare/e67d34e..v7.3.0) - 2024-06-08
#### Bug Fixes
- **(early-init)** more checks before trying to restore the background color - ([2ff90e7](https://github.com/abougouffa/minemacs/commit/2ff90e7)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** disable on some problematic commands - ([b1b51bb](https://github.com/abougouffa/minemacs/commit/b1b51bb)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** update examples - ([e67d34e](https://github.com/abougouffa/minemacs/commit/e67d34e)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** restore `yasnippet` - ([8b4bcba](https://github.com/abougouffa/minemacs/commit/8b4bcba)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `tempel` obsolete - ([1d74136](https://github.com/abougouffa/minemacs/commit/1d74136)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `spdx` obsolete - ([e696e3b](https://github.com/abougouffa/minemacs/commit/e696e3b)) - [@abougouffa](https://github.com/abougouffa)
- **(god)** initial support for `god-mode` - ([70da847](https://github.com/abougouffa/minemacs/commit/70da847)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(robot)** extract `rosbag-info-mode` to a separate package - ([8215399](https://github.com/abougouffa/minemacs/commit/8215399)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** remove `text-scale-*` bindings to `C-+`, `C-=` & `C--` - ([d34270f](https://github.com/abougouffa/minemacs/commit/d34270f)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** minor edits, set cursor type to `bar` - ([6d48bd8](https://github.com/abougouffa/minemacs/commit/6d48bd8)) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** add keybindings from Cape's example - ([841eac8](https://github.com/abougouffa/minemacs/commit/841eac8)) - [@abougouffa](https://github.com/abougouffa)
- **(helpful)** bind useless `C-h h` to `helpful-at-point` - ([589ebdd](https://github.com/abougouffa/minemacs/commit/589ebdd)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** `q` quits `macrostep-mode` when expanding macros - ([c6c465b](https://github.com/abougouffa/minemacs/commit/c6c465b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([09813f8](https://github.com/abougouffa/minemacs/commit/09813f8)) - [@abougouffa](https://github.com/abougouffa)
- drop Vim-like movement HJKL - ([1283a08](https://github.com/abougouffa/minemacs/commit/1283a08)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.2.0](https://github.com/abougouffa/minemacs/compare/e5bb799..v7.2.0) - 2024-06-07
#### Bug Fixes
- **(org)** async export bug due to obsolete `minemacs-core-modules` - ([3b28a95](https://github.com/abougouffa/minemacs/commit/3b28a95)) - [@abougouffa](https://github.com/abougouffa)
- use `kill-current-buffer` instead of `kill-this-buffer` - ([8532fed](https://github.com/abougouffa/minemacs/commit/8532fed)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(faq)** add an answer for `vterm-module` compilation issue - ([8b7b552](https://github.com/abougouffa/minemacs/commit/8b7b552)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update code examples in `skel/config.el` - ([c864239](https://github.com/abougouffa/minemacs/commit/c864239)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(docs)** make `edraw` obsolete - ([de857d8](https://github.com/abougouffa/minemacs/commit/de857d8)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `logos` obsolete - ([c6905b5](https://github.com/abougouffa/minemacs/commit/c6905b5)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `sr-speedbar` - ([11179be](https://github.com/abougouffa/minemacs/commit/11179be)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** replace `jiralib2` with `jiralib` + port my stuff to it - ([d14d869](https://github.com/abougouffa/minemacs/commit/d14d869)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** initial support for `window-purpose` (to be tested) - ([5e20103](https://github.com/abougouffa/minemacs/commit/5e20103)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- save some lines - ([5757000](https://github.com/abougouffa/minemacs/commit/5757000)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(biblio)** minor edit - ([e5bb799](https://github.com/abougouffa/minemacs/commit/e5bb799)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([883f259](https://github.com/abougouffa/minemacs/commit/883f259)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- don't overwrite the `trailing-whitespace` color - ([adf2c00](https://github.com/abougouffa/minemacs/commit/adf2c00)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(jiralib2)** enable inserting ticket summary - ([2e29afa](https://github.com/abougouffa/minemacs/commit/2e29afa)) - [@abougouffa](https://github.com/abougouffa)
- **(markdown)** enable highlighting syntax & code blocks fontification - ([b3f03cf](https://github.com/abougouffa/minemacs/commit/b3f03cf)) - [@abougouffa](https://github.com/abougouffa)
- **(tributary)** auto load some commands - ([0056312](https://github.com/abougouffa/minemacs/commit/0056312)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([01cc748](https://github.com/abougouffa/minemacs/commit/01cc748)) - [@abougouffa](https://github.com/abougouffa)
- avoid flickering UI on initialization - ([d37e4e2](https://github.com/abougouffa/minemacs/commit/d37e4e2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([b30ddc9](https://github.com/abougouffa/minemacs/commit/b30ddc9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.1.0](https://github.com/abougouffa/minemacs/compare/5d688f7..v7.1.0) - 2024-06-05
#### Bug Fixes
- **(core)** more robust `+dir-locals-open-or-create` - ([3e80a92](https://github.com/abougouffa/minemacs/commit/3e80a92)) - [@abougouffa](https://github.com/abougouffa)
- **(me-writing-mode)** load after `olivetti` (fix CI failure on Windows) - ([5ee5a28](https://github.com/abougouffa/minemacs/commit/5ee5a28)) - [@abougouffa](https://github.com/abougouffa)
- **(projection)** restore `projection-*` extensions - ([106ed5a](https://github.com/abougouffa/minemacs/commit/106ed5a)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** force v0.22.0 of C++ grammar (fix syntax highlighting) - ([b10bca5](https://github.com/abougouffa/minemacs/commit/b10bca5)) - [@abougouffa](https://github.com/abougouffa)
- remove references to `me-core-ui` - ([ea3397b](https://github.com/abougouffa/minemacs/commit/ea3397b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(builtin)** remove customization for `elec-pair` - ([50f09a4](https://github.com/abougouffa/minemacs/commit/50f09a4)) - [@abougouffa](https://github.com/abougouffa)
- **(debug)** make `realgud` obsolete - ([091a11e](https://github.com/abougouffa/minemacs/commit/091a11e)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** restore `smartparens` - ([02373fc](https://github.com/abougouffa/minemacs/commit/02373fc)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `cargo.el` - ([77fffc8](https://github.com/abougouffa/minemacs/commit/77fffc8)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `rustic` - ([bfb5d53](https://github.com/abougouffa/minemacs/commit/bfb5d53)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** replace `tmux.el` with `emamux` - ([1f819f2](https://github.com/abougouffa/minemacs/commit/1f819f2)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `mlscroll` - ([3ee9ce3](https://github.com/abougouffa/minemacs/commit/3ee9ce3)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `pulsar` obsolete - ([fa1a38d](https://github.com/abougouffa/minemacs/commit/fa1a38d)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** replace `visual-fill-column` with `olivetti` - ([686ccca](https://github.com/abougouffa/minemacs/commit/686ccca)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** make `gee` and `gerrit` obsolete, only keep `repo-transient` - ([3cde4b1](https://github.com/abougouffa/minemacs/commit/3cde4b1)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** move all `+map!` and `+map-local!` blocks to `me-evil` - ([401bb41](https://github.com/abougouffa/minemacs/commit/401bb41)) - [@abougouffa](https://github.com/abougouffa)
- move all `SPC` leader keybindings to `me-evil` - ([e2f69db](https://github.com/abougouffa/minemacs/commit/e2f69db)) - [@abougouffa](https://github.com/abougouffa)
- remove `me-splash` and simplify modules loading section - ([5d688f7](https://github.com/abougouffa/minemacs/commit/5d688f7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** accept `:package` & `:module` in `+map` & `+map-local!` - ([7993ed3](https://github.com/abougouffa/minemacs/commit/7993ed3)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** better integration with `nerd-icons` - ([a9facf2](https://github.com/abougouffa/minemacs/commit/a9facf2)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** restore the `evil-mc` hack for `smartparens` - ([c3030be](https://github.com/abougouffa/minemacs/commit/c3030be)) - [@abougouffa](https://github.com/abougouffa)
- **(logview)** minor tweak for RDK logs sumbode - ([b7a5e21](https://github.com/abougouffa/minemacs/commit/b7a5e21)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** bind `expreg-expand` & `expreg-contract` to `v` & `V` - ([aec189a](https://github.com/abougouffa/minemacs/commit/aec189a)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** bind `*` to `magit-delete-thing` - ([9890086](https://github.com/abougouffa/minemacs/commit/9890086)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** DRY - ([6500f90](https://github.com/abougouffa/minemacs/commit/6500f90)) - [@abougouffa](https://github.com/abougouffa)
- merge `me-core-ui` in `me-ui` - ([f5a5729](https://github.com/abougouffa/minemacs/commit/f5a5729)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([30e5d69](https://github.com/abougouffa/minemacs/commit/30e5d69)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v7.0.0](https://github.com/abougouffa/minemacs/compare/a98aea9..v7.0.0) - 2024-06-03
#### Bug Fixes
- **(beardbolt)** correct macro name, cleaner macro implementation - ([9d9b615](https://github.com/abougouffa/minemacs/commit/9d9b615)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** do Elisp stuff after loading `elisp-mode` - ([1d21307](https://github.com/abougouffa/minemacs/commit/1d21307)) - [@abougouffa](https://github.com/abougouffa)
- **(forge)** remove buggy transient binding - ([3d3b70a](https://github.com/abougouffa/minemacs/commit/3d3b70a)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-selection-mode)** deffer a litter bit more (!) - ([a118f2e](https://github.com/abougouffa/minemacs/commit/a118f2e)) - [@abougouffa](https://github.com/abougouffa)
- **(iedit)** add the keybindings - ([f24ff3c](https://github.com/abougouffa/minemacs/commit/f24ff3c)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** correctly load the package - ([f102c7d](https://github.com/abougouffa/minemacs/commit/f102c7d)) - [@abougouffa](https://github.com/abougouffa)
- **(nerd-icons-corfu)** load after `corfu` - ([b5b5473](https://github.com/abougouffa/minemacs/commit/b5b5473)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** remove `.vscode` from identifiers (conflict with ~/.vscode) - ([9c96067](https://github.com/abougouffa/minemacs/commit/9c96067)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** start at the right time, `prog-mode-hook` is too late - ([f05486e](https://github.com/abougouffa/minemacs/commit/f05486e)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- update documentation - ([f44cbcb](https://github.com/abougouffa/minemacs/commit/f44cbcb)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(builtin)** enable `repeat-mode` by default - ([0adb6da](https://github.com/abougouffa/minemacs/commit/0adb6da)) - [@abougouffa](https://github.com/abougouffa)
- **(calendar)** initial support for `org-timeblock` - ([0d68027](https://github.com/abougouffa/minemacs/commit/0d68027)) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** remove `cape-capf-super` unused hacks - ([f1f9182](https://github.com/abougouffa/minemacs/commit/f1f9182)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add the `+mode-alist-add-ts-modes!` macro - ([3430240](https://github.com/abougouffa/minemacs/commit/3430240)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `super-save` obsolete - ([aff8cca](https://github.com/abougouffa/minemacs/commit/aff8cca)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `selection-highlight-mode` obsolete - ([7bbeceb](https://github.com/abougouffa/minemacs/commit/7bbeceb)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `highlight-indentation-guides` obsolete - ([a0d8edf](https://github.com/abougouffa/minemacs/commit/a0d8edf)) - [@abougouffa](https://github.com/abougouffa)
- **(formal)** make obsolete - ([0516d30](https://github.com/abougouffa/minemacs/commit/0516d30)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** add initial support for Meow modal editing - ([925895d](https://github.com/abougouffa/minemacs/commit/925895d)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-cursors)** add initial support for `multiple-cursors` - ([c3c8565](https://github.com/abougouffa/minemacs/commit/c3c8565)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** make `ffip` obsolete - ([eea6f5d](https://github.com/abougouffa/minemacs/commit/eea6f5d)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** initial support for `hurl-mode` (to replace `restclient` someday) - ([5ec1bec](https://github.com/abougouffa/minemacs/commit/5ec1bec)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** simplify some `use-package` blocks - ([2b2606e](https://github.com/abougouffa/minemacs/commit/2b2606e)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** minor edit - ([3f81efc](https://github.com/abougouffa/minemacs/commit/3f81efc)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** merge `corfu` related customizations - ([31167a6](https://github.com/abougouffa/minemacs/commit/31167a6)) - [@abougouffa](https://github.com/abougouffa)
- **(embark)** simplify and cleanup duplicate hook - ([ef74d5e](https://github.com/abougouffa/minemacs/commit/ef74d5e)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** move all Evil related configs/packages to `me-evil` - ([5543a6c](https://github.com/abougouffa/minemacs/commit/5543a6c)) - [@abougouffa](https://github.com/abougouffa)
- **(keybinding)** move `general` to `me-evil` - ([65e7bc0](https://github.com/abougouffa/minemacs/commit/65e7bc0)) - [@abougouffa](https://github.com/abougouffa)
- **(vertico)** use only one `use-package` block - ([6d84749](https://github.com/abougouffa/minemacs/commit/6d84749)) - [@abougouffa](https://github.com/abougouffa)
- move `disable-theme` advice to `me-builtin` - ([72be5a8](https://github.com/abougouffa/minemacs/commit/72be5a8)) - [@abougouffa](https://github.com/abougouffa)
- move optional modules from `core` to `modules` - ([6216b10](https://github.com/abougouffa/minemacs/commit/6216b10)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(beardbolt)** use my fork until it get merged upstream - ([77b17d1](https://github.com/abougouffa/minemacs/commit/77b17d1)) - [@abougouffa](https://github.com/abougouffa)
- **(better-jumper)** move the Evil related remaps to `me-evil` - ([d32692c](https://github.com/abougouffa/minemacs/commit/d32692c)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** remap some keybindings for more useful commands - ([6b4262e](https://github.com/abougouffa/minemacs/commit/6b4262e)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** unset some annoying commands - ([f3459f2](https://github.com/abougouffa/minemacs/commit/f3459f2)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** bind ESC to `keyboard-escape-quit` when in Emacs mode - ([4b27a80](https://github.com/abougouffa/minemacs/commit/4b27a80)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** add keybindings (inspired by the official example) - ([7c6fdd7](https://github.com/abougouffa/minemacs/commit/7c6fdd7)) - [@abougouffa](https://github.com/abougouffa)
- **(display-line-numbers)** use absolute numbers instead of relative - ([5cb3fb1](https://github.com/abougouffa/minemacs/commit/5cb3fb1)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** use upstream repo (meow related changes merged) - ([63253d7](https://github.com/abougouffa/minemacs/commit/63253d7)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** use my fork until it gets merged - ([cf7e2a5](https://github.com/abougouffa/minemacs/commit/cf7e2a5)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** minor edit - ([4438997](https://github.com/abougouffa/minemacs/commit/4438997)) - [@abougouffa](https://github.com/abougouffa)
- **(elisp-mode)** better font lock for Elisp shorthands - ([c800d99](https://github.com/abougouffa/minemacs/commit/c800d99)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** remove unused bitmaps - ([998e97e](https://github.com/abougouffa/minemacs/commit/998e97e)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** add a command to toggle hiding all blocks - ([82a2016](https://github.com/abougouffa/minemacs/commit/82a2016)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** bind `hs-toggle-hiding` to `C-c f` - ([ef414c8](https://github.com/abougouffa/minemacs/commit/ef414c8)) - [@abougouffa](https://github.com/abougouffa)
- **(logview)** add a custom log submode for RDK - ([fc61864](https://github.com/abougouffa/minemacs/commit/fc61864)) - [@abougouffa](https://github.com/abougouffa)
- **(macrostep)** bind `macrostep-expand` to `C-c m` - ([3c83c3f](https://github.com/abougouffa/minemacs/commit/3c83c3f)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** bind `meow-kill-whole-line` to `S` - ([7e29bf5](https://github.com/abougouffa/minemacs/commit/7e29bf5)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** minor keybindings tweaks, trying to find my comfort! - ([0be36c7](https://github.com/abougouffa/minemacs/commit/0be36c7)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** tinker Meow commands - ([58e4b43](https://github.com/abougouffa/minemacs/commit/58e4b43)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** display a warning if Meow and Evil are both enabled - ([724d1f3](https://github.com/abougouffa/minemacs/commit/724d1f3)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** quit `corfu` completion when exiting insert state - ([7f6cf6a](https://github.com/abougouffa/minemacs/commit/7f6cf6a)) - [@abougouffa](https://github.com/abougouffa)
- **(meow)** bind more keys - ([1a96d77](https://github.com/abougouffa/minemacs/commit/1a96d77)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** replace obsolete function - ([ee639bd](https://github.com/abougouffa/minemacs/commit/ee639bd)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** move `evil-textobj-tree-sitter` to `me-evil` - ([2ca1f68](https://github.com/abougouffa/minemacs/commit/2ca1f68)) - [@abougouffa](https://github.com/abougouffa)
- **(project-x)** add `.projectile` and `.vscode` as project identifiers - ([5b70a55](https://github.com/abougouffa/minemacs/commit/5b70a55)) - [@abougouffa](https://github.com/abougouffa)
- **(undo-fu-session)** lazy load - ([dc4caa5](https://github.com/abougouffa/minemacs/commit/dc4caa5)) - [@abougouffa](https://github.com/abougouffa)
- **(windmove)** add a prefix for moving between windows - ([22ac29d](https://github.com/abougouffa/minemacs/commit/22ac29d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([eb36378](https://github.com/abougouffa/minemacs/commit/eb36378)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([4f2dd83](https://github.com/abougouffa/minemacs/commit/4f2dd83)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c2ac0c0](https://github.com/abougouffa/minemacs/commit/c2ac0c0)) - [@abougouffa](https://github.com/abougouffa)
- make use of `+mode-alist-add-ts-modes!` - ([bc94578](https://github.com/abougouffa/minemacs/commit/bc94578)) - [@abougouffa](https://github.com/abougouffa)
- more info on the message displayed after load - ([a98aea9](https://github.com/abougouffa/minemacs/commit/a98aea9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.8.2](https://github.com/abougouffa/minemacs/compare/9f70fd7..v6.8.2) - 2024-05-28
#### Documentation
- add `tmux` to the list of external dependencies - ([e53b232](https://github.com/abougouffa/minemacs/commit/e53b232)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(jq-mode)** add `+xq-interactively` for XML - ([297e31c](https://github.com/abougouffa/minemacs/commit/297e31c)) - [@abougouffa](https://github.com/abougouffa)
- **(restclient)** use my fork - ([7c5c378](https://github.com/abougouffa/minemacs/commit/7c5c378)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** add initial support for `tributary` (Confluence) - ([b03a091](https://github.com/abougouffa/minemacs/commit/b03a091)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(parinfer-rust)** simplify conditions, remove unnecessary (!) hacks - ([9f70fd7](https://github.com/abougouffa/minemacs/commit/9f70fd7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8b61854](https://github.com/abougouffa/minemacs/commit/8b61854)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.8.1](https://github.com/abougouffa/minemacs/compare/f73cea1..v6.8.1) - 2024-05-28
#### Bug Fixes
- **(tmux)** disable on Windows - ([98d1b9f](https://github.com/abougouffa/minemacs/commit/98d1b9f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(pet)** better activation condition - ([f73cea1](https://github.com/abougouffa/minemacs/commit/f73cea1)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2c8a584](https://github.com/abougouffa/minemacs/commit/2c8a584)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.8.0](https://github.com/abougouffa/minemacs/compare/c65e497..v6.8.0) - 2024-05-27
#### Bug Fixes
- **(eaf)** remove references to `minemacs-fonts` - ([c65e497](https://github.com/abougouffa/minemacs/commit/c65e497)) - [@abougouffa](https://github.com/abougouffa)
- **(valgrind)** better detection of the project root - ([57fca4b](https://github.com/abougouffa/minemacs/commit/57fca4b)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** update the modules list - ([ed97985](https://github.com/abougouffa/minemacs/commit/ed97985)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(jiralib2)** add `+jira-insert-ticket-link` - ([384fd38](https://github.com/abougouffa/minemacs/commit/384fd38)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add initial support for `org-jira` - ([222e9dd](https://github.com/abougouffa/minemacs/commit/222e9dd)) - [@abougouffa](https://github.com/abougouffa)
- **(services)** add a new module for services, move `jiralib2` and `org-jira` - ([8590218](https://github.com/abougouffa/minemacs/commit/8590218)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `tmux` - ([d6ba7e7](https://github.com/abougouffa/minemacs/commit/d6ba7e7)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `ob-restclient` - ([8d99771](https://github.com/abougouffa/minemacs/commit/8d99771)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `restclient-test` - ([7c20b9c](https://github.com/abougouffa/minemacs/commit/7c20b9c)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `restclient` - ([209bb6b](https://github.com/abougouffa/minemacs/commit/209bb6b)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add initial support for `golden-ratio` - ([013af49](https://github.com/abougouffa/minemacs/commit/013af49)) - [@abougouffa](https://github.com/abougouffa)
- move `jiralib2` from `me-vc` to `me-project` - ([3cebfc3](https://github.com/abougouffa/minemacs/commit/3cebfc3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(better-jumper)** use instead of `evil-jump-[forward/backward]` - ([c4f0a8c](https://github.com/abougouffa/minemacs/commit/c4f0a8c)) - [@abougouffa](https://github.com/abougouffa)
- **(enlight)** don't pop as initial buff when a file is opened via args - ([cd64605](https://github.com/abougouffa/minemacs/commit/cd64605)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-snipe)** minor edits - ([cbad2a9](https://github.com/abougouffa/minemacs/commit/cbad2a9)) - [@abougouffa](https://github.com/abougouffa)
- **(project-tab-groups)** use a better group naming function - ([010f58a](https://github.com/abougouffa/minemacs/commit/010f58a)) - [@abougouffa](https://github.com/abougouffa)
- minor tweaks related to profiling MinEmacs - ([4de550d](https://github.com/abougouffa/minemacs/commit/4de550d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.7.4](https://github.com/abougouffa/minemacs/compare/ffaa6c1..v6.7.4) - 2024-05-26
#### Tweaks
- **(cocogitto)** better stashing on dirty work directory - ([ffaa6c1](https://github.com/abougouffa/minemacs/commit/ffaa6c1)) - [@abougouffa](https://github.com/abougouffa)
- add group for the top level `minemacs` group - ([bb35d3f](https://github.com/abougouffa/minemacs/commit/bb35d3f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.7.3](https://github.com/abougouffa/minemacs/compare/7d3be45..v6.7.3) - 2024-05-26
#### Refactoring
- use `+package-disabled-p` when needed - ([4dbe652](https://github.com/abougouffa/minemacs/commit/4dbe652)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(doom-modeline)** hide indent information - ([7d3be45](https://github.com/abougouffa/minemacs/commit/7d3be45)) - [@abougouffa](https://github.com/abougouffa)
- prefer lazy loading for almost all packages - ([299e340](https://github.com/abougouffa/minemacs/commit/299e340)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.7.2](https://github.com/abougouffa/minemacs/compare/4f91a0a..v6.7.2) - 2024-05-26
#### Bug Fixes
- **(enlight)** temporary disable on Emacs 28 - ([09e6588](https://github.com/abougouffa/minemacs/commit/09e6588)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(doom-modeline)** minor tweaks - ([4f91a0a](https://github.com/abougouffa/minemacs/commit/4f91a0a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9cbe61f](https://github.com/abougouffa/minemacs/commit/9cbe61f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.7.1](https://github.com/abougouffa/minemacs/compare/41b7f6c..v6.7.1) - 2024-05-26
#### Features
- **(core-ui)** move to `light-dashboard` successor `enlight` [#168] - ([41b7f6c](https://github.com/abougouffa/minemacs/commit/41b7f6c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.7.0](https://github.com/abougouffa/minemacs/compare/11f3045..v6.7.0) - 2024-05-26
#### Bug Fixes
- **(project-tab-groups)** correctly set tab group naming function - ([977fbab](https://github.com/abougouffa/minemacs/commit/977fbab)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ai)** initial support for `elisa` - ([0fe89d2](https://github.com/abougouffa/minemacs/commit/0fe89d2)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** add benchmarking option using `benchmark-init` - ([7ab3307](https://github.com/abougouffa/minemacs/commit/7ab3307)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add support for `project-x` - ([6e1c164](https://github.com/abougouffa/minemacs/commit/6e1c164)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add support for `magit-file-icons` - ([efb7c4f](https://github.com/abougouffa/minemacs/commit/efb7c4f)) - [@abougouffa](https://github.com/abougouffa)
- **(workspaces)** add support for `project-tab-groups` - ([e8e9d50](https://github.com/abougouffa/minemacs/commit/e8e9d50)) - [@abougouffa](https://github.com/abougouffa)
- **(workspaces)** make `tabspaces` obsolete - ([313d2ff](https://github.com/abougouffa/minemacs/commit/313d2ff)) - [@abougouffa](https://github.com/abougouffa)
- better lazy loading! - ([f2271f6](https://github.com/abougouffa/minemacs/commit/f2271f6)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- make use of the builtin `locate-dominating-file` - ([9fcac8f](https://github.com/abougouffa/minemacs/commit/9fcac8f)) - [@abougouffa](https://github.com/abougouffa)
- refactor and make use of `cl-callf` when possible - ([1658153](https://github.com/abougouffa/minemacs/commit/1658153)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(eros)** minor edit - ([3cdda14](https://github.com/abougouffa/minemacs/commit/3cdda14)) - [@abougouffa](https://github.com/abougouffa)
- **(gc)** minor tweaks and cleanup - ([42f93be](https://github.com/abougouffa/minemacs/commit/42f93be)) - [@abougouffa](https://github.com/abougouffa)
- **(gc)** experimenting with `gc-cons-percentage` without `gcmh` - ([676a3b0](https://github.com/abougouffa/minemacs/commit/676a3b0)) - [@abougouffa](https://github.com/abougouffa)
- **(llm)** minor refactoring - ([dc4372f](https://github.com/abougouffa/minemacs/commit/dc4372f)) - [@abougouffa](https://github.com/abougouffa)
- **(octave)** use `octave-maybe-mode` for `.m` - ([c03e197](https://github.com/abougouffa/minemacs/commit/c03e197)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** minor cleanup - ([3ddb276](https://github.com/abougouffa/minemacs/commit/3ddb276)) - [@abougouffa](https://github.com/abougouffa)
- **(org-agenda)** stick to the defaults for some options - ([11f3045](https://github.com/abougouffa/minemacs/commit/11f3045)) - [@abougouffa](https://github.com/abougouffa)
- **(verb)** add keybindings - ([f3d782f](https://github.com/abougouffa/minemacs/commit/f3d782f)) - [@abougouffa](https://github.com/abougouffa)
- more lazy and deferred stuff - ([0c363e0](https://github.com/abougouffa/minemacs/commit/0c363e0)) - [@abougouffa](https://github.com/abougouffa)
- use `project-prompter` when relevant - ([4ab109a](https://github.com/abougouffa/minemacs/commit/4ab109a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d42ce53](https://github.com/abougouffa/minemacs/commit/d42ce53)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.6.0](https://github.com/abougouffa/minemacs/compare/178a740..v6.6.0) - 2024-05-23
#### Bug Fixes
- **(keybindings)** bind `auto-insert` to the insert menu - ([178a740](https://github.com/abougouffa/minemacs/commit/178a740)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(debug)** add support for `objdump-disassemble` - ([9887a12](https://github.com/abougouffa/minemacs/commit/9887a12)) - [@abougouffa](https://github.com/abougouffa)
- remove `me-binary` module and the `objdump` stuff - ([e3766b0](https://github.com/abougouffa/minemacs/commit/e3766b0)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(spdx)** add some insert keybindings - ([91e9ad2](https://github.com/abougouffa/minemacs/commit/91e9ad2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.5.0](https://github.com/abougouffa/minemacs/compare/2e3f35b..v6.5.0) - 2024-05-23
#### Features
- **(core-ui)** make `spacious-padding` obsolete - ([078f79d](https://github.com/abougouffa/minemacs/commit/078f79d)) - [@abougouffa](https://github.com/abougouffa)
- **(gtd)** initial support for `org-gtd` - ([9ce4266](https://github.com/abougouffa/minemacs/commit/9ce4266)) - [@abougouffa](https://github.com/abougouffa)
- **(gtd)** empty module for Getting Things Done workflow - ([5f4ea3f](https://github.com/abougouffa/minemacs/commit/5f4ea3f)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `add-node-modules-path` - ([d5f490e](https://github.com/abougouffa/minemacs/commit/d5f490e)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** replace unused `ack` with `rg` - ([d0f9d63](https://github.com/abougouffa/minemacs/commit/d0f9d63)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(use-package)** remove unnecessary `t` argument from `:demand` - ([1e91729](https://github.com/abougouffa/minemacs/commit/1e91729)) - [@abougouffa](https://github.com/abougouffa)
- add options file for `ctags` (to be used later) - ([5f8ef60](https://github.com/abougouffa/minemacs/commit/5f8ef60)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor edit - ([e5de394](https://github.com/abougouffa/minemacs/commit/e5de394)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(parinfer-rust)** remove temporary hack after being fixed upstream - ([8eda977](https://github.com/abougouffa/minemacs/commit/8eda977)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(c-mode)** use K&R style by default - ([d09be4b](https://github.com/abougouffa/minemacs/commit/d09be4b)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more ignored environment variables - ([21e13c8](https://github.com/abougouffa/minemacs/commit/21e13c8)) - [@abougouffa](https://github.com/abougouffa)
- **(crm)** indicate in the prompt about `completing-read-multiple` - ([02cd1a0](https://github.com/abougouffa/minemacs/commit/02cd1a0)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** take `vhdl-ts-mode` and `verilog-ts-mode` into account - ([2e3f35b](https://github.com/abougouffa/minemacs/commit/2e3f35b)) - [@abougouffa](https://github.com/abougouffa)
- **(minibuffer)** don't move cursor to the prompt region - ([786a089](https://github.com/abougouffa/minemacs/commit/786a089)) - [@abougouffa](https://github.com/abougouffa)
- **(org-modern)** stick to the defaults for lists and stars - ([adc38d8](https://github.com/abougouffa/minemacs/commit/adc38d8)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** hook only to the base mode `lisp-data-mode` - ([ac06034](https://github.com/abougouffa/minemacs/commit/ac06034)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** set `use-package` options after tweaking its settings - ([9cf959c](https://github.com/abougouffa/minemacs/commit/9cf959c)) - [@abougouffa](https://github.com/abougouffa)
- **(wgrep)** move from `me-completion` to `me-editor` - ([212c7a8](https://github.com/abougouffa/minemacs/commit/212c7a8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ce049d5](https://github.com/abougouffa/minemacs/commit/ce049d5)) - [@abougouffa](https://github.com/abougouffa)
- enable `compilation-shell-minor-mode` in terminals - ([fb0e773](https://github.com/abougouffa/minemacs/commit/fb0e773)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.4.1](https://github.com/abougouffa/minemacs/compare/e09a2c2..v6.4.1) - 2024-05-22
#### Bug Fixes
- **(flymake-guile)** use a mirror on GitHub (Framagit issues) - ([9a3786a](https://github.com/abougouffa/minemacs/commit/9a3786a)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core-ui)** enable the lightweight `light-dashboard` - ([f368331](https://github.com/abougouffa/minemacs/commit/f368331)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add support for `spdx` - ([46e78aa](https://github.com/abougouffa/minemacs/commit/46e78aa)) - [@abougouffa](https://github.com/abougouffa)
- **(email)** add initial support for `mu4e-crypto` - ([82a0716](https://github.com/abougouffa/minemacs/commit/82a0716)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor edits - ([d4349f2](https://github.com/abougouffa/minemacs/commit/d4349f2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** add an option to disable proxies via environment variable - ([e09a2c2](https://github.com/abougouffa/minemacs/commit/e09a2c2)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** move Elisp customization to a separate package - ([958462a](https://github.com/abougouffa/minemacs/commit/958462a)) - [@abougouffa](https://github.com/abougouffa)
- **(verb)** use the default keybinding - ([9c47e37](https://github.com/abougouffa/minemacs/commit/9c47e37)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.4.0](https://github.com/abougouffa/minemacs/compare/60d176a..v6.4.0) - 2024-05-21
#### Bug Fixes
- `defalias` (hence, `defun`) isn't guaranteed to return the name - ([5d7231b](https://github.com/abougouffa/minemacs/commit/5d7231b)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- update the list of modules - ([5b06b53](https://github.com/abougouffa/minemacs/commit/5b06b53)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(bootstrap)** add support for `satch` and `once` - ([d23861e](https://github.com/abougouffa/minemacs/commit/d23861e)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** restore the `+eglot-help-at-point` command - ([ba2e628](https://github.com/abougouffa/minemacs/commit/ba2e628)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** extra performance tweaks via `+eglot-optimization-mode` - ([9ebb7b5](https://github.com/abougouffa/minemacs/commit/9ebb7b5)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add initial support for `find-file-in-project` - ([9a215f3](https://github.com/abougouffa/minemacs/commit/9a215f3)) - [@abougouffa](https://github.com/abougouffa)
- **(tags)** add initial support for `ggtags - ([f5cbe58](https://github.com/abougouffa/minemacs/commit/f5cbe58)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult-notes)** use ripgrep only if available - ([1a3f3c6](https://github.com/abougouffa/minemacs/commit/1a3f3c6)) - [@abougouffa](https://github.com/abougouffa)
- **(dumb-jump)** prefer `xref` interface over obsolete commands - ([60d176a](https://github.com/abougouffa/minemacs/commit/60d176a)) - [@abougouffa](https://github.com/abougouffa)
- **(edebug)** decouple from `elisp-mode` and inhibit bindings - ([c7f996f](https://github.com/abougouffa/minemacs/commit/c7f996f)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** better performance by disabling logging to events buffer - ([82cfe06](https://github.com/abougouffa/minemacs/commit/82cfe06)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** make it believe that `projectile` is available to get `fzf-project` - ([78e7d24](https://github.com/abougouffa/minemacs/commit/78e7d24)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** move non-LSP source code tagging packages to `me-tags` - ([4bcaef3](https://github.com/abougouffa/minemacs/commit/4bcaef3)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7887c8c](https://github.com/abougouffa/minemacs/commit/7887c8c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.3.0](https://github.com/abougouffa/minemacs/compare/440936f..v6.3.0) - 2024-05-21
#### Bug Fixes
- **(core)** use-package `::trigger-commands` implementation - ([95de9a5](https://github.com/abougouffa/minemacs/commit/95de9a5)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** fix the provided feature name - ([fb8eb8f](https://github.com/abougouffa/minemacs/commit/fb8eb8f)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** use `:autoload` instead of `:functions` in `use-package` - ([72f7ce1](https://github.com/abougouffa/minemacs/commit/72f7ce1)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** disable auto usage of fast strategy on long buffers - ([55432e1](https://github.com/abougouffa/minemacs/commit/55432e1)) - [@abougouffa](https://github.com/abougouffa)
- **(pet)** hook `pet-mode` in Python - ([440936f](https://github.com/abougouffa/minemacs/commit/440936f)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** autoload `project-remember-projects-under` - ([90c0d95](https://github.com/abougouffa/minemacs/commit/90c0d95)) - [@abougouffa](https://github.com/abougouffa)
- **(transient)** auto load some commonly used macros - ([bd6921b](https://github.com/abougouffa/minemacs/commit/bd6921b)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** fix grammar recipe for XML - ([95c80b3](https://github.com/abougouffa/minemacs/commit/95c80b3)) - [@abougouffa](https://github.com/abougouffa)
- avoid problems on TTY only Emacs (via Termux) - ([98c4d23](https://github.com/abougouffa/minemacs/commit/98c4d23)) - [@abougouffa](https://github.com/abougouffa)
- better implementation of the first file hooks - ([5c606a0](https://github.com/abougouffa/minemacs/commit/5c606a0)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([3a54d3e](https://github.com/abougouffa/minemacs/commit/3a54d3e)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** don't add the banner to the documentation - ([f65d6bd](https://github.com/abougouffa/minemacs/commit/f65d6bd)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** regenerate the documentation - ([4b9d610](https://github.com/abougouffa/minemacs/commit/4b9d610)) - [@abougouffa](https://github.com/abougouffa)
- **(documentation)** regenerate documentation - ([258ca84](https://github.com/abougouffa/minemacs/commit/258ca84)) - [@abougouffa](https://github.com/abougouffa)
- add a comment for some TODOs - ([9e239c6](https://github.com/abougouffa/minemacs/commit/9e239c6)) - [@abougouffa](https://github.com/abougouffa)
- better comments - ([1e5ceb7](https://github.com/abougouffa/minemacs/commit/1e5ceb7)) - [@abougouffa](https://github.com/abougouffa)
- simpler banner in `init.el` - ([b97b2f8](https://github.com/abougouffa/minemacs/commit/b97b2f8)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(checkers)** make `flymake-quickdef` and it dependents obsoletes - ([522a351](https://github.com/abougouffa/minemacs/commit/522a351)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+advice-once!` - ([63ba2a5](https://github.com/abougouffa/minemacs/commit/63ba2a5)) - [@abougouffa](https://github.com/abougouffa)
- **(debug)** add support for `beardbolt` - ([7b025e4](https://github.com/abougouffa/minemacs/commit/7b025e4)) - [@abougouffa](https://github.com/abougouffa)
- **(debug)** add support for `rmsbolt` - ([276dd20](https://github.com/abougouffa/minemacs/commit/276dd20)) - [@abougouffa](https://github.com/abougouffa)
- **(dired)** add `+dired-here` - ([d91071d](https://github.com/abougouffa/minemacs/commit/d91071d)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add support for `zones` - ([caed009](https://github.com/abougouffa/minemacs/commit/caed009)) - [@abougouffa](https://github.com/abougouffa)
- **(embedded)** add support for `vhdl-ts-mode` - ([35eb8e3](https://github.com/abougouffa/minemacs/commit/35eb8e3)) - [@abougouffa](https://github.com/abougouffa)
- **(embedded)** add support for `verilog-ts-mode` - ([af14fd0](https://github.com/abougouffa/minemacs/commit/af14fd0)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add my fork of `neotree` which supports `nerd-icons` - ([43445d8](https://github.com/abougouffa/minemacs/commit/43445d8)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-collection)** add several backends - ([5471c98](https://github.com/abougouffa/minemacs/commit/5471c98)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** add initial Matlab integration - ([8e02bce](https://github.com/abougouffa/minemacs/commit/8e02bce)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `zig-mode` - ([b4c9f59](https://github.com/abougouffa/minemacs/commit/b4c9f59)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `llvm-ts-mode` - ([805251c](https://github.com/abougouffa/minemacs/commit/805251c)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add support for `projection-dape` - ([b6140c3](https://github.com/abougouffa/minemacs/commit/b6140c3)) - [@abougouffa](https://github.com/abougouffa)
- **(reformatter)** initial support for `reformatter` - ([8767b78](https://github.com/abougouffa/minemacs/commit/8767b78)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add grammar for Zig - ([31e62f2](https://github.com/abougouffa/minemacs/commit/31e62f2)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add a grammar for LLVM - ([07c5b50](https://github.com/abougouffa/minemacs/commit/07c5b50)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add support for `lacarte` - ([b044298](https://github.com/abougouffa/minemacs/commit/b044298)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** add `:trigger-commands` option - ([9e72e96](https://github.com/abougouffa/minemacs/commit/9e72e96)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- provide feature in `me-gdb` - ([7e85327](https://github.com/abougouffa/minemacs/commit/7e85327)) - [@abougouffa](https://github.com/abougouffa)
- minor edit - ([8494462](https://github.com/abougouffa/minemacs/commit/8494462)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(consult-eglot)** rewrite a condition - ([8502500](https://github.com/abougouffa/minemacs/commit/8502500)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unnecessary condition - ([9fc1774](https://github.com/abougouffa/minemacs/commit/9fc1774)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** better way of adding extra grammar recipes - ([ce0d766](https://github.com/abougouffa/minemacs/commit/ce0d766)) - [@abougouffa](https://github.com/abougouffa)
- move less used stuff from `me-lib` to `me-lib-extra` - ([2dc8d06](https://github.com/abougouffa/minemacs/commit/2dc8d06)) - [@abougouffa](https://github.com/abougouffa)
- make some unused macros obsolete - ([4b12fba](https://github.com/abougouffa/minemacs/commit/4b12fba)) - [@abougouffa](https://github.com/abougouffa)
- rearrange customization groups - ([728d965](https://github.com/abougouffa/minemacs/commit/728d965)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(dired)** don't reinvent the wheel (`dired-jump`) - ([1ab230d](https://github.com/abougouffa/minemacs/commit/1ab230d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(battery)** defer displaying battery status - ([a5b3446](https://github.com/abougouffa/minemacs/commit/a5b3446)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** make additional options custom instead of var - ([02f6694](https://github.com/abougouffa/minemacs/commit/02f6694)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** remove obsolete variable - ([98538a9](https://github.com/abougouffa/minemacs/commit/98538a9)) - [@abougouffa](https://github.com/abougouffa)
- **(code-cells)** prefer `ein` only if not disabled - ([74a9f2d](https://github.com/abougouffa/minemacs/commit/74a9f2d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edit - ([335a886](https://github.com/abougouffa/minemacs/commit/335a886)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better obsolescence message - ([37253c7](https://github.com/abougouffa/minemacs/commit/37253c7)) - [@abougouffa](https://github.com/abougouffa)
- **(core-ui)** make `dashboard` obsolete (it slows startup) - ([8b22c5f](https://github.com/abougouffa/minemacs/commit/8b22c5f)) - [@abougouffa](https://github.com/abougouffa)
- **(denote)** defer enabling `denote-rename-buffer-mode` - ([308fe71](https://github.com/abougouffa/minemacs/commit/308fe71)) - [@abougouffa](https://github.com/abougouffa)
- **(dired)** better defaults - ([a4ff541](https://github.com/abougouffa/minemacs/commit/a4ff541)) - [@abougouffa](https://github.com/abougouffa)
- **(dired-aux)** ask before creating destination dirs - ([2bf7d2f](https://github.com/abougouffa/minemacs/commit/2bf7d2f)) - [@abougouffa](https://github.com/abougouffa)
- **(dired-aux)** rename VC files with VC - ([db8a3eb](https://github.com/abougouffa/minemacs/commit/db8a3eb)) - [@abougouffa](https://github.com/abougouffa)
- **(dired-x)** better defaults - ([cd05541](https://github.com/abougouffa/minemacs/commit/cd05541)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-nasm)** don't enable by default - ([6829158](https://github.com/abougouffa/minemacs/commit/6829158)) - [@abougouffa](https://github.com/abougouffa)
- **(helpful)** remap `describe-*` to `helpful-*` equivalents - ([519da81](https://github.com/abougouffa/minemacs/commit/519da81)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** minor edits - ([7d62083](https://github.com/abougouffa/minemacs/commit/7d62083)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-todos)** toggle via `SPC m t` - ([5d4c52f](https://github.com/abougouffa/minemacs/commit/5d4c52f)) - [@abougouffa](https://github.com/abougouffa)
- **(matlab)** enable only when Matlab is installed on the system - ([634f531](https://github.com/abougouffa/minemacs/commit/634f531)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** load the `spell-fu` only if `jinx` isn't available - ([3a7edcf](https://github.com/abougouffa/minemacs/commit/3a7edcf)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete)** update `yasnippet` / `yasnippet-cape` settings - ([f807b36](https://github.com/abougouffa/minemacs/commit/f807b36)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** enable on more Lisp modes - ([2450032](https://github.com/abougouffa/minemacs/commit/2450032)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `treesitter-context` obsolete - ([f34ef7e](https://github.com/abougouffa/minemacs/commit/f34ef7e)) - [@abougouffa](https://github.com/abougouffa)
- **(transient)** don't load immediately - ([b338706](https://github.com/abougouffa/minemacs/commit/b338706)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** minor edit - ([08e6629](https://github.com/abougouffa/minemacs/commit/08e6629)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** don't add Nix, already added upstream - ([b2f9c1d](https://github.com/abougouffa/minemacs/commit/b2f9c1d)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `anzu` obsolete - ([5266669](https://github.com/abougouffa/minemacs/commit/5266669)) - [@abougouffa](https://github.com/abougouffa)
- **(vlf)** use ELPA instead of MELPA for an updated version - ([1b7ac7f](https://github.com/abougouffa/minemacs/commit/1b7ac7f)) - [@abougouffa](https://github.com/abougouffa)
- **(ztree)** use GNU ELPA mirror - ([e6f071b](https://github.com/abougouffa/minemacs/commit/e6f071b)) - [@abougouffa](https://github.com/abougouffa)
- faster startup by deferring more stuff - ([34f3323](https://github.com/abougouffa/minemacs/commit/34f3323)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4542ecb](https://github.com/abougouffa/minemacs/commit/4542ecb)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0a68ed8](https://github.com/abougouffa/minemacs/commit/0a68ed8)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([c8ec252](https://github.com/abougouffa/minemacs/commit/c8ec252)) - [@abougouffa](https://github.com/abougouffa)
- faster loading by deferring some stuff - ([d69d00d](https://github.com/abougouffa/minemacs/commit/d69d00d)) - [@abougouffa](https://github.com/abougouffa)
- enhance startup time by correctly deferring some packages - ([3fee224](https://github.com/abougouffa/minemacs/commit/3fee224)) - [@abougouffa](https://github.com/abougouffa)
- correctly defer loading some packages + small cleanup - ([dd455b2](https://github.com/abougouffa/minemacs/commit/dd455b2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6fa809e](https://github.com/abougouffa/minemacs/commit/6fa809e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.2.2](https://github.com/abougouffa/minemacs/compare/e03e2d1..v6.2.2) - 2024-05-16
#### Documentation
- **(external-tools)** regenerate - ([5b83f5a](https://github.com/abougouffa/minemacs/commit/5b83f5a)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update screenshot - ([77a92a5](https://github.com/abougouffa/minemacs/commit/77a92a5)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** add more ignores - ([4703c6e](https://github.com/abougouffa/minemacs/commit/4703c6e)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(multi-vterm)** move display buffer properties to `me-window` - ([596442a](https://github.com/abougouffa/minemacs/commit/596442a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- add SQLite to the external tools - ([e03e2d1](https://github.com/abougouffa/minemacs/commit/e03e2d1)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.2.1](https://github.com/abougouffa/minemacs/compare/a0b672b..v6.2.1) - 2024-05-16
#### Bug Fixes
- don't use `:ensure-system-package` (not available on Emacs 28) - ([a0b672b](https://github.com/abougouffa/minemacs/commit/a0b672b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.2.0](https://github.com/abougouffa/minemacs/compare/76cc8b6..v6.2.0) - 2024-05-16
#### Bug Fixes
- **(core)** fix implementation of `+with-proxies` and `+with-no-proxies` - ([2e5d123](https://github.com/abougouffa/minemacs/commit/2e5d123)) - [@abougouffa](https://github.com/abougouffa)
- use proxies in async update - ([a20a23e](https://github.com/abougouffa/minemacs/commit/a20a23e)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([76cc8b6](https://github.com/abougouffa/minemacs/commit/76cc8b6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(bootstrap)** add support for system packages dependencies - ([3494b59](https://github.com/abougouffa/minemacs/commit/3494b59)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `pet` (Python Executable Tracker) - ([3492b7f](https://github.com/abougouffa/minemacs/commit/3492b7f)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(core)** restore old proxy behavior (issues with deferred stuff) - ([51eaafb](https://github.com/abougouffa/minemacs/commit/51eaafb)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** revert to using global proxies - ([b288093](https://github.com/abougouffa/minemacs/commit/b288093)) - [@abougouffa](https://github.com/abougouffa)
- **(envrc)** make use of `:ensure-system-package` - ([cbbdc14](https://github.com/abougouffa/minemacs/commit/cbbdc14)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** obsolete `pyenv` and `pyvenv` (to be replaced with `pet`) - ([650b10c](https://github.com/abougouffa/minemacs/commit/650b10c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([47c8810](https://github.com/abougouffa/minemacs/commit/47c8810)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.1.1](https://github.com/abougouffa/minemacs/compare/ff44052..v6.1.1) - 2024-05-15
#### Tweaks
- **(natural-langs)** make `go-translate` obsolete - ([4a34b9a](https://github.com/abougouffa/minemacs/commit/4a34b9a)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([ff44052](https://github.com/abougouffa/minemacs/commit/ff44052)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.1.0](https://github.com/abougouffa/minemacs/compare/87e6c5f..v6.1.0) - 2024-05-15
#### Bug Fixes
- **(core)** use `+with-proxies` when updating - ([0452e5c](https://github.com/abougouffa/minemacs/commit/0452e5c)) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** do Evil stuff after loading Evil - ([b5f55ab](https://github.com/abougouffa/minemacs/commit/b5f55ab)) - [@abougouffa](https://github.com/abougouffa)
- apply Evil-specific tweaks only when Evil is loaded - ([96d917d](https://github.com/abougouffa/minemacs/commit/96d917d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** better implementation of proxies - ([401c43b](https://github.com/abougouffa/minemacs/commit/401c43b)) - [@abougouffa](https://github.com/abougouffa)
- **(data)** add `jq-mode` with `yq` integration for YAML - ([f678163](https://github.com/abougouffa/minemacs/commit/f678163)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for impostman - ([20bf7f2](https://github.com/abougouffa/minemacs/commit/20bf7f2)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for verb.el - ([d990039](https://github.com/abougouffa/minemacs/commit/d990039)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** change the proxy handling mechanism (don't enable globally) - ([67bbfde](https://github.com/abougouffa/minemacs/commit/67bbfde)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** regenerate loaddefs - ([87e6c5f](https://github.com/abougouffa/minemacs/commit/87e6c5f)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `osm` obsolete - ([da908cb](https://github.com/abougouffa/minemacs/commit/da908cb)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1801878](https://github.com/abougouffa/minemacs/commit/1801878)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.7](https://github.com/abougouffa/minemacs/compare/26fae16..v6.0.7) - 2024-05-11
#### Bug Fixes
- bootstrapping error when Emacs lack some builtin features - ([26fae16](https://github.com/abougouffa/minemacs/commit/26fae16)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([1198fcc](https://github.com/abougouffa/minemacs/commit/1198fcc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.6](https://github.com/abougouffa/minemacs/compare/6331379..v6.0.6) - 2024-05-03
#### Bug Fixes
- **(transient)** fix void symbol issue on Emacs 30 - ([624cc7e](https://github.com/abougouffa/minemacs/commit/624cc7e)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(documentation)** regenerate the documentation - ([6331379](https://github.com/abougouffa/minemacs/commit/6331379)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(org)** remove unneeded `:after` blocks - ([b540f01](https://github.com/abougouffa/minemacs/commit/b540f01)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** register conditionally disabled packages - ([87e8b1a](https://github.com/abougouffa/minemacs/commit/87e8b1a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8a7fb31](https://github.com/abougouffa/minemacs/commit/8a7fb31)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.5](https://github.com/abougouffa/minemacs/compare/b88214b..v6.0.5) - 2024-05-01
#### Revert
- **(early-init)** minor simplification (not working on Emacs 28.2) - ([b88214b](https://github.com/abougouffa/minemacs/commit/b88214b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.4](https://github.com/abougouffa/minemacs/compare/99fb7b1..v6.0.4) - 2024-05-01
#### Documentation
- **(init)** update documentation - ([9450101](https://github.com/abougouffa/minemacs/commit/9450101)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(early-init)** minor simplification - ([d5e9365](https://github.com/abougouffa/minemacs/commit/d5e9365)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(early-init)** remove duplicate parameters set in `spacious-padding` - ([99fb7b1](https://github.com/abougouffa/minemacs/commit/99fb7b1)) - [@abougouffa](https://github.com/abougouffa)
- **(gerrit)** remove `gerrit-section`, uses obsolete functionalities - ([493a74e](https://github.com/abougouffa/minemacs/commit/493a74e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.3](https://github.com/abougouffa/minemacs/compare/557f0a7..v6.0.3) - 2024-04-26
#### Bug Fixes
- **(auctex)** revert to the prvious revision (regression on Windows) - ([557f0a7](https://github.com/abougouffa/minemacs/commit/557f0a7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(auctex)** pin to an older version due to an issue with Windows - ([747f779](https://github.com/abougouffa/minemacs/commit/747f779)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d51e6c7](https://github.com/abougouffa/minemacs/commit/d51e6c7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.2](https://github.com/abougouffa/minemacs/compare/1fa5265..v6.0.2) - 2024-04-25
#### Tweaks
- **(dirvish)** don't show Git status - ([60c24a1](https://github.com/abougouffa/minemacs/commit/60c24a1)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** don't show Git messages - ([1fa5265](https://github.com/abougouffa/minemacs/commit/1fa5265)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3e182d8](https://github.com/abougouffa/minemacs/commit/3e182d8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.1](https://github.com/abougouffa/minemacs/compare/eb99c88..v6.0.1) - 2024-04-22
#### Documentation
- **(documentation)** regenerate the documentation - ([eb99c88](https://github.com/abougouffa/minemacs/commit/eb99c88)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([67bade8](https://github.com/abougouffa/minemacs/commit/67bade8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v6.0.0](https://github.com/abougouffa/minemacs/compare/6c80599..v6.0.0) - 2024-04-15
#### Tweaks
- **(skel)** update modules list - ([4b9a0c4](https://github.com/abougouffa/minemacs/commit/4b9a0c4)) - [@abougouffa](https://github.com/abougouffa)
- bump `tabspaces` version - ([c43f0e7](https://github.com/abougouffa/minemacs/commit/c43f0e7)) - [@abougouffa](https://github.com/abougouffa)
- make `lsp` + `dap` obsolete in favor of `eglot` + `dape` - ([6c80599](https://github.com/abougouffa/minemacs/commit/6c80599)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.2.0](https://github.com/abougouffa/minemacs/compare/3b3ac48..v5.2.0) - 2024-04-15
#### Documentation
- regenerate the documentation - ([ee6866c](https://github.com/abougouffa/minemacs/commit/ee6866c)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** don't run tests on Emacs 28.1 & 29.1 - ([92a2467](https://github.com/abougouffa/minemacs/commit/92a2467)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** cleanup extra stuff to avoid problems - ([f08045e](https://github.com/abougouffa/minemacs/commit/f08045e)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(core)** remove an old fix intended for Emacs 29.1 - ([233b4ed](https://github.com/abougouffa/minemacs/commit/233b4ed)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(biblio)** make `zotxt` obsolete, never really used! - ([cd1ae59](https://github.com/abougouffa/minemacs/commit/cd1ae59)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** minor performance tweaks - ([3b3ac48](https://github.com/abougouffa/minemacs/commit/3b3ac48)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** don't auto cleanup MinEmacs' directory - ([58cbde9](https://github.com/abougouffa/minemacs/commit/58cbde9)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unused `+sensitive-data-mode` - ([592b335](https://github.com/abougouffa/minemacs/commit/592b335)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unneeded `+package-download-from-urls` - ([0e0c070](https://github.com/abougouffa/minemacs/commit/0e0c070)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** bind `dirvish-side` - ([8891bf3](https://github.com/abougouffa/minemacs/commit/8891bf3)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-lisp)** make `elisp-demos` obsolete - ([dcd02af](https://github.com/abougouffa/minemacs/commit/dcd02af)) - [@abougouffa](https://github.com/abougouffa)
- **(envrc)** prevent an annoying error when `direnv` isn't installed - ([1250c6b](https://github.com/abougouffa/minemacs/commit/1250c6b)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** make `treemacs` obsolete - ([1b9b00c](https://github.com/abougouffa/minemacs/commit/1b9b00c)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** remove unused `sr-speedbar` - ([401d4bc](https://github.com/abougouffa/minemacs/commit/401d4bc)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add a LaTeX class for MPDI template - ([8dd590d](https://github.com/abougouffa/minemacs/commit/8dd590d)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `nix-update` & `guix` obsolete - ([6401380](https://github.com/abougouffa/minemacs/commit/6401380)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make `bitwarden` obsolete - ([9d35669](https://github.com/abougouffa/minemacs/commit/9d35669)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** make `code-review` obsolete - ([868e99f](https://github.com/abougouffa/minemacs/commit/868e99f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([77e928a](https://github.com/abougouffa/minemacs/commit/77e928a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.1.3](https://github.com/abougouffa/minemacs/compare/bc6b6de..v5.1.3) - 2024-04-04
#### Bug Fixes
- **(protobuf-ts-mode)** use a working repo - ([bc6b6de](https://github.com/abougouffa/minemacs/commit/bc6b6de)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** add Emacs 29.3 to the CI matrix - ([3caff17](https://github.com/abougouffa/minemacs/commit/3caff17)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(vc)** remove unneeded `with-eval-after-load` - ([937d19d](https://github.com/abougouffa/minemacs/commit/937d19d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(selection-highlight)** enable after startup - ([8636072](https://github.com/abougouffa/minemacs/commit/8636072)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add ts modes to `auto-mode-alist` - ([d6a0227](https://github.com/abougouffa/minemacs/commit/d6a0227)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([035dbc6](https://github.com/abougouffa/minemacs/commit/035dbc6)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([34d5edd](https://github.com/abougouffa/minemacs/commit/34d5edd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.1.2](https://github.com/abougouffa/minemacs/compare/v5.1.1..v5.1.2) - 2024-03-13
#### Bug Fixes
- **(combobulate)** temporary disable on Windows - ([9068009](https://github.com/abougouffa/minemacs/commit/9068009)) - [@abougouffa](https://github.com/abougouffa)
- **(combobulate)** disable the problematic `html-ts-mode` submodule - ([40994e9](https://github.com/abougouffa/minemacs/commit/40994e9)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** hide the vertical bar correctly - ([c784e12](https://github.com/abougouffa/minemacs/commit/c784e12)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** add support for searching the current project via `project.el` - ([273f0db](https://github.com/abougouffa/minemacs/commit/273f0db)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** fix FAQ link - ([551c9b2](https://github.com/abougouffa/minemacs/commit/551c9b2)) - Ezequiel Birman
- **(selection-highlight)** delay enabling it by 2.0 to avoid issues - ([d135281](https://github.com/abougouffa/minemacs/commit/d135281)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** document `MINEMACS_LOAD_ALL_MODULES` - ([74331b0](https://github.com/abougouffa/minemacs/commit/74331b0)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ui)** add `logos` - ([9095fcc](https://github.com/abougouffa/minemacs/commit/9095fcc)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** initial support for `spacious-padding` - ([ad8a44d](https://github.com/abougouffa/minemacs/commit/ad8a44d)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** make `solaire-mode` obsolete - ([b5c958f](https://github.com/abougouffa/minemacs/commit/b5c958f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** remove scratch project from `project-prefix-map` - ([32fbfa8](https://github.com/abougouffa/minemacs/commit/32fbfa8)) - [@abougouffa](https://github.com/abougouffa)
- **(denote)** remove obsolete configs - ([a912ab8](https://github.com/abougouffa/minemacs/commit/a912ab8)) - [@abougouffa](https://github.com/abougouffa)
- **(fzf)** bind `find-project` to `SPC /` (analogue to `SPC :`) - ([fdb62bf](https://github.com/abougouffa/minemacs/commit/fdb62bf)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-vterm)** remap `multi-vterm-project` to `project-shell` - ([b00936a](https://github.com/abougouffa/minemacs/commit/b00936a)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** declare `treesit` as pseudo package when TS is available - ([97f679c](https://github.com/abougouffa/minemacs/commit/97f679c)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto+html-ts-mode)** pin HTML grammar to v0.20.1 - ([fd4994d](https://github.com/abougouffa/minemacs/commit/fd4994d)) - [@abougouffa](https://github.com/abougouffa)
- **(treesitter-context)** suppress `treesitter-context-fold-mode` - ([6905f23](https://github.com/abougouffa/minemacs/commit/6905f23)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.1.1](https://github.com/abougouffa/minemacs/compare/v5.1.0..v5.1.1) - 2024-03-03
#### Bug Fixes
- **(denote)** remove obsolete - ([553097a](https://github.com/abougouffa/minemacs/commit/553097a)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** add `cc-isearch-menu` - ([3be4ef2](https://github.com/abougouffa/minemacs/commit/3be4ef2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** use `treesitter-context-fold` for code folding - ([caabcf8](https://github.com/abougouffa/minemacs/commit/caabcf8)) - [@abougouffa](https://github.com/abougouffa)
- make the code folding spaghetti code obsolete - ([aa8dbb2](https://github.com/abougouffa/minemacs/commit/aa8dbb2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(rust-mode)** enable deriving from `treesit` when available - ([34a0cae](https://github.com/abougouffa/minemacs/commit/34a0cae)) - [@abougouffa](https://github.com/abougouffa)
- **(treesitter-context)** remove old hack (merged upstream) - ([a0c8b3d](https://github.com/abougouffa/minemacs/commit/a0c8b3d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([7bbcb13](https://github.com/abougouffa/minemacs/commit/7bbcb13)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.1.0](https://github.com/abougouffa/minemacs/compare/v5.0.1..v5.1.0) - 2024-02-27
#### Bug Fixes
- **(eldoc-box)** do not enable in terminal mode - ([10f660c](https://github.com/abougouffa/minemacs/commit/10f660c)) - [@abougouffa](https://github.com/abougouffa)
- **(pdfgrep)** autoload commands + require `pdf-isearch` - ([70ce45c](https://github.com/abougouffa/minemacs/commit/70ce45c)) - [@abougouffa](https://github.com/abougouffa)
- **(treesitter-context)** correctly set colors variables - ([9fc6b46](https://github.com/abougouffa/minemacs/commit/9fc6b46)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(external-tools)** regenerate the list - ([eab6060](https://github.com/abougouffa/minemacs/commit/eab6060)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** regenerate the list - ([a394f8e](https://github.com/abougouffa/minemacs/commit/a394f8e)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(tools)** add `fzf` support - ([fcc6358](https://github.com/abougouffa/minemacs/commit/fcc6358)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** setup tooltip font in `+setup-fonts` - ([d4b608d](https://github.com/abougouffa/minemacs/commit/d4b608d)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** add `fzf` - ([d3d7d7c](https://github.com/abougouffa/minemacs/commit/d3d7d7c)) - [@abougouffa](https://github.com/abougouffa)
- **(nix-update)** install despite `nix` availability - ([1454e75](https://github.com/abougouffa/minemacs/commit/1454e75)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f69310b](https://github.com/abougouffa/minemacs/commit/f69310b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.0.1](https://github.com/abougouffa/minemacs/compare/v5.0.0..v5.0.1) - 2024-02-25
#### Bug Fixes
- **(email)** temporary disable `org-msg` until fixed for `mu` 1.12.0 - ([e317cfa](https://github.com/abougouffa/minemacs/commit/e317cfa)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** make msgs of `minemacs-enable-proxy` logs instead of infos - ([68a821b](https://github.com/abougouffa/minemacs/commit/68a821b)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** use a less intrusive face for threads - ([52d8768](https://github.com/abougouffa/minemacs/commit/52d8768)) - [@abougouffa](https://github.com/abougouffa)
- **(sr-speedbar)** disable icons - ([41f0f31](https://github.com/abougouffa/minemacs/commit/41f0f31)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v5.0.0](https://github.com/abougouffa/minemacs/compare/v4.16.4..v5.0.0) - 2024-02-23
#### Bug Fixes
- **(compile-multi)** must be installed before `projection` - ([001428d](https://github.com/abougouffa/minemacs/commit/001428d)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** do not redefine the segments - ([76603aa](https://github.com/abougouffa/minemacs/commit/76603aa)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** run bump and upgrade asynchronously - ([445ba4b](https://github.com/abougouffa/minemacs/commit/445ba4b)) - [@abougouffa](https://github.com/abougouffa)
- **(docs)** add `pdfgrep` - ([12872d2](https://github.com/abougouffa/minemacs/commit/12872d2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `makefile-executor` obsolete - ([f42dc43](https://github.com/abougouffa/minemacs/commit/f42dc43)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add `projection` for better out-of-the-box experience - ([35855b2](https://github.com/abougouffa/minemacs/commit/35855b2)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** make `project-cmake` obsolete - ([c3630f7](https://github.com/abougouffa/minemacs/commit/c3630f7)) - [@abougouffa](https://github.com/abougouffa)
- replace `ibuffer-project` with `projection-ibuffer` - ([baaa638](https://github.com/abougouffa/minemacs/commit/baaa638)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(prog)** remove the unused `posframe-plus` project - ([bb8e43e](https://github.com/abougouffa/minemacs/commit/bb8e43e)) - [@abougouffa](https://github.com/abougouffa)
- remove obsolete commands - ([79ec179](https://github.com/abougouffa/minemacs/commit/79ec179)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([393ce5b](https://github.com/abougouffa/minemacs/commit/393ce5b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([420c43e](https://github.com/abougouffa/minemacs/commit/420c43e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.16.4](https://github.com/abougouffa/minemacs/compare/v4.16.3..v4.16.4) - 2024-02-20
#### Features
- **(prog)** add an experimental support for Clink - ([0db4963](https://github.com/abougouffa/minemacs/commit/0db4963)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(clink)** auto enable when available - ([cc19a02](https://github.com/abougouffa/minemacs/commit/cc19a02)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** bump versions of `cpptools` and `code-debug` - ([39c2465](https://github.com/abougouffa/minemacs/commit/39c2465)) - [@abougouffa](https://github.com/abougouffa)
- **(gdb)** minor edits in the GDB integration - ([32f1afc](https://github.com/abougouffa/minemacs/commit/32f1afc)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([bdc8039](https://github.com/abougouffa/minemacs/commit/bdc8039)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([06f7d26](https://github.com/abougouffa/minemacs/commit/06f7d26)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.16.3](https://github.com/abougouffa/minemacs/compare/v4.16.2..v4.16.3) - 2024-02-16
#### Bug Fixes
- **(backup)** remove dead code and avoid errors when viewing backups - ([d87b73d](https://github.com/abougouffa/minemacs/commit/d87b73d)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(real-backup)** make `real-backup` as a separate package - ([f892016](https://github.com/abougouffa/minemacs/commit/f892016)) - [@abougouffa](https://github.com/abougouffa)
- **(real-backup)** bump package, new version with new features - ([bf02dc2](https://github.com/abougouffa/minemacs/commit/bf02dc2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.16.2](https://github.com/abougouffa/minemacs/compare/v4.16.1..v4.16.2) - 2024-02-15
#### Tweaks
- **(backup)** rename buffer when viewing a backup file - ([4a8568a](https://github.com/abougouffa/minemacs/commit/4a8568a)) - [@abougouffa](https://github.com/abougouffa)
- **(backup)** autoload - ([151b527](https://github.com/abougouffa/minemacs/commit/151b527)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8f24e85](https://github.com/abougouffa/minemacs/commit/8f24e85)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([a14238a](https://github.com/abougouffa/minemacs/commit/a14238a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.16.1](https://github.com/abougouffa/minemacs/compare/v4.16.0..v4.16.1) - 2024-02-15
#### Bug Fixes
- **(treesit-auto)** do not create parsers for non-installed grammars - ([a5be915](https://github.com/abougouffa/minemacs/commit/a5be915)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(backup)** bump version, update docs - ([8ec19bc](https://github.com/abougouffa/minemacs/commit/8ec19bc)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(backup)** signal an error if in non-visiting buffer - ([a7c845c](https://github.com/abougouffa/minemacs/commit/a7c845c)) - [@abougouffa](https://github.com/abougouffa)
- **(backup)** more tweaks and features for `backup-each-save` - ([b5a6ffb](https://github.com/abougouffa/minemacs/commit/b5a6ffb)) - [@abougouffa](https://github.com/abougouffa)
- **(backup-each-save)** add backup cleanup support - ([65656f4](https://github.com/abougouffa/minemacs/commit/65656f4)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.16.0](https://github.com/abougouffa/minemacs/compare/v4.15.0..v4.16.0) - 2024-02-14
#### Tweaks
- **(treesit-auto)** enable treesit parsers even in non-treesit modes - ([8af4ad8](https://github.com/abougouffa/minemacs/commit/8af4ad8)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** install `elisp` grammar - ([2422c85](https://github.com/abougouffa/minemacs/commit/2422c85)) - [@abougouffa](https://github.com/abougouffa)
- **(ts-movement)** better detection of treesit enabled modes - ([09b365d](https://github.com/abougouffa/minemacs/commit/09b365d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.15.0](https://github.com/abougouffa/minemacs/compare/v4.14.0..v4.15.0) - 2024-02-14
#### Bug Fixes
- **(ecryptfs)** `epa` needed if `ecryptfs-mount-private` is invoked early - ([d4a52d1](https://github.com/abougouffa/minemacs/commit/d4a52d1)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+sensitive-data-mode` - ([fec482a](https://github.com/abougouffa/minemacs/commit/fec482a)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `devdocs` - ([94c0654](https://github.com/abougouffa/minemacs/commit/94c0654)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add Protocol Buffers support - ([12e8494](https://github.com/abougouffa/minemacs/commit/12e8494)) - [@abougouffa](https://github.com/abougouffa)
- **(rfc-mode)** add a mode to download and display RFCs - ([707d80c](https://github.com/abougouffa/minemacs/commit/707d80c)) - [@abougouffa](https://github.com/abougouffa)
- integrate a tweaked version of `backup-each-save` - ([bd0a364](https://github.com/abougouffa/minemacs/commit/bd0a364)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(dockerfile)** add more tools (WIP) - ([e78a7a5](https://github.com/abougouffa/minemacs/commit/e78a7a5)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** fix the `locked` rule - ([23c1a74](https://github.com/abougouffa/minemacs/commit/23c1a74)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** update all packages on `make update` - ([c583a41](https://github.com/abougouffa/minemacs/commit/c583a41)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(citre)** load after the first C/C++ file - ([ebc4431](https://github.com/abougouffa/minemacs/commit/ebc4431)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** ignore `.repo` when generating files list - ([7cc4608](https://github.com/abougouffa/minemacs/commit/7cc4608)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** add a command to generate `gtags.files` - ([ed5bbee](https://github.com/abougouffa/minemacs/commit/ed5bbee)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** decrease default font size - ([a8ce083](https://github.com/abougouffa/minemacs/commit/a8ce083)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** no need for version check, fixed upstream - ([a2e2d82](https://github.com/abougouffa/minemacs/commit/a2e2d82)) - [@abougouffa](https://github.com/abougouffa)
- **(magit-todos)** do not enable (too slow on big code bases) - ([e845bcb](https://github.com/abougouffa/minemacs/commit/e845bcb)) - [@abougouffa](https://github.com/abougouffa)
- **(vertico)** enable `vertico-mouse-mode` - ([48b10dd](https://github.com/abougouffa/minemacs/commit/48b10dd)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** change `frame-title-format` - ([0ea96fb](https://github.com/abougouffa/minemacs/commit/0ea96fb)) - [@abougouffa](https://github.com/abougouffa)
- update packages versions - ([b6963a8](https://github.com/abougouffa/minemacs/commit/b6963a8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9738ac4](https://github.com/abougouffa/minemacs/commit/9738ac4)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ee40277](https://github.com/abougouffa/minemacs/commit/ee40277)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.14.0](https://github.com/abougouffa/minemacs/compare/v4.13.3..v4.14.0) - 2024-02-08
#### Bug Fixes
- **(dape)** disable on Emacs 28 (requires new `jsonrpc`) - ([a2c8f98](https://github.com/abougouffa/minemacs/commit/a2c8f98)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([e78ef9e](https://github.com/abougouffa/minemacs/commit/e78ef9e)) - [@abougouffa](https://github.com/abougouffa)
- regenerate documentation - ([b1d3779](https://github.com/abougouffa/minemacs/commit/b1d3779)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add proxy setup - ([06fc5ec](https://github.com/abougouffa/minemacs/commit/06fc5ec)) - [@abougouffa](https://github.com/abougouffa)
- **(embedded)** add `+serial-run-command-on-host` - ([c200b53](https://github.com/abougouffa/minemacs/commit/c200b53)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `treesitter-context` - ([a22fd0b](https://github.com/abougouffa/minemacs/commit/a22fd0b)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `breadcrumb` - ([5788292](https://github.com/abougouffa/minemacs/commit/5788292)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** automatically enable proxies when set - ([bf1f0be](https://github.com/abougouffa/minemacs/commit/bf1f0be)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** move `+serial-*` to `me-lib`, several enhancements - ([20a41ee](https://github.com/abougouffa/minemacs/commit/20a41ee)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-quickdef)** include docstring in the generated function - ([e9db03d](https://github.com/abougouffa/minemacs/commit/e9db03d)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-quickdef)** better args handling in `flymake-clang-tidy` - ([ff6c407](https://github.com/abougouffa/minemacs/commit/ff6c407)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([664a3b2](https://github.com/abougouffa/minemacs/commit/664a3b2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.13.3](https://github.com/abougouffa/minemacs/compare/v4.13.2..v4.13.3) - 2024-02-04
#### Bug Fixes
- **(core)** buggy `+package-disabled-p` - ([1da5b30](https://github.com/abougouffa/minemacs/commit/1da5b30)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(external-tools)** regenerate - ([5db1bb2](https://github.com/abougouffa/minemacs/commit/5db1bb2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult-eglot)** make use of `+package-disabled-p` - ([d2c45d1](https://github.com/abougouffa/minemacs/commit/d2c45d1)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add an option to check for modules in `+package-disabled-p` - ([c348bbb](https://github.com/abougouffa/minemacs/commit/c348bbb)) - [@abougouffa](https://github.com/abougouffa)
- **(ellama)** auto load installed models - ([0a6f71d](https://github.com/abougouffa/minemacs/commit/0a6f71d)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** make use of `+package-disabled-p` to control installing evil packages - ([3d69084](https://github.com/abougouffa/minemacs/commit/3d69084)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** update the list - ([eef78cc](https://github.com/abougouffa/minemacs/commit/eef78cc)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-quickdef)** more accurate regexp in `flymake-clang-tidy` - ([c674dd2](https://github.com/abougouffa/minemacs/commit/c674dd2)) - [@abougouffa](https://github.com/abougouffa)
- **(xcscope)** disable on Windows - ([ee7db68](https://github.com/abougouffa/minemacs/commit/ee7db68)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.13.2](https://github.com/abougouffa/minemacs/compare/v4.13.1..v4.13.2) - 2024-01-31
#### Documentation
- **(external-tools)** regenerate the list - ([f18f495](https://github.com/abougouffa/minemacs/commit/f18f495)) - [@abougouffa](https://github.com/abougouffa)
- regenerate documentation - ([ee5fe28](https://github.com/abougouffa/minemacs/commit/ee5fe28)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(doom-themes)** defer loading the Org extension - ([4e782c8](https://github.com/abougouffa/minemacs/commit/4e782c8)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([647b0f2](https://github.com/abougouffa/minemacs/commit/647b0f2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.13.1](https://github.com/abougouffa/minemacs/compare/v4.13.0..v4.13.1) - 2024-01-31
#### Miscellaneous Chores
- **(ci)** add Emacs 29.2 to the CI matrix - ([ec7b395](https://github.com/abougouffa/minemacs/commit/ec7b395)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(external-tools)** prefer GitHub links when available, update the list - ([68126dd](https://github.com/abougouffa/minemacs/commit/68126dd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.13.0](https://github.com/abougouffa/minemacs/compare/v4.12.1..v4.13.0) - 2024-01-31
#### Bug Fixes
- **(flymake)** fix finding the `.clang-tidy` file - ([fd128f6](https://github.com/abougouffa/minemacs/commit/fd128f6)) - [@abougouffa](https://github.com/abougouffa)
- **(ts-movement)** install only when Emacs has tree-sitter support - ([90ef5da](https://github.com/abougouffa/minemacs/commit/90ef5da)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(external-tools)** update the list of external tools - ([7687639](https://github.com/abougouffa/minemacs/commit/7687639)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** regenerate the list - ([4f7e4c1](https://github.com/abougouffa/minemacs/commit/4f7e4c1)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add an option to load all modules - ([4cbdf1e](https://github.com/abougouffa/minemacs/commit/4cbdf1e)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+directory-root-containing-file` - ([3ee9727](https://github.com/abougouffa/minemacs/commit/3ee9727)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add a `flymake` backend for `clang-tidy` - ([f652ab1](https://github.com/abougouffa/minemacs/commit/f652ab1)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `ts-movement` - ([a0e5a3d](https://github.com/abougouffa/minemacs/commit/a0e5a3d)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** make use of the new `MINEMACS_LOAD_ALL_MODULES` envvar - ([2e24998](https://github.com/abougouffa/minemacs/commit/2e24998)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** add an option for `+xmllint-indent` to set indentation - ([d8af747](https://github.com/abougouffa/minemacs/commit/d8af747)) - [@abougouffa](https://github.com/abougouffa)
- **(citre)** make use of `+directory-root-containing-file` - ([d411a14](https://github.com/abougouffa/minemacs/commit/d411a14)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** add `yq` - ([dc0da27](https://github.com/abougouffa/minemacs/commit/dc0da27)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** install XML grammar - ([a853a72](https://github.com/abougouffa/minemacs/commit/a853a72)) - [@abougouffa](https://github.com/abougouffa)
- **(ts-movement)** move `+ts-movement-maybe` to `:init` - ([554bee5](https://github.com/abougouffa/minemacs/commit/554bee5)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0700633](https://github.com/abougouffa/minemacs/commit/0700633)) - [@abougouffa](https://github.com/abougouffa)
- declare more external tools - ([ad6de25](https://github.com/abougouffa/minemacs/commit/ad6de25)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.12.1](https://github.com/abougouffa/minemacs/compare/v4.12.0..v4.12.1) - 2024-01-28
#### Documentation
- **(external-tools)** regenerate the list - ([13dc390](https://github.com/abougouffa/minemacs/commit/13dc390)) - [@abougouffa](https://github.com/abougouffa)
- **(external-tools)** update the list - ([f8a8262](https://github.com/abougouffa/minemacs/commit/f8a8262)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** declare external dependencies - ([82088d2](https://github.com/abougouffa/minemacs/commit/82088d2)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example in `init-tweaks.el` - ([2beb3d7](https://github.com/abougouffa/minemacs/commit/2beb3d7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.12.0](https://github.com/abougouffa/minemacs/compare/v4.11.0..v4.12.0) - 2024-01-28
#### Bug Fixes
- **(dirvish)** ignore previewing `*.po` files - ([79dabfb](https://github.com/abougouffa/minemacs/commit/79dabfb)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(external-tools)** update the list - ([45405e2](https://github.com/abougouffa/minemacs/commit/45405e2)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update links - ([a6e5785](https://github.com/abougouffa/minemacs/commit/a6e5785)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** show pictures - ([27e7e7e](https://github.com/abougouffa/minemacs/commit/27e7e7e)) - [@abougouffa](https://github.com/abougouffa)
- regenerate documentation - ([f4211e3](https://github.com/abougouffa/minemacs/commit/f4211e3)) - [@abougouffa](https://github.com/abougouffa)
- add a list of external tools (WIP) - ([f31b94b](https://github.com/abougouffa/minemacs/commit/f31b94b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+binary-file-p` - ([2cc53c6](https://github.com/abougouffa/minemacs/commit/2cc53c6)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `rscope` - ([431d3b2](https://github.com/abougouffa/minemacs/commit/431d3b2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `eopengrok` support - ([6dde0da](https://github.com/abougouffa/minemacs/commit/6dde0da)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `rtags` support - ([f311840](https://github.com/abougouffa/minemacs/commit/f311840)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `eopengrok` support - ([2efe395](https://github.com/abougouffa/minemacs/commit/2efe395)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(docs)** move documentation to a separate directory - ([897d5ab](https://github.com/abougouffa/minemacs/commit/897d5ab)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** update `DOCS.md` path - ([6f1de1c](https://github.com/abougouffa/minemacs/commit/6f1de1c)) - [@abougouffa](https://github.com/abougouffa)
- **(version)** v4.11.0 - ([0d9da59](https://github.com/abougouffa/minemacs/commit/0d9da59)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(rtags)** defer until explicitly enabled - ([41395b7](https://github.com/abougouffa/minemacs/commit/41395b7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.11.0](https://github.com/abougouffa/minemacs/compare/v4.10.0..v4.11.0) - 2024-01-28
#### Bug Fixes
- **(builtin)** fix loading the `+whitespace-auto-cleanup-mode` hook - ([2245c31](https://github.com/abougouffa/minemacs/commit/2245c31)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** ignore previewing `*.po` files - ([6bd406d](https://github.com/abougouffa/minemacs/commit/6bd406d)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(prog)** add a comment - ([4da1401](https://github.com/abougouffa/minemacs/commit/4da1401)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+binary-file-p` - ([e9fee97](https://github.com/abougouffa/minemacs/commit/e9fee97)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `rtags` support - ([5b11206](https://github.com/abougouffa/minemacs/commit/5b11206)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `eopengrok` support - ([8a876f5](https://github.com/abougouffa/minemacs/commit/8a876f5)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `xcscope` - ([111c68f](https://github.com/abougouffa/minemacs/commit/111c68f)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `semantic-refactor` for use with non-LSP workspaces - ([d0aec15](https://github.com/abougouffa/minemacs/commit/d0aec15)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `ack` - ([98279ac](https://github.com/abougouffa/minemacs/commit/98279ac)) - [@abougouffa](https://github.com/abougouffa)
- add `me-ai` module - ([6e293e3](https://github.com/abougouffa/minemacs/commit/6e293e3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** map `SPC t c` to toggle `+whitespace-auto-cleanup-mode` - ([7db8582](https://github.com/abougouffa/minemacs/commit/7db8582)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** change default TAB behavior - ([262a43f](https://github.com/abougouffa/minemacs/commit/262a43f)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** make the default tab width 4 - ([3ae0f65](https://github.com/abougouffa/minemacs/commit/3ae0f65)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** better color for displaying trailing white space - ([bcf5089](https://github.com/abougouffa/minemacs/commit/bcf5089)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** add `+whitespace-auto-cleanup-mode` - ([f7804eb](https://github.com/abougouffa/minemacs/commit/f7804eb)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** map `gl` and `gh` to jump forward and backward - ([cdd2c9d](https://github.com/abougouffa/minemacs/commit/cdd2c9d)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add `me-nano` in the examples - ([a2c58cc](https://github.com/abougouffa/minemacs/commit/a2c58cc)) - [@abougouffa](https://github.com/abougouffa)
- open `clang-[format|tidy]` in YAML mode - ([0f1e448](https://github.com/abougouffa/minemacs/commit/0f1e448)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.10.0](https://github.com/abougouffa/minemacs/compare/v4.9.0..v4.10.0) - 2024-01-24
#### Bug Fixes
- **(core)** `+scratch-open-...` conflict with `project-cmake` - ([b9e4cbb](https://github.com/abougouffa/minemacs/commit/b9e4cbb)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** refactor and fix persistent buffer implementation - ([99ea57d](https://github.com/abougouffa/minemacs/commit/99ea57d)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** include the full list of obsolete modules - ([d33e0fa](https://github.com/abougouffa/minemacs/commit/d33e0fa)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** reintegrate `cmake-mode` and `cmake-font-lock` - ([03e9bf1](https://github.com/abougouffa/minemacs/commit/03e9bf1)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `eglot-booster` as a package - ([f9e77d5](https://github.com/abougouffa/minemacs/commit/f9e77d5)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(docker)** add a docker image with utils included (WIP) - ([5baf8b0](https://github.com/abougouffa/minemacs/commit/5baf8b0)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** move a variable to `me-vars` - ([e12ee0c](https://github.com/abougouffa/minemacs/commit/e12ee0c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** minor tweak and documentation change - ([ea9ab4c](https://github.com/abougouffa/minemacs/commit/ea9ab4c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better implementation of persistent scratch buffers - ([99645cf](https://github.com/abougouffa/minemacs/commit/99645cf)) - [@abougouffa](https://github.com/abougouffa)
- **(treemacs)** smaller font size for root nodes - ([d35f55c](https://github.com/abougouffa/minemacs/commit/d35f55c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9ff3223](https://github.com/abougouffa/minemacs/commit/9ff3223)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([e1d6b6f](https://github.com/abougouffa/minemacs/commit/e1d6b6f)) - [@abougouffa](https://github.com/abougouffa)
- make NetExtender integration obsolete, not used any more - ([ca2c631](https://github.com/abougouffa/minemacs/commit/ca2c631)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.9.0](https://github.com/abougouffa/minemacs/compare/v4.8.1..v4.9.0) - 2024-01-21
#### Features
- **(prog)** add `citre` for Universal Tags (`ctags`) support - ([6bf0f8b](https://github.com/abougouffa/minemacs/commit/6bf0f8b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(citre)** use the detected project root by default - ([c723218](https://github.com/abougouffa/minemacs/commit/c723218)) - [@abougouffa](https://github.com/abougouffa)
- **(whitespace)** smartly auto cleanup trailing white space on save - ([f892550](https://github.com/abougouffa/minemacs/commit/f892550)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2f04c2a](https://github.com/abougouffa/minemacs/commit/2f04c2a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.8.1](https://github.com/abougouffa/minemacs/compare/v4.8.0..v4.8.1) - 2024-01-20
#### Bug Fixes
- **(evil-vimish-fold)** enable the mode globally - ([64e1cbf](https://github.com/abougouffa/minemacs/commit/64e1cbf)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(code-folding)** cleanup dead code - ([9b27509](https://github.com/abougouffa/minemacs/commit/9b27509)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** use `add-hook` instead of `+add-hook!` in simple cases - ([e1bc8a0](https://github.com/abougouffa/minemacs/commit/e1bc8a0)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** better hook documentation in `+make-first-file-hook!` - ([a32eb79](https://github.com/abougouffa/minemacs/commit/a32eb79)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.8.0](https://github.com/abougouffa/minemacs/compare/v4.7.0..v4.8.0) - 2024-01-20
#### Bug Fixes
- **(core)** fix `+make-first-file-hook!` behavior in daemon mode - ([6dbfae6](https://github.com/abougouffa/minemacs/commit/6dbfae6)) - [@abougouffa](https://github.com/abougouffa)
- **(pyenv)** missing argument in a `+log!` statement - ([ace25b8](https://github.com/abougouffa/minemacs/commit/ace25b8)) - [@abougouffa](https://github.com/abougouffa)
- **(pyenv)** ensure before enabling it globally (daemon hanging issue) - ([5984552](https://github.com/abougouffa/minemacs/commit/5984552)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(code-folding)** better code folding experience (from Doom Emacs) - ([f76a571](https://github.com/abougouffa/minemacs/commit/f76a571)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add `vimish-fold` and `evil-vimish-fold` - ([13d1107](https://github.com/abougouffa/minemacs/commit/13d1107)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add `highlight-indent-guides`, but as an opt-in (slow!) - ([0792ea4](https://github.com/abougouffa/minemacs/commit/0792ea4)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** make `xmllint` the default for `nxml-mode` - ([eeac59e](https://github.com/abougouffa/minemacs/commit/eeac59e)) - [@abougouffa](https://github.com/abougouffa)
- **(hideshow)** additional modes' rules - ([641dd76](https://github.com/abougouffa/minemacs/commit/641dd76)) - [@abougouffa](https://github.com/abougouffa)
- **(repo)** restore the default repository, fix merged - ([a37537e](https://github.com/abougouffa/minemacs/commit/a37537e)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([20e3e5d](https://github.com/abougouffa/minemacs/commit/20e3e5d)) - [@abougouffa](https://github.com/abougouffa)
- don't disable some packages based on `executable-find` - ([c466611](https://github.com/abougouffa/minemacs/commit/c466611)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.7.0](https://github.com/abougouffa/minemacs/compare/v4.6.3..v4.7.0) - 2024-01-17
#### Bug Fixes
- **(repo)** use my fork until it gets merged upstream - ([fb9363b](https://github.com/abougouffa/minemacs/commit/fb9363b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(init)** more first file special hooks - ([55e6bb3](https://github.com/abougouffa/minemacs/commit/55e6bb3)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `pyenv` via `pyenv.el` - ([9bb5b84](https://github.com/abougouffa/minemacs/commit/9bb5b84)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add more Gerrit/repo utilities from ChromeOS's `dev-util` - ([6f25cf3](https://github.com/abougouffa/minemacs/commit/6f25cf3)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(treesit-fold)** still buggy, crashing in C/C++ files - ([fec0f2f](https://github.com/abougouffa/minemacs/commit/fec0f2f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** enable `hs-minor-mode` in `nxml-mode` - ([e8338bc](https://github.com/abougouffa/minemacs/commit/e8338bc)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** prioritize `minemacs-first-file` over `minemacs-first-X-file` - ([c846a0f](https://github.com/abougouffa/minemacs/commit/c846a0f)) - [@abougouffa](https://github.com/abougouffa)
- **(org-contrib)** ensure using the right branch - ([0c7d38d](https://github.com/abougouffa/minemacs/commit/0c7d38d)) - [@abougouffa](https://github.com/abougouffa)
- **(pyvenv)** remove unnecessary tweaks - ([2436e29](https://github.com/abougouffa/minemacs/commit/2436e29)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** exclude the `VIRTUAL_ENV` from saved env vars - ([7f8c0da](https://github.com/abougouffa/minemacs/commit/7f8c0da)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([29a7467](https://github.com/abougouffa/minemacs/commit/29a7467)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.6.3](https://github.com/abougouffa/minemacs/compare/v4.6.2..v4.6.3) - 2024-01-14
#### Bug Fixes
- **(core)** ignore case when matching regexps in `+make-first-file-hook!` - ([6241629](https://github.com/abougouffa/minemacs/commit/6241629)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(skel)** add an example of `init-tweaks.el` - ([57d796f](https://github.com/abougouffa/minemacs/commit/57d796f)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- obfuscate email addresses with `rot13` - ([3c3c709](https://github.com/abougouffa/minemacs/commit/3c3c709)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(evil-iedit-state)** use `:after iedit` instead - ([0521268](https://github.com/abougouffa/minemacs/commit/0521268)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.6.2](https://github.com/abougouffa/minemacs/compare/v4.6.1..v4.6.2) - 2024-01-14
#### Refactoring
- **(obsolete/flycheck)** minor edits - ([f34a85b](https://github.com/abougouffa/minemacs/commit/f34a85b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(multi-cursors)** remove unused `multiple-cursors` - ([8226319](https://github.com/abougouffa/minemacs/commit/8226319)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-cursors)** better packages loading - ([f81a573](https://github.com/abougouffa/minemacs/commit/f81a573)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([92eb5ef](https://github.com/abougouffa/minemacs/commit/92eb5ef)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.6.1](https://github.com/abougouffa/minemacs/compare/v4.6.0..v4.6.1) - 2024-01-13
#### Bug Fixes
- **(core)** better implementation of `objdump-disassemble-mode` - ([b3f4149](https://github.com/abougouffa/minemacs/commit/b3f4149)) - [@abougouffa](https://github.com/abougouffa)
- **(org-contrib)** use my mirror repo (sr.ht) isn't stable - ([056bfe8](https://github.com/abougouffa/minemacs/commit/056bfe8)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- update copyright year - ([45d5132](https://github.com/abougouffa/minemacs/commit/45d5132)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(early-init)** move `LSP_USE_PLISTS` to `me-lsp` - ([d1aa86a](https://github.com/abougouffa/minemacs/commit/d1aa86a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(binary)** don't disassemble by default - ([18a24e9](https://github.com/abougouffa/minemacs/commit/18a24e9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.6.0](https://github.com/abougouffa/minemacs/compare/v4.5.4..v4.6.0) - 2024-01-13
#### Documentation
- **(core)** add documentation for `+font--get-valid-args` - ([8c3cb2c](https://github.com/abougouffa/minemacs/commit/8c3cb2c)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documenatation - ([b738333](https://github.com/abougouffa/minemacs/commit/b738333)) - [@abougouffa](https://github.com/abougouffa)
- minor documentation tweaks - ([3e9cfb2](https://github.com/abougouffa/minemacs/commit/3e9cfb2)) - [@abougouffa](https://github.com/abougouffa)
- regenerate the documentation - ([649d3d5](https://github.com/abougouffa/minemacs/commit/649d3d5)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** add support for `evil-textobj-tree-sitter-get-textobj` - ([a8325b0](https://github.com/abougouffa/minemacs/commit/a8325b0)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-fold)** restore support after fixes (still WIP) - ([874fa85](https://github.com/abougouffa/minemacs/commit/874fa85)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(git)** add `user-config` to `.gitignore` - ([f22165b](https://github.com/abougouffa/minemacs/commit/f22165b)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(org)** simplify the code and add documentation - ([874a87b](https://github.com/abougouffa/minemacs/commit/874a87b)) - [@abougouffa](https://github.com/abougouffa)
- better convention for advice functions naming - ([569f10b](https://github.com/abougouffa/minemacs/commit/569f10b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** use `file-name-concat` in `+load` - ([08cc205](https://github.com/abougouffa/minemacs/commit/08cc205)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs-gdb)** directly use my recipe - ([92719c7](https://github.com/abougouffa/minemacs/commit/92719c7)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile-executor)** add keybindings - ([8871df4](https://github.com/abougouffa/minemacs/commit/8871df4)) - [@abougouffa](https://github.com/abougouffa)
- **(org-contrib)** make it lazy - ([1b0777c](https://github.com/abougouffa/minemacs/commit/1b0777c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.5.4](https://github.com/abougouffa/minemacs/compare/v4.5.3..v4.5.4) - 2024-01-10
#### Bug Fixes
- **(core)** better inference of filename in `+package-download-from-urls` - ([b7e0739](https://github.com/abougouffa/minemacs/commit/b7e0739)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add Nix to the list of languages [#140] - ([63ff477](https://github.com/abougouffa/minemacs/commit/63ff477)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(cape)** restore `cape-dict` (it in fact useful) [#150] - ([5caddc4](https://github.com/abougouffa/minemacs/commit/5caddc4)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** cache the downloaded `loaddefs-gen.el` file - ([0d26f30](https://github.com/abougouffa/minemacs/commit/0d26f30)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** make use of `+ignore-root` to exclude `x-win` sessions - ([b677fd6](https://github.com/abougouffa/minemacs/commit/b677fd6)) - [@abougouffa](https://github.com/abougouffa)
- **(cape)** disable unused `cape-dict` [#150] - ([ed2bae5](https://github.com/abougouffa/minemacs/commit/ed2bae5)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better exclusion of `recentf` files in `+ignore-roots` - ([4e619ae](https://github.com/abougouffa/minemacs/commit/4e619ae)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.5.3](https://github.com/abougouffa/minemacs/compare/v4.5.2..v4.5.3) - 2024-01-08
#### Bug Fixes
- **(scratch)** always replace the default scratch with the persistent one - ([e4a0ed9](https://github.com/abougouffa/minemacs/commit/e4a0ed9)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([3ee82a9](https://github.com/abougouffa/minemacs/commit/3ee82a9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.5.2](https://github.com/abougouffa/minemacs/compare/v4.5.1..v4.5.2) - 2024-01-08
#### Bug Fixes
- **(core)** avoid issues when evaluating buffer name variables [#150] - ([dd653fe](https://github.com/abougouffa/minemacs/commit/dd653fe)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** proper convention to forget remote zombie projects - ([aca7981](https://github.com/abougouffa/minemacs/commit/aca7981)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** fix the issue of upgrading Tramp on Emacs 29.1 - ([4a8a0a2](https://github.com/abougouffa/minemacs/commit/4a8a0a2)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate the documentation - ([c2dbbd9](https://github.com/abougouffa/minemacs/commit/c2dbbd9)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(recentf)** better exclusion of remote files - ([f6f1a10](https://github.com/abougouffa/minemacs/commit/f6f1a10)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** use up-to-date Tramp, remove old Magit related fix - ([984c5da](https://github.com/abougouffa/minemacs/commit/984c5da)) - [@abougouffa](https://github.com/abougouffa)
- **(tramp)** take the MacOS case into account in `stty` workaround - ([3267853](https://github.com/abougouffa/minemacs/commit/3267853)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.5.1](https://github.com/abougouffa/minemacs/compare/v4.5.0..v4.5.1) - 2024-01-07
#### Bug Fixes
- **(jinx)** don't show compile buffer on failure in `+jinx-load-module` - ([008dbaa](https://github.com/abougouffa/minemacs/commit/008dbaa)) - [@abougouffa](https://github.com/abougouffa)
- **(multi-vterm)** change work dir correctly on remote dedicated terminal - ([aed3220](https://github.com/abougouffa/minemacs/commit/aed3220)) - [@abougouffa](https://github.com/abougouffa)
- **(python)** correct the condition for tweaking `pyenv` integration - ([f79dfe7](https://github.com/abougouffa/minemacs/commit/f79dfe7)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(eglot)** add experimental `eglot-booster`! - ([6b2a8d4](https://github.com/abougouffa/minemacs/commit/6b2a8d4)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** initial support for `pyvenv` (WIP) - ([8a34f24](https://github.com/abougouffa/minemacs/commit/8a34f24)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** format remote files using local formatters - ([74c113b](https://github.com/abougouffa/minemacs/commit/74c113b)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot-booster)** rename file - ([762da5f](https://github.com/abougouffa/minemacs/commit/762da5f)) - [@abougouffa](https://github.com/abougouffa)
- **(jinx)** log the error message in `+jinx-load-module` - ([72db594](https://github.com/abougouffa/minemacs/commit/72db594)) - [@abougouffa](https://github.com/abougouffa)
- **(python)** better `pyenv` integration (WIP) - ([685bdcf](https://github.com/abougouffa/minemacs/commit/685bdcf)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([85008d7](https://github.com/abougouffa/minemacs/commit/85008d7)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.5.0](https://github.com/abougouffa/minemacs/compare/v4.4.0..v4.5.0) - 2024-01-04
#### Features
- **(embedded)** add support for DTS via `dts-mode` & `virtual-dts-mode` - ([2c2d7e7](https://github.com/abougouffa/minemacs/commit/2c2d7e7)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(natural-langs)** restore jinx - ([6014efa](https://github.com/abougouffa/minemacs/commit/6014efa)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** restore `jinx` and `spell-fu` examples - ([9d0f9f5](https://github.com/abougouffa/minemacs/commit/9d0f9f5)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** smaller internal border - ([3eddc3b](https://github.com/abougouffa/minemacs/commit/3eddc3b)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-numbers)** define the right number format for `dts-mode` - ([9f48b02](https://github.com/abougouffa/minemacs/commit/9f48b02)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** use `jinx` if available or fallback to `spell-fu` - ([153cbd0](https://github.com/abougouffa/minemacs/commit/153cbd0)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** remove obsolete examples - ([e99daea](https://github.com/abougouffa/minemacs/commit/e99daea)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.4.0](https://github.com/abougouffa/minemacs/compare/v4.3.3..v4.4.0) - 2024-01-03
#### Features
- **(core)** add function to get/set the standard values - ([540ad79](https://github.com/abougouffa/minemacs/commit/540ad79)) - [@abougouffa](https://github.com/abougouffa)
- **(fun)** add `wordel` - ([d37b0d1](https://github.com/abougouffa/minemacs/commit/d37b0d1)) - [@abougouffa](https://github.com/abougouffa)
- **(nano)** add initial N Λ N O Emacs UI (WIP) - ([761278f](https://github.com/abougouffa/minemacs/commit/761278f)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** make jinx obsolete, add `flyspell-correct` - ([1d10383](https://github.com/abougouffa/minemacs/commit/1d10383)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** add an internal border of 15px - ([539e42c](https://github.com/abougouffa/minemacs/commit/539e42c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** be more intelligent when trying to load the theme - ([4e23071](https://github.com/abougouffa/minemacs/commit/4e23071)) - [@abougouffa](https://github.com/abougouffa)
- **(editorconfig)** trigger on the first file, exclude compressed files - ([c4a703c](https://github.com/abougouffa/minemacs/commit/c4a703c)) - [@abougouffa](https://github.com/abougouffa)
- **(envrc)** ensure loading envrc for babel source blocks - ([9cc51e9](https://github.com/abougouffa/minemacs/commit/9cc51e9)) - [@abougouffa](https://github.com/abougouffa)
- **(envrc)** disable on Windows - ([cf348a7](https://github.com/abougouffa/minemacs/commit/cf348a7)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** `treesit-auto-langs` set incorrectly - ([3c5d6e9](https://github.com/abougouffa/minemacs/commit/3c5d6e9)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** add `minemacs-obsolete-modules-dir` - ([fb1a605](https://github.com/abougouffa/minemacs/commit/fb1a605)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([8f8b8f8](https://github.com/abougouffa/minemacs/commit/8f8b8f8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.3.3](https://github.com/abougouffa/minemacs/compare/v4.3.2..v4.3.3) - 2023-12-29
#### Miscellaneous Chores
- **(makefile)** add `locked` rule - ([2bbb4ee](https://github.com/abougouffa/minemacs/commit/2bbb4ee)) - [@abougouffa](https://github.com/abougouffa)
- **(makefile)** fix the `update` rule - ([038ec19](https://github.com/abougouffa/minemacs/commit/038ec19)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(core)** minor refactor - ([d4db4cf](https://github.com/abougouffa/minemacs/commit/d4db4cf)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(fonts)** move font & script settings to `me-lib` - ([aa4de70](https://github.com/abougouffa/minemacs/commit/aa4de70)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(init)** cleanup irrelevant comments/logs - ([f7c841d](https://github.com/abougouffa/minemacs/commit/f7c841d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f460d8f](https://github.com/abougouffa/minemacs/commit/f460d8f)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.3.2](https://github.com/abougouffa/minemacs/compare/v4.3.1..v4.3.2) - 2023-12-29
#### Refactoring
- **(core)** change the signature of `+github-latest-release` - ([47087b8](https://github.com/abougouffa/minemacs/commit/47087b8)) - [@abougouffa](https://github.com/abougouffa)
- move `+github-latest-release` to `me-lib` - ([bca42ac](https://github.com/abougouffa/minemacs/commit/bca42ac)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(jinx)** hook if the compilation is easy (Unix or Win+MSYS) [#147] - ([f09dd85](https://github.com/abougouffa/minemacs/commit/f09dd85)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example of how to force loading an obsolete module - ([293008d](https://github.com/abougouffa/minemacs/commit/293008d)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example of `jinx-languages` - ([6976d38](https://github.com/abougouffa/minemacs/commit/6976d38)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.3.1](https://github.com/abougouffa/minemacs/compare/v4.3.0..v4.3.1) - 2023-12-28
#### Documentation
- regenerate documentation - ([524a903](https://github.com/abougouffa/minemacs/commit/524a903)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(compile-multi)** enable integration for `consult` and `embark` - ([47bcf99](https://github.com/abougouffa/minemacs/commit/47bcf99)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** regenerate loaddefs - ([b37ed85](https://github.com/abougouffa/minemacs/commit/b37ed85)) - [@abougouffa](https://github.com/abougouffa)
- **(spell-fu)** move dict registration macro to the obsolete module - ([116aa8e](https://github.com/abougouffa/minemacs/commit/116aa8e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.3.0](https://github.com/abougouffa/minemacs/compare/v4.2.4..v4.3.0) - 2023-12-28
#### Bug Fixes
- **(natural-langs)** fallback to `spell-fu` on Windows [#147] - ([9e91a1e](https://github.com/abougouffa/minemacs/commit/9e91a1e)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** remove obsolete `spell-fu` config example [#146] - ([8e6bc6d](https://github.com/abougouffa/minemacs/commit/8e6bc6d)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate documentation - ([e0891b2](https://github.com/abougouffa/minemacs/commit/e0891b2)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- remove unnecessary straight recipe names - ([28b5aaf](https://github.com/abougouffa/minemacs/commit/28b5aaf)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(compile-multi)** install the Embark and Consult extensions - ([1bac5a4](https://github.com/abougouffa/minemacs/commit/1bac5a4)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-collection)** remove the `corfu` hack, merged upstream - ([8947f29](https://github.com/abougouffa/minemacs/commit/8947f29)) - [@abougouffa](https://github.com/abougouffa)
- **(jinx)** add `jinx--load-module` to `minemacs-build-functions` - ([3357935](https://github.com/abougouffa/minemacs/commit/3357935)) - [@abougouffa](https://github.com/abougouffa)
- **(vterm)** move `C-l/h/k/j` to directions - ([58198b8](https://github.com/abougouffa/minemacs/commit/58198b8)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([44cb788](https://github.com/abougouffa/minemacs/commit/44cb788)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.2.4](https://github.com/abougouffa/minemacs/compare/v4.2.3..v4.2.4) - 2023-12-25
#### Features
- **(files)** add support for `ztree` - ([ef01c78](https://github.com/abougouffa/minemacs/commit/ef01c78)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** use `jinx` for spell checking instead of `spell-fu` - ([b2b62bb](https://github.com/abougouffa/minemacs/commit/b2b62bb)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `osm` - ([9e8ae0a](https://github.com/abougouffa/minemacs/commit/9e8ae0a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(jinx)** enable only when Emacs is built with modules support - ([c603e55](https://github.com/abougouffa/minemacs/commit/c603e55)) - [@abougouffa](https://github.com/abougouffa)
- **(natural-langs)** make `spell-fu` obsolete - ([79aba80](https://github.com/abougouffa/minemacs/commit/79aba80)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.2.3](https://github.com/abougouffa/minemacs/compare/v4.2.2..v4.2.3) - 2023-12-25
#### Bug Fixes
- **(evil-collection)** fix `corfu--setup` signature - ([feeae81](https://github.com/abougouffa/minemacs/commit/feeae81)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(cog)** add contributors, remove the long v0.1.0 changelog entry - ([2368c5b](https://github.com/abougouffa/minemacs/commit/2368c5b)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(app-launcher)** update recipe - ([22f34d1](https://github.com/abougouffa/minemacs/commit/22f34d1)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- bump packages versions - ([4cd8ed5](https://github.com/abougouffa/minemacs/commit/4cd8ed5)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.2.2](https://github.com/abougouffa/minemacs/compare/v4.2.1..v4.2.2) - 2023-12-24
#### Bug Fixes
- **(cape)** enable `cape-elisp-block` in `org-mode` only - ([8331f8e](https://github.com/abougouffa/minemacs/commit/8331f8e)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** pin to a working commit - ([9dee456](https://github.com/abougouffa/minemacs/commit/9dee456)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.2.1](https://github.com/abougouffa/minemacs/compare/v4.2.0..v4.2.1) - 2023-12-24
#### Bug Fixes
- **(core)** better management of first files hooks [#142] - ([6d20b61](https://github.com/abougouffa/minemacs/commit/6d20b61)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** better implementation for `+eglot-auto-enable` [#142] - ([26f6784](https://github.com/abougouffa/minemacs/commit/26f6784)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot+lsp)** remove unneeded loop in auto-enable LSP/Eglot - ([4ad574a](https://github.com/abougouffa/minemacs/commit/4ad574a)) - [@abougouffa](https://github.com/abougouffa)
- **(elec-pair)** disable auto-pairing of "<" in `org-mode` - ([ecb3675](https://github.com/abougouffa/minemacs/commit/ecb3675)) - [@Hmanhng](https://github.com/Hmanhng)
- **(recentf)** load early to work correctly on non-daemon Emacs [#142] - ([01f6f30](https://github.com/abougouffa/minemacs/commit/01f6f30)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate documentation - ([69b20ed](https://github.com/abougouffa/minemacs/commit/69b20ed)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(elisp-mode)** make use of `+setq-hook!` - ([8748e8b](https://github.com/abougouffa/minemacs/commit/8748e8b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(lsp)** update to the new auto-enable convention - ([6856b09](https://github.com/abougouffa/minemacs/commit/6856b09)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([f5a74cc](https://github.com/abougouffa/minemacs/commit/f5a74cc)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.2.0](https://github.com/abougouffa/minemacs/compare/v4.1.3..v4.2.0) - 2023-12-19
#### Bug Fixes
- **(cape)** rename obsolete `cape-symbol` to `cape-elisp-symbol` - ([6313c3d](https://github.com/abougouffa/minemacs/commit/6313c3d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** don't alias `loaddefs-generate` to `make-directory-autoloads` - ([00b241f](https://github.com/abougouffa/minemacs/commit/00b241f)) - [@abougouffa](https://github.com/abougouffa)
- **(saveplace)** enable at init to work with files passed as args - ([b91c3cc](https://github.com/abougouffa/minemacs/commit/b91c3cc)) - [@abougouffa](https://github.com/abougouffa)
- **(saveplace)** enable before opening the first file [#142] - ([69aedcc](https://github.com/abougouffa/minemacs/commit/69aedcc)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** buggy detection for non-installed grammars [#140] - ([c770acb](https://github.com/abougouffa/minemacs/commit/c770acb)) - [@abougouffa](https://github.com/abougouffa)
- **(undo-fu-session)** load early - ([c57c147](https://github.com/abougouffa/minemacs/commit/c57c147)) - [@abougouffa](https://github.com/abougouffa)
- **(undo-fu-session)** fix renamed global mode - ([8452ff5](https://github.com/abougouffa/minemacs/commit/8452ff5)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move constants to `me-vars` - ([1d7aeb7](https://github.com/abougouffa/minemacs/commit/1d7aeb7)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- restore the original `minemacs-directory-arg-p` constant - ([9a1e46d](https://github.com/abougouffa/minemacs/commit/9a1e46d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(window)** wider help windows - ([c8c4d10](https://github.com/abougouffa/minemacs/commit/c8c4d10)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([396efdb](https://github.com/abougouffa/minemacs/commit/396efdb)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([10a02e0](https://github.com/abougouffa/minemacs/commit/10a02e0)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.1.3](https://github.com/abougouffa/minemacs/compare/v4.1.2..v4.1.3) - 2023-12-16
#### Bug Fixes
- **(super-save)** correct a renamed customization variable - ([98ab319](https://github.com/abougouffa/minemacs/commit/98ab319)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- regenerate DOCS.md - ([c11dbc4](https://github.com/abougouffa/minemacs/commit/c11dbc4)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** minor edit - ([9325ba5](https://github.com/abougouffa/minemacs/commit/9325ba5)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** allow updating packages downloaded from URLs - ([7cff195](https://github.com/abougouffa/minemacs/commit/7cff195)) - [@abougouffa](https://github.com/abougouffa)
- **(diffview)** add keybindings - ([d7b1998](https://github.com/abougouffa/minemacs/commit/d7b1998)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib2)** make open status customizable - ([fbca453](https://github.com/abougouffa/minemacs/commit/fbca453)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a50fdba](https://github.com/abougouffa/minemacs/commit/a50fdba)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.1.2](https://github.com/abougouffa/minemacs/compare/v4.1.1..v4.1.2) - 2023-12-14
#### Bug Fixes
- **(bitbake)** define keybindings consistently - ([f424e76](https://github.com/abougouffa/minemacs/commit/f424e76)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(bootstrap)** add docstring to `+straight-prune-build-cache` - ([ab1707e](https://github.com/abougouffa/minemacs/commit/ab1707e)) - [@abougouffa](https://github.com/abougouffa)
- generate the documentation and mention it in README - ([6e5408d](https://github.com/abougouffa/minemacs/commit/6e5408d)) - [@abougouffa](https://github.com/abougouffa)
- minor edits and formatting - ([3daabd4](https://github.com/abougouffa/minemacs/commit/3daabd4)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(data)** replace `+csv-rainbow` with `rainbow-csv` - ([00a1cb0](https://github.com/abougouffa/minemacs/commit/00a1cb0)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add `gee` for Gerrit support in Emacs (useful for Yocto) - ([316099a](https://github.com/abougouffa/minemacs/commit/316099a)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add initial support for `diffview` - ([dff169e](https://github.com/abougouffa/minemacs/commit/dff169e)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** enable documentation generation using - ([309c71f](https://github.com/abougouffa/minemacs/commit/309c71f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bitbake)** tailor bitbake modes and add keybindings - ([f0e38c7](https://github.com/abougouffa/minemacs/commit/f0e38c7)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** demote errors when loading modules unless in debug mode - ([6b3b4cc](https://github.com/abougouffa/minemacs/commit/6b3b4cc)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** minor edit - ([2e3c4ed](https://github.com/abougouffa/minemacs/commit/2e3c4ed)) - [@abougouffa](https://github.com/abougouffa)
- **(super-save)** add more trigger commands - ([49601c9](https://github.com/abougouffa/minemacs/commit/49601c9)) - [@abougouffa](https://github.com/abougouffa)
- **(super-save)** use the default idle duration (5s) - ([d6e030d](https://github.com/abougouffa/minemacs/commit/d6e030d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c2624e2](https://github.com/abougouffa/minemacs/commit/c2624e2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.1.1](https://github.com/abougouffa/minemacs/compare/v4.1.0..v4.1.1) - 2023-12-10
#### Documentation
- **(readme)** add a note on `general-describe-keybindings` - ([376266a](https://github.com/abougouffa/minemacs/commit/376266a)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update documentation - ([d8b62ce](https://github.com/abougouffa/minemacs/commit/d8b62ce)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(latex)** add `latex-preview-pane` - ([525c936](https://github.com/abougouffa/minemacs/commit/525c936)) - [@abougouffa](https://github.com/abougouffa)
- **(pdf-tools)** save/restore position in PDFs using `pdf-view-restore` - ([1e392e1](https://github.com/abougouffa/minemacs/commit/1e392e1)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(readme)** prettify - ([39677df](https://github.com/abougouffa/minemacs/commit/39677df)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** make use of `minemacs-assets-dir` - ([5106970](https://github.com/abougouffa/minemacs/commit/5106970)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.1.0](https://github.com/abougouffa/minemacs/compare/v4.0.3..v4.1.0) - 2023-12-09
#### Bug Fixes
- **(core)** ensure `minemacs-extra-packages-dir` exists - ([8bd5f50](https://github.com/abougouffa/minemacs/commit/8bd5f50)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** mark only non-installed grammar for install - ([fd0477f](https://github.com/abougouffa/minemacs/commit/fd0477f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+download-package-from-urls` to pkgs from non-VC URLs - ([4300caf](https://github.com/abougouffa/minemacs/commit/4300caf)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add support for `selection-highlight-mode` - ([e08c0c5](https://github.com/abougouffa/minemacs/commit/e08c0c5)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add support for `sr-speedbar` - ([dfc4c32](https://github.com/abougouffa/minemacs/commit/dfc4c32)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** add `julia-repl` - ([f5aaca2](https://github.com/abougouffa/minemacs/commit/f5aaca2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** initial support for `quickrun` - ([ce60604](https://github.com/abougouffa/minemacs/commit/ce60604)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `html-ts-mode` - ([483224c](https://github.com/abougouffa/minemacs/commit/483224c)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for just files - ([42e336d](https://github.com/abougouffa/minemacs/commit/42e336d)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** 29.2 is not yet available - ([375ead4](https://github.com/abougouffa/minemacs/commit/375ead4)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** don't fail on Emacs snapshot - ([ad5a28c](https://github.com/abougouffa/minemacs/commit/ad5a28c)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** add Emacs 28.1 & 29.2 to the matrix - ([5f09582](https://github.com/abougouffa/minemacs/commit/5f09582)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- correct a typo in a commit - ([86de1df](https://github.com/abougouffa/minemacs/commit/86de1df)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** use a dashed line as a display fill column indicator - ([68736c1](https://github.com/abougouffa/minemacs/commit/68736c1)) - [@abougouffa](https://github.com/abougouffa)
- **(combobulate)** use `M-S-<up/down/left/right>` to avoid conflict - ([30ee5ee](https://github.com/abougouffa/minemacs/commit/30ee5ee)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** use canonical function naming - ([84f653f](https://github.com/abougouffa/minemacs/commit/84f653f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** make use of `rename-visited-file` when available - ([ccefd5c](https://github.com/abougouffa/minemacs/commit/ccefd5c)) - [@abougouffa](https://github.com/abougouffa)
- **(core-ui)** don't make line numbers small, doesn't integrate well - ([9527fe1](https://github.com/abougouffa/minemacs/commit/9527fe1)) - [@abougouffa](https://github.com/abougouffa)
- **(drag-stuff)** more intuitive keybindings - ([b3a925f](https://github.com/abougouffa/minemacs/commit/b3a925f)) - [@abougouffa](https://github.com/abougouffa)
- **(selection-highlight-mode)** use a different face than region - ([0991708](https://github.com/abougouffa/minemacs/commit/0991708)) - [@abougouffa](https://github.com/abougouffa)
- **(sr-speedbar)** remove unnecessary require - ([b49c064](https://github.com/abougouffa/minemacs/commit/b49c064)) - [@abougouffa](https://github.com/abougouffa)
- **(with-editor)** don't load on `julia-repl` - ([393d297](https://github.com/abougouffa/minemacs/commit/393d297)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.0.3](https://github.com/abougouffa/minemacs/compare/v4.0.2..v4.0.3) - 2023-12-08
#### Features
- **(ui)** add `anzu` to show number of matches in modeline - ([2f3b9e8](https://github.com/abougouffa/minemacs/commit/2f3b9e8)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** automatically detected all available modules - ([8456f69](https://github.com/abougouffa/minemacs/commit/8456f69)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** don't force loading all packages in normal mode - ([baf864b](https://github.com/abougouffa/minemacs/commit/baf864b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(super-save)** remove the hack (merged upstream) - ([3bbe563](https://github.com/abougouffa/minemacs/commit/3bbe563)) - [@abougouffa](https://github.com/abougouffa)
- **(super-save)** temporary support for `super-save-all-buffers` - ([bf671e2](https://github.com/abougouffa/minemacs/commit/bf671e2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([192503a](https://github.com/abougouffa/minemacs/commit/192503a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.0.2](https://github.com/abougouffa/minemacs/compare/v4.0.1..v4.0.2) - 2023-12-07
#### Features
- **(editor)** use the new `super-save` instead of `auto-save` - ([86dc4ec](https://github.com/abougouffa/minemacs/commit/86dc4ec)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** simplify and remove dead code - ([6c3a45e](https://github.com/abougouffa/minemacs/commit/6c3a45e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.0.1](https://github.com/abougouffa/minemacs/compare/v4.0.0..v4.0.1) - 2023-12-07
#### Bug Fixes
- **(core)** avoid `thing-at-point` errors - ([0a0dac7](https://github.com/abougouffa/minemacs/commit/0a0dac7)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** don't reload theme on frame creation [#136] - ([defe63f](https://github.com/abougouffa/minemacs/commit/defe63f)) - [@abougouffa](https://github.com/abougouffa)
- move accidentally created `me-smartparens.el` to `obsolete` - ([a8c77a7](https://github.com/abougouffa/minemacs/commit/a8c77a7)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(editor)** add support for `auto-save` - ([ef19196](https://github.com/abougouffa/minemacs/commit/ef19196)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add `org-re-reveal`, `org-re-reveal-citeproc` & `oer-reveal` - ([3b1e01c](https://github.com/abougouffa/minemacs/commit/3b1e01c)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `guix` - ([920dee5](https://github.com/abougouffa/minemacs/commit/920dee5)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(core)** insert lines to separate sections in `me-lib` - ([e1cf22a](https://github.com/abougouffa/minemacs/commit/e1cf22a)) - [@abougouffa](https://github.com/abougouffa)
- comment - ([1df87a0](https://github.com/abougouffa/minemacs/commit/1df87a0)) - [@abougouffa](https://github.com/abougouffa)
- code formatting - ([7b4fe80](https://github.com/abougouffa/minemacs/commit/7b4fe80)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** remove unneeded checks in persistent scratch - ([5bccc99](https://github.com/abougouffa/minemacs/commit/5bccc99)) - [@abougouffa](https://github.com/abougouffa)
- **(splash)** minor edit - ([5297c42](https://github.com/abougouffa/minemacs/commit/5297c42)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(ui)** restore the smaller lines numbers tweak - ([88a9282](https://github.com/abougouffa/minemacs/commit/88a9282)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(backports)** avoid problems on Emacs 28 - ([9ce07ee](https://github.com/abougouffa/minemacs/commit/9ce07ee)) - [@abougouffa](https://github.com/abougouffa)
- **(backports)** better compatibility with Emacs 28 - ([c09eca1](https://github.com/abougouffa/minemacs/commit/c09eca1)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** better conditions for `mu4e` and `elfeed` - ([bc51ebf](https://github.com/abougouffa/minemacs/commit/bc51ebf)) - [@abougouffa](https://github.com/abougouffa)
- **(elec-pair)** don't complete / in Org (annoying when writing paths) - ([931f584](https://github.com/abougouffa/minemacs/commit/931f584)) - [@abougouffa](https://github.com/abougouffa)
- **(lib)** make `+single-file` a command - ([926262b](https://github.com/abougouffa/minemacs/commit/926262b)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** use filled numbers for hints - ([a76e2c0](https://github.com/abougouffa/minemacs/commit/a76e2c0)) - [@abougouffa](https://github.com/abougouffa)
- **(tab-bar)** better defaults + use of nerd-fonts for the close button - ([b6bb265](https://github.com/abougouffa/minemacs/commit/b6bb265)) - [@abougouffa](https://github.com/abougouffa)
- **(tldr)** minor tweaks - ([bc26fd9](https://github.com/abougouffa/minemacs/commit/bc26fd9)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** conditionally install/configure packages - ([0446e34](https://github.com/abougouffa/minemacs/commit/0446e34)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([051eb74](https://github.com/abougouffa/minemacs/commit/051eb74)) - [@abougouffa](https://github.com/abougouffa)
- multiple minor tweaks and edits - ([229dd56](https://github.com/abougouffa/minemacs/commit/229dd56)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v4.0.0](https://github.com/abougouffa/minemacs/compare/v3.11.0..v4.0.0) - 2023-11-30
#### Bug Fixes
- **(core)** undefined variable on Emacs 28 - ([6749e49](https://github.com/abougouffa/minemacs/commit/6749e49)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** loading `me-lib` fails on Emacs 28 - ([b199ffc](https://github.com/abougouffa/minemacs/commit/b199ffc)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** load `early-config` instead of `early-init` - ([295cf57](https://github.com/abougouffa/minemacs/commit/295cf57)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(core)** minor documentation edits - ([0160a85](https://github.com/abougouffa/minemacs/commit/0160a85)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better documentation and comments - ([a21d66f](https://github.com/abougouffa/minemacs/commit/a21d66f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more documentation for core functions - ([312485f](https://github.com/abougouffa/minemacs/commit/312485f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(files)** add `dired-rsync` - ([0fc0c9b](https://github.com/abougouffa/minemacs/commit/0fc0c9b)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(skel)** correct some typos - ([1ade54f](https://github.com/abougouffa/minemacs/commit/1ade54f)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** move persistent scratch routines to `me-lib` - ([fa44fa3](https://github.com/abougouffa/minemacs/commit/fa44fa3)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** move all core tweaks to `me-lib` - ([aca3d52](https://github.com/abougouffa/minemacs/commit/aca3d52)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(fonts)** remove obsolete aliases - ([f127b7f](https://github.com/abougouffa/minemacs/commit/f127b7f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([58e5105](https://github.com/abougouffa/minemacs/commit/58e5105)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([8c2cfae](https://github.com/abougouffa/minemacs/commit/8c2cfae)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.8](https://github.com/abougouffa/minemacs/compare/v3.10.7..v3.10.8) - 2023-11-26
#### Bug Fixes
- **(org)** stick Org to the built-in (stable) version - ([7c8635b](https://github.com/abougouffa/minemacs/commit/7c8635b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(tools)** add `nix-ts-mode` and tweak Eglot servers to run for it - ([25b98eb](https://github.com/abougouffa/minemacs/commit/25b98eb)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(treesit-auto)** add Nix, use the `treesit-auto-langs` variable - ([600691e](https://github.com/abougouffa/minemacs/commit/600691e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.7](https://github.com/abougouffa/minemacs/compare/v3.10.6..v3.10.7) - 2023-11-26
#### Features
- **(checkers)** add `flymake-pyre` - ([184f4b2](https://github.com/abougouffa/minemacs/commit/184f4b2)) - [@abougouffa](https://github.com/abougouffa)
- **(completion)** add `wgrep` (integrates with `consult` & `embark`) - ([4b66448](https://github.com/abougouffa/minemacs/commit/4b66448)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for `direnv` - ([aefdfa1](https://github.com/abougouffa/minemacs/commit/aefdfa1)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add support for Nix - ([0868ba9](https://github.com/abougouffa/minemacs/commit/0868ba9)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(builtin)** disable `auto-save-visited-mode` - ([ca6725a](https://github.com/abougouffa/minemacs/commit/ca6725a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** let straight decide where to get builtin packages - ([0f7300a](https://github.com/abougouffa/minemacs/commit/0f7300a)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** bind `grep`/`file` alongside with `rg`/`fd` - ([845bfb8](https://github.com/abougouffa/minemacs/commit/845bfb8)) - [@abougouffa](https://github.com/abougouffa)
- **(org-modern)** disable rendering checkboxes as unicode chars - ([ee8f74a](https://github.com/abougouffa/minemacs/commit/ee8f74a)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** add Nix-REPL to the REPLs display rule - ([ca2923c](https://github.com/abougouffa/minemacs/commit/ca2923c)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([1136d0a](https://github.com/abougouffa/minemacs/commit/1136d0a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([72086e9](https://github.com/abougouffa/minemacs/commit/72086e9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.6](https://github.com/abougouffa/minemacs/compare/v3.10.5..v3.10.6) - 2023-11-25
#### Nitpicks, changes with no side effect
- minor refactors - ([d65b966](https://github.com/abougouffa/minemacs/commit/d65b966)) - [@abougouffa](https://github.com/abougouffa)
- correct typos - ([de4184f](https://github.com/abougouffa/minemacs/commit/de4184f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** remove `pgformatter` as it is merged upstream - ([3328cea](https://github.com/abougouffa/minemacs/commit/3328cea)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** enable `auto-save-visited-mode` - ([81c0441](https://github.com/abougouffa/minemacs/commit/81c0441)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-notes)** use only for Denote - ([d0bb198](https://github.com/abougouffa/minemacs/commit/d0bb198)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove the never used `me-meow` module - ([ab863c9](https://github.com/abougouffa/minemacs/commit/ab863c9)) - [@abougouffa](https://github.com/abougouffa)
- **(keybinding)** bind `other-window-prefix` to `SPC O` - ([aafcb2d](https://github.com/abougouffa/minemacs/commit/aafcb2d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([181815a](https://github.com/abougouffa/minemacs/commit/181815a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.5](https://github.com/abougouffa/minemacs/compare/v3.10.4..v3.10.5) - 2023-11-23
#### Features
- **(checkers)** add `flymake-nasm` - ([25eb2a2](https://github.com/abougouffa/minemacs/commit/25eb2a2)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** add `flymake-pmd` - ([79b09b0](https://github.com/abougouffa/minemacs/commit/79b09b0)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** add `flymake-relint` to check regexps in Elisp - ([8639ad4](https://github.com/abougouffa/minemacs/commit/8639ad4)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `sideline-blame` - ([0527103](https://github.com/abougouffa/minemacs/commit/0527103)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** make `blamer` obsolete - ([625d1d4](https://github.com/abougouffa/minemacs/commit/625d1d4)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(checkers)** remove dead code - ([b52cdc0](https://github.com/abougouffa/minemacs/commit/b52cdc0)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(checkers)** code formatting - ([762c1c4](https://github.com/abougouffa/minemacs/commit/762c1c4)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(ui)** remove `sideline-blame` - ([189ce9c](https://github.com/abougouffa/minemacs/commit/189ce9c)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(elec-pair)** don't pair * in Org mode - ([c466e01](https://github.com/abougouffa/minemacs/commit/c466e01)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** activate more checkers for Python - ([36dac31](https://github.com/abougouffa/minemacs/commit/36dac31)) - [@abougouffa](https://github.com/abougouffa)
- **(sideline)** change date format for `sideline-blame` - ([557af19](https://github.com/abougouffa/minemacs/commit/557af19)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** add more rules for displaying help buffers - ([a45e026](https://github.com/abougouffa/minemacs/commit/a45e026)) - [@abougouffa](https://github.com/abougouffa)
- bump package versions - ([efdf896](https://github.com/abougouffa/minemacs/commit/efdf896)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.4](https://github.com/abougouffa/minemacs/compare/v3.10.3..v3.10.4) - 2023-11-21
#### Bug Fixes
- **(parinfer)** ensure that the directory exits (#129) - ([620cd92](https://github.com/abougouffa/minemacs/commit/620cd92)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** disable the buggy `treesit-fold` - ([94b31d6](https://github.com/abougouffa/minemacs/commit/94b31d6)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** add a caveat to `config.el` - ([a9dd90f](https://github.com/abougouffa/minemacs/commit/a9dd90f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(builtin)** use `electric-pair-mode` for the moment - ([8301aee](https://github.com/abougouffa/minemacs/commit/8301aee)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** initial support for `project-cmake` - ([92d4b89](https://github.com/abougouffa/minemacs/commit/92d4b89)) - [@abougouffa](https://github.com/abougouffa)
- **(ui)** add `solaire-mode` - ([ed68c64](https://github.com/abougouffa/minemacs/commit/ed68c64)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(core-ui)** remove `lin` - ([fb76722](https://github.com/abougouffa/minemacs/commit/fb76722)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(editor)** make `smartparens` obsolete - ([64a3601](https://github.com/abougouffa/minemacs/commit/64a3601)) - [@abougouffa](https://github.com/abougouffa)
- **(electric-pair)** more rules for Org-mode and Markdown - ([3cd3a7d](https://github.com/abougouffa/minemacs/commit/3cd3a7d)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** remove duplicate settings - ([0712396](https://github.com/abougouffa/minemacs/commit/0712396)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** show `man` and `woman` on a dedicated side window - ([95f2637](https://github.com/abougouffa/minemacs/commit/95f2637)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([76756f5](https://github.com/abougouffa/minemacs/commit/76756f5)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.3](https://github.com/abougouffa/minemacs/compare/v3.10.2..v3.10.3) - 2023-11-20
#### Bug Fixes
- **(transient)** install from Elpa on Emacs 28 - ([978a698](https://github.com/abougouffa/minemacs/commit/978a698)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core-ui)** add `lin` - ([d3e90a3](https://github.com/abougouffa/minemacs/commit/d3e90a3)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add `+project-forget-duplicate-projects` helper - ([87345a3](https://github.com/abougouffa/minemacs/commit/87345a3)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** minor tweak - ([fcb11fe](https://github.com/abougouffa/minemacs/commit/fcb11fe)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** define menu with `transient` instead of `hydra` - ([e615b37](https://github.com/abougouffa/minemacs/commit/e615b37)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** centralize global keybindings for builtin stuff - ([166233b](https://github.com/abougouffa/minemacs/commit/166233b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dape)** add keybindings - ([28dea66](https://github.com/abougouffa/minemacs/commit/28dea66)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** now the batteries are already included! - ([6308572](https://github.com/abougouffa/minemacs/commit/6308572)) - [@abougouffa](https://github.com/abougouffa)
- **(denote)** better integration with Emacs' builtin packages - ([029d78b](https://github.com/abougouffa/minemacs/commit/029d78b)) - [@abougouffa](https://github.com/abougouffa)
- **(denote)** autoload command aliases and tweak keybindings - ([383fe1a](https://github.com/abougouffa/minemacs/commit/383fe1a)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** add debug section - ([af3566f](https://github.com/abougouffa/minemacs/commit/af3566f)) - [@abougouffa](https://github.com/abougouffa)
- **(ligature)** make it obsolete, buggy when the font have no ligatures - ([2392012](https://github.com/abougouffa/minemacs/commit/2392012)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** better cleanup of duplicate projects - ([086eff9](https://github.com/abougouffa/minemacs/commit/086eff9)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ea121ee](https://github.com/abougouffa/minemacs/commit/ea121ee)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([02da9d2](https://github.com/abougouffa/minemacs/commit/02da9d2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.2](https://github.com/abougouffa/minemacs/compare/v3.10.1..v3.10.2) - 2023-11-19
#### Documentation
- **(readme)** update screenshot - ([c36712c](https://github.com/abougouffa/minemacs/commit/c36712c)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(flymake)** add `flymenu-flymake` - ([8d9cf1c](https://github.com/abougouffa/minemacs/commit/8d9cf1c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(spell-fu)** minor changes - ([37e3bfd](https://github.com/abougouffa/minemacs/commit/37e3bfd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.1](https://github.com/abougouffa/minemacs/compare/v3.10.0..v3.10.1) - 2023-11-19
#### Bug Fixes
- **(window)** restore current buffer on title bar - ([b715232](https://github.com/abougouffa/minemacs/commit/b715232)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(flymake)** add `flymake-guile` - ([1dcdd21](https://github.com/abougouffa/minemacs/commit/1dcdd21)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** add `flymake-cppcheck` - ([9cd9ede](https://github.com/abougouffa/minemacs/commit/9cd9ede)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** add `julia-ts-mode` - ([1956bee](https://github.com/abougouffa/minemacs/commit/1956bee)) - [@abougouffa](https://github.com/abougouffa)
- **(math)** restore Maxima configuration - ([9a2b356](https://github.com/abougouffa/minemacs/commit/9a2b356)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(builtin)** correct typos in comments - ([4719f17](https://github.com/abougouffa/minemacs/commit/4719f17)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(window)** better window placement for REPL buffers - ([7e66878](https://github.com/abougouffa/minemacs/commit/7e66878)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** show help/helpful/info buffers in a dedicated window - ([16f7d57](https://github.com/abougouffa/minemacs/commit/16f7d57)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** smaller help window (40%) - ([4416062](https://github.com/abougouffa/minemacs/commit/4416062)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.10.0](https://github.com/abougouffa/minemacs/compare/v3.9.1..v3.10.0) - 2023-11-19
#### Bug Fixes
- **(plantuml)** buggy `use-package` block - ([66b23c6](https://github.com/abougouffa/minemacs/commit/66b23c6)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(checkers)** add `flymake-quickdef` - ([8db462e](https://github.com/abougouffa/minemacs/commit/8db462e)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** remove old `flymake-easy` - ([1e8c126](https://github.com/abougouffa/minemacs/commit/1e8c126)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** add `flymake-shellcheck` - ([895a328](https://github.com/abougouffa/minemacs/commit/895a328)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** add `flymake-ruff` - ([cf838c7](https://github.com/abougouffa/minemacs/commit/cf838c7)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** add `flymake-collection` - ([a7ae42c](https://github.com/abougouffa/minemacs/commit/a7ae42c)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** add a backend for Codespell - ([b15b6cb](https://github.com/abougouffa/minemacs/commit/b15b6cb)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** add `flymake-plantuml` - ([b17b039](https://github.com/abougouffa/minemacs/commit/b17b039)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** add Bandit backend for Python - ([c326db8](https://github.com/abougouffa/minemacs/commit/c326db8)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `web-mode` - ([efee175](https://github.com/abougouffa/minemacs/commit/efee175)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(checkers)** remove `flymake-ruff`, present in `flymake-collection` - ([33c0fc3](https://github.com/abougouffa/minemacs/commit/33c0fc3)) - [@abougouffa](https://github.com/abougouffa)
- **(checkers)** `flymake-shellcheck` included in `flymake-collection` - ([8b38ff5](https://github.com/abougouffa/minemacs/commit/8b38ff5)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** add formatters for SQL - ([c6ceef0](https://github.com/abougouffa/minemacs/commit/c6ceef0)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake)** auto enable when relevant - ([e21d176](https://github.com/abougouffa/minemacs/commit/e21d176)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-quickdef)** advice to auto generate `+flymake-*-load` func - ([e12c8a7](https://github.com/abougouffa/minemacs/commit/e12c8a7)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** add a template for shell scripts in Org mode - ([fd1b3ae](https://github.com/abougouffa/minemacs/commit/fd1b3ae)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.9.1](https://github.com/abougouffa/minemacs/compare/v3.9.0..v3.9.1) - 2023-11-18
#### Bug Fixes
- **(vars)** buggy variable set - ([8ca2aba](https://github.com/abougouffa/minemacs/commit/8ca2aba)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.9.0](https://github.com/abougouffa/minemacs/compare/v3.8.1..v3.9.0) - 2023-11-18
#### Bug Fixes
- **(treesit-fold)** wrong `:after` block - ([2745d3b](https://github.com/abougouffa/minemacs/commit/2745d3b)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(formal)** add some modes for formal verification/proof tools - ([05d8c02](https://github.com/abougouffa/minemacs/commit/05d8c02)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- minor code formatting - ([34c10e2](https://github.com/abougouffa/minemacs/commit/34c10e2)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(auctex-latexmk)** remove unnecessary hook - ([db88d81](https://github.com/abougouffa/minemacs/commit/db88d81)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** rename some variables - ([f552bd6](https://github.com/abougouffa/minemacs/commit/f552bd6)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** minor changes - ([24a19a7](https://github.com/abougouffa/minemacs/commit/24a19a7)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(io)** `crux-open-with` provides this functionality - ([0fba2b7](https://github.com/abougouffa/minemacs/commit/0fba2b7)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(apheleia)** remove unnecessary formatters (included in upstream) - ([5acf050](https://github.com/abougouffa/minemacs/commit/5acf050)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** hide time icon - ([728d803](https://github.com/abougouffa/minemacs/commit/728d803)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-multiedit)** minor keybinding changes - ([79cd9c8](https://github.com/abougouffa/minemacs/commit/79cd9c8)) - [@abougouffa](https://github.com/abougouffa)
- **(fonts)** rename function - ([3cb67a1](https://github.com/abougouffa/minemacs/commit/3cb67a1)) - [@abougouffa](https://github.com/abougouffa)
- **(parinfer-rust)** better check for `parinfer-rust` compatibility - ([636d673](https://github.com/abougouffa/minemacs/commit/636d673)) - [@abougouffa](https://github.com/abougouffa)
- **(vterm)** minor keybinding changes - ([7bfc6dd](https://github.com/abougouffa/minemacs/commit/7bfc6dd)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** cleanup irrelevant code - ([b39dfd2](https://github.com/abougouffa/minemacs/commit/b39dfd2)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** more accurate display buffer rules - ([28f5f4f](https://github.com/abougouffa/minemacs/commit/28f5f4f)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([c688dac](https://github.com/abougouffa/minemacs/commit/c688dac)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.8.1](https://github.com/abougouffa/minemacs/compare/v3.8.0..v3.8.1) - 2023-11-17
#### Bug Fixes
- **(treesit-fold)** load only when `treesit` is available - ([c033b65](https://github.com/abougouffa/minemacs/commit/c033b65)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(multi-cursors)** add `multiple-cursors` (dep of `combobulate`) - ([879200e](https://github.com/abougouffa/minemacs/commit/879200e)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** exit minibuffer from anywhere using `S-ESC` - ([070a9bd](https://github.com/abougouffa/minemacs/commit/070a9bd)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** additional adapters for GO and JS - ([664c17b](https://github.com/abougouffa/minemacs/commit/664c17b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.8.0](https://github.com/abougouffa/minemacs/compare/v3.7.0..v3.8.0) - 2023-11-17
#### Bug Fixes
- **(binary)** don't objdump remote files - ([0fb752f](https://github.com/abougouffa/minemacs/commit/0fb752f)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better environement variables management - ([818131c](https://github.com/abougouffa/minemacs/commit/818131c)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** load the right local configuration files - ([eae5274](https://github.com/abougouffa/minemacs/commit/eae5274)) - [@abougouffa](https://github.com/abougouffa)
- **(dirvish)** load immediately if a directory is passed to Emacs as arg - ([5340fd8](https://github.com/abougouffa/minemacs/commit/5340fd8)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** remove unused `ox-pandoc` - ([a544398](https://github.com/abougouffa/minemacs/commit/a544398)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** correctly check if project root is a directory - ([7b31d73](https://github.com/abougouffa/minemacs/commit/7b31d73)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(core)** update init file documentation - ([b65ee69](https://github.com/abougouffa/minemacs/commit/b65ee69)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update documentation - ([32ed27e](https://github.com/abougouffa/minemacs/commit/32ed27e)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** enable local (machine-specific) configurations - ([fb5c52c](https://github.com/abougouffa/minemacs/commit/fb5c52c)) - [@abougouffa](https://github.com/abougouffa)
- **(debug)** initial support for `dape` - ([125d68c](https://github.com/abougouffa/minemacs/commit/125d68c)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** use my `treesit-fold` fork of `ts-fold` - ([0fe2482](https://github.com/abougouffa/minemacs/commit/0fe2482)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** add `+open-with-default-app` - ([b84e73e](https://github.com/abougouffa/minemacs/commit/b84e73e)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make Chezmoi obsolete (migrated the simpler GNU Stow) - ([609781a](https://github.com/abougouffa/minemacs/commit/609781a)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `app-launcher` - ([9881bbc](https://github.com/abougouffa/minemacs/commit/9881bbc)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** minor edit - ([2885f64](https://github.com/abougouffa/minemacs/commit/2885f64)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** setup `gud` parameters separately - ([5c568d8](https://github.com/abougouffa/minemacs/commit/5c568d8)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simplify user config loading and ignoring - ([77d6cf2](https://github.com/abougouffa/minemacs/commit/77d6cf2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(binary)** move the remote file check to `+binary-objdump-p` - ([f4a21da](https://github.com/abougouffa/minemacs/commit/f4a21da)) - [@abougouffa](https://github.com/abougouffa)
- **(blamer)** store avatars in MinEmacs' cache - ([6b6ef59](https://github.com/abougouffa/minemacs/commit/6b6ef59)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** don't exit minibuffer on mouse click - ([396d9ee](https://github.com/abougouffa/minemacs/commit/396d9ee)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add an option to disable all user config files - ([5c6739e](https://github.com/abougouffa/minemacs/commit/5c6739e)) - [@abougouffa](https://github.com/abougouffa)
- **(dape)** initial configuration for adapters - ([9b73891](https://github.com/abougouffa/minemacs/commit/9b73891)) - [@abougouffa](https://github.com/abougouffa)
- **(doom-modeline)** decrease height form 35 to 28 - ([4b3fa3d](https://github.com/abougouffa/minemacs/commit/4b3fa3d)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** make `evil-escape` obsolete - ([b107371](https://github.com/abougouffa/minemacs/commit/b107371)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** bind `+open-with-default-app` to `SPC o SPC` - ([d65c6a0](https://github.com/abougouffa/minemacs/commit/d65c6a0)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `ts-fold` obsolete - ([ee7208a](https://github.com/abougouffa/minemacs/commit/ee7208a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2651ac6](https://github.com/abougouffa/minemacs/commit/2651ac6)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([1018a12](https://github.com/abougouffa/minemacs/commit/1018a12)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.7.0](https://github.com/abougouffa/minemacs/compare/v3.6.1..v3.7.0) - 2023-11-12
#### Bug Fixes
- **(consult-dir)** load after `vertico` - ([7c27876](https://github.com/abougouffa/minemacs/commit/7c27876)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** don't try to add inexistant directories - ([50b8448](https://github.com/abougouffa/minemacs/commit/50b8448)) - [@abougouffa](https://github.com/abougouffa)
- **(sudo-edit)** no extra overhead - ([c82ac39](https://github.com/abougouffa/minemacs/commit/c82ac39)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(faq)** add a Tramp related question - ([dce96a0](https://github.com/abougouffa/minemacs/commit/dce96a0)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** comment commands that are overwritten elsewhere - ([8c6e4d1](https://github.com/abougouffa/minemacs/commit/8c6e4d1)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ace-window)** explicitly include Ace + add keybindings - ([2a7b4b5](https://github.com/abougouffa/minemacs/commit/2a7b4b5)) - [@abougouffa](https://github.com/abougouffa)
- **(files)** add `sudo-edit` - ([faa1299](https://github.com/abougouffa/minemacs/commit/faa1299)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** explicitly add `avy` - ([005f1eb](https://github.com/abougouffa/minemacs/commit/005f1eb)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `with-editor` - ([8ae2ed8](https://github.com/abougouffa/minemacs/commit/8ae2ed8)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** minor Makefile edit - ([8ca3831](https://github.com/abougouffa/minemacs/commit/8ca3831)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** minor tweaks - ([1b3f32c](https://github.com/abougouffa/minemacs/commit/1b3f32c)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** test MinEmacs in daemon mode - ([4efaf20](https://github.com/abougouffa/minemacs/commit/4efaf20)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** run actions on workflow changes - ([a48d307](https://github.com/abougouffa/minemacs/commit/a48d307)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(jiralib2)** avoid confusing variable names - ([8d227b8](https://github.com/abougouffa/minemacs/commit/8d227b8)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(completion)** correctly load `vertico` and `corfu` extensions - ([0159597](https://github.com/abougouffa/minemacs/commit/0159597)) - [@abougouffa](https://github.com/abougouffa)
- **(completion)** some code formatting - ([3fd8e40](https://github.com/abougouffa/minemacs/commit/3fd8e40)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simpler `+sudo-save-buffer` - ([cccbb9a](https://github.com/abougouffa/minemacs/commit/cccbb9a)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** don't reinvent the wheel - ([d746085](https://github.com/abougouffa/minemacs/commit/d746085)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(autoinsert)** disable `auto-insert-mode`, bind to `SPC f i` - ([a07f438](https://github.com/abougouffa/minemacs/commit/a07f438)) - [@abougouffa](https://github.com/abougouffa)
- **(blamer)** a little smaller font size - ([890f6bb](https://github.com/abougouffa/minemacs/commit/890f6bb)) - [@abougouffa](https://github.com/abougouffa)
- **(chezmoi)** correctly load extensions - ([376bcfc](https://github.com/abougouffa/minemacs/commit/376bcfc)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** cleanup accidentally added `elpa` directories - ([aae6016](https://github.com/abougouffa/minemacs/commit/aae6016)) - [@abougouffa](https://github.com/abougouffa)
- **(embark)** better keybindings - ([da58755](https://github.com/abougouffa/minemacs/commit/da58755)) - [@abougouffa](https://github.com/abougouffa)
- **(embark)** bind `embark-act` to `C-²` (for French AZERTY keyboards) - ([14385db](https://github.com/abougouffa/minemacs/commit/14385db)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** accept `html` and `htm` extensions when converting to PDF - ([113515e](https://github.com/abougouffa/minemacs/commit/113515e)) - [@abougouffa](https://github.com/abougouffa)
- **(treemacs)** no rename for keybinding - ([88b7e3e](https://github.com/abougouffa/minemacs/commit/88b7e3e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.6.1](https://github.com/abougouffa/minemacs/compare/v3.6.0..v3.6.1) - 2023-11-10
#### Bug Fixes
- **(daemon)** empty font list if called too early - ([e99e989](https://github.com/abougouffa/minemacs/commit/e99e989)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(completion)** add `consult-dir` - ([877e677](https://github.com/abougouffa/minemacs/commit/877e677)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- use `use-package`'s `:bind` to bind keys - ([3637597](https://github.com/abougouffa/minemacs/commit/3637597)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult)** add keybinding for `consult-yank-pop` - ([8ff02d2](https://github.com/abougouffa/minemacs/commit/8ff02d2)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** replace `org-roam` example with `denote`'s one - ([174f78a](https://github.com/abougouffa/minemacs/commit/174f78a)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** remove `affe`, buggy and stops randomly - ([bf54bcd](https://github.com/abougouffa/minemacs/commit/bf54bcd)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cbad02d](https://github.com/abougouffa/minemacs/commit/cbad02d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.6.0](https://github.com/abougouffa/minemacs/compare/v3.5.0..v3.6.0) - 2023-11-09
#### Bug Fixes
- **(builtin)** use `emacs` pseudo-package instead of `x-win` - ([da0e57d](https://github.com/abougouffa/minemacs/commit/da0e57d)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** load `me-compat` before `me-builtin` - ([c5f39db](https://github.com/abougouffa/minemacs/commit/c5f39db)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(consult)** fill initial query using `consult-customize` - ([600d22b](https://github.com/abougouffa/minemacs/commit/600d22b)) - [@abougouffa](https://github.com/abougouffa)
- move `transient` to the end of `me-builtin` - ([e7d7d07](https://github.com/abougouffa/minemacs/commit/e7d7d07)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cocogitto)** display error message when not in VC directory - ([e2b85e6](https://github.com/abougouffa/minemacs/commit/e2b85e6)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** bind `consult-isearch-history` to `SPC s i` - ([542e5f3](https://github.com/abougouffa/minemacs/commit/542e5f3)) - [@abougouffa](https://github.com/abougouffa)
- **(consult)** add more useful keybindings - ([1fb5994](https://github.com/abougouffa/minemacs/commit/1fb5994)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** update recipe repositories on update - ([9240670](https://github.com/abougouffa/minemacs/commit/9240670)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** use `isearch` instead of `evil-search` - ([1182d73](https://github.com/abougouffa/minemacs/commit/1182d73)) - [@abougouffa](https://github.com/abougouffa)
- **(isearch)** enable ring scrolling using `UP`/`DOWN` & `C-j`/`C-k` - ([04e985f](https://github.com/abougouffa/minemacs/commit/04e985f)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** add keybinding for `keep-lines` - ([6228628](https://github.com/abougouffa/minemacs/commit/6228628)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.5.0](https://github.com/abougouffa/minemacs/compare/v3.4.2..v3.5.0) - 2023-11-08
#### Bug Fixes
- **(core)** first file stuff loaded immediately when in daemon mode - ([5711ac8](https://github.com/abougouffa/minemacs/commit/5711ac8)) - [@abougouffa](https://github.com/abougouffa)
- **(treemacs)** remove `treemacs-evil` [#123] - ([96d6936](https://github.com/abougouffa/minemacs/commit/96d6936)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** move `me-defaults` to `me-builtin` - ([7a645ae](https://github.com/abougouffa/minemacs/commit/7a645ae)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** move generic daemon tweaks to `me-builtin` - ([c38c671](https://github.com/abougouffa/minemacs/commit/c38c671)) - [@abougouffa](https://github.com/abougouffa)
- minor refactor, regenerate loaddefs - ([5807929](https://github.com/abougouffa/minemacs/commit/5807929)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.4.2](https://github.com/abougouffa/minemacs/compare/v3.4.1..v3.4.2) - 2023-11-07
#### Documentation
- **(readme)** minor edit - ([d3a494c](https://github.com/abougouffa/minemacs/commit/d3a494c)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(completion)** use `nerd-icons-corfu` instead of `kind-icons` - ([98c284f](https://github.com/abougouffa/minemacs/commit/98c284f)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** show buffer size in mode line - ([b31d0ef](https://github.com/abougouffa/minemacs/commit/b31d0ef)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** trigger github pages action only on Markdown changes - ([df2e586](https://github.com/abougouffa/minemacs/commit/df2e586)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor edit - ([21e790d](https://github.com/abougouffa/minemacs/commit/21e790d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edits - ([4168a1f](https://github.com/abougouffa/minemacs/commit/4168a1f)) - [@abougouffa](https://github.com/abougouffa)
- **(fonts)** minor change - ([68dc71b](https://github.com/abougouffa/minemacs/commit/68dc71b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.4.1](https://github.com/abougouffa/minemacs/compare/v3.4.0..v3.4.1) - 2023-11-05
#### Bug Fixes
- **(electric)** fix sh/bash keywords extraction from grammar - ([9341311](https://github.com/abougouffa/minemacs/commit/9341311)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(ui)** add support for `pulsar` - ([a526580](https://github.com/abougouffa/minemacs/commit/a526580)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** fine tune when CI get invoked - ([f05b572](https://github.com/abougouffa/minemacs/commit/f05b572)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(init)** better contrast for ASCII banner - ([889ae07](https://github.com/abougouffa/minemacs/commit/889ae07)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** cleanup useless `minemacs-theme` set - ([679693a](https://github.com/abougouffa/minemacs/commit/679693a)) - [@abougouffa](https://github.com/abougouffa)
- code cleanup and minor rewrites - ([4b28273](https://github.com/abougouffa/minemacs/commit/4b28273)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.4.0](https://github.com/abougouffa/minemacs/compare/v3.3.3..v3.4.0) - 2023-11-04
#### Bug Fixes
- **(init)** correct way to handle loading `init.el` in Org async export - ([d8fe648](https://github.com/abougouffa/minemacs/commit/d8fe648)) - [@abougouffa](https://github.com/abougouffa)
- remove problematic `polymode` configuration (makes markdown unusable) - ([3754ecd](https://github.com/abougouffa/minemacs/commit/3754ecd)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(init)** document load and hooks order - ([e3c7bb5](https://github.com/abougouffa/minemacs/commit/e3c7bb5)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** remove redundant information - ([a68f3aa](https://github.com/abougouffa/minemacs/commit/a68f3aa)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** add hooks/files load order - ([48efe7c](https://github.com/abougouffa/minemacs/commit/48efe7c)) - [@abougouffa](https://github.com/abougouffa)
- minor fix in hooks order - ([f581114](https://github.com/abougouffa/minemacs/commit/f581114)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(electric)** electric indent on keywords - ([cf52c65](https://github.com/abougouffa/minemacs/commit/cf52c65)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(init)** add banner - ([3f5c207](https://github.com/abougouffa/minemacs/commit/3f5c207)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** kill splash screen at the last time - ([244b883](https://github.com/abougouffa/minemacs/commit/244b883)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more robust `minemacs-first-*-file-hook` - ([6fb7628](https://github.com/abougouffa/minemacs/commit/6fb7628)) - [@abougouffa](https://github.com/abougouffa)
- **(splash)** add a banner to the splash screen - ([b35987b](https://github.com/abougouffa/minemacs/commit/b35987b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.3.3](https://github.com/abougouffa/minemacs/compare/v3.3.2..v3.3.3) - 2023-11-03
#### Bug Fixes
- **(tramp)** set persistency file correctly - ([da81579](https://github.com/abougouffa/minemacs/commit/da81579)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add `+delete-this-file-and-buffer` - ([46de381](https://github.com/abougouffa/minemacs/commit/46de381)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(init)** minor rewrite - ([d545d04](https://github.com/abougouffa/minemacs/commit/d545d04)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(keybindings)** use my commands for delete/sudo instead of `crux`'s - ([f98c353](https://github.com/abougouffa/minemacs/commit/f98c353)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5cc06b6](https://github.com/abougouffa/minemacs/commit/5cc06b6)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.3.2](https://github.com/abougouffa/minemacs/compare/v3.3.1..v3.3.2) - 2023-11-02
#### Refactoring
- **(elisp)** move Elisp tweaks to `me-builtin` - ([76dbbfa](https://github.com/abougouffa/minemacs/commit/76dbbfa)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(keybindings)** add keybindings for zooming text - ([1470550](https://github.com/abougouffa/minemacs/commit/1470550)) - [@abougouffa](https://github.com/abougouffa)
- **(polymode)** enable in Markdown/GFM - ([4ff674a](https://github.com/abougouffa/minemacs/commit/4ff674a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([2d17f9a](https://github.com/abougouffa/minemacs/commit/2d17f9a)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([1386571](https://github.com/abougouffa/minemacs/commit/1386571)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.3.1](https://github.com/abougouffa/minemacs/compare/v3.3.0..v3.3.1) - 2023-11-01
#### Bug Fixes
- **(latex)** remove buggy dead code - ([8537095](https://github.com/abougouffa/minemacs/commit/8537095)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.3.0](https://github.com/abougouffa/minemacs/compare/v3.2.1..v3.3.0) - 2023-11-01
#### Bug Fixes
- **(corfu)** correct function name in the hook - ([333ad2e](https://github.com/abougouffa/minemacs/commit/333ad2e)) - [@abougouffa](https://github.com/abougouffa)
- **(evil-mc)** move and fix `evil-escape` integration - ([8cc883d](https://github.com/abougouffa/minemacs/commit/8cc883d)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** minor tweaks - ([eaf4581](https://github.com/abougouffa/minemacs/commit/eaf4581)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(corfu)** canonize hook function name - ([cbf53a7](https://github.com/abougouffa/minemacs/commit/cbf53a7)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** move fontification tweaks to `me-latex` - ([22fe929](https://github.com/abougouffa/minemacs/commit/22fe929)) - [@abougouffa](https://github.com/abougouffa)
- **(vertico)** better way to extract extensions directory - ([559abe9](https://github.com/abougouffa/minemacs/commit/559abe9)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(vertico)** better way to extract extensions directory - ([56ac0ae](https://github.com/abougouffa/minemacs/commit/56ac0ae)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(cape)** move `cape-capf-super` stuff to `me-completion` - ([dbfc114](https://github.com/abougouffa/minemacs/commit/dbfc114)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** add a command to do corfu completions in minibuffer - ([c85ba97](https://github.com/abougouffa/minemacs/commit/c85ba97)) - [@abougouffa](https://github.com/abougouffa)
- **(corfu)** use TAB and S-TAB for next/previous - ([cb3f35d](https://github.com/abougouffa/minemacs/commit/cb3f35d)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([de449e3](https://github.com/abougouffa/minemacs/commit/de449e3)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.2.1](https://github.com/abougouffa/minemacs/compare/v3.2.0..v3.2.1) - 2023-10-31
#### Bug Fixes
- **(fonts)** autoload `plistp` (fatal on Emacs 28) - ([3d51c8a](https://github.com/abougouffa/minemacs/commit/3d51c8a)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** correctly manage the different `MINEMACS_IGNORE_*` vars - ([d68e20d](https://github.com/abougouffa/minemacs/commit/d68e20d)) - [@abougouffa](https://github.com/abougouffa)
- don't apply fonts too early - ([c393d1c](https://github.com/abougouffa/minemacs/commit/c393d1c)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(skel)** add `obsolete/me-lexic` to the list - ([621bf07](https://github.com/abougouffa/minemacs/commit/621bf07)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(math)** obsolete Maxima configuration as I'm not using it - ([2bb4192](https://github.com/abougouffa/minemacs/commit/2bb4192)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.2.0](https://github.com/abougouffa/minemacs/compare/v3.1.3..v3.2.0) - 2023-10-31
#### Bug Fixes
- **(backports)** correct load path for back ports - ([3627f8c](https://github.com/abougouffa/minemacs/commit/3627f8c)) - [@abougouffa](https://github.com/abougouffa)
- treat `tree-sitter` as pseudo package only when built with treesit - ([62f5988](https://github.com/abougouffa/minemacs/commit/62f5988)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(changelog)** minor fix - ([b873dd4](https://github.com/abougouffa/minemacs/commit/b873dd4)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** update the documentation - ([74d5dd1](https://github.com/abougouffa/minemacs/commit/74d5dd1)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(undo)** use builtin `undo-redo` instead of `undo-fu` - ([e9f1f4f](https://github.com/abougouffa/minemacs/commit/e9f1f4f)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** add an option to always demand packages - ([49cdf0d](https://github.com/abougouffa/minemacs/commit/49cdf0d)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** enable Emacs 28, 29 and snapshot on Linux and MacOS - ([5869e1b](https://github.com/abougouffa/minemacs/commit/5869e1b)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** run a step of MinEmacs in always demand mode - ([6d430dc](https://github.com/abougouffa/minemacs/commit/6d430dc)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(ci)** minor refactor of init script - ([ca2bad4](https://github.com/abougouffa/minemacs/commit/ca2bad4)) - [@abougouffa](https://github.com/abougouffa)
- **(highlight-numbers)** rewrite regexp with `rx` - ([d46593a](https://github.com/abougouffa/minemacs/commit/d46593a)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** make use of `:unless` - ([b04adb1](https://github.com/abougouffa/minemacs/commit/b04adb1)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(ci)** simplify `minemacs-root-dir` deduction - ([988463d](https://github.com/abougouffa/minemacs/commit/988463d)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** load fonts early - ([2c2d1ce](https://github.com/abougouffa/minemacs/commit/2c2d1ce)) - [@abougouffa](https://github.com/abougouffa)
- **(lexic)** make lexic obsolete - ([9e15401](https://github.com/abougouffa/minemacs/commit/9e15401)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete)** sync keybindings for `expand-region` to match `expreg` - ([87fc42e](https://github.com/abougouffa/minemacs/commit/87fc42e)) - [@abougouffa](https://github.com/abougouffa)
- **(systemd)** use the Company backend as Capf - ([aea6966](https://github.com/abougouffa/minemacs/commit/aea6966)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** nom need to add AWK support, merged upstream - ([c16c71d](https://github.com/abougouffa/minemacs/commit/c16c71d)) - [@abougouffa](https://github.com/abougouffa)
- **(x86-lookup)** auto download the PDF if not available - ([f677cc2](https://github.com/abougouffa/minemacs/commit/f677cc2)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([cede9ee](https://github.com/abougouffa/minemacs/commit/cede9ee)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.1.3](https://github.com/abougouffa/minemacs/compare/v3.1.2..v3.1.3) - 2023-10-29
#### Bug Fixes
- **(fonts)** correctly set fonts with `custom-theme-set-faces` - ([c5d39f9](https://github.com/abougouffa/minemacs/commit/c5d39f9)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** mark `seq` as builtin - ([d72f9ad](https://github.com/abougouffa/minemacs/commit/d72f9ad)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** move backports to a separate directory - ([1f24de5](https://github.com/abougouffa/minemacs/commit/1f24de5)) - [@abougouffa](https://github.com/abougouffa)
- **(fonts)** inherit `default` in `fixed-pitch` and `fixed-pitch-serif` - ([01c0dea](https://github.com/abougouffa/minemacs/commit/01c0dea)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([99a5ae8](https://github.com/abougouffa/minemacs/commit/99a5ae8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.1.2](https://github.com/abougouffa/minemacs/compare/v3.1.1..v3.1.2) - 2023-10-28
#### Bug Fixes
- **(vars)** fix environment variable for disabling `early-config.el` - ([7e22ce2](https://github.com/abougouffa/minemacs/commit/7e22ce2)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** minor update - ([35878bd](https://github.com/abougouffa/minemacs/commit/35878bd)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- remove the disclaimer on Windows - ([6c545b6](https://github.com/abougouffa/minemacs/commit/6c545b6)) - [@abougouffa](https://github.com/abougouffa)
- simplify the code - ([d6ba81b](https://github.com/abougouffa/minemacs/commit/d6ba81b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(eaf)** use `:when` to condition `use-package` - ([a182961](https://github.com/abougouffa/minemacs/commit/a182961)) - [@abougouffa](https://github.com/abougouffa)
- **(early-init)** better titlebar on MacOS! - ([f7efd93](https://github.com/abougouffa/minemacs/commit/f7efd93)) - [@abougouffa](https://github.com/abougouffa)
- **(markdown)** add some local keybindings - ([3c8fce8](https://github.com/abougouffa/minemacs/commit/3c8fce8)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.1.1](https://github.com/abougouffa/minemacs/compare/v3.1.0..v3.1.1) - 2023-10-28
#### Tweaks
- **(cocogitto)** display a message after finishing - ([92bd36b](https://github.com/abougouffa/minemacs/commit/92bd36b)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.1.0](https://github.com/abougouffa/minemacs/compare/v3.0.3..v3.1.0) - 2023-10-28
#### Bug Fixes
- **(window)** fix warning window position - ([b2e48f0](https://github.com/abougouffa/minemacs/commit/b2e48f0)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(calfw)** document a command - ([19f2c37](https://github.com/abougouffa/minemacs/commit/19f2c37)) - [@abougouffa](https://github.com/abougouffa)
- **(faq)** convert to markdown - ([c75c88f](https://github.com/abougouffa/minemacs/commit/c75c88f)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** add support for editing Gitlab CI YAML files - ([089ded9](https://github.com/abougouffa/minemacs/commit/089ded9)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(modules)** remove the obsolete `me-lisp` module - ([20c6280](https://github.com/abougouffa/minemacs/commit/20c6280)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(consult-eglot)** better check for LSP - ([fe4316a](https://github.com/abougouffa/minemacs/commit/fe4316a)) - [@abougouffa](https://github.com/abougouffa)
- **(consult-notes)** minor refactor - ([a05792a](https://github.com/abougouffa/minemacs/commit/a05792a)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** sort modules, enable `me-workspaces`, disable `me-binary` - ([c1b3ec0](https://github.com/abougouffa/minemacs/commit/c1b3ec0)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** minor refactor - ([172a803](https://github.com/abougouffa/minemacs/commit/172a803)) - [@abougouffa](https://github.com/abougouffa)
- **(fonts)** add more Iosevka fonts - ([44083e4](https://github.com/abougouffa/minemacs/commit/44083e4)) - [@abougouffa](https://github.com/abougouffa)
- **(nov)** remove unneeded UI tweaks - ([4e3fd06](https://github.com/abougouffa/minemacs/commit/4e3fd06)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** make `org-present` obsolete - ([c6445db](https://github.com/abougouffa/minemacs/commit/c6445db)) - [@abougouffa](https://github.com/abougouffa)
- **(ros)** autoload some commands - ([5e6a410](https://github.com/abougouffa/minemacs/commit/5e6a410)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** minor `config.el` refactor - ([3a01518](https://github.com/abougouffa/minemacs/commit/3a01518)) - [@abougouffa](https://github.com/abougouffa)
- **(spell-fu)** remove obsolete alias - ([6a03238](https://github.com/abougouffa/minemacs/commit/6a03238)) - [@abougouffa](https://github.com/abougouffa)
- remove unneeded `:mode` blocks - ([05ca6d2](https://github.com/abougouffa/minemacs/commit/05ca6d2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.0.3](https://github.com/abougouffa/minemacs/compare/v3.0.2..v3.0.3) - 2023-10-28
#### Bug Fixes
- **(fonts)** buggy check for installed fonts - ([31c1029](https://github.com/abougouffa/minemacs/commit/31c1029)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(fonts)** add some documentation - ([c458053](https://github.com/abougouffa/minemacs/commit/c458053)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- add Github pages deployment - ([10aa065](https://github.com/abougouffa/minemacs/commit/10aa065)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(fonts)** more default fonts settings - ([fbf6429](https://github.com/abougouffa/minemacs/commit/fbf6429)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** more font setting examples in `skel/config.el` - ([129d917](https://github.com/abougouffa/minemacs/commit/129d917)) - [@abougouffa](https://github.com/abougouffa)
- better modules loading - ([de54ff2](https://github.com/abougouffa/minemacs/commit/de54ff2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.0.2](https://github.com/abougouffa/minemacs/compare/v3.0.1..v3.0.2) - 2023-10-27
#### Tweaks
- **(fonts)** code cleanup - ([dbaccf0](https://github.com/abougouffa/minemacs/commit/dbaccf0)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.0.1](https://github.com/abougouffa/minemacs/compare/v3.0.0..v3.0.1) - 2023-10-27
#### Documentation
- **(core)** add a description to `minemacs-fonts-plist` - ([53ce66b](https://github.com/abougouffa/minemacs/commit/53ce66b)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** minor edits - ([61f806e](https://github.com/abougouffa/minemacs/commit/61f806e)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** more font setting examples - ([f627965](https://github.com/abougouffa/minemacs/commit/f627965)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(fonts)** accept full `font-spec/set-face-attribute` arguments (#120) - ([6ec46f1](https://github.com/abougouffa/minemacs/commit/6ec46f1)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** update loaddefs - ([a533aea](https://github.com/abougouffa/minemacs/commit/a533aea)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v3.0.0](https://github.com/abougouffa/minemacs/compare/v2.0.2..v3.0.0) - 2023-10-26
#### Documentation
- **(readme)** convert to Markdown - ([817db39](https://github.com/abougouffa/minemacs/commit/817db39)) - [@abougouffa](https://github.com/abougouffa)
- **(readme)** fix link - ([7c26e8a](https://github.com/abougouffa/minemacs/commit/7c26e8a)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add an example to how to fix `vterm` compilation - ([19a5213](https://github.com/abougouffa/minemacs/commit/19a5213)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** change font handling - ([7c4a8b8](https://github.com/abougouffa/minemacs/commit/7c4a8b8)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add helper function `+font-installed-p` - ([0200750](https://github.com/abougouffa/minemacs/commit/0200750)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** add an option to keep the "check if disabled" advice - ([f08c370](https://github.com/abougouffa/minemacs/commit/f08c370)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(makefile)** minor tweak - ([c44d135](https://github.com/abougouffa/minemacs/commit/c44d135)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(project)** use `use-package` `:hook` block - ([96b6d8b](https://github.com/abougouffa/minemacs/commit/96b6d8b)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(nerd-icons)** auto-install fonts when they aren't installed - ([6615a59](https://github.com/abougouffa/minemacs/commit/6615a59)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** include examples of the new font setting - ([35dad97](https://github.com/abougouffa/minemacs/commit/35dad97)) - [@abougouffa](https://github.com/abougouffa)
- **(vterm)** don't install if Emacs don't support modules - ([c86aa7c](https://github.com/abougouffa/minemacs/commit/c86aa7c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v2.0.2](https://github.com/abougouffa/minemacs/compare/v2.0.1..v2.0.2) - 2023-10-25
#### Bug Fixes
- **(cocogitto)** fix the `+cocogitto-bump` command - ([45a4e0a](https://github.com/abougouffa/minemacs/commit/45a4e0a)) - [@abougouffa](https://github.com/abougouffa)
- **(cocogitto)** fix change log generation - ([91abc8a](https://github.com/abougouffa/minemacs/commit/91abc8a)) - [@abougouffa](https://github.com/abougouffa)
- **(dashboard)** adapt to the new `use-package` disabled packages check (#119) - ([a1c959b](https://github.com/abougouffa/minemacs/commit/a1c959b)) - [@abougouffa](https://github.com/abougouffa)
- **(use-package)** fix the real cause of #119 - ([00345fe](https://github.com/abougouffa/minemacs/commit/00345fe)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(use-package)** add comment - ([1adbfeb](https://github.com/abougouffa/minemacs/commit/1adbfeb)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add helper function `+varplist-get` - ([f5f8474](https://github.com/abougouffa/minemacs/commit/f5f8474)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** minor refactoring - ([3df04e7](https://github.com/abougouffa/minemacs/commit/3df04e7)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(dashboard)** the bug was caused by the `use-package` advice - ([1f62efd](https://github.com/abougouffa/minemacs/commit/1f62efd)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(dashboard)** use `:unless` instead of `:when (not ...)` - ([729f656](https://github.com/abougouffa/minemacs/commit/729f656)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** don't fail when trying to load inexistant file in `+load` - ([18a47bd](https://github.com/abougouffa/minemacs/commit/18a47bd)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v2.0.1](https://github.com/abougouffa/minemacs/compare/v2.0.0..v2.0.1) - 2023-10-22
#### Bug Fixes
- **(evil)** evil repeat error - ([69b2258](https://github.com/abougouffa/minemacs/commit/69b2258)) - [@donneyluck](https://github.com/donneyluck)
- **(ts-fold)** ensure enabling on `yaml-ts-mode` - ([d3565cc](https://github.com/abougouffa/minemacs/commit/d3565cc)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** fix CI badge - ([30cc7a2](https://github.com/abougouffa/minemacs/commit/30cc7a2)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** cleanup MacOS M1 test (paid) - ([6f014c8](https://github.com/abougouffa/minemacs/commit/6f014c8)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** fix failure detection - ([9b63d42](https://github.com/abougouffa/minemacs/commit/9b63d42)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** try to enable testing on Apple M1 - ([a4f95c8](https://github.com/abougouffa/minemacs/commit/a4f95c8)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** split CI jobs by OS to separate workflows - ([15c1c17](https://github.com/abougouffa/minemacs/commit/15c1c17)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better failure detection - ([665681d](https://github.com/abougouffa/minemacs/commit/665681d)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** fix error extraction regexp - ([6e739da](https://github.com/abougouffa/minemacs/commit/6e739da)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** use an independent initialization script for CI - ([a6c7c7e](https://github.com/abougouffa/minemacs/commit/a6c7c7e)) - [@abougouffa](https://github.com/abougouffa)
- **(make)** fix CI rule - ([c76ae0c](https://github.com/abougouffa/minemacs/commit/c76ae0c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- remove CI specific code - ([e1286b3](https://github.com/abougouffa/minemacs/commit/e1286b3)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(eglot)** register LSP server for AWK - ([5587589](https://github.com/abougouffa/minemacs/commit/5587589)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v2.0.0](https://github.com/abougouffa/minemacs/compare/v1.7.1..v2.0.0) - 2023-10-21
#### Bug Fixes
- **(vterm)** disable on Windows - ([e082919](https://github.com/abougouffa/minemacs/commit/e082919)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** mention the CI actions - ([ef86e67](https://github.com/abougouffa/minemacs/commit/ef86e67)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** rename `build.yaml` to `ci.yaml` - ([5087247](https://github.com/abougouffa/minemacs/commit/5087247)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** refactor and enable MacOS and Windows - ([ee6eb96](https://github.com/abougouffa/minemacs/commit/ee6eb96)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** test on MacOS - ([f3d160f](https://github.com/abougouffa/minemacs/commit/f3d160f)) - [@abougouffa](https://github.com/abougouffa)
- **(cocogitto)** merge push and pull request checks - ([4a5c562](https://github.com/abougouffa/minemacs/commit/4a5c562)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(keybindings)** bind ecryptfs/netextender only when relevant - ([5724330](https://github.com/abougouffa/minemacs/commit/5724330)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.7.1](https://github.com/abougouffa/minemacs/compare/v1.7.0..v1.7.1) - 2023-10-21
#### Bug Fixes
- **(use-package)** better checking for disabled packages - ([71b2ad6](https://github.com/abougouffa/minemacs/commit/71b2ad6)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** better reporting - ([db7606a](https://github.com/abougouffa/minemacs/commit/db7606a)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** fix failure checking - ([3eec9f1](https://github.com/abougouffa/minemacs/commit/3eec9f1)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** fix Emacs version extraction regexp - ([df45156](https://github.com/abougouffa/minemacs/commit/df45156)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** disable `fail-fast` strategy - ([fe90ae9](https://github.com/abougouffa/minemacs/commit/fe90ae9)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better reporting - ([64b6f26](https://github.com/abougouffa/minemacs/commit/64b6f26)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** enable Emacs 28 & 29 + better reporting - ([8e53daf](https://github.com/abougouffa/minemacs/commit/8e53daf)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better report generation - ([516ec07](https://github.com/abougouffa/minemacs/commit/516ec07)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better report generation - ([fc610e5](https://github.com/abougouffa/minemacs/commit/fc610e5)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- better conditional loading - ([b27a088](https://github.com/abougouffa/minemacs/commit/b27a088)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(plantuml)** remove unneeded exec mode tweak - ([9e549f9](https://github.com/abougouffa/minemacs/commit/9e549f9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.7.0](https://github.com/abougouffa/minemacs/compare/v1.6.1..v1.7.0) - 2023-10-21
#### Bug Fixes
- **(blamer)** edge case when launching Emacs from tty - ([95b52ae](https://github.com/abougouffa/minemacs/commit/95b52ae)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** check for `fringe` before using `set-fringe-style` - ([9a34479](https://github.com/abougouffa/minemacs/commit/9a34479)) - [@abougouffa](https://github.com/abougouffa)
- **(dockerfile-mode)** disable when Emacs have builtin tree-sitter - ([9919244](https://github.com/abougouffa/minemacs/commit/9919244)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** correctly call `+delete-file-or-directory` - ([ebb8ed5](https://github.com/abougouffa/minemacs/commit/ebb8ed5)) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** pin to a stable commit - ([bbc8bce](https://github.com/abougouffa/minemacs/commit/bbc8bce)) - [@abougouffa](https://github.com/abougouffa)
- fix several deferred packages issues - ([60145fd](https://github.com/abougouffa/minemacs/commit/60145fd)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** add workflow status badge - ([792a5f5](https://github.com/abougouffa/minemacs/commit/792a5f5)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** add `awk-ts-mode` - ([66657f7](https://github.com/abougouffa/minemacs/commit/66657f7)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(ci)** minor tweaks for CI - ([c62a1cf](https://github.com/abougouffa/minemacs/commit/c62a1cf)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better management of output data - ([58456af](https://github.com/abougouffa/minemacs/commit/58456af)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** better CI run - ([10594e0](https://github.com/abougouffa/minemacs/commit/10594e0)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** run MinEmacs directly in CI - ([f2bc02a](https://github.com/abougouffa/minemacs/commit/f2bc02a)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** run MinEmacs in CI mode, with all modules - ([40109b3](https://github.com/abougouffa/minemacs/commit/40109b3)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** WIP - ([ec6d024](https://github.com/abougouffa/minemacs/commit/ec6d024)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** WIP - ([8bab9ea](https://github.com/abougouffa/minemacs/commit/8bab9ea)) - [@abougouffa](https://github.com/abougouffa)
- **(ci)** WIP build CI - ([35ea7da](https://github.com/abougouffa/minemacs/commit/35ea7da)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- edit the `non-executable-script.sh` example - ([a1be1b5](https://github.com/abougouffa/minemacs/commit/a1be1b5)) - [@abougouffa](https://github.com/abougouffa)
- add an example for `non-executable-script.sh` - ([e316607](https://github.com/abougouffa/minemacs/commit/e316607)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- move advice from `me-builtin` to `me-defaults` - ([d285a56](https://github.com/abougouffa/minemacs/commit/d285a56)) - [@abougouffa](https://github.com/abougouffa)
- extract reusable `+delete-file-or-directory` function, refactor code - ([597df98](https://github.com/abougouffa/minemacs/commit/597df98)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** load all packages when MinEmacs is invoked in CI context - ([09bc0be](https://github.com/abougouffa/minemacs/commit/09bc0be)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** auto-cleanup MinEmacs directory (`eln-cache`, ...) - ([a8abb44](https://github.com/abougouffa/minemacs/commit/a8abb44)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** cleanup zombie projects on exit - ([fd07d8b](https://github.com/abougouffa/minemacs/commit/fd07d8b)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** refactor and rename straight cache cleaning function - ([f06b06a](https://github.com/abougouffa/minemacs/commit/f06b06a)) - [@abougouffa](https://github.com/abougouffa)
- **(straight)** cleanup old byte compiled Elisp - ([733c89b](https://github.com/abougouffa/minemacs/commit/733c89b)) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** remove the explicit pin as upstream gets fixed - ([b423753](https://github.com/abougouffa/minemacs/commit/b423753)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** add support for AWK - ([3ad602e](https://github.com/abougouffa/minemacs/commit/3ad602e)) - [@abougouffa](https://github.com/abougouffa)
- make EAF obsolete - ([8c4ff6f](https://github.com/abougouffa/minemacs/commit/8c4ff6f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5971a8a](https://github.com/abougouffa/minemacs/commit/5971a8a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([183ebd9](https://github.com/abougouffa/minemacs/commit/183ebd9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.6.1](https://github.com/abougouffa/minemacs/compare/v1.6.0..v1.6.1) - 2023-10-16
#### Bug Fixes
- **(+writing-mode)** fix enable/disable hooks - ([7f21ba9](https://github.com/abougouffa/minemacs/commit/7f21ba9)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** revert `minemacs-first-org-file`, it causes Org export to fail - ([2466391](https://github.com/abougouffa/minemacs/commit/2466391)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(blamer)** disable when in `+writing-mode` - ([478d4f6](https://github.com/abougouffa/minemacs/commit/478d4f6)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** more generic `+env-save` - ([46fda3c](https://github.com/abougouffa/minemacs/commit/46fda3c)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.6.0](https://github.com/abougouffa/minemacs/compare/v1.5.0..v1.6.0) - 2023-10-15
#### Features
- **(core)** add `+make-first-file-hook!` - ([5bc1777](https://github.com/abougouffa/minemacs/commit/5bc1777)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** define some first file hooks - ([580a089](https://github.com/abougouffa/minemacs/commit/580a089)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(extras)** simplify `+cocogitto-bump` - ([702c268](https://github.com/abougouffa/minemacs/commit/702c268)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** conditionally disable `dockerfile-mode` MinEmacs way - ([8785b5a](https://github.com/abougouffa/minemacs/commit/8785b5a)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(treesit-auto)** remove protobuf temporary hack, fixed upstream - ([cc7d74e](https://github.com/abougouffa/minemacs/commit/cc7d74e)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(elisp)** make use of minemacs-first-elisp-file - ([1a3c3a5](https://github.com/abougouffa/minemacs/commit/1a3c3a5)) - [@abougouffa](https://github.com/abougouffa)
- **(jiralib2)** add a helper command to get active ticket IDs - ([58f1eca](https://github.com/abougouffa/minemacs/commit/58f1eca)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** make use of first org file hook to ensure deferring Org - ([dc4807b](https://github.com/abougouffa/minemacs/commit/dc4807b)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add a configuration example for `jiralib2` - ([773aadd](https://github.com/abougouffa/minemacs/commit/773aadd)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([b05eb06](https://github.com/abougouffa/minemacs/commit/b05eb06)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.5.0](https://github.com/abougouffa/minemacs/compare/v1.4.10..v1.5.0) - 2023-10-14
#### Features
- **(core)** add `+first-line-empty-p` helper - ([d83aa23](https://github.com/abougouffa/minemacs/commit/d83aa23)) - [@abougouffa](https://github.com/abougouffa)
- **(notes)** migrate from `org-roam` to `denote` - ([ef9fbb6](https://github.com/abougouffa/minemacs/commit/ef9fbb6)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** add support for `jiralib2` WIP - ([6c562ac](https://github.com/abougouffa/minemacs/commit/6c562ac)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(version)** v1.4.11 - ([7256aba](https://github.com/abougouffa/minemacs/commit/7256aba)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(builtin)** conditionally enable `treesit` stuff the MinEmacs way - ([b50775d](https://github.com/abougouffa/minemacs/commit/b50775d)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(denote)** unbind `consult-notes-org-roam` autoloads - ([ff3c86f](https://github.com/abougouffa/minemacs/commit/ff3c86f)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([eeb9e1d](https://github.com/abougouffa/minemacs/commit/eeb9e1d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.10](https://github.com/abougouffa/minemacs/compare/v1.4.9..v1.4.10) - 2023-10-10
#### Bug Fixes
- **(recentf)** more explicit exclusion regexps - ([79e93fd](https://github.com/abougouffa/minemacs/commit/79e93fd)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** fix language name to exclude Protobuf - ([0a2d2de](https://github.com/abougouffa/minemacs/commit/0a2d2de)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(vc)** add `blamer` support - ([e44184f](https://github.com/abougouffa/minemacs/commit/e44184f)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor changes in `minemacs-update-restore-locked` - ([7ac449f](https://github.com/abougouffa/minemacs/commit/7ac449f)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(blamer)** make font size 10% smaller than default - ([fcdf95e](https://github.com/abougouffa/minemacs/commit/fcdf95e)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** rename command + unselect region after kill - ([775f350](https://github.com/abougouffa/minemacs/commit/775f350)) - [@abougouffa](https://github.com/abougouffa)
- **(hydra)** hydra is behaving strangely, it gets loaded immediately - ([4d352ce](https://github.com/abougouffa/minemacs/commit/4d352ce)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** remove `evil-textobj-tree-sitter` - ([d4ac03d](https://github.com/abougouffa/minemacs/commit/d4ac03d)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([27da8da](https://github.com/abougouffa/minemacs/commit/27da8da)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([de76af5](https://github.com/abougouffa/minemacs/commit/de76af5)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.9](https://github.com/abougouffa/minemacs/compare/v1.4.8..v1.4.9) - 2023-10-09
#### Bug Fixes
- **(cocogitto)** buggy command on pre-release - ([e2d8a7a](https://github.com/abougouffa/minemacs/commit/e2d8a7a)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(changelog)** add historical v0.1.0 + links to old commits - ([23ca7fc](https://github.com/abougouffa/minemacs/commit/23ca7fc)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- **(cocogitto)** bump `actions/checkout` and `cocogitto/cocogitto-action` - ([ea4b575](https://github.com/abougouffa/minemacs/commit/ea4b575)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** do not save/load environment variables in Windows - ([9f8ec81](https://github.com/abougouffa/minemacs/commit/9f8ec81)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** enable restoring lockfile from backup - ([44c0b35](https://github.com/abougouffa/minemacs/commit/44c0b35)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.8](https://github.com/abougouffa/minemacs/compare/v1.4.7..v1.4.8) - 2023-10-08
#### Documentation
- **(skel)** update modules documentation and examples - ([020af86](https://github.com/abougouffa/minemacs/commit/020af86)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** add some helper commands for text cleanup - ([396993a](https://github.com/abougouffa/minemacs/commit/396993a)) - [@abougouffa](https://github.com/abougouffa)
- **(extra)** add a convenience command for Cocogitto - ([24f73eb](https://github.com/abougouffa/minemacs/commit/24f73eb)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** regenerate loaddefs - ([307dbef](https://github.com/abougouffa/minemacs/commit/307dbef)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** bind a helper command - ([6e2d8a4](https://github.com/abougouffa/minemacs/commit/6e2d8a4)) - [@abougouffa](https://github.com/abougouffa)
- **(markdown)** better integration with Pandoc - ([ae73311](https://github.com/abougouffa/minemacs/commit/ae73311)) - [@abougouffa](https://github.com/abougouffa)
- **(tapspaces)** ensure reading package list at startup - ([b512fce](https://github.com/abougouffa/minemacs/commit/b512fce)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([698fca2](https://github.com/abougouffa/minemacs/commit/698fca2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.7](https://github.com/abougouffa/minemacs/compare/v1.4.6..v1.4.7) - 2023-10-07
#### Bug Fixes
- **(core)** autoload error - ([6c53635](https://github.com/abougouffa/minemacs/commit/6c53635)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.6](https://github.com/abougouffa/minemacs/compare/v1.4.5..v1.4.6) - 2023-10-07
#### Refactoring
- **(latex)** move some vars from builtin to latex - ([65190dd](https://github.com/abougouffa/minemacs/commit/65190dd)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** autoload some `vc-git` commands - ([17149e0](https://github.com/abougouffa/minemacs/commit/17149e0)) - [@abougouffa](https://github.com/abougouffa)
- **(latex)** additional tweaks - ([92c28a9](https://github.com/abougouffa/minemacs/commit/92c28a9)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.5](https://github.com/abougouffa/minemacs/compare/v1.4.4..v1.4.5) - 2023-10-07
#### Bug Fixes
- **(+writing-mode)** autoload `+writing-global-mode` - ([f4291ce](https://github.com/abougouffa/minemacs/commit/f4291ce)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(+writing-mode)** disassociate major modes from derived major modes - ([d5f6104](https://github.com/abougouffa/minemacs/commit/d5f6104)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** regenerate loaddefs - ([180b550](https://github.com/abougouffa/minemacs/commit/180b550)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.4](https://github.com/abougouffa/minemacs/compare/v1.4.3..v1.4.4) - 2023-10-07
#### Bug Fixes
- **(+writing-mode)** enable globally for a set of modes - ([38d5339](https://github.com/abougouffa/minemacs/commit/38d5339)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(ui)** add keybinding for `+writing-global-mode` - ([5ea190d](https://github.com/abougouffa/minemacs/commit/5ea190d)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.3](https://github.com/abougouffa/minemacs/compare/v1.4.2..v1.4.3) - 2023-10-07
#### Bug Fixes
- **(treesit-auto)** minor bug when deleting `protobuf` - ([28cd503](https://github.com/abougouffa/minemacs/commit/28cd503)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- add some examples and file templates - ([f3c1df2](https://github.com/abougouffa/minemacs/commit/f3c1df2)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.2](https://github.com/abougouffa/minemacs/compare/v1.4.1..v1.4.2) - 2023-10-07
#### Bug Fixes
- **(core)** fix a comment to make `parinfer-rust-mode` happy - ([9551edc](https://github.com/abougouffa/minemacs/commit/9551edc)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(builtin)** fix a typo - ([1a5b6d1](https://github.com/abougouffa/minemacs/commit/1a5b6d1)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(+writing-mode)** add `+writing-global-mode` (fix #117) - ([2cabf08](https://github.com/abougouffa/minemacs/commit/2cabf08)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `minemacs-update-restore-locked` - ([b0276fa](https://github.com/abougouffa/minemacs/commit/b0276fa)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** default `speedbar` config from Crafted Emacs - ([b0fd9f4](https://github.com/abougouffa/minemacs/commit/b0fd9f4)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better error handling in `minemacs-update-restore-locked` - ([ffe08fa](https://github.com/abougouffa/minemacs/commit/ffe08fa)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** add pulse line - ([7d54316](https://github.com/abougouffa/minemacs/commit/7d54316)) - [@abougouffa](https://github.com/abougouffa)
- **(treesit-auto)** temporary disable `protobuf` (see #114) - ([af0f970](https://github.com/abougouffa/minemacs/commit/af0f970)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([51ac487](https://github.com/abougouffa/minemacs/commit/51ac487)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.1](https://github.com/abougouffa/minemacs/compare/v1.4.0..v1.4.1) - 2023-10-05
#### Features
- **(natural-langs)** add support for `reverso` - ([da98f62](https://github.com/abougouffa/minemacs/commit/da98f62)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.4.0](https://github.com/abougouffa/minemacs/compare/v1.3.0..v1.4.0) - 2023-10-05
#### Bug Fixes
- **(tramp)** fix Tramp bug (thanks Phundrak) - ([d40fb21](https://github.com/abougouffa/minemacs/commit/d40fb21)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(defaults)** enable `context-menu-mode` - ([505c301](https://github.com/abougouffa/minemacs/commit/505c301)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add support for `makefile-executor` - ([e65bb1a](https://github.com/abougouffa/minemacs/commit/e65bb1a)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(hl-todo)** add the `FIX` keyword - ([5dd383e](https://github.com/abougouffa/minemacs/commit/5dd383e)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete)** add `ts-fold` to `tree-sitter` - ([8e48553](https://github.com/abougouffa/minemacs/commit/8e48553)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add `forge` config example to `config.el` - ([c2c900a](https://github.com/abougouffa/minemacs/commit/c2c900a)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([c990ed3](https://github.com/abougouffa/minemacs/commit/c990ed3)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.3.0](https://github.com/abougouffa/minemacs/compare/v1.2.0..v1.3.0) - 2023-09-17
#### Bug Fixes
- **(core)** correctly handle edge cases in `+env-save` - ([6047d92](https://github.com/abougouffa/minemacs/commit/6047d92)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** respect `visual-line-mode` - ([672f31c](https://github.com/abougouffa/minemacs/commit/672f31c)) - [@abougouffa](https://github.com/abougouffa)
- **(regexp)** fix keybindings - ([49d7ff7](https://github.com/abougouffa/minemacs/commit/49d7ff7)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(emacs-lisp)** add a comment - ([0c4b0a4](https://github.com/abougouffa/minemacs/commit/0c4b0a4)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update copyright year - ([ae42456](https://github.com/abougouffa/minemacs/commit/ae42456)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(prog)** add support for FreeBasic via `fb-mode` - ([510c299](https://github.com/abougouffa/minemacs/commit/510c299)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- cleanup changelog - ([2ea7834](https://github.com/abougouffa/minemacs/commit/2ea7834)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- clean file header - ([d2de9a2](https://github.com/abougouffa/minemacs/commit/d2de9a2)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(builtin)** disable auto desktop saving - ([fcdb25e](https://github.com/abougouffa/minemacs/commit/fcdb25e)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** bump `codelldb` and `cpptools` versions - ([25c78f0](https://github.com/abougouffa/minemacs/commit/25c78f0)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** bump `cpptools` version - ([8092276](https://github.com/abougouffa/minemacs/commit/8092276)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** replace `expand-region` with `expreg` - ([ea64eaa](https://github.com/abougouffa/minemacs/commit/ea64eaa)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** restore `evil-snipe` - ([a4f2f05](https://github.com/abougouffa/minemacs/commit/a4f2f05)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** use π as prefix for `project-` - ([efef895](https://github.com/abougouffa/minemacs/commit/efef895)) - [@abougouffa](https://github.com/abougouffa)
- **(lisp)** drop support for `me-lisp` - ([a616cb8](https://github.com/abougouffa/minemacs/commit/a616cb8)) - [@abougouffa](https://github.com/abougouffa)
- **(obsolete)** minor edit in the `unicode-fonts` package - ([7f96d85](https://github.com/abougouffa/minemacs/commit/7f96d85)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** remove useless class - ([31c5960](https://github.com/abougouffa/minemacs/commit/31c5960)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** revert FACILE format to PDF/A-1b - ([62aa675](https://github.com/abougouffa/minemacs/commit/62aa675)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** additional latex classes - ([74e6914](https://github.com/abougouffa/minemacs/commit/74e6914)) - [@abougouffa](https://github.com/abougouffa)
- **(vc)** enable `magit-todos` - ([5244743](https://github.com/abougouffa/minemacs/commit/5244743)) - [@abougouffa](https://github.com/abougouffa)
- **(xml)** auto detect `*.xmpi` files - ([ee119c7](https://github.com/abougouffa/minemacs/commit/ee119c7)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([700c92b](https://github.com/abougouffa/minemacs/commit/700c92b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d12d29b](https://github.com/abougouffa/minemacs/commit/d12d29b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6da0a15](https://github.com/abougouffa/minemacs/commit/6da0a15)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([aaf5857](https://github.com/abougouffa/minemacs/commit/aaf5857)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0d4bb85](https://github.com/abougouffa/minemacs/commit/0d4bb85)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([917b968](https://github.com/abougouffa/minemacs/commit/917b968)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d40764a](https://github.com/abougouffa/minemacs/commit/d40764a)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.2.0](https://github.com/abougouffa/minemacs/compare/v1.1.0..v1.2.0) - 2023-08-18
#### Bug Fixes
- **(core)** fix deprecated `pcase` statement - ([5d8ed44](https://github.com/abougouffa/minemacs/commit/5d8ed44)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** ignore stderr when saving system envvars - ([2b638db](https://github.com/abougouffa/minemacs/commit/2b638db)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** temporary disable `evil-snipe` - ([ca6ff07](https://github.com/abougouffa/minemacs/commit/ca6ff07)) - [@abougouffa](https://github.com/abougouffa)
- **(magit)** temporary disable `magit-todos` - ([2812e8d](https://github.com/abougouffa/minemacs/commit/2812e8d)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** fix keybindings when mu4e is not auto-started - ([f4c41db](https://github.com/abougouffa/minemacs/commit/f4c41db)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** typo (#106) - ([cf899af](https://github.com/abougouffa/minemacs/commit/cf899af)) - [@abougouffa](https://github.com/abougouffa)
- **(smerge)** autoload the hydra menu - ([767792d](https://github.com/abougouffa/minemacs/commit/767792d)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(core)** minor documentation edit - ([154f945](https://github.com/abougouffa/minemacs/commit/154f945)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** add documentation for some constants - ([33207e0](https://github.com/abougouffa/minemacs/commit/33207e0)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(data)** add support for `gnuplot` - ([62fb2ad](https://github.com/abougouffa/minemacs/commit/62fb2ad)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** enable evil-textobj-tree-sitter (WIP) - ([c1b9cc4](https://github.com/abougouffa/minemacs/commit/c1b9cc4)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** enable `ox-odt` export backend - ([f533a45](https://github.com/abougouffa/minemacs/commit/f533a45)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** enable `ox-koma-letter` export backend - ([ebfdd71](https://github.com/abougouffa/minemacs/commit/ebfdd71)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** additional templates - ([ef5d4f2](https://github.com/abougouffa/minemacs/commit/ef5d4f2)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- organize templates - ([d4e73e9](https://github.com/abougouffa/minemacs/commit/d4e73e9)) - [@abougouffa](https://github.com/abougouffa)
- update the bug report template - ([5a21e6c](https://github.com/abougouffa/minemacs/commit/5a21e6c)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(daemon)** make use of `+add-hook!` - ([918256a](https://github.com/abougouffa/minemacs/commit/918256a)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** rename hook functions - ([73a0a06](https://github.com/abougouffa/minemacs/commit/73a0a06)) - [@abougouffa](https://github.com/abougouffa)
- move `smerge` to `me-builtin` - ([c6dca7e](https://github.com/abougouffa/minemacs/commit/c6dca7e)) - [@abougouffa](https://github.com/abougouffa)
- group `use-package` related hacks - ([bc25440](https://github.com/abougouffa/minemacs/commit/bc25440)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bootstrap)** expand `use-package` minimally unless in debug mode - ([fc883b6](https://github.com/abougouffa/minemacs/commit/fc883b6)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove `+fill-scratch-buffer` - ([1f31061](https://github.com/abougouffa/minemacs/commit/1f31061)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove `+unix.el` - ([2dd871e](https://github.com/abougouffa/minemacs/commit/2dd871e)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** regenerate loaddefs - ([64bb338](https://github.com/abougouffa/minemacs/commit/64bb338)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove some unused macros - ([0e10572](https://github.com/abougouffa/minemacs/commit/0e10572)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** update `me-loaddefs` - ([f5c6fcd](https://github.com/abougouffa/minemacs/commit/f5c6fcd)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** remove unused `+debug!` macro - ([e401223](https://github.com/abougouffa/minemacs/commit/e401223)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** provide `+lazy-delay` - ([1ab081d](https://github.com/abougouffa/minemacs/commit/1ab081d)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** simplify `+env-save` - ([0da8387](https://github.com/abougouffa/minemacs/commit/0da8387)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better envvars management - ([7180101](https://github.com/abougouffa/minemacs/commit/7180101)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edit in `+env-save` - ([9f2c175](https://github.com/abougouffa/minemacs/commit/9f2c175)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** bump `cpptools` version - ([5de872d](https://github.com/abougouffa/minemacs/commit/5de872d)) - [@abougouffa](https://github.com/abougouffa)
- **(dap)** bump default `cpptools` version - ([048c1d6](https://github.com/abougouffa/minemacs/commit/048c1d6)) - [@abougouffa](https://github.com/abougouffa)
- **(dap-cpptools)** bump version - ([1634dfd](https://github.com/abougouffa/minemacs/commit/1634dfd)) - [@abougouffa](https://github.com/abougouffa)
- **(dap-python)** use `debugpy` instead of `ptvsd` - ([4f88b64](https://github.com/abougouffa/minemacs/commit/4f88b64)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** ignore case in completion - ([36523e6](https://github.com/abougouffa/minemacs/commit/36523e6)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** set `apropos-do-all` - ([0decadd](https://github.com/abougouffa/minemacs/commit/0decadd)) - [@abougouffa](https://github.com/abougouffa)
- **(edraw)** customize files - ([029e722](https://github.com/abougouffa/minemacs/commit/029e722)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** move a custom function - ([d61510c](https://github.com/abougouffa/minemacs/commit/d61510c)) - [@abougouffa](https://github.com/abougouffa)
- **(git-commit)** simplify the config - ([2445798](https://github.com/abougouffa/minemacs/commit/2445798)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** butter bindings for `sudo*` - ([79fe849](https://github.com/abougouffa/minemacs/commit/79fe849)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** replace :commands with :functions - ([bd327ba](https://github.com/abougouffa/minemacs/commit/bd327ba)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** explicitly enable `ox-beamer` - ([6dff7a2](https://github.com/abougouffa/minemacs/commit/6dff7a2)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** load `ox-*` after `ox` - ([b758dae](https://github.com/abougouffa/minemacs/commit/b758dae)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** add optional arg in `+project-scan-for-projects` - ([23a617d](https://github.com/abougouffa/minemacs/commit/23a617d)) - [@abougouffa](https://github.com/abougouffa)
- **(smartparens)** enable globally - ([b2cec8b](https://github.com/abougouffa/minemacs/commit/b2cec8b)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** additional `org-mode` templates - ([b54e45d](https://github.com/abougouffa/minemacs/commit/b54e45d)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** move additional templates to `assets/` - ([fddcedd](https://github.com/abougouffa/minemacs/commit/fddcedd)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** minor edits in `+env-deny-vars` - ([d566710](https://github.com/abougouffa/minemacs/commit/d566710)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([3a3fa2b](https://github.com/abougouffa/minemacs/commit/3a3fa2b)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([d6b01b6](https://github.com/abougouffa/minemacs/commit/d6b01b6)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([a68a092](https://github.com/abougouffa/minemacs/commit/a68a092)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([30fee3b](https://github.com/abougouffa/minemacs/commit/30fee3b)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([c9398a0](https://github.com/abougouffa/minemacs/commit/c9398a0)) - [@abougouffa](https://github.com/abougouffa)
- split `me-lisp` into `me-emacs-lisp`, `me-common-lisp`, `me-scheme` & `me-clojure` - ([d06712e](https://github.com/abougouffa/minemacs/commit/d06712e)) - [@abougouffa](https://github.com/abougouffa)
- update `+html2pdf` templates directory - ([54bf131](https://github.com/abougouffa/minemacs/commit/54bf131)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([4869483](https://github.com/abougouffa/minemacs/commit/4869483)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ab9084e](https://github.com/abougouffa/minemacs/commit/ab9084e)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.1.0](https://github.com/abougouffa/minemacs/compare/v1.0.3..v1.1.0) - 2023-07-18
#### Bug Fixes
- **(+writing-mode)** don't suppose Org to be loaded (#103) - ([6bedf30](https://github.com/abougouffa/minemacs/commit/6bedf30)) - [@abougouffa](https://github.com/abougouffa)
- **(bootstratp)** disable `exec-path-from-shell` in Windows (#99) - ([59b4864](https://github.com/abougouffa/minemacs/commit/59b4864)) - [@abougouffa](https://github.com/abougouffa)
- **(builtin)** use-package error - ([285f46c](https://github.com/abougouffa/minemacs/commit/285f46c)) - [@abougouffa](https://github.com/abougouffa)
- **(dumb-jump)** fixes find reference in C/C++ - ([02a7f39](https://github.com/abougouffa/minemacs/commit/02a7f39)) - [@abougouffa](https://github.com/abougouffa)
- **(elisp)** autoload a function - ([61a5862](https://github.com/abougouffa/minemacs/commit/61a5862)) - [@abougouffa](https://github.com/abougouffa)
- **(init)** don't load environment vars on Win (#104) - ([5a38524](https://github.com/abougouffa/minemacs/commit/5a38524)) - [@abougouffa](https://github.com/abougouffa)
- **(netextender)** use the right command function - ([541931f](https://github.com/abougouffa/minemacs/commit/541931f)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** warn on Windows - ([279f58e](https://github.com/abougouffa/minemacs/commit/279f58e)) - [@abougouffa](https://github.com/abougouffa)
- fix some byte compiler errors - ([7a46079](https://github.com/abougouffa/minemacs/commit/7a46079)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- update modules documentation - ([5828984](https://github.com/abougouffa/minemacs/commit/5828984)) - [@abougouffa](https://github.com/abougouffa)
- update the screenshot - ([805f96b](https://github.com/abougouffa/minemacs/commit/805f96b)) - [@abougouffa](https://github.com/abougouffa)
- update screenshot - ([b84b2e0](https://github.com/abougouffa/minemacs/commit/b84b2e0)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(cape)** helpers to make use of `cape-super-capf` - ([ac587e0](https://github.com/abougouffa/minemacs/commit/ac587e0)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better management of environment variables - ([87e498e](https://github.com/abougouffa/minemacs/commit/87e498e)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** make `cov` obsolete - ([4516c9b](https://github.com/abougouffa/minemacs/commit/4516c9b)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** add `chezmoi` support - ([c5386d3](https://github.com/abougouffa/minemacs/commit/c5386d3)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- update issue template - ([66054d4](https://github.com/abougouffa/minemacs/commit/66054d4)) - [@abougouffa](https://github.com/abougouffa)
- add bug report template - ([10e3411](https://github.com/abougouffa/minemacs/commit/10e3411)) - [@abougouffa](https://github.com/abougouffa)
- minor Makefile edit - ([a5d2e65](https://github.com/abougouffa/minemacs/commit/a5d2e65)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(elisp)** minor refactoring - ([dd32645](https://github.com/abougouffa/minemacs/commit/dd32645)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** minor edits - ([5bcb8d2](https://github.com/abougouffa/minemacs/commit/5bcb8d2)) - [@abougouffa](https://github.com/abougouffa)
- **(which-key)** use explicit function symbol - ([01d4a5a](https://github.com/abougouffa/minemacs/commit/01d4a5a)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core-ui)** make use of `+add-hook!` - ([3a35113](https://github.com/abougouffa/minemacs/commit/3a35113)) - [@abougouffa](https://github.com/abougouffa)
- move more settings to `me-builtin` - ([3f89b75](https://github.com/abougouffa/minemacs/commit/3f89b75)) - [@abougouffa](https://github.com/abougouffa)
- move more settings to `me-builtin` - ([ac827ff](https://github.com/abougouffa/minemacs/commit/ac827ff)) - [@abougouffa](https://github.com/abougouffa)
- move more settings from `me-defaults` to `me-builtin` - ([f0da499](https://github.com/abougouffa/minemacs/commit/f0da499)) - [@abougouffa](https://github.com/abougouffa)
- move more customization to `me-builtin` - ([24e6d3b](https://github.com/abougouffa/minemacs/commit/24e6d3b)) - [@abougouffa](https://github.com/abougouffa)
- move more settings to `me-builtin` - ([dcf61b7](https://github.com/abougouffa/minemacs/commit/dcf61b7)) - [@abougouffa](https://github.com/abougouffa)
- minor refactoring - ([90defba](https://github.com/abougouffa/minemacs/commit/90defba)) - [@abougouffa](https://github.com/abougouffa)
- move all built-in packages configs to `me-builtin` - ([f3aeaa2](https://github.com/abougouffa/minemacs/commit/f3aeaa2)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(core)** remove `exec-path-from-shell` - ([8dfb0d9](https://github.com/abougouffa/minemacs/commit/8dfb0d9)) - [@abougouffa](https://github.com/abougouffa)
- **(header2)** remove it (use autoinsert instead) - ([b50c299](https://github.com/abougouffa/minemacs/commit/b50c299)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(biblio)** install `citar-org-roam` only when needed - ([04590b6](https://github.com/abougouffa/minemacs/commit/04590b6)) - [@abougouffa](https://github.com/abougouffa)
- **(calfw)** change keybinding - ([29dc09d](https://github.com/abougouffa/minemacs/commit/29dc09d)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** make auto bury customizable - ([9469b40](https://github.com/abougouffa/minemacs/commit/9469b40)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** minor edit - ([b474c52](https://github.com/abougouffa/minemacs/commit/b474c52)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** ignore more envvars - ([7a49948](https://github.com/abougouffa/minemacs/commit/7a49948)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** better management of environment variables - ([fc202b6](https://github.com/abougouffa/minemacs/commit/fc202b6)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** double-check for elfeed - ([55f3eae](https://github.com/abougouffa/minemacs/commit/55f3eae)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** make `unicode-fonts` obsolete - ([9e1af9a](https://github.com/abougouffa/minemacs/commit/9e1af9a)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** remove `goggles` - ([c06f811](https://github.com/abougouffa/minemacs/commit/c06f811)) - [@abougouffa](https://github.com/abougouffa)
- **(header2)** cleaner default header - ([8d5cb21](https://github.com/abougouffa/minemacs/commit/8d5cb21)) - [@abougouffa](https://github.com/abougouffa)
- **(hideif)** do not enable by default - ([ec7fe35](https://github.com/abougouffa/minemacs/commit/ec7fe35)) - [@abougouffa](https://github.com/abougouffa)
- **(json)** enable `json-mode` commands in `json-ts-mode` - ([72f5f90](https://github.com/abougouffa/minemacs/commit/72f5f90)) - [@abougouffa](https://github.com/abougouffa)
- **(keybindings)** use original command names - ([5f608b2](https://github.com/abougouffa/minemacs/commit/5f608b2)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** clean unused modes - ([b0a6c9f](https://github.com/abougouffa/minemacs/commit/b0a6c9f)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** add `use-package` example - ([9905990](https://github.com/abougouffa/minemacs/commit/9905990)) - [@abougouffa](https://github.com/abougouffa)
- **(tempel)** enable user-defined snippets - ([59d8b1f](https://github.com/abougouffa/minemacs/commit/59d8b1f)) - [@abougouffa](https://github.com/abougouffa)
- **(undo-fu)** set `evil-undo-system` with `setopt` - ([7841b94](https://github.com/abougouffa/minemacs/commit/7841b94)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** better OS checking - ([5f194cd](https://github.com/abougouffa/minemacs/commit/5f194cd)) - [@abougouffa](https://github.com/abougouffa)
- **(vars)** add a disclaimer when Windows is detected - ([7763e04](https://github.com/abougouffa/minemacs/commit/7763e04)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([5c18fc3](https://github.com/abougouffa/minemacs/commit/5c18fc3)) - [@abougouffa](https://github.com/abougouffa)
- remove empty `me-gnus` module - ([730c411](https://github.com/abougouffa/minemacs/commit/730c411)) - [@abougouffa](https://github.com/abougouffa)
- update packages versions - ([52262dd](https://github.com/abougouffa/minemacs/commit/52262dd)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([ad02cb3](https://github.com/abougouffa/minemacs/commit/ad02cb3)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([9ba6301](https://github.com/abougouffa/minemacs/commit/9ba6301)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([0921dbf](https://github.com/abougouffa/minemacs/commit/0921dbf)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.0.3](https://github.com/abougouffa/minemacs/compare/v1.0.2..v1.0.3) - 2023-06-26
#### Bug Fixes
- **(daemon)** defer checking `+mu4e-available-p` (#79) - ([52d72de](https://github.com/abougouffa/minemacs/commit/52d72de)) - [@abougouffa](https://github.com/abougouffa)
- **(drag-stuff)** avoid keybinding conflict - ([54343a4](https://github.com/abougouffa/minemacs/commit/54343a4)) - [@abougouffa](https://github.com/abougouffa)
- **(eglot)** add `rust-ts-mode` to auto enable modes - ([9edfb5c](https://github.com/abougouffa/minemacs/commit/9edfb5c)) - [@abougouffa](https://github.com/abougouffa)
- **(keybinding)** fix a typo, bind new command - ([c7f1fe3](https://github.com/abougouffa/minemacs/commit/c7f1fe3)) - [@donneyluck](https://github.com/donneyluck)
- **(keybinding)** handle `SPC u SPC u ...` - ([77a4948](https://github.com/abougouffa/minemacs/commit/77a4948)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** update obsolete functions - ([7041206](https://github.com/abougouffa/minemacs/commit/7041206)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** accept list grouped disabled packages - ([e86a19a](https://github.com/abougouffa/minemacs/commit/e86a19a)) - [@abougouffa](https://github.com/abougouffa)
- **(editor)** add support for `drag-stuff` - ([b42e987](https://github.com/abougouffa/minemacs/commit/b42e987)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** basic locking functions - ([75a28ba](https://github.com/abougouffa/minemacs/commit/75a28ba)) - [@abougouffa](https://github.com/abougouffa)
- **(prog)** add `ts-fold` for `treesit` - ([16db93a](https://github.com/abougouffa/minemacs/commit/16db93a)) - [@abougouffa](https://github.com/abougouffa)
- **(tools)** restore `affe` - ([693de26](https://github.com/abougouffa/minemacs/commit/693de26)) - [@abougouffa](https://github.com/abougouffa)
- add `me-calendar` WIP - ([665f39f](https://github.com/abougouffa/minemacs/commit/665f39f)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- minor tweak in Cocogitto conf - ([f6e6baa](https://github.com/abougouffa/minemacs/commit/f6e6baa)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(init)** minor edit - ([819c3cd](https://github.com/abougouffa/minemacs/commit/819c3cd)) - [@abougouffa](https://github.com/abougouffa)
- **(ligature)** enable only on supported systems - ([87d640a](https://github.com/abougouffa/minemacs/commit/87d640a)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(prog)** minor fixes and tweaks - ([ddb7267](https://github.com/abougouffa/minemacs/commit/ddb7267)) - [@abougouffa](https://github.com/abougouffa)
- **(tabspaces)** better check - ([985b1f3](https://github.com/abougouffa/minemacs/commit/985b1f3)) - [@abougouffa](https://github.com/abougouffa)
- simplify `use-package`'s `:hook` forms - ([cd75b1b](https://github.com/abougouffa/minemacs/commit/cd75b1b)) - [@abougouffa](https://github.com/abougouffa)
#### Revert
- **(init)** fixes an error on Emacs 28 - ([32a461d](https://github.com/abougouffa/minemacs/commit/32a461d)) - [@abougouffa](https://github.com/abougouffa)
- simplify `use-package`'s `:hook` forms (fix E28) - ([cd83f88](https://github.com/abougouffa/minemacs/commit/cd83f88)) - [@abougouffa](https://github.com/abougouffa)
- rewrite conditional package installs - ([0643de4](https://github.com/abougouffa/minemacs/commit/0643de4)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(bootstrap)** set `use-package-verbose` accordingly - ([7e1dc87](https://github.com/abougouffa/minemacs/commit/7e1dc87)) - [@abougouffa](https://github.com/abougouffa)
- **(cmake)** use built-in `cmake-ts-mode` - ([e75ba00](https://github.com/abougouffa/minemacs/commit/e75ba00)) - [@abougouffa](https://github.com/abougouffa)
- **(compile)** minor tweaks - ([4af1675](https://github.com/abougouffa/minemacs/commit/4af1675)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** make `minemacs-build-functions` a special hook - ([459981b](https://github.com/abougouffa/minemacs/commit/459981b)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** simplify a condition - ([3bd1f76](https://github.com/abougouffa/minemacs/commit/3bd1f76)) - [@abougouffa](https://github.com/abougouffa)
- **(epa)** ask for passphrase in Emacs minibuffer - ([601972b](https://github.com/abougouffa/minemacs/commit/601972b)) - [@abougouffa](https://github.com/abougouffa)
- **(flymake-easy)** autoload `flymake-easy-load` - ([a21b7cc](https://github.com/abougouffa/minemacs/commit/a21b7cc)) - [@abougouffa](https://github.com/abougouffa)
- **(forge)** demand after `magit` - ([709bf10](https://github.com/abougouffa/minemacs/commit/709bf10)) - [@abougouffa](https://github.com/abougouffa)
- **(io)** minor edits in `+html2pdf` - ([c274a6c](https://github.com/abougouffa/minemacs/commit/c274a6c)) - [@abougouffa](https://github.com/abougouffa)
- **(ltex)** do not use by default for LaTeX modes - ([5056403](https://github.com/abougouffa/minemacs/commit/5056403)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** set mu4e-modeline icons after `nerd-icons` - ([cb5686a](https://github.com/abougouffa/minemacs/commit/cb5686a)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** add locking mechanism - ([079f070](https://github.com/abougouffa/minemacs/commit/079f070)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** better icons for `mu4e-modeline` - ([71cb363](https://github.com/abougouffa/minemacs/commit/71cb363)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** do not show message - ([a653069](https://github.com/abougouffa/minemacs/commit/a653069)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** take account of multiple emails in auto BCC - ([5587715](https://github.com/abougouffa/minemacs/commit/5587715)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add some preconfigured LaTeX classes - ([198c56f](https://github.com/abougouffa/minemacs/commit/198c56f)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add babel tangle bindings - ([6e5af80](https://github.com/abougouffa/minemacs/commit/6e5af80)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** use booktabs by default - ([718c187](https://github.com/abougouffa/minemacs/commit/718c187)) - [@abougouffa](https://github.com/abougouffa)
- **(project)** bind `project-execute-extended-command` - ([06a8eb6](https://github.com/abougouffa/minemacs/commit/06a8eb6)) - [@abougouffa](https://github.com/abougouffa)
- **(ros)** better dependency management - ([f64ff69](https://github.com/abougouffa/minemacs/commit/f64ff69)) - [@abougouffa](https://github.com/abougouffa)
- **(skel)** update examples - ([abc3c91](https://github.com/abougouffa/minemacs/commit/abc3c91)) - [@abougouffa](https://github.com/abougouffa)
- **(smartparens)** disable expensive navigation features - ([77ca837](https://github.com/abougouffa/minemacs/commit/77ca837)) - [@abougouffa](https://github.com/abougouffa)
- **(ts-fold)** use my fork - ([ed087a5](https://github.com/abougouffa/minemacs/commit/ed087a5)) - [@abougouffa](https://github.com/abougouffa)
- **(window)** do not deffer - ([a75d587](https://github.com/abougouffa/minemacs/commit/a75d587)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([6940f9f](https://github.com/abougouffa/minemacs/commit/6940f9f)) - [@abougouffa](https://github.com/abougouffa)
- prefer built-in `cmake-ts-mode` - ([3839539](https://github.com/abougouffa/minemacs/commit/3839539)) - [@abougouffa](https://github.com/abougouffa)
- rewrite conditional package installs - ([da579e7](https://github.com/abougouffa/minemacs/commit/da579e7)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([b3f3474](https://github.com/abougouffa/minemacs/commit/b3f3474)) - [@abougouffa](https://github.com/abougouffa)

- - -

## [v1.0.2](https://github.com/abougouffa/minemacs/compare/v1.0.1..v1.0.2) - 2023-06-17
#### Bug Fixes
- **(bootstrap)** do store disabled packages with configured ones - ([04650b1](https://github.com/abougouffa/minemacs/commit/04650b1)) - [@abougouffa](https://github.com/abougouffa)
- **(buffer)** trim extra spaces in `+fill-scratch-buffer` - ([4887558](https://github.com/abougouffa/minemacs/commit/4887558)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** fix persistent scratch hooks/functions - ([309e5a5](https://github.com/abougouffa/minemacs/commit/309e5a5)) - [@abougouffa](https://github.com/abougouffa)
- **(emacs)** fix the `+def-dedicated-tab!` macro (#76) - ([054940b](https://github.com/abougouffa/minemacs/commit/054940b)) - [@abougouffa](https://github.com/abougouffa)
- **(evil)** repeat last command "." fix - ([1cca61c](https://github.com/abougouffa/minemacs/commit/1cca61c)) - [@abougouffa](https://github.com/abougouffa)
- **(evil+parinfer)** disable parinfer on some commands - ([a46a79f](https://github.com/abougouffa/minemacs/commit/a46a79f)) - [@abougouffa](https://github.com/abougouffa)
#### Documentation
- **(readme)** remove duplicate paragraph (#75) - ([77f05eb](https://github.com/abougouffa/minemacs/commit/77f05eb)) - [@abougouffa](https://github.com/abougouffa)
#### Features
- **(core)** use persistent scratch buffers by default - ([936dae0](https://github.com/abougouffa/minemacs/commit/936dae0)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** add `+define-dedicated-workspace!` - ([cae91b2](https://github.com/abougouffa/minemacs/commit/cae91b2)) - [@abougouffa](https://github.com/abougouffa)
- define dedicated tab for `vterm` - ([109d902](https://github.com/abougouffa/minemacs/commit/109d902)) - [@abougouffa](https://github.com/abougouffa)
- add persistent scratch hacks from Doom - ([17b421d](https://github.com/abougouffa/minemacs/commit/17b421d)) - [@abougouffa](https://github.com/abougouffa)
#### Miscellaneous Chores
- update cocogitto configuration file - ([e97faa3](https://github.com/abougouffa/minemacs/commit/e97faa3)) - [@abougouffa](https://github.com/abougouffa)
#### Nitpicks, changes with no side effect
- **(emacs)** rename a macro argument - ([833b03d](https://github.com/abougouffa/minemacs/commit/833b03d)) - [@abougouffa](https://github.com/abougouffa)
- minor edits - ([2f69a75](https://github.com/abougouffa/minemacs/commit/2f69a75)) - [@abougouffa](https://github.com/abougouffa)
- add optional quotes - ([eeaf3b8](https://github.com/abougouffa/minemacs/commit/eeaf3b8)) - [@abougouffa](https://github.com/abougouffa)
#### Refactoring
- **(core)** minor enhancement - ([5ccce03](https://github.com/abougouffa/minemacs/commit/5ccce03)) - [@abougouffa](https://github.com/abougouffa)
- **(eaf)** use a new strategy to disable - ([c2a11c8](https://github.com/abougouffa/minemacs/commit/c2a11c8)) - [@abougouffa](https://github.com/abougouffa)
- **(kind-icon)** more explicit setup - ([5728453](https://github.com/abougouffa/minemacs/commit/5728453)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** make use of `+define-dedicated-workspace!` - ([2ecc219](https://github.com/abougouffa/minemacs/commit/2ecc219)) - [@abougouffa](https://github.com/abougouffa)
#### Tweaks
- **(core)** minor edits in persistant scratch buffer - ([8258734](https://github.com/abougouffa/minemacs/commit/8258734)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** accept optional args in `+hook-once!` - ([9db34ce](https://github.com/abougouffa/minemacs/commit/9db34ce)) - [@abougouffa](https://github.com/abougouffa)
- **(core)** accept functions with args in `+define-dedicated-workspace!` - ([7ca7b66](https://github.com/abougouffa/minemacs/commit/7ca7b66)) - [@abougouffa](https://github.com/abougouffa)
- **(daemon)** remove unneeded hack - ([8b5d2df](https://github.com/abougouffa/minemacs/commit/8b5d2df)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** don't ask about new files, better buffer naming - ([a5ce718](https://github.com/abougouffa/minemacs/commit/a5ce718)) - [@abougouffa](https://github.com/abougouffa)
- **(defaults)** add some default directories - ([662fdf0](https://github.com/abougouffa/minemacs/commit/662fdf0)) - [@abougouffa](https://github.com/abougouffa)
- **(elfeed)** run in dedicated workspace - ([0a0c29c](https://github.com/abougouffa/minemacs/commit/0a0c29c)) - [@abougouffa](https://github.com/abougouffa)
- **(mu4e)** trash icalendar mails after reply - ([66caba7](https://github.com/abougouffa/minemacs/commit/66caba7)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** minor edits - ([0f16835](https://github.com/abougouffa/minemacs/commit/0f16835)) - [@abougouffa](https://github.com/abougouffa)
- **(org)** add a book class that passes the FACILE test - ([571bf73](https://github.com/abougouffa/minemacs/commit/571bf73)) - [@abougouffa](https://github.com/abougouffa)
- **(org-msg)** remember last directory when adding attachements - ([499d731](https://github.com/abougouffa/minemacs/commit/499d731)) - [@abougouffa](https://github.com/abougouffa)
- bump packages versions - ([53c88d7](https://github.com/abougouffa/minemacs/commit/53c88d7)) - [@abougouffa](https://github.com/abougouffa)
- rename to `+def-dedicated-tab!`, return function symbol - ([8e3c27b](https://github.com/abougouffa/minemacs/commit/8e3c27b)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([e2ad2ea](https://github.com/abougouffa/minemacs/commit/e2ad2ea)) - [@abougouffa](https://github.com/abougouffa)
- regenerate loaddefs - ([de85b88](https://github.com/abougouffa/minemacs/commit/de85b88)) - [@abougouffa](https://github.com/abougouffa)

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
