# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

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
- **(default)** hide `tab-bar` tabs - (023c0f4) - Abdelhak Bougouffa
- **(defaults)** make use of `+add-hook!` - (d731c77) - Abdelhak Bougouffa

- - -

## v1.0.0 - 2023-06-12
#### Bug Fixes
- **(binary)** fix `objdump-disassemble-mode` - (3f77e8d) - Abdelhak Bougouffa
- **(binary)** better management of objdump - (41e20ec) - Abdelhak Bougouffa
- **(binary)** temporary disable auto `hexl-mode` (#67) - (ad08679) - Abdelhak Bougouffa
- **(cape)** hook capfs the right way - (31c733d) - Abdelhak Bougouffa
- **(daemon)** check if `mu4e` is available - (0950451) - Abdelhak Bougouffa
- **(eaf)** remove undefined function - (21548b7) - Abdelhak Bougouffa
- **(epa-file)** ensure enabling `epa-file` (#67) - (568bb5d) - Abdelhak Bougouffa
- **(keybinding)** remove duplicate binding for workspace - (d391634) - Abdelhak Bougouffa
- **(media)** problematic executable check (#65) - (08cf1ef) - Abdelhak Bougouffa
- **(org-roam)** autosync - (d62475b) - donneyluck
- **(org-roam)** autosync database (#68) - (bd4cd61) - Abdelhak Bougouffa
- **(org-roam-ui)** use another keybinding (#68) - (5ba3042) - Abdelhak Bougouffa
- **(tempel)** do not overwrite Capf - (d7ba8b7) - Abdelhak Bougouffa
#### Documentation
- **(mu4e-alert)** function documentation en comments - (e9004e0) - Abdelhak Bougouffa
- **(skel)** add an example - (f1bc80b) - Abdelhak Bougouffa
- update README - (0dbf11c) - Abdelhak Bougouffa
- tiny fix in README - (c292c96) - Abdelhak Bougouffa
- update README - (392fdd8) - Abdelhak Bougouffa
- update README to include the new variable - (444f2c5) - Abdelhak Bougouffa
- include the new environment vars in README - (2077726) - Abdelhak Bougouffa
- minor updates - (868bb15) - Abdelhak Bougouffa
#### Features
- **(code-cells)** initial support - (64b041f) - Abdelhak Bougouffa
- **(combobulate)** initial support - (fea6426) - Abdelhak Bougouffa
- **(core)** add `+setq-hook!` & `+unsetq-hook!` from Doom - (5cdab26) - Abdelhak Bougouffa
- **(core)** add `+add-hook!` & `+remove-hook!` from Doom - (61bb207) - Abdelhak Bougouffa
- **(core)** disable individual modules packages - (4b91dc0) - Abdelhak Bougouffa
- **(docs)** add `pandoc-mode` - (569f328) - Abdelhak Bougouffa
- **(email)** add `org-mime` - (bb7a610) - Abdelhak Bougouffa
- **(ibuffer-project)** group buffers by projects in ibuffer - (67c8d2d) - Abdelhak Bougouffa
- **(lisp)** additional Common Lisp packages - (128cd50) - Abdelhak Bougouffa
- **(ox-pandoc)** initial support - (c7c882f) - Abdelhak Bougouffa
#### Nitpicks, changes with no side effect
- **(keybindings)** remove extra space - (f7d7969) - Abdelhak Bougouffa
- **(mixed-pitch)** sort list elements - (9d210bb) - Abdelhak Bougouffa
- **(mu4e)** the `mu4e` command is already autoloaded - (8fd5850) - Abdelhak Bougouffa
- **(mu4e)** minor formatting - (6d7ed85) - Abdelhak Bougouffa
#### Refactoring
- **(flymake)** small simplification - (6fa0fe3) - Abdelhak Bougouffa
- **(mu4e)** minor UI edits - (117a9f4) - Abdelhak Bougouffa
- **(netextender)** better error management - (f1c2442) - Abdelhak Bougouffa
- **(netextender)** better way to manage the custom command - (852a7d8) - Abdelhak Bougouffa
- move `+eglot-auto-enable` and `+lsp-auto-enable` - (1017235) - Abdelhak Bougouffa
- use `keymap[-global]-set` instead of `define-key` - (1e36c9a) - Abdelhak Bougouffa
- use `use-package`'s `:hook` as much as possible - (e31ca12) - Abdelhak Bougouffa
#### Revert
- **(binary)** temporary disable auto `hexl-mode` (#67) - (71309a2) - Abdelhak Bougouffa
#### Tweaks
- **(auctex)** better defaults - (a4152ef) - Abdelhak Bougouffa
- **(binary)** better deferring - (761337c) - Abdelhak Bougouffa
- **(binary)** simplify condition - (0b85122) - Abdelhak Bougouffa
- **(cape)** better integration with `pcomplete` - (0cad15f) - Abdelhak Bougouffa
- **(cape)** tweak the cape backends - (7540b43) - Abdelhak Bougouffa
- **(consult-eglot)** better check for `consult-lsp` - (811b74b) - Abdelhak Bougouffa
- **(core)** rename `+quoted` to `+quoted-p` - (99834d5) - Abdelhak Bougouffa
- **(core)** add `minemacs-run-build-functions` - (04041bb) - Abdelhak Bougouffa
- **(core)** save a list of packages configured by MinEmacs - (bdd3898) - Abdelhak Bougouffa
- **(core)** add `minemacs-after-loading-modules-hook` - (0e4c53d) - Abdelhak Bougouffa
- **(core)** centralize `minemacs-ignore-user-config` - (fd4a585) - Abdelhak Bougouffa
- **(core)** add new env vars to disable user config - (4e229dd) - Abdelhak Bougouffa
- **(corfu)** better integration with `eshell` - (174e3d8) - Abdelhak Bougouffa
- **(dap)** update cpptools & codelldb default versions - (b9f673c) - Abdelhak Bougouffa
- **(dashboard)** simplify condition - (56fc4a2) - Abdelhak Bougouffa
- **(defaults)** better TAB behavior, dired, scripts, ... - (0ea4bd7) - Abdelhak Bougouffa
- **(dired)** enable adding mail attachements from `dired` - (6a3fd7d) - Abdelhak Bougouffa
- **(doc-view)** enable continuous mode - (9af3adc) - Abdelhak Bougouffa
- **(doom-modeline)** enable word count - (921ee7f) - Abdelhak Bougouffa
- **(doom-modeline)** show time icon - (19c78b5) - Abdelhak Bougouffa
- **(doom-themes)** apply org tweaks - (3b4847d) - Abdelhak Bougouffa
- **(ecryptfs)** make passphrase file customizable - (f35fa77) - Abdelhak Bougouffa
- **(eglot)** add a helper function - (56f430d) - Abdelhak Bougouffa
- **(emacs)** ask for output file in `+screenshot-svg` - (77a89a7) - Abdelhak Bougouffa
- **(embark)** bind to `SPC a` instead of `SPC .` - (3d7537c) - Abdelhak Bougouffa
- **(embark-consult)** activate on `embark-collect` - (3020308) - Abdelhak Bougouffa
- **(evil)** minor edits - (5aeb051) - Abdelhak Bougouffa
- **(flymake)** add hydra menu - (4407230) - Abdelhak Bougouffa
- **(forge)** remove obsolete var, add merge keybinding - (63a235d) - Abdelhak Bougouffa
- **(hideif)** more intelligent integration - (34d6a31) - Abdelhak Bougouffa
- **(init)** use `file-truename` in `+load` - (01353db) - Abdelhak Bougouffa
- **(io)** add `pandoc` as backend for `+html2pdf` - (74bf4c4) - Abdelhak Bougouffa
- **(keybinding)** bind `bury-buffer` - (e71564a) - Abdelhak Bougouffa
- **(latex)** check for `latexmk` before activation - (179ed3d) - Abdelhak Bougouffa
- **(lisp)** limit geiser scheme implementations - (b28be92) - Abdelhak Bougouffa
- **(ltex)** make `eglot-ltex-language` local-safe - (a0e2ed2) - Abdelhak Bougouffa
- **(magit-imerge)** add keybinding - (f18e907) - Abdelhak Bougouffa
- **(mu4e)** open in a dedicated workspace/tab - (5a04ae8) - Abdelhak Bougouffa
- **(mu4e)** add variable to control auto-start in daemon - (936dc80) - Abdelhak Bougouffa
- **(mu4e)** auto save google accounts on registration - (f099f67) - Abdelhak Bougouffa
- **(mu4e)** don't reply to self, copy the header instead - (d52f58e) - Abdelhak Bougouffa
- **(mu4e-alert)** use icon only if it exists - (f531cda) - Abdelhak Bougouffa
- **(nerd-icons)** register `nerd-icons-install-fonts` - (c83ef08) - Abdelhak Bougouffa
- **(nerd-icons)** minor edit - (bd108a4) - Abdelhak Bougouffa
- **(org)** set custom TODO keywords - (62865a5) - Abdelhak Bougouffa
- **(org)** restore `pcomplete` - (20785e2) - Abdelhak Bougouffa
- **(org)** print the right file name when exporting - (e55d3d6) - Abdelhak Bougouffa
- **(org)** disable annoying completion - (c427db1) - Abdelhak Bougouffa
- **(org)** better set latex classes and default packages (#69) - (2bcfb17) - Abdelhak Bougouffa
- **(org)** consider language when exporting to PDF (#69) - (dc9517f) - Abdelhak Bougouffa
- **(org)** add a way to disable lower case keywords - (314f1eb) - Abdelhak Bougouffa
- **(org-msg)** additional keybinding - (a48150f) - Abdelhak Bougouffa
- **(org-roam)** bigger space for tags in completion - (7a68af9) - Abdelhak Bougouffa
- **(org-roam)** show tags in `vertico` + autosync - (4033d75) - Abdelhak Bougouffa
- **(realgud)** use realgud:gdb for GDB supported languages - (3a4be4f) - Abdelhak Bougouffa
- **(scheme)** use guile by default - (4f01f23) - Abdelhak Bougouffa
- **(skel)** add an example in `early-config.el` - (16b197d) - Abdelhak Bougouffa
- **(skel)** add package disabling example - (51375d8) - Abdelhak Bougouffa
- **(skel)** update `org-roam` config example - (1e9d244) - Abdelhak Bougouffa
- **(spell-fu)** correctly check CamelCase words - (4dc0a79) - Abdelhak Bougouffa
- **(spell-fu)** update macro name to follow the convention - (867f362) - Abdelhak Bougouffa
- **(tabspaces)** auto switch to scratch on create - (7d1447a) - Abdelhak Bougouffa
- **(tabspaces)** auto rename the first tab to default - (f7b42c6) - Abdelhak Bougouffa
- **(tabspaces)** minor edit - (ade261d) - Abdelhak Bougouffa
- **(tempel)** minor tweak - (ed4b7cf) - Abdelhak Bougouffa
- **(tldr)** register `tldr-update-docs` as build fn - (856e474) - Abdelhak Bougouffa
- bump packages versions - (1e999eb) - Abdelhak Bougouffa
- regenerate loadddefs - (926c9c9) - Abdelhak Bougouffa
- bump packages versions - (7576d14) - Abdelhak Bougouffa
- defer `forge` & `code-review` - (4afad72) - Abdelhak Bougouffa
- provide file names - (4db86af) - Abdelhak Bougouffa
- bump packages versions - (875cd46) - Abdelhak Bougouffa
- bump packages versions - (e5fa45f) - Abdelhak Bougouffa
- beautify hydra menus - (d77704a) - Abdelhak Bougouffa
- bump packages versions - (b55c182) - Abdelhak Bougouffa
- bump packages versions - (b29eda4) - Abdelhak Bougouffa
- bump packages versions - (3cd0eac) - Abdelhak Bougouffa
- better use of `executable-find` - (a976cab) - Abdelhak Bougouffa
- bump packages versions - (01ddbfc) - Abdelhak Bougouffa

- - -

## v0.4.0 - 2023-05-27
#### Bug Fixes
- **(citar)** avoid using `all-the-icons` until it is loaded - (960d978) - Abdelhak Bougouffa
- **(code-review)** use fixed fork, unpin closql & forge - (79423c5) - Abdelhak Bougouffa
- **(elfeed)** correct key for +elfeed-download-image - (4b69a74) - DarkBuffalo
- **(flymake)** use custom icons only when suitable - (1e84c48) - Abdelhak Bougouffa
- **(ltex)** use ltex-ls as TCP server, add helpers - (fe290c2) - Abdelhak Bougouffa
#### Documentation
- **(backports)** update function documentation - (c5ab9fe) - Abdelhak Bougouffa
- **(skel)** update comment - (125d82a) - Abdelhak Bougouffa
#### Features
- **(editor)** add header2 support - (0017976) - Abdelhak Bougouffa
- **(elfeed)** add yt-dlp support - (8b8e611) - DarkBuffalo
#### Nitpicks, changes with no side effect
- **(defaults)** add an optional argument - (a02e5cc) - Abdelhak Bougouffa
- **(defaults)** minor edit - (6fc983d) - Abdelhak Bougouffa
- **(evil-collection)** cleanup previous fix - (21ddd4d) - Abdelhak Bougouffa
- **(project)** code formatting - (3827863) - Abdelhak Bougouffa
#### Refactoring
- **(backports)** precise condition - (22a09fa) - Abdelhak Bougouffa
- **(core)** minor edit - (a0861ff) - Abdelhak Bougouffa
- **(ecryptfs)** partial rewrite - (e1e4034) - Abdelhak Bougouffa
- move pcache directory customization to me-defaults - (f10b3cc) - Abdelhak Bougouffa
#### Revert
- **(ltex)** remove server commands - (f02b0a7) - Abdelhak Bougouffa
#### Tweaks
- **(bootstrap)** call build functions interactively - (63b92d4) - Abdelhak Bougouffa
- **(bootstrap)** minor refactor - (cefcdac) - Abdelhak Bougouffa
- **(bootstrap)** do not ask when running build functions - (3d390ea) - Abdelhak Bougouffa
- **(bootstrap)** run build functions on update - (940fdaa) - Abdelhak Bougouffa
- **(clang-format)** add ts modes - (e317bbf) - Abdelhak Bougouffa
- **(compile)** move compilation options to `me-prog` - (f949b46) - Abdelhak Bougouffa
- **(core)** move package updating routines - (5bbfe68) - Abdelhak Bougouffa
- **(core)** update loaddefs - (71eb596) - Abdelhak Bougouffa
- **(dashboard)** add an option to disable it - (ee30640) - Abdelhak Bougouffa
- **(defaults)** set variables with `setopt` - (75fbb43) - Abdelhak Bougouffa
- **(defaults)** better scrolling settings - (f1b4639) - Abdelhak Bougouffa
- **(defaults)** more UI customization - (d7e7f98) - Abdelhak Bougouffa
- **(defaults)** move `password` and `auth-source` to `me-builtin` - (dc3a0da) - Abdelhak Bougouffa
- **(defaults)** correctly set `show-trailing-whitespace` - (997f6b2) - Abdelhak Bougouffa
- **(defaults)** set additional paths - (9e8c7b9) - Abdelhak Bougouffa
- **(eaf)** disable pdf-viewer - (c35356b) - Abdelhak Bougouffa
- **(early-init)** prefer loading newer Elisp files - (9bdf851) - Abdelhak Bougouffa
- **(eglot)** do not assume servers to be executable - (51dd7b8) - Abdelhak Bougouffa
- **(elfeed)** Enhance customizability - (d2f124a) - Abdelhak Bougouffa
- **(highlight-numbers)** enable for `conf-mode`, tweak regexp - (849dba3) - Abdelhak Bougouffa
- **(init)** use `lisp-interaction-mode` for scratch - (5f755e4) - Abdelhak Bougouffa
- **(io)** code refactoring - (c5ef698) - Abdelhak Bougouffa
- **(lexic)** fix local keybindigs - (ab56fb4) - Abdelhak Bougouffa
- **(ltex)** add `tex-mode` - (0e344a8) - Abdelhak Bougouffa
- **(ltex)** minor edits, add documentation - (8e0421c) - Abdelhak Bougouffa
- **(ltex)** use Eglot's server/port syntax - (faec1c5) - Abdelhak Bougouffa
- **(ltex-ls)** additional languages - (ea19fa6) - Abdelhak Bougouffa
- **(macrostep)** remove unneeded hack (fixed upstream) - (0f0b5ef) - Abdelhak Bougouffa
- **(macrostep)** use upstream repo, apply a hack - (c048d3c) - Abdelhak Bougouffa
- **(media)** automatically open Youtube links in MPV - (1fcc323) - Abdelhak Bougouffa
- **(mu4e)** use `nerd-icons` - (5407ab5) - Abdelhak Bougouffa
- **(nerd-icons)** set an icon for matlab/octave files - (972d3a4) - Abdelhak Bougouffa
- **(realgud)** add local binding for treesit modes - (51d8078) - Abdelhak Bougouffa
- **(realgud)** minor tweaks and fixes - (8365877) - Abdelhak Bougouffa
- **(realgud)** define commands - (920ab8d) - Abdelhak Bougouffa
- **(realgud-lldb)** remove unneeded autoload - (689a1da) - Abdelhak Bougouffa
- **(skel)** update the skeleton's modules list - (1e86106) - Abdelhak Bougouffa
- **(tempel)** bind TAB and S-TAB to next/previous - (dff6996) - Abdelhak Bougouffa
- **(ui)** remove unused themes - (d8f4e3b) - Abdelhak Bougouffa
- **(ui)** add `nerd-icons` explicitly - (33e3ffe) - Abdelhak Bougouffa
- **(xclip)** remove useless `+xclip--enable-in-tty-h` - (0481b8e) - Abdelhak Bougouffa
- bump packages versions - (69e9235) - Abdelhak Bougouffa
- use `nerd-icons` instead of `all-the-icons` - (403a3a1) - Abdelhak Bougouffa
- bump packages versions - (6525e40) - Abdelhak Bougouffa
- bump packages versions - (8b90843) - Abdelhak Bougouffa
- bump packages versions - (272bb17) - Abdelhak Bougouffa
- bump packages versions - (bffed94) - Abdelhak Bougouffa
- bump packages versions - (53caf4d) - Abdelhak Bougouffa
- bump packages versions - (c89d5cc) - Abdelhak Bougouffa
- bump packages versions - (693efa0) - Abdelhak Bougouffa
- register package-specific build functions - (ef635c4) - Abdelhak Bougouffa

- - -

## v0.3.0 - 2023-05-03
#### Bug Fixes
- **(aphelia)** adapt to the new upstream changes - (06f7776) - Abdelhak Bougouffa
- **(backports)** fix a bug causing straight to fail (#51) - (ded8596) - Abdelhak Bougouffa
- **(daemon)** avoid running `mu4e` repeatedly - (c784d05) - Abdelhak Bougouffa
- **(dashboard)** do not show when Emacs started with a file - (ebe1a9a) - Abdelhak Bougouffa
- **(io)** fix +html2pdf output file name - (a8d4435) - Abdelhak Bougouffa
- **(meow)** add safety guards to avoid conflict with evil - (db20803) - Abdelhak Bougouffa
- **(straight)** clone the full packages repos - (4f51035) - Abdelhak Bougouffa
- **(straight)** use master on Emacs 28, develop on newer versions - (7f7f33d) - Abdelhak Bougouffa
- **(tempel)** wrong parenthesis disabling corfu in other modes - (1ad3b33) - Abdelhak Bougouffa
- **(use-package)** `:pin-ref` problem on Emacs28 (#49) - (e37b984) - Abdelhak Bougouffa
- **(vc)** pin problematic packages to working versions - (424fd54) - Abdelhak Bougouffa
#### Documentation
- **(readme)** fix broken link - (87247a5) - Abdelhak Bougouffa
#### Features
- **(core)** add `minemacs-update` command - (5692b8d) - Abdelhak Bougouffa
- **(core)** add the `+hook-once!` macro - (1eaa535) - Abdelhak Bougouffa
- **(docs)** initial support for `poly-markdown` - (2460e18) - Abdelhak Bougouffa
- **(io)** add a helper to save URLs to HTML snapshots - (24e03db) - Abdelhak Bougouffa
- **(meow)** WIP optional evil replacement - (954a549) - Abdelhak Bougouffa
- **(modeling)** add `medelica-mode` - (86e8c09) - Abdelhak Bougouffa
- **(prog)** add `hy-mode` - (031b5ba) - Abdelhak Bougouffa
- **(ui)** add all-the-icons for ibuffer - (40d8bb8) - Abdelhak Bougouffa
- **(use-package)** add the `:pin-ref` keyword to use `straight-x` - (cc4f11b) - Abdelhak Bougouffa
#### Miscellaneous Chores
- **(git)** track straight's default pins - (52390b7) - Abdelhak Bougouffa
- provide `make update` - (4d6af7d) - Abdelhak Bougouffa
- save straight's versions when cleaning - (8795bbf) - Abdelhak Bougouffa
#### Nitpicks, changes with no side effect
- **(biblio)** minor edit - (cc742e7) - Abdelhak Bougouffa
- **(core)** simplify a condition - (4a82a91) - Abdelhak Bougouffa
- **(core)** rename a parameter - (4dd3285) - Abdelhak Bougouffa
- **(vc)** remove extra spaces - (b8d9d91) - Abdelhak Bougouffa
- add files headers and footers - (298543e) - Abdelhak Bougouffa
#### Refactoring
- **(mu4e)** remove commented code - (a766768) - Abdelhak Bougouffa
- **(use-package)** small cleanup - (0c3dc30) - Abdelhak Bougouffa
- make use of `+hook-once!` - (43b12bb) - Abdelhak Bougouffa
#### Revert
- **(corfu)** restore in `(org/markdown)-mode` - (be458ac) - Abdelhak Bougouffa
#### Tweaks
- **(bootstrap)** revert to straight's develop branch - (797e115) - Abdelhak Bougouffa
- **(core)** make sure `+emacs-features-p` returns a boolean - (2c194de) - Abdelhak Bougouffa
- **(core)** accept hook symbol in `+hook-once!` - (4a08072) - Abdelhak Bougouffa
- **(core)** fallback to a builtin theme if `minemacs-theme` fails - (48b0b62) - Abdelhak Bougouffa
- **(corfu)** disable in `org-mode` and `markdown-mode` - (9360b69) - Abdelhak Bougouffa
- **(daemon)** ensure keeping `mu4e` alive in background - (2358e3d) - Abdelhak Bougouffa
- **(early-init)** set straight branch to develop - (d9688e1) - Abdelhak Bougouffa
- **(ecryptfs)** better support - (7034976) - Abdelhak Bougouffa
- **(eldoc-box)** remove special case, fixed upstream - (f38adf1) - Abdelhak Bougouffa
- **(email)** refine attachement detection regexp - (7b72d76) - Abdelhak Bougouffa
- **(evil-collection)** restore `mu4e` - (3c60a72) - Abdelhak Bougouffa
- **(gts-translate)** add an option to choose translation langs - (8e4d74f) - Abdelhak Bougouffa
- **(latex)** enable `hs-minor-mode` - (29d22a4) - Abdelhak Bougouffa
- **(latex)** better fontification - (9d31c01) - Abdelhak Bougouffa
- **(latex)** additional tweaks - (ad5adae) - Abdelhak Bougouffa
- **(logview)** set custom files paths - (f817a8f) - Abdelhak Bougouffa
- **(maxima)** use locally installed packages - (024a05e) - Abdelhak Bougouffa
- **(mu4e)** don't ask for the alias when there is only one - (6bda132) - Abdelhak Bougouffa
- **(mu4e)** minor edits - (94a8c1f) - Abdelhak Bougouffa
- **(notes)** better org-roam protocol handling - (9a9748c) - Abdelhak Bougouffa
- **(org)** prefer using `latexmk` or `tectonic` when found - (4e1267d) - Abdelhak Bougouffa
- **(pcache)** create the cache in the cache directory - (28f2c04) - Abdelhak Bougouffa
- **(straight)** update packages - (1dd3044) - Abdelhak Bougouffa
- **(straight)** add pin file - (03e13d2) - Abdelhak Bougouffa
- **(tempel)** restrict `org/markdown` to tempel capf - (439b6aa) - Abdelhak Bougouffa
- **(treemacs)** enable `evil` support - (2176941) - Abdelhak Bougouffa
- bump packages versions - (6a94af1) - Abdelhak Bougouffa
- bump package versions - (8372b89) - Abdelhak Bougouffa
- bump package versions - (0f6a2fd) - Abdelhak Bougouffa
- update loaddefs - (09a9cea) - Abdelhak Bougouffa

- - -

## v0.2.0 - 2023-04-01
#### Bug Fixes
- **(auctex)** require `tex` - (104a41e) - Abdelhak Bougouffa
- **(backports)** add `scratch-buffer` (#41) - (f01f80b) - Abdelhak Bougouffa
- **(compile)** remove accidentally added quote - (69bd4b3) - Abdelhak Bougouffa
- **(consult)** no initial fill when in `minibuffer` #37 - (5c30bcd) - Abdelhak Bougouffa
- **(dashboard)** load after `evil-collection` (#42) - (d3b0976) - Abdelhak Bougouffa
- **(defaults)** setup default hooks early - (720da8c) - Abdelhak Bougouffa
- **(docker)** better handling of `Dockerfile`s - (69544e2) - Abdelhak Bougouffa
- **(ebnf-mode)** fix a typo - (f63014f) - Abdelhak Bougouffa
- **(editor)** temporary disable `unicode-fonts` - (a116b7b) - Abdelhak Bougouffa
- **(eglot-box)** better integration with `tab-bar` and `tool-bar` - (f43d7ff) - Abdelhak Bougouffa
- **(evil)** temporary disable `evil-escape` - (22f9a6d) - Abdelhak Bougouffa
- **(evil)** the right way to use `evil-search` - (a5c61ab) - Abdelhak Bougouffa
- **(evil-mc)** avoid inserting the first `evil-escape` char - (99559a8) - Abdelhak Bougouffa
- **(mu4e)** fixes related to evil-collection - (56533ad) - Abdelhak Bougouffa
- **(mu4e)** mu 1.10 UI and evil fixes - (2398bd0) - Abdelhak Bougouffa
- **(pdf-tools)** make sure to use it to show PDFs - (d8bc950) - Abdelhak Bougouffa
- disable packages causing problems on the last build - (6b2c3ae) - Abdelhak Bougouffa
#### Documentation
- **(skel)** more use cases of `early-config.el` - (b6c2104) - Abdelhak Bougouffa
- add a header image in README - (2130b3c) - Abdelhak Bougouffa
#### Features
- **(dashboard)** add dashboard - (f0d5a10) - Abdelhak Bougouffa
- **(doc-view)** use SVG when available - (b9a7715) - Abdelhak Bougouffa
- **(evil-multiedit)** initial support - (535f2ba) - Abdelhak Bougouffa
- **(lisp)** add support for Clojure (via cider) - (7546f08) - Abdelhak Bougouffa
- **(lisp)** add more geiser backends - (2f95ff1) - Abdelhak Bougouffa
- **(lsp-bridge)** initial support (WIP) - (5820016) - Abdelhak Bougouffa
- **(mermaid)** initial support - (5718db8) - Abdelhak Bougouffa
- add the missing template for `+html2pdf` - (c3c6fd2) - Abdelhak Bougouffa
- add more backends to `+html2pdf` - (8b025d0) - Abdelhak Bougouffa
#### Miscellaneous Chores
- **(changlog)** remove - (8d54ac2) - Abdelhak Bougouffa
- move templates to assets - (4042455) - Abdelhak Bougouffa
- move pictures to `assets` - (2f9809a) - Abdelhak Bougouffa
- add names for the workflows - (1d41152) - Abdelhak Bougouffa
- add `clean_pcache` target in Makefile - (a7e02d1) - Abdelhak Bougouffa
- add the "v" prefix in cocogitto - (0a70fbc) - Abdelhak Bougouffa
- bump cocogitto version to 3.4 - (a1c5ab7) - Abdelhak Bougouffa
#### Nitpicks, changes with no side effect
- **(elisp)** minor edits - (bf3a4e0) - Abdelhak Bougouffa
- **(macrostep)** update recipe - (9054280) - Abdelhak Bougouffa
- **(math)** format recipes - (284c970) - Abdelhak Bougouffa
- **(pdf-tools)** simplify - (427c0c1) - Abdelhak Bougouffa
- **(vc)** remove unneeded package - (6baf7b0) - Abdelhak Bougouffa
- minor edits - (81dc6a6) - Abdelhak Bougouffa
#### Refactoring
- **(robot)** make ROS commands customizable - (c041258) - Abdelhak Bougouffa
- **(writeroom-mode)** hook via `use-package` - (7c0832e) - Abdelhak Bougouffa
- move `transient` to `me-builtin` - (0761b3d) - Abdelhak Bougouffa
- define MinEmacs sub-groups - (a8f563c) - Abdelhak Bougouffa
#### Revert
- restore disabled packages, using emacs@6bf441ff11540 - (6e00f68) - Abdelhak Bougouffa
- replace `writeroom-mode` with simpler config - (4c2255d) - Abdelhak Bougouffa
- replace `yasnippet` with `tempel` - (b1edd7e) - Abdelhak Bougouffa
- remove `lsp-bridge` - (34ce221) - Abdelhak Bougouffa
#### Tweaks
- **(+writing-mode)** increase text scale - (ddfb23a) - Abdelhak Bougouffa
- **(builtin)** pin `map` and `let-alist` - (6eff30b) - Abdelhak Bougouffa
- **(compile)** enable ANSI colors, restore savehist integration - (c701113) - Abdelhak Bougouffa
- **(compile)** add a message on toggle burying buffer - (6c7c28d) - Abdelhak Bougouffa
- **(core)** remove obsolete keybinding aliases - (48c53de) - Abdelhak Bougouffa
- **(core)** define a group for MinEmacs' custom variables - (56c37a8) - Abdelhak Bougouffa
- **(defaults)** set `custom-theme-directory` - (fa621b6) - Abdelhak Bougouffa
- **(defaults)** enable drag and drop of regions - (fb1d75d) - Abdelhak Bougouffa
- **(defaults)** do not use system tooltips - (3adf802) - Abdelhak Bougouffa
- **(doom-modeline)** update the main modeline layout - (f8c367e) - Abdelhak Bougouffa
- **(eaf)** minor edits, start in emacs state - (d818ab6) - Abdelhak Bougouffa
- **(eglot)** disable annoying reporting in echo area - (23af99e) - Abdelhak Bougouffa
- **(ein)** load org-babel the right way - (2a68535) - Abdelhak Bougouffa
- **(init)** simplify the `file-name-handler-alist` hack - (9dd1345) - Abdelhak Bougouffa
- **(latex)** edit keybindings - (f6fbe96) - Abdelhak Bougouffa
- **(mu4e)** disable the new `mu4e-modeline-mode` - (e7af964) - Abdelhak Bougouffa
- **(mu4e)** small UI tweak - (b3588e9) - Abdelhak Bougouffa
- **(mu4e-alert)** better filtering of spams - (0c54543) - Abdelhak Bougouffa
- **(org)** update keybindings - (7675c7a) - Abdelhak Bougouffa
- **(org)** dynamically set latex fragments scale - (26f1fd9) - Abdelhak Bougouffa
- **(straight)** add repo hash to the build directory - (4ad4c3a) - Abdelhak Bougouffa
- **(vars)** decrease default fonts size - (ba61d68) - Abdelhak Bougouffa
- **(window)** wider window for `lexic` - (6d2550c) - Abdelhak Bougouffa
- **(writeroom)** minor edits - (a1bc1a8) - Abdelhak Bougouffa
- make `yasnippet` conf obsolete - (e8025e9) - Abdelhak Bougouffa

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).