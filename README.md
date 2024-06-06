# Ansible minor mode

## Requirement

- yasnippet
- auto-complete

## Installation

It's available on [Melpa](http://melpa.milkbox.net/)

    M-x package-install ansible

## Usage

### Enable minor mode

    M-x ansible

or hook

    (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))

### Snippets for yasnippet

- Ansible module snippet

### Dictionary for auto-complete

- Ansible module key dictionary

### Ansible Vault support

Set up a password in `ansible-vault-password-file`: (default value:
"~/.vault_pass")

    (setq ansible-vault-password-file "path/to/pwd/file")

Bind keys:

    (global-set-key (kbd "C-c b") 'ansible-decrypt-buffer)
    (global-set-key (kbd "C-c g") 'ansible-encrypt-buffer)

You can also set automatic {en,de}cryption by adding
`ansible-auto-decrypt-encrypt` to `ansible-hook`:

    (add-hook 'ansible-hook 'ansible-auto-decrypt-encrypt)
