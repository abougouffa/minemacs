FROM manjarolinux/build:20240121
LABEL version="0.1"
LABEL maintainer="Abdelhak Bougouffa"
LABEL release-date="2024-01-23"

# Create a user for yay building, and delete it's password
RUN useradd yaybuilder -m
RUN passwd -d yaybuilder
# Allow the yaybuilder user for password-less sudo
RUN printf 'root ALL=(ALL) ALL\n' | tee -a /etc/sudoers
RUN printf 'yaybuilder ALL=(ALL) ALL\n' | tee -a /etc/sudoers

# Update Pacman's DB
RUN pacman -Syu --noconfirm bash yay

# C/C++
RUN pacman -S --noconfirm gcc gdb valgrind llvm lldb lldb-mi clang ccls

# Rust
RUN pacman -S --noconfirm rustup

# Python
RUN pacman -S --noconfirm pyright python-lsp-server

# Common Lisp
RUN pacman -S --noconfirm sbcl ecl clisp quicklisp roswell

# Plotting and graphs
RUN pacman -S --noconfirm dot-language-server graphviz

# Others
RUN sudo -u yaybuilder yay -S --noconfirm lemminx emacs-lsp-booster-git

CMD ["bash"]
