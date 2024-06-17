# MinEmacs' genesis

Since a few years, Emacs forms the foundation of my workflow, serving as my
go-to tool for various tasks such as document writing, academic papers writing
and editing, email management, staying updated with news, developing software
using multiple programming languages and for different domains including:
robotics, embedded systems, embedded Linux, CI/CD, among other things.

Before Emacs, I was mainly using VSCode and QtCreator with a little bit of
Vim/Neovim. Hence, I wanted, in the beginning, a VIM-style configuration
framework that is both robust and straightforward.
[Spacemacs](https://github.com/syl20bnr/spacemacs) was the first framework I've
tested, I liked the idea of using `SPC` as a leader key, but I didn't like the
way Spacemacs packs things in layers, and imposes a unique way of writing your
configuration.

I discovered then [Doom Emacs](https://github.com/doomemacs/doomemacs), which I
found a remarkable piece of software that introduced me to the world of Emacs.
Nonetheless, my experience with it turned out to be **less enjoyable** later. In
fact, before I started the [MinEmacs](https://github.com/abougouffa/minemacs)
project back in September 2022, I encountered numerous issues with Doom Emacs.
Occasionally, after running the `doom upgrade` command, everything would cease
to function properly. These problems always seemed to arise during my busiest
days, causing unnecessary additional stress. To be honest, I think at that time,
big parts of Doom Emacs where being rewritten, which cased these annoying
breakages then, but I think it should be more stable now. However, Doom Emacs
started to feel overly complex as a configuration framework. It incorporated a
command line interface, an extensive library with extra features, numerous
unnecessary hacks to tweak Emacs behavior for a negligible improvement in
startup time, configuration modules that tightly combined various packages in an
opinionated manner, CI commands, and even a profile manager! Each of these
features introduced extra complexity and more failure points at every layer.

As a result, MinEmacs emerged as my personal configuration framework for Emacs,
_and it continues to serve that purpose_. I'm trying to tailor it to my specific
needs while maintaining its modularity and portability. MinEmacs is changing
constantly, you can refer to the [change log](/docs/CHANGELOG.md) for more
information about the evolution of MinEmacs.

MinEmacs was mainly based on Evil and General (for the `SPC` leader), even
though Evil still supported via the `me-evil` module, I'm moving away recently
from Evil to embrace the classic Emacs experience. Vim is awesome, and Evil does
a great job in emulating Vim functionalities in Emacs. However, using Emacs via
Evil hides a lot of Emacs beauty and alters the classical Emacs experience, and
to be honest, Evil is quite slow and sometimes messy, and doesn't integrate well
with other packages. For these reasons and other, I started embracing Emacs as
it is, trying to make use of its features as they are intended to be used.
