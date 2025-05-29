# dotfiles [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

> A selected subset of the dotfiles I use on a daily basis.

## Motivation

To synchronize dotfiles on GitHub does not requires special reasons.
Nonetheless, I like to think someone may find useful to go through mines, especially the ones for Bash and Emacs.
I have built these files --- especially the big ones --- over time while working on different machines, different contexts (home, academia, enterprise/industry), and different Linux distributions.
So there's a chance that you'll find there that particular alias, function or generic snippet that also suits your needs.
> **NOTE**: An expert eye will immediately see that `.bashrc` and `.emacs` are two huge monsters.
> Despite the monolithic approach is not usually regarded as the best one (and it certainly goes against the fundamental Unix philosophy), I prefer to carry with me only a few files, and I don't particularly care about subdividing stuff between `.bashrc` and `.profile` or fragmenting the Emacs configuration in a number of little files to be put under `$HOME/.emacs.d`.
> If in the future the aforementioned files will grow beyond manageable size, then and only then I will consider a segregated approach: for now, I just don't need it.

## Installation

At the present time there is a little script (`linux_config_bootstrap.sh`) for ``installing'' the files in the current user's `$HOME` (in GNU/Linux).
Nevertheless, I made that script simply for copying the files on some remote machines that made difficult to adopt a more sophisticated strategy (don't ask).
A proper installer which can also deal with backup of the previous configuration and manage some sort of snapshot mechanism could be eventually implemented, but it's not a priority as of today.
